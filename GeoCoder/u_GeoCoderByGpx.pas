{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_GeoCoderByGpx;

interface

uses
  Classes,
  sysutils,
  i_GeoCoder,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_VectorItemSubsetBuilder,
  i_ValueToStringConverter,
  u_GeoCoderLocalBasic;

type
  EGeoCoderERR = class(Exception);
  EDirNotExist = class(EGeoCoderERR);

  TGeoCoderByGpx = class(TGeoCoderLocalBasic)
  private
    FValueToStringConverterConfig: IValueToStringConverterConfig;
    procedure SearchInGpxFile(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AFile : string ;
      const ASearch : widestring;
      const AList : IInterfaceListSimple;
      const AValueConverter: IValueToStringConverter
    );
  protected
    function DoSearch(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
    constructor Create(
      const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
      const APlacemarkFactory: IGeoCodePlacemarkFactory;
      const AValueToStringConverterConfig: IValueToStringConverterConfig
      );
  end;

implementation

uses
  StrUtils,
  XMLIntf,
  XMLDoc,
  t_GeoTypes,
  i_VectorDataItemSimple,
  u_InterfaceListSimple;

{ TGeoCoderByGpx }
constructor TGeoCoderByGpx.Create(
  const AVectorItemSubsetBuilderFactory: IVectorItemSubsetBuilderFactory;
  const APlacemarkFactory: IGeoCodePlacemarkFactory;
  const AValueToStringConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AVectorItemSubsetBuilderFactory, APlacemarkFactory);
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\gpx')) then
    raise EDirNotExist.Create('not found .\userdata\gpx\! skip GeoCoderByGpx');
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function ItemExist(
  const AValue: IVectorDataItemPoint;
  const AList: IInterfaceListSimple
):boolean;
var
  i, j: Integer;
  VPlacemark: IVectorDataItemPoint;
  str1,str2 : string;
begin
  Result := false;
  for i := 0 to AList.Count - 1 do begin
    VPlacemark := IVectorDataItemPoint(AList.Items[i]);
    j:= posex(')', VPlacemark.Name);
    str1 := copy(VPlacemark.Name,j,length(VPlacemark.Name)-(j+1));
    j:= posex(')', AValue.Name);
    str2 := copy(AValue.Name, j, length(AValue.Name) - (j+1));
    if str1 = str2 then begin
      if
        abs(VPlacemark.GetPoint.Point.x-AValue.GetPoint.Point.x) +
        abs(VPlacemark.GetPoint.Point.Y-AValue.GetPoint.Point.Y) < 0.05
      then begin
        Result := true;
        Break;
      end;
    end;
  end;
end;

procedure TGeoCoderByGpx.SearchInGpxFile(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AFile: string ;
  const ASearch: widestring;
  const AList: IInterfaceListSimple;
  const AValueConverter: IValueToStringConverter
);
var
  VNode: IXMLNode;
  VPlacemarkNode: IXMLNode;
  VPoint: TDoublePoint;
  VAddress: string;
  VDesc: string;
  VFullDesc: string;
  VPlace: IVectorDataItemPoint;
  VFormatSettings: TFormatSettings;
  VXMLDocument: IXMLDocument;
  i, j : integer;
  VSearch : AnsiString;
  Vskip: boolean;
begin
  VFormatSettings.DecimalSeparator := '.';
  VSearch := AnsiString(AnsiUpperCase(ASearch));
  VXMLDocument := TXMLDocument.Create(nil);
  VXMLDocument.LoadFromFile(AFile);
  VNode := VXMLDocument.DocumentElement;
  try
    if (VNode <> nil) and (VNode.ChildNodes.Count > 0) then begin
      for i := 0 to VNode.ChildNodes.Count - 1 do begin
        if VNode.ChildNodes[i].NodeName = 'wpt' then begin
          VPlacemarkNode := VNode.ChildNodes[i];
          for j := 0 to VPlacemarkNode.GetAttributeNodes.getcount - 1 do begin
            if VPlacemarkNode.GetAttributeNodes.get(j).GetNodeName = 'lon' then
              VPoint.X := StrToFloat(VPlacemarkNode.GetAttributeNodes.get(j).gettext, VFormatSettings);
            if VPlacemarkNode.GetAttributeNodes.get(j).GetNodeName = 'lat' then
              VPoint.Y := StrToFloat(VPlacemarkNode.GetAttributeNodes.get(j).gettext, VFormatSettings);
          end;
          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then
            VDesc := VDesc + #$D#$A +'Elevation '+VPlacemarkNode.ChildNodes.FindNode('ele').Text;
          VDesc := VDesc + #$D#$A +'[ '+AValueConverter.LonLatConvert(VPoint)+' ]';
          VFullDesc := VAddress + '<br>' + VDesc + '<br><b>'+ AFile +'</b>';

          Vskip := True;
          if Pos(VSearch, AnsiUpperCase(VAddress)) <> 0 then begin
            Vskip := False
          end else if Pos(VSearch, AnsiUpperCase(VDesc)) <> 0 then begin
            Vskip := False
          end;
          if not Vskip then begin
            VPlace := PlacemarkFactory.Build(VPoint, VAddress, VDesc, VFullDesc, 4);
            Vskip := ItemExist(Vplace, AList);
            if not Vskip then begin
              AList.Add(VPlace);
            end;
          end;
        end;
      end;
    end;
  except
  end;
end;

function TGeoCoderByGpx.DoSearch(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  VList: IInterfaceListSimple;
  vpath: string;
  VFolder: string;
  VSearchRec: TSearchRec;
  VMySearch: string;
  VValueConverter: IValueToStringConverter;
begin
  VMySearch := ASearch;
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  while PosEx('  ', VMySearch) > 0 do VMySearch := ReplaceStr(VMySearch, '  ', ' ');
  VList := TInterfaceListSimple.Create;
  VFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))+'userdata\gpx\');
  if FindFirst(VFolder + '*.gpx', faAnyFile, VSearchRec) = 0 then begin
    repeat
      if (VSearchRec.Attr and faDirectory) = faDirectory then begin
        continue;
      end;
      vpath := VFolder + VSearchRec.Name;
      SearchInGpxFile(ACancelNotifier, AOperationID, Vpath, VMySearch, vlist, VValueConverter);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    until FindNext(VSearchRec) <> 0;
  end;
  Result := VList;
end;
end.
