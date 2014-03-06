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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_GeoCoderByGpx;

interface

uses
  SysUtils,
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
      const AFile: String;
      const ASearch: WideString;
      const AList: IInterfaceListSimple;
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
  if not DirectoryExists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'userdata\gpx')) then
    raise EDirNotExist.Create('not found .\userdata\gpx\! skip GeoCoderByGpx');
  FValueToStringConverterConfig := AValueToStringConverterConfig;
end;

function ItemExist(
  const AValue: IVectorDataItemPoint;
  const AList: IInterfaceListSimple
): Boolean;
var
  I, J: Integer;
  VPlacemark: IVectorDataItemPoint;
  VStr1, VStr2: String;
begin
  Result := false;
  for I := 0 to AList.Count - 1 do begin
    VPlacemark := IVectorDataItemPoint(AList.Items[I]);
    J:= posex(')', VPlacemark.Name);
    VStr1 := copy(VPlacemark.Name, J, length(VPlacemark.Name) - (J + 1));
    J:= posex(')', AValue.Name);
    VStr2 := copy(AValue.Name, J, length(AValue.Name) - (J + 1));
    if VStr1 = VStr2 then begin
      if
        abs(VPlacemark.GetPoint.Point.x - AValue.GetPoint.Point.x) +
        abs(VPlacemark.GetPoint.Point.Y - AValue.GetPoint.Point.Y) < 0.05
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
  const AFile: String;
  const ASearch: WideString;
  const AList: IInterfaceListSimple;
  const AValueConverter: IValueToStringConverter
);
var
  VNode: IXMLNode;
  VPlacemarkNode: IXMLNode;
  VPoint: TDoublePoint;
  VAddress: String;
  VDesc: String;
  VFullDesc: String;
  VPlace: IVectorDataItemPoint;
  VFormatSettings: TFormatSettings;
  VXMLDocument: IXMLDocument;
  I, J: Integer;
  VSearch: AnsiString;
  Vskip: Boolean;
begin
  VFormatSettings.DecimalSeparator := '.';
  VSearch := AnsiString(AnsiUpperCase(ASearch));
  VXMLDocument := TXMLDocument.Create(nil);
  VXMLDocument.LoadFromFile(AFile);
  VNode := VXMLDocument.DocumentElement;
  try
    if (VNode <> nil) and (VNode.ChildNodes.Count > 0) then begin
      for I := 0 to VNode.ChildNodes.Count - 1 do begin
        if VNode.ChildNodes[I].NodeName = 'wpt' then begin
          VPlacemarkNode := VNode.ChildNodes[I];
          for J := 0 to VPlacemarkNode.GetAttributeNodes.getcount - 1 do begin
            if VPlacemarkNode.GetAttributeNodes.get(J).GetNodeName = 'lon' then
              VPoint.X := StrToFloat(VPlacemarkNode.GetAttributeNodes.get(J).gettext, VFormatSettings);
            if VPlacemarkNode.GetAttributeNodes.get(J).GetNodeName = 'lat' then
              VPoint.Y := StrToFloat(VPlacemarkNode.GetAttributeNodes.get(J).gettext, VFormatSettings);
          end;
          VAddress := VPlacemarkNode.ChildNodes.FindNode('name').Text;
          VDesc := '';
          if VPlacemarkNode.ChildNodes.FindNode('desc') <> nil then
            VDesc := VPlacemarkNode.ChildNodes.FindNode('desc').Text;
          if VPlacemarkNode.ChildNodes.FindNode('ele') <> nil then
            VDesc := VDesc + #$D#$A + 'Elevation ' + VPlacemarkNode.ChildNodes.FindNode('ele').Text;
          VDesc := VDesc + #$D#$A + '[ ' + AValueConverter.LonLatConvert(VPoint) + ' ]';
          VFullDesc := VAddress + '<br>' + VDesc + '<br><b>' + AFile + '</b>';

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
  Vpath: String;
  VFolder: String;
  VSearchRec: TSearchRec;
  VMySearch: String;
  VValueConverter: IValueToStringConverter;
begin
  VMySearch := ASearch;
  VValueConverter := FValueToStringConverterConfig.GetStatic;
  while PosEx('  ', VMySearch) > 0 do VMySearch := ReplaceStr(VMySearch, '  ', ' ');
  VList := TInterfaceListSimple.Create;
  VFolder := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)) + 'userdata\gpx\');
  if FindFirst(VFolder + '*.gpx', faAnyFile, VSearchRec) = 0 then begin
    repeat
      if (VSearchRec.Attr and faDirectory) = faDirectory then begin
        Continue;
      end;
      Vpath := VFolder + VSearchRec.Name;
      SearchInGpxFile(ACancelNotifier, AOperationID, Vpath, VMySearch, Vlist, VValueConverter);
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Exit;
      end;
    until FindNext(VSearchRec) <> 0;
  end;
  Result := VList;
end;
end.
