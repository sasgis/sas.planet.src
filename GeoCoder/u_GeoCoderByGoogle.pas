{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_GeoCoderByGoogle;

interface

uses
  Classes,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_DownloadRequest,
  i_DownloadResult,
  u_GeoCoderBasic;

type
  TGeoCoderByGoogle = class(TGeoCoderBasic)
  protected
    function PrepareRequest(
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IDownloadRequest; override;
    function ParseResultToPlacemarksList(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AResult: IDownloadResultOk;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IInterfaceListSimple; override;
  public
  end;

implementation

uses
  SysUtils,
  forms,
  XMLIntf,
  XMLDoc,
  t_GeoTypes,
  i_GeoCoder,
  i_CoordConverter,
  u_InterfaceListSimple,
  u_ResStrings,
  u_GeoTostr,
  u_GeoCodePlacemark;

{ TGeoCoderByGoogle }

function TGeoCoderByGoogle.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  Stream: TMemoryStream;
  Node: IXMLNode;
  PlacemarkNode, PointNode, AddressNode: IXMLNode;
  i: Integer;
  StringList: TStringList;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceListSimple;
  VFormatSettings: TFormatSettings;
  XMLDocument: TXMLDocument;
  VPointStr: string;
begin
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;
  Stream := TMemoryStream.Create;
  StringList := TStringList.Create;
  XMLDocument := TXMLDocument.Create(application);
  try
    Stream.Write(AResult.Data.Buffer^, AResult.Data.Size);
    XMLDocument.LoadFromStream(Stream);
    Node := XMLDocument.DocumentElement;
    Node := Node.ChildNodes.FindNode('Response');
    if (Node <> nil) and (Node.ChildNodes.Count > 0) then begin
      for i := 0 to Node.ChildNodes.Count - 1 do begin
        if Node.ChildNodes[i].NodeName = 'Placemark' then begin
          PlacemarkNode := Node.ChildNodes[i];
          AddressNode := PlacemarkNode.ChildNodes.FindNode('address');
          PointNode := PlacemarkNode.ChildNodes.FindNode('Point');
          PointNode := PointNode.ChildNodes.FindNode('coordinates');
          if (AddressNode <> nil) and (PointNode <> nil) then begin
            VPointStr := PointNode.Text;
            ExtractStrings([','], [], PChar(VPointStr), StringList);
            try
              VPoint.X := StrToFloat(StringList[0], VFormatSettings);
              VPoint.Y := StrToFloat(StringList[1], VFormatSettings);
            except
              raise EParserError.CreateFmt(SAS_ERR_CoordParseError, [StringList[1], StringList[0]]);
            end;
            VPlace := TGeoCodePlacemark.Create(VPoint, AddressNode.Text, '', '', 4);
            VList.Add(VPlace);
            StringList.Clear;
          end;
        end;
      end;
    end;
    Result := VList;
  finally
    XMLDocument.Free;
    StringList.free;
    Stream.Free;
  end;
end;

function TGeoCoderByGoogle.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  i: integer;
begin
  VSearch := ASearch;
  for i := 1 to length(VSearch) do begin
    if VSearch[i] = ' ' then begin
      VSearch[i] := '+';
    end;
  end;
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  Result :=
    PrepareRequestByURL(
      'http://maps.google.com/maps/geo?q=' +
      URLEncode(AnsiToUtf8(VSearch)) +
      '&output=xml' + SAS_STR_GoogleSearchLanguage +
      '&key=ABQIAAAA5M1y8mUyWUMmpR1jcFhV0xSHfE-V63071eGbpDusLfXwkeh_OhT9fZIDm0qOTP0Zey_W5qEchxtoeA' +
      '&ll=' + R2StrPoint(ALocalConverter.GetCenterLonLat.x) + ',' + R2StrPoint(ALocalConverter.GetCenterLonLat.y) +
      '&spn=' + R2StrPoint(VLonLatRect.Right - VLonLatRect.Left) + ',' + R2StrPoint(VLonLatRect.Top - VLonLatRect.Bottom)
    );
end;

end.
