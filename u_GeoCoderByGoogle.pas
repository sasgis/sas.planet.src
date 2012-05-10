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
  forms,
  u_GeoTostr,
  XMLIntf,
  XMLDoc,
  i_CoordConverter,
  u_GeoCoderBasic;

type
  TGeoCoderByGoogle = class(TGeoCoderBasic)
  protected
    function PrepareURL(const ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(
      const AStr: string;
      const ASearch: WideString
    ): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderByGoogle }

function TGeoCoderByGoogle.ParseStringToPlacemarksList(
  const AStr: string;
  const ASearch: WideString
): IInterfaceList;
var
  Stream: TMemoryStream;
  Node: IXMLNode;
  PlacemarkNode, PointNode, AddressNode: IXMLNode;
  i: Integer;
  StringList: TStringList;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
  XMLDocument: TXMLDocument;
  VPointStr: string;
begin
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  Stream := TMemoryStream.Create;
  StringList := TStringList.Create;
  XMLDocument := TXMLDocument.Create(application);
  try
    Stream.Write(AStr[1], length(AStr));
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

function TGeoCoderByGoogle.PrepareURL(const ASearch: WideString): string;
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
  VConverter := FLocalConverter.GetGeoConverter;
  VZoom := FLocalConverter.GetZoom;
  VMapRect := FLocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
  Result := 'http://maps.google.com/maps/geo?q=' +
    URLEncode(AnsiToUtf8(VSearch)) +
    '&output=xml' + SAS_STR_GoogleSearchLanguage +
    '&key=ABQIAAAA5M1y8mUyWUMmpR1jcFhV0xSHfE-V63071eGbpDusLfXwkeh_OhT9fZIDm0qOTP0Zey_W5qEchxtoeA' +
    '&ll=' + R2StrPoint(FLocalConverter.GetCenterLonLat.x) + ',' + R2StrPoint(FLocalConverter.GetCenterLonLat.y) +
    '&spn=' + R2StrPoint(VLonLatRect.Right - VLonLatRect.Left) + ',' + R2StrPoint(VLonLatRect.Top - VLonLatRect.Bottom);
end;

end.
