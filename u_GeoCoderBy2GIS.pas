{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_GeoCoderBy2GIS;

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
  TGeoCoderBy2GIS = class(TGeoCoderBasic)
  protected
    function PrepareURL(const ASearch: WideString): string; override;
    function ParseStringToPlacemarksList(const AStr: string; const ASearch: WideString): IInterfaceList; override;
  public
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_GeoCoder,
  u_ResStrings,
  u_GeoCodePlacemark;

{ TGeoCoderBy2GIS }

function TGeoCoderBy2GIS.ParseStringToPlacemarksList(
  const AStr: string; const ASearch: WideString): IInterfaceList;
var
  Stream:TMemoryStream;
  Node:IXMLNode;
  PlacemarkNode, AddressNode:IXMLNode;
  i:Integer;
  VPoint: TDoublePoint;
  VDesc: string;
  VFullDesc: string;
  VPlace: IGeoCodePlacemark;
  VList: IInterfaceList;
  VFormatSettings: TFormatSettings;
  XMLDocument:TXMLDocument;
begin
  if AStr = '' then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceList.Create;
  Stream:=TMemoryStream.Create;
  XMLDocument:=TXMLDocument.Create(application);
  try
    Stream.Write(AStr[1],length(AStr));
    XMLDocument.LoadFromStream(Stream);
    Node:=XMLDocument.DocumentElement;
    Node:=Node.ChildNodes.FindNode('result');
    if (Node<>nil) and (Node.ChildNodes.Count>0) then begin
      for i:=0 to Node.ChildNodes.Count-1 do begin
        if Node.ChildNodes[i].NodeName='filial' then begin
          try
            PlacemarkNode:=Node.ChildNodes[i];
            AddressNode:=PlacemarkNode.ChildNodes.FindNode('name');
            VPoint.X:=StrToFloat(PlacemarkNode.ChildNodes.FindNode('lon').Text, VFormatSettings);
            VPoint.Y:=StrToFloat(PlacemarkNode.ChildNodes.FindNode('lat').Text, VFormatSettings);
            VDesc:=PlacemarkNode.ChildNodes.FindNode('city_name').Text+', '+
                   PlacemarkNode.ChildNodes.FindNode('address').text;
            VFullDesc:='http://sasgis.ru/stat/2GIS/2gis.php?id='+PlacemarkNode.ChildNodes.FindNode('id').Text+
                       '&hash='+PlacemarkNode.ChildNodes.FindNode('hash').Text;
            if (AddressNode<>nil) then begin
              VPlace := TGeoCodePlacemark.Create(VPoint, AddressNode.Text, VDesc, VFullDesc, 4);
              VList.Add(VPlace);
            end;
          except

          end;
        end;
      end;
    end;
    Result := VList;
  finally
    XMLDocument.Free;
    Stream.Free;
  end;
end;

function TGeoCoderBy2GIS.PrepareURL(const ASearch: WideString): string;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VRadius: integer;
begin
  VSearch := ASearch;
  VConverter:=FLocalConverter.GetGeoConverter;
  VZoom := FLocalConverter.GetZoom;
  VMapRect := FLocalConverter.GetRectInMapPixelFloat;
  VConverter.CheckPixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  VRadius:=round(FLocalConverter.GetGeoConverter.Datum.CalcDist(VLonLatRect.TopLeft,VLonLatRect.BottomRight));
  if VRadius>40000 then begin
    VRadius:=40000;
  end;
  //point='+R2StrPoint(FCurrentPos.x)+','+R2StrPoint(FCurrentPos.y)+'&radius=40000&where=новосибирск'
  Result := 'http://catalog.api.2gis.ru/search?what='+URLEncode(AnsiToUtf8(VSearch))+
            '&point='+R2StrPoint(FLocalConverter.GetCenterLonLat.x)+','+R2StrPoint(FLocalConverter.GetCenterLonLat.y)+
            '&radius='+inttostr(VRadius)+
            '&page=1&pagesize=50&key=ruihvk0699&version=1.3&sort=relevance&output=xml';
end;

end.
