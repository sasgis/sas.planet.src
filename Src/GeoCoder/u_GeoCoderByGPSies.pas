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

unit u_GeoCoderByGPSies;

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
  TGeoCoderByGPSies = class(TGeoCoderBasic)
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
  XMLIntf,
  XMLDoc,
  SysUtils,
  t_GeoTypes,
  i_GeoCoder,
  i_VectorDataItemSimple,
  i_CoordConverter,
  u_GeoToStrFunc,
  u_InterfaceListSimple,
  u_ResStrings;

{ TGeoCoderByGPSies }

function TGeoCoderByGPSies.ParseResultToPlacemarksList(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AResult: IDownloadResultOk;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IInterfaceListSimple;
var
  Stream: TMemoryStream;
  Node: IXMLNode;
  PlacemarkNode, AddressNode: IXMLNode;
  i: Integer;
  VPoint: TDoublePoint;
  VDesc: string;
  VFullDesc: string;
  VPlace: IVectorDataItem;
  VList: IInterfaceListSimple;
  VFormatSettings: TFormatSettings;
  XMLDocument: IXMLDocument;

begin
  if AResult.Data.Size <= 0 then begin
    raise EParserError.Create(SAS_ERR_EmptyServerResponse);
  end;
  VFormatSettings.DecimalSeparator := '.';
  VList := TInterfaceListSimple.Create;
  XMLDocument := TXMLDocument.Create(nil);
  Stream := TMemoryStream.Create;
  try
    Stream.Write(AResult.Data.Buffer^, AResult.Data.Size);
    XMLDocument.LoadFromStream(Stream);
    Node := XMLDocument.DocumentElement;
    Node := Node.ChildNodes.FindNode('tracks');
    if (Node <> nil) and (Node.ChildNodes.Count > 0) then begin
      for i := 0 to Node.ChildNodes.Count - 1 do begin
        if Node.ChildNodes[i].NodeName = 'track' then begin
          try
            PlacemarkNode := Node.ChildNodes[i];
            AddressNode := PlacemarkNode.ChildNodes.FindNode('title');
            if (AddressNode <> nil) then begin
              VPoint.X := StrToFloat(PlacemarkNode.ChildNodes.FindNode('startPointLon').Text, VFormatSettings);
              VPoint.Y := StrToFloat(PlacemarkNode.ChildNodes.FindNode('startPointLat').Text, VFormatSettings);
              VDesc := 'Track Length: ' + PlacemarkNode.ChildNodes.FindNode('trackLengthM').Text + #10#13;
              VDesc := VDesc + 'Trackpoints: ' + PlacemarkNode.ChildNodes.FindNode('countTrackpoints').Text + #10#13;
              VFullDesc := '<b>' + AddressNode.Text + '</b><br>';
              if PlacemarkNode.ChildNodes.FindNode('description') <> nil  then begin
                if Length(PlacemarkNode.ChildNodes.FindNode('description').Text)>128 then
                  VDesc := VDesc + Copy(PlacemarkNode.ChildNodes.FindNode('description').Text,1,128) + ' >>>'
                else
                  VDesc := VDesc + PlacemarkNode.ChildNodes.FindNode('description').Text;
                VFullDesc := VFullDesc + PlacemarkNode.ChildNodes.FindNode('description').Text + '<br>';
              end;
              if PlacemarkNode.ChildNodes.FindNode('fileId') <> nil then begin
                VFullDesc := VFullDesc + '<br>View: <a target=blank href=http://www.gpsies.com/map.do?fileId=' + PlacemarkNode.ChildNodes.FindNode('fileId').Text
               + '>http://www.gpsies.com/map.do?fileId=' + PlacemarkNode.ChildNodes.FindNode('fileId').Text + '</a><br>';
              end;
              if PlacemarkNode.ChildNodes.FindNode('downloadLink') <> nil then begin
                VFullDesc := VFullDesc + '<br>Download: <a href=' + PlacemarkNode.ChildNodes.FindNode('downloadLink').Text
                + '>' + PlacemarkNode.ChildNodes.FindNode('downloadLink').Text + '</a><br>';
              end;
              VPlace := PlacemarkFactory.Build(VPoint, AddressNode.Text, VDesc, VFullDesc, 4);
              VList.Add(VPlace);
            end;
          except
          end;
        end;
      end;
    end;
    Result := VList;
  finally
    Stream.Free;
  end;
end;

function TGeoCoderByGPSies.PrepareRequest(
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IDownloadRequest;
var
  VSearch: String;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLonLatRect: TDoubleRect;

begin

  VSearch := ASearch;
  VConverter := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapRect := ALocalConverter.GetRectInMapPixelFloat;
  VConverter.ValidatePixelRectFloat(VMapRect, VZoom);
  VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);

  Result :=
    PrepareRequestByURL(
      'http://www.gpsies.com/api.do?key=yzzgoiguqwvdeeaa&searchText=' + URLEncode(AnsiToUtf8(VSearch)) + '&filetype=kml&limit=20&BBOX=' +
      R2AnsiStrPoint(VLonLatRect.Left) + ',' + R2AnsiStrPoint(VLonLatRect.Bottom) + ',' +
      R2AnsiStrPoint(VLonLatRect.Right) + ',' + R2AnsiStrPoint(VLonLatRect.Top)
    );
end;

end.

