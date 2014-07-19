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

unit u_AvailPicsDD;

interface

uses
  SysUtils,
  Classes,
  XMLIntf,
  XMLDoc,
  strutils,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadRequest,
  i_MapSvcScanStorage,
  u_AvailPicsAbstract;

type
  TAvailPicsDD = class(TAvailPicsByKey)
  Private
   FLayerKey: string;
   FResultFactory: IDownloadResultFactory;
  public
    procedure AfterConstruction; override;

    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;

    property LayerKey: string read FLayerKey write FLayerKey;

  end;
  TAvailPicsDataDoorsID = (dd1=1, dd2=2, dd3=3, dd4=4, dd5=5);
  TAvailPicsDataDoors = array [TAvailPicsDataDoorsID] of TAvailPicsDD;

procedure GenerateAvailPicsDD(
  var ADDs: TAvailPicsDataDoors;
  const AResultFactory: IDownloadResultFactory;
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AMapSvcScanStorage: IMapSvcScanStorage
);

implementation

uses
  forms,
  windows,
  i_BinaryData,
  i_Downloader,
  i_NotifierOperation,
  u_GeoToStrFunc,
  u_DownloaderHttp,
  u_BinaryData,
  u_Notifier,
  u_DownloadRequest,
  u_NotifierOperation,
  u_TileRequestBuilderHelpers;

procedure GenerateAvailPicsDD(
  var ADDs: TAvailPicsDataDoors;
  const AResultFactory: IDownloadResultFactory;
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AMapSvcScanStorage: IMapSvcScanStorage
);
var
  j: TAvailPicsDataDoorsID;
begin
  Assert(AResultFactory<>nil);
  for j := Low(TAvailPicsDataDoorsID) to High(TAvailPicsDataDoorsID) do begin
    if (nil=ADDs[j]) then begin
      ADDs[j] := TAvailPicsDD.Create(ATileInfoPtr, AMapSvcScanStorage);
      ADDs[j].FResultFactory := AResultFactory;
      case Ord(j) of
      1: begin
        ADDs[j].FBaseStorageName := 'DD_WV1';
        ADDs[j].LayerKey :='c4453cc2-6e13-4a39-91ce-972e567a15d8'; // WorldView-1
      end;
      2: begin
        ADDs[j].FBaseStorageName := 'DD_WV2';
        ADDs[j].LayerKey :='2f864ade-2820-4ddd-9a51-b1d2f4b66e18'; // WorldView-2
      end;
      3: begin
        ADDs[j].FBaseStorageName := 'DD_QB';
        ADDs[j].LayerKey :='1798eda6-9987-407e-8373-eb324d5b31fd'; // QuickBird
      end;
      4: begin
        ADDs[j].FBaseStorageName := 'DD_GE';
        ADDs[j].LayerKey :='cb547543-5619-464d-a0ee-4ff5ff2e7dab'; // GeoEye
      end;
      5: begin
        ADDs[j].FBaseStorageName := 'DD_IK';
        ADDs[j].LayerKey :='f8ff73f4-7632-4dda-b276-5dca821a8281'; // Ikonos
      end;
      else
        ADDs[j].LayerKey:='';
      end;
    end;
  end;
end;

{ TAvailPicsDD }

procedure TAvailPicsDD.AfterConstruction;
begin
  inherited;
  FDefaultKey := '';
end;

function TAvailPicsDD.ContentType: String;
begin
  Result := 'text/xml';
end;

function TAvailPicsDD.ParseResponse(const AResultOk: IDownloadResultOk): Integer;
var
  XMLDocument: TXMLDocument;
  Node, SubNode: IXMLNode;
  PlacemarkNode: IXMLNode;
  VDate, VcatalogID: String;
  Vsource, V_uid: String;
  VposList: String;
  VAddResult: Boolean;
  i, j: integer;
  VParams: TStrings;
  VMemoryStream: TMemoryStream;

  function CatalogID_to_GeoFuseGeoEyeID(const ASrcId: String): String;
  var p: Integer;
  begin
    p := System.Pos('_', ASrcId);
    if (p>0) then begin
      // geoeye
      Result := System.Copy(ASrcId, 1, (p - 1));
      // dup + tail
      Result := Result + Result + System.Copy(ASrcId, p, Length(ASrcId));
    end else begin
      // TODO: ikonos
      Result := ASrcId;
    end;
  end;

var
  VItemExists: Boolean;
  VItemFetched: TDateTime;
begin
  VMemoryStream := TMemoryStream.Create;
  VMemoryStream.Position := 0;
  VMemoryStream.SetSize(AResultOk.Data.Size);
  CopyMemory(VMemoryStream.Memory, AResultOk.Data.Buffer, AResultOk.Data.Size);
  Result := 0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  if (nil=VMemoryStream) or (0=VMemoryStream.Size) then
    Exit;

  XMLDocument := TXMLDocument.Create(Application);
  XMLDocument.LoadFromStream(VMemoryStream);
  Node := XMLDocument.DocumentElement;
  Node := Node.ChildNodes[0];
  Node := Node.ChildNodes[0];
  Node := Node.ChildNodes[0];
    if (Node <> nil) and (Node.ChildNodes.Count > 0) then begin
      for i := 0 to Node.ChildNodes.Count - 1 do begin
        PlacemarkNode := Node.ChildNodes[i];
        if PlacemarkNode.NodeName = 'Product' then begin
        Vsource := PlacemarkNode.GetAttribute('name');
        V_uid := PlacemarkNode.GetAttribute('uid');

        PlacemarkNode := PlacemarkNode.ChildNodes.FindNode('Footprints');
        for j := 0 to PlacemarkNode.ChildNodes.Count - 1 do  begin
          SubNode := PlacemarkNode.ChildNodes[j];
          if subNode.nodename='Footprint' then begin
          try
            VParams := nil;
            VParams := TStringList.Create;
            VDate := copy(SubNode.GetAttribute('acq_date'), 1, 10);
            VDate[5] := DateSeparator;
            VDate[8] := DateSeparator;

            VposList := SubNode.GetAttribute('uid');
            VParams.Values['uid'] := VposList;

            VItemExists := ItemExists(FBaseStorageName, VposList, @VItemFetched);

            VposList := SubNode.GetAttribute('acq_date');
            VParams.Values['acq_date'] := VposList;

            VcatalogID := SubNode.GetAttribute('name');
            VParams.Values['CatalogID'] := VcatalogID;

            VposList := SubNode.GetAttribute('incidence_angle');
            VParams.Values['incidence_angle'] := VposList;

            VposList := SubNode.GetAttribute('cloud_cover');
            VParams.Values['cloud_cover'] := VposList;

            VParams.Values['Provider'] := 'www.datadoors.net';

            SubNode := SubNode.ChildNodes.FindNode('Geometry');
            VposList := SubNode.text;
            VposList := ReplaceStr(VposList, ',',' ');
            VposList := ReplaceStr(VposList, '(','');
            VposList := ReplaceStr(VposList, ')','');
            VposList := ReplaceStr(VposList, 'MULTIPOLYGON','');
            VParams.Values['Geometry'] := VposList;
            VParams.Values['Source'] := Vsource;
            VParams.Values['Source:uid'] := V_uid;

            if length(VcatalogID)<>0 then
            if (FLayerKey ='c4453cc2-6e13-4a39-91ce-972e567a15d8') or
               (FLayerKey ='2f864ade-2820-4ddd-9a51-b1d2f4b66e18') or
               (FLayerKey ='1798eda6-9987-407e-8373-eb324d5b31fd') then begin
              // add preview and metadata
              VParams.Values['IMAGE_FILE_URL'] := 'https://browse.digitalglobe.com/imagefinder/showBrowseImage?catalogId=' + VcatalogID + '&imageHeight=1024&imageWidth=1024';
              VParams.Values['METADATA_URL'] := 'https://browse.digitalglobe.com/imagefinder/showBrowseMetadata?buffer=1.0&catalogId=' + VcatalogID + '&imageHeight=natres&imageWidth=natres';
            end else begin
              VParams.Values['IMAGE_FILE_URL'] := 'http://search.kosmosnimki.ru/QuickLookImage.ashx?id=' + VcatalogID;
              VParams.Values['METADATA_URL'] := 'http://search.kosmosnimki.ru/QuickLookInfo.aspx?id=' + VcatalogID;
              // link to full geoeye metadata
              //if (FLayerKey = 'cb547543-5619-464d-a0ee-4ff5ff2e7dab') then
                //VParams.Values['FULL_METADATA_URL'] := 'http://geofuse.geoeye.com/landing/image-details/Default.aspx?id='+CatalogID_to_GeoFuseGeoEyeID(VcatalogID);
            end;


            VposList := ReplaceStr(Vsource, 'DigitalGlobe ', '');
            VposList := ReplaceStr(VposList, 'GeoEye ', '');
            VposList := 'DD:' + VposList;

            VAddResult := FTileInfoPtr.AddImageProc(
              Self,
              VDate,
              VposList,
              VItemExists,
              VItemFetched,
              VParams
            );
            FreeAndNil(VParams);
            if VAddResult then begin
              Inc(Result);
            end;
           except
            if (nil<>VParams) then begin
              try
                VParams.Free;
              except
              end;
              VParams := nil;
            end;
          end;
      end;
     end;
    end;
   end;
  end;
 end;

function TAvailPicsDD.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var
  VPostData: IBinaryData;
  VPostdataStr: string;
  VHttpData: string;
  VDownloader: IDownloader; // TDownloaderHttp;
  VPostRequest: IDownloadPostRequest; // POST
  VHeader: string;
  VLink: string;
  VStrPostData: AnsiString;
  VResultOk: IDownloadResultOk;
  VResult: IDownloadResult;

  VCancelNotifier: INotifierOperation;
  VResultWithRespond: IDownloadResultWithServerRespond;
  V_user_guest_uid: string;
//  V_streaming_uid: string;
  V_UserTokenUid: string;

begin
  VLink := 'http://www.datadoors.net/webservices/datadoors26.asmx';
  VHeader :='User-Agent: Opera/9.80 (Windows NT 6.1; U; ru) Presto/2.10.289 Version/12.01'+#$D#$A+
    'Host: www.datadoors.net'+#$D#$A+
    'Accept: text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/webp, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1'+#$D#$A+
    'Accept-Language: ru-RU,ru;q=0.9,en;q=0.8'+#$D#$A+
    'Referer: http://www.datadoors.net/DataDoorsWeb/I3FlexClient/I3FlexClient.swf?rev=18'+#$D#$A+
    'Connection: Keep-Alive'+#$D#$A+
    'Content-type: text/xml; charset=utf-8'+#$D#$A+
    'SOAPAction: "http://www.datadoors.net/services/2.6/ApplicationParameters"';
  VStrPostData :=
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:s="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+#$D#$A+
    '  <SOAP-ENV:Body>'+#$D#$A+
    '    <ApplicationParameters xmlns="http://www.datadoors.net/services/2.6/">'+#$D#$A+
    '      <Application>DDWC</Application>'+#$D#$A+
    '    </ApplicationParameters>'+#$D#$A+
    '  </SOAP-ENV:Body>'+#$D#$A+
    '</SOAP-ENV:Envelope>';
  VPostData := TBinaryData.CreateByAnsiString(VPostdataStr);
  VPostRequest := TDownloadPostRequest.Create(
                   Vlink,
                   VHeader,
                   VPostData,
                   AInetConfig.GetStatic
                  );
  VDownloader := TDownloaderHttp.Create(FResultFactory);
  VCancelNotifier := TNotifierOperationFake.Create;
  VResult := VDownloader.DoRequest(
              VPostRequest,
              VCancelNotifier,
              VCancelNotifier.CurrentOperation
             );
  if Supports(VResult, IDownloadResultWithServerRespond, VResultWithRespond) then
   if Supports(VResult, IDownloadResultOk, VResultOk) then begin
     SetLength(VHttpData, VResultOk.Data.Size);
     Move(VResultOk.Data.Buffer^, VHttpData[1], VResultOk.Data.Size);
    end;
  V_user_guest_uid := GetBetween(VHttpData, 'key="user_guest_uid" value="', '"'); // 5794ce45-fd31-4591-b28c-0ace80b8db8b
//  V_streaming_uid  := GetBetween(VHttpData ,'key="streaming_uid" value="', '"'); // 3d834b43-99b4-4302-9e47-2438d096458f

  VStrPostData :=
    '<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:s="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+#$D#$A+
    '  <SOAP-ENV:Body>'+#$D#$A+
    '    <AuthenticateGuest xmlns="http://www.datadoors.net/services/2.6/">'+#$D#$A+
    '      <guestUid>'+ V_user_guest_uid + '</guestUid>' + #$D#$A+
    '      <application>DDWC</application>'+#$D#$A+
    '    </AuthenticateGuest>'+#$D#$A+
    '  </SOAP-ENV:Body>'+#$D#$A+
    '</SOAP-ENV:Envelope>';
  VPostData := TBinaryData.CreateByAnsiString(VPostdataStr);
  VHeader :='User-Agent: Opera/9.80 (Windows NT 6.1; U; ru) Presto/2.10.289 Version/12.01'+#$D#$A+
    'Host: www.datadoors.net'+#$D#$A+
    'Accept: text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/webp, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1'+#$D#$A+
    'Accept-Language: ru-RU,ru;q=0.9,en;q=0.8'+#$D#$A+
    'Referer: http://www.datadoors.net/DataDoorsWeb/I3FlexClient/I3FlexClient.swf?rev=18'+#$D#$A+
    'Connection: Keep-Alive'+#$D#$A+
    'Content-type: text/xml; charset=utf-8'+#$D#$A+
    'SOAPAction: "http://www.datadoors.net/services/2.6/AuthenticateGuest"';

  VLink := 'https://www.datadoors.net/webservices/auth26.asmx';
  VPostRequest := TDownloadPostRequest.Create(
                    Vlink,
                    VHeader,
                    VPostData,
                    AInetConfig.GetStatic
                  );
  VResult := VDownloader.DoRequest(
              VPostRequest,
              VCancelNotifier,
              VCancelNotifier.CurrentOperation
            );
  if Supports(VResult, IDownloadResultWithServerRespond, VResultWithRespond) then
   if Supports(VResult, IDownloadResultOk, VResultOk) then begin
     SetLength(VHttpData, VResultOk.Data.Size);
     Move(VResultOk.Data.Buffer^, VHttpData[1], VResultOk.Data.Size);
    end;
  V_UserTokenUid := GetBetween(VHttpData, '<ResponseValue>', '</ResponseValue>');

  if length(V_UserTokenUid)=36 then
  VPostDataStr :='<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:s="http://www.w3.org/2001/XMLSchema" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">'+#$D#$A+
    '  <SOAP-ENV:Header>'+#$D#$A+
    '    <tns:DdSoapHeader MyAttribute="" xmlns="http://www.datadoors.net/services/2.6/" xmlns:tns="http://www.datadoors.net/services/2.6/">'+#$D#$A+
    '      <UserTokenUid>'+V_UserTokenUid + '</UserTokenUid>' + #$D#$A+
    '      <ApplicationName>DDWC</ApplicationName>'+#$D#$A+
    '      <CultureName>en-US</CultureName>'+#$D#$A+
    '      <CurrencyCode>USD</CurrencyCode>'+#$D#$A+
    '      <IsAuthenticated>false</IsAuthenticated>'+#$D#$A+
    '    </tns:DdSoapHeader>'+#$D#$A+
    '  </SOAP-ENV:Header>'+#$D#$A+
    '  <SOAP-ENV:Body>'+#$D#$A+
    '    <GetProductFootprintsByCriteria xmlns="http://www.datadoors.net/services/2.6/">'+#$D#$A+
    '      <Criteria>'+#$D#$A+
    '        <UserUID>'+V_user_guest_uid + '</UserUID>' + #$D#$A+
    '        <ProductUID>'+LayerKey + '</ProductUID>' + #$D#$A+
    '        <AOI>MULTIPOLYGON((('+#$D#$A+
    RoundEx(FTileInfoPtr.TileRect.Left, 8) + ' '+RoundEx(FTileInfoPtr.TileRect.Top, 8) + ','+
    RoundEx(FTileInfoPtr.TileRect.Left, 8) + ' '+RoundEx(FTileInfoPtr.TileRect.Bottom, 8) + ','+
    RoundEx(FTileInfoPtr.TileRect.Right, 8) + ' '+RoundEx(FTileInfoPtr.TileRect.Top, 8) + ','+
    RoundEx(FTileInfoPtr.TileRect.Right, 8) + ' '+RoundEx(FTileInfoPtr.TileRect.Bottom, 8) + ','+
    RoundEx(FTileInfoPtr.TileRect.Left, 8) + ' '+RoundEx(FTileInfoPtr.TileRect.Top, 8)+
    ')))</AOI>' + #$D#$A+
    '        <MetadataCriteria CloudCover="5" UnusableData="-1" IncidenceAngle="-1" SunAngle="-1" SnowCover="-1" Quality="-1" Accuracy="-1" RelevantLicensing="false"/>'+#$D#$A+
    '      </Criteria>'+#$D#$A+
    '    </GetProductFootprintsByCriteria>'+#$D#$A+
    '  </SOAP-ENV:Body>'+#$D#$A+
    '</SOAP-ENV:Envelope>'
  else VPostDataStr := '';

 VPostData := TBinaryData.CreateByAnsiString(VPostdataStr);

 VHeader := 'Host: www.datadoors.net'+#$D#$A+
  'Accept: text/html, application/xml;q=0.9, application/xhtml+xml, image/png, image/webp, image/jpeg, image/gif, image/x-xbitmap, */*;q=0.1'+#$D#$A+
  'Accept-Language: ru-RU,ru;q=0.9,en;q=0.8'+#$D#$A+
  'Referer: http://www.datadoors.net/DataDoorsWeb/I3FlexClient/I3FlexClient.swf?rev=18'+#$D#$A+
  'Connection: Keep-Alive'+#$D#$A+
  'Content-type: text/xml; charset=utf-8'+#$D#$A+
  'SOAPAction: "http://www.datadoors.net/services/2.6/GetProductFootprintsByCriteria"';

     Result := TDownloadPostRequest.Create(
              'http://www.datadoors.net/webservices/datadoors26.asmx',
              VHeader,
              VPostData,
              AInetConfig.GetStatic
               );
end;


end.
