unit u_AvailPicsKosmosnimki;

interface

uses
  SysUtils,
  Classes,
  i_InetConfig,
  i_DownloadResult,
  i_DownloadResultFactory,
  i_DownloadRequest,
  i_MapSvcScanStorage,
  u_AvailPicsAbstract;

type
  TAvailPicsKS = class(TAvailPicsAbstract)
  private
    FKSMode: Byte;
    FMinDate : string;
    FMaxDate : string;
    FMaxCloudCover : Byte;
    FEveryYear : string;
    FMaxOfNadir : Byte;
    FResultFactory: IDownloadResultFactory;
    function GetPlainJsonKosmosnimkiText(
      const AResultOk: IDownloadResultOk;
      const AList: TStrings
    ): Boolean;
    function GetSimpleJsonKosmosnimkiText(
      const AResultOk: IDownloadResultOk;
      const AList: TStrings
    ): Boolean;
    function GetUnzippedJsonKosmosnimkiText(
      const AResultOk: IDownloadResultOk;
      const AList: TStrings
    ): Boolean;
    function PrepareToParseString(const AStrValue: String): String;
    procedure PrepareStringList(const AList: Tstrings);
    function MakePostString: string;
  public
    function ContentType: String; override;

    function ParseResponse(const AResultOk: IDownloadResultOk): Integer; override;

    function GetRequest(const AInetConfig: IInetConfig): IDownloadRequest; override;
  end;

  TAvailPicsKosmosnimkiID = (
    ks_GE1=1,
    ks_WV1=2,
    ks_WV2=3,
    ks_QB2=4,
    ks_ERB=5,
    ks_IK=6,
    ks_ERA=7,
    ks_PLE=8,
    ks_SPT5=9,
    ks_SPT3=10,
    ks_SPT1=11,
    ks_SPT4=12,
    ks_SPT2=13
  );
  TAvailPicsKosmosnimki = array [TAvailPicsKosmosnimkiID] of TAvailPicsKS;

procedure GenerateAvailPicsKS(
  var AKSs: TAvailPicsKosmosnimki;
  const AResultFactory: IDownloadResultFactory;
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AMapSvcScanStorage: IMapSvcScanStorage
);

implementation

uses
  Windows,
  ALZLibExGZ,
  i_BinaryData,
  i_Downloader,
  u_GeoToStr,
  u_DownloadRequest,
  u_StreamReadOnlyByBinaryData,
  u_InetFunc,
  u_BinaryData;

procedure GenerateAvailPicsKS(
  var AKSs: TAvailPicsKosmosnimki;
  const AResultFactory: IDownloadResultFactory;
  const ATileInfoPtr: PAvailPicsTileInfo;
  const AMapSvcScanStorage: IMapSvcScanStorage
);
var
  j: TAvailPicsKosmosnimkiID;
begin
  Assert(AResultFactory<>nil);
  for j := Low(TAvailPicsKosmosnimkiID) to High(TAvailPicsKosmosnimkiID) do begin
    if (nil=AKSs[j]) then begin
      AKSs[j] := TAvailPicsKS.Create(
        ATileInfoPtr,
        AMapSvcScanStorage
      );
      with AKSs[j] do begin
        FResultFactory := AResultFactory;
        FKSMode := Ord(j);
        FMinDate := '1993-01-01';
        FMaxDate := FormatDateTime('yyyy-mm-dd', Now);
        FMaxCloudCover := 100;
        FEveryYear := 'false';
        FMaxOfNadir := 90;
      end;
    end;
  end;
end;

{ TAvailPicsKS }

function TAvailPicsKS.ContentType: String;
begin
  Result := 'text/html; charset=utf-8'; //'text/plain' // 'text/plain;charset=utf-8'   // 'text/html'
end;

function TAvailPicsKS.ParseResponse(const AResultOk: IDownloadResultOk): Integer;

  procedure _InitParams(var AList: TStrings; const AText: string);
  begin
    if (nil=AList) then begin
      AList := TStringList.Create;
      AList.Delimiter := ',';
      AList.NameValueSeparator := ':';
    end;
    AList.DelimitedText := AText;
  end;

  function _MakeGeometryString(const AParams: TStrings): string;
  var
    i: Integer;
    VFirst: String;
  begin
    Result := AParams.Values['x1'] + ' ' + AParams.Values['y1'];
    VFirst := Result;
    
    for i := 2 to 4 do begin
      Result := Result + ' ' + AParams.Values['x'+IntToStr(i)] +
                         ' ' + AParams.Values['y'+IntToStr(i)];
    end;

    Result := Result + ' ' + VFirst;
  end;

  function _InternalAddItem(
    const AParams: TStrings;
    var AExternalResultCount: Integer
  ): Boolean;
  var
    VOutParams: TStrings;
    i: Integer;
    VName: String;
    VDate, VRealID, VID: String;
    VItemSubStorage: String;
    VItemExisting: Boolean;
    VItemFetched: TDateTime;
  begin
    VOutParams := TStringList.Create;
    try
      // copy params
      for i := 0 to AParams.Count-1 do begin
        VName := AParams.Names[i];
        VOutParams.Values[VName] := AParams.ValueFromIndex[i];
      end;
      
      // add some fields
      VOutParams.Values['ProviderName'] := 'Kosmosnimki';

      // image and storage identifiers
      VItemSubStorage := VOutParams.Values['sat_name'];
      VDate := VOutParams.Values['date'];
      if (Length(VDate)>=10) then begin
        VDate[5] := DateSeparator;
        VDate[8] := DateSeparator;
      end;
      VRealID := VOutParams.Values['id'];
      VID := VDate+' ['+VRealID+'] '+VItemSubStorage;

      // check SPOT 5 order
      VName := VOutParams.Values['prod_order'];
      if (0<Length(VName)) then begin
        VItemSubStorage := VItemSubStorage + '_' + VName;
        case VName[1] of
          '5': VName := '2.5m Color';
          '3': VName := '5m Color';
          '1': VName := '10m Color';
          '4': VName := '2.5m BW';
          '2': VName := '5m BW';
          else VName := '';
        end;
        if (0<Length(VName)) then begin
          VOutParams.Values['spot5products'] := VName;
          VID := VID + ' ' + VName;
        end;
      end;

      // add geometry
      VOutParams.Values['Geometry'] := _MakeGeometryString(VOutParams);

      // check if new
      VItemExisting := ItemExists(
        FBaseStorageName+'_'+VItemSubStorage,
        VRealID,
        @VItemFetched
      );

      // add image
      Result := FTileInfoPtr.AddImageProc(
        Self,
        VDate,
        VID,
        VItemExisting,
        VItemFetched,
        VOutParams
      );
      FreeAndNil(VOutParams);

      // inc count
      if Result then begin
        Inc(AExternalResultCount);
      end;
    finally
      FreeAndNil(VOutParams);
    end;
  end;

var
  VIndex: Integer;
  VList: TStringList;
  VLine: String;
  VParams: TStrings;
begin
  Result:=0;

  if (not Assigned(FTileInfoPtr.AddImageProc)) then
    Exit;

  VParams := nil;
  VList := TStringList.Create;
  try
    // try to get plain text (unzip if gzipped)
    if not GetPlainJsonKosmosnimkiText(AResultOk, VList) then
      Exit;

    // full JSON parser for Kosmosnimki
    VIndex := 0;

    while VIndex<VList.Count do begin
      VLine :=VList[VIndex];
      VLine := System.Copy(VLine, 2, Length(VLine)-2);
      VLine := StringReplace(VLine, '"', '', [rfReplaceAll]);

      _InitParams(VParams, VLine);

      if Assigned(VParams) then begin
        // add item
        _InternalAddItem(VParams, Result);
      end;

      // goto next line
      Inc(VIndex);
    end;
  finally
    FreeAndNil(VList);
    FreeAndNil(VParams);
  end;
end;

function TAvailPicsKS.MakePostString: string;
var
  VText: String;
begin
(*
satellites	GE-1,WV01,WV02,QB02,EROS-B,IK-2,EROS-A1,Pleiades,SPOT 5
spot5products	5,3,1,4,2
min_date	2003-01-01
max_date	2013-02-12
max_cloud_cover	50
every_year	false
max_off_nadir	90
wkt	POLYGON((56.60156 61.20494,56.75537 61.20494,56.75537 61.09875,56.60156 61.09875,56.60156 61.20494));
*)
  VText := '';
  case FKSMode of
    1: Result := 'GE-1';
    2: Result := 'WV01';
    3: Result := 'WV02';
    4: Result := 'QB02';
    5: Result := 'EROS-B';
    6: Result := 'IK-2';
    7: Result := 'EROS-A1';
    8: Result := 'Pleiades';
    else begin
      Result := 'SPOT 5';
      VText := IntToStr(FKSMode-8);
    end;
  end;

  Result := 'satellites=' + Result;

  if (0<Length(VText)) then begin
    // add SPOT
    Result := Result + '&spot5products=' + VText;
  end;

  Result := Result + '&min_date=' + FMinDate + '&max_date=' + FMaxDate;
  Result := Result + '&max_cloud_cover=' + IntToStr(FMaxCloudCover);
  Result := Result + '&every_year=' + FEveryYear;
  Result := Result + '&max_off_nadir=' + IntToStr(FMaxOfNadir);

  // first=last
  VText := RoundEx(FTileInfoPtr.TileRect.Left, 8)+' '+RoundEx(FTileInfoPtr.TileRect.Top, 8);

  Result := Result +  '&wkt=POLYGON((' +
    VText+','+
    RoundEx(FTileInfoPtr.TileRect.Left, 8)+' '+RoundEx(FTileInfoPtr.TileRect.Bottom, 8)+','+
    RoundEx(FTileInfoPtr.TileRect.Right, 8)+' '+RoundEx(FTileInfoPtr.TileRect.Top, 8)+','+
    RoundEx(FTileInfoPtr.TileRect.Right, 8)+' '+RoundEx(FTileInfoPtr.TileRect.Bottom, 8)+','+
    VText+
    '));';
end;

function TAvailPicsKS.GetPlainJsonKosmosnimkiText(
  const AResultOk: IDownloadResultOk;
  const AList: TStrings
): Boolean;
begin
  Result := FALSE;

  if (0=AResultOk.Data.Size) or (nil=AResultOk.Data.Buffer) then
    Exit;

  if IsGZipped(AResultOk.RawResponseHeader) then begin
    // gzipped
    try
      // try to unzip
      Result := GetUnzippedJsonKosmosnimkiText(AResultOk, AList);
    except
      // try as plain text
      Result := GetSimpleJsonKosmosnimkiText(AResultOk, AList);
    end;
  end else begin
    // plain
    Result := GetSimpleJsonKosmosnimkiText(AResultOk, AList);
  end;
end;

function TAvailPicsKS.GetRequest(const AInetConfig: IInetConfig): IDownloadRequest;
var
  VPostData: IBinaryData;
  VPostdataStr: AnsiString;

  VLink: String;
  VHeader: String;

begin
  VHeader :='User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.52 Safari/537.17'+#$D#$A+
    'Host: search.kosmosnimki.ru'+#$D#$A+
    'Connection: keep-alive'+#$D#$A+
    'Accept: */*'+#$D#$A+
    'Origin: http://search.kosmosnimki.ru'+#$D#$A+
    'X-Requested-With: XMLHttpRequest'+#$D#$A+
    'Content-Type: application/x-www-form-urlencoded'+#$D#$A+
    'Referer: http://search.kosmosnimki.ru/index.html'+#$D#$A+
    'Accept-Encoding: gzip,deflate,sdch'+#$D#$A+
    'Accept-Language: ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4'+#$D#$A+
    'Accept-Charset: windows-1251,utf-8;q=0.7,*;q=0.3'+#$D#$A;
    //'Accept-Encoding: deflate'+#$D#$A+

  VLink := 'http://search.kosmosnimki.ru/QuicklooksJson.ashx'; // json

  // Формируем строку запроса на получение списка снимков
  VPostDataStr := MakePostString;

  VPostData :=
    TBinaryData.CreateByAnsiString(VPostDataStr);

  Result :=TDownloadPostRequest.Create(
           VLink,
           VHeader,
           VPostData,
           AInetConfig.GetStatic
           );
end;

function TAvailPicsKS.GetSimpleJsonKosmosnimkiText(
  const AResultOk: IDownloadResultOk;
  const AList: TStrings
): Boolean;
var
  VSimpleText: String;
begin
  SetString(VSimpleText, PChar(AResultOk.Data.Buffer), AResultOk.Data.Size);

  VSimpleText := PrepareToParseString(VSimpleText);
  PrepareStringList(AList);
  AList.DelimitedText := VSimpleText;

  Result := (AList.Count>1);
end;

function TAvailPicsKS.GetUnzippedJsonKosmosnimkiText(
  const AResultOk: IDownloadResultOk;
  const AList: TStrings
): Boolean;
var
  VZipped: TStreamReadOnlyByBinaryData;
  VUnzipped: TMemoryStream;
  VStrValue: String;
begin
  VUnzipped := nil;
  VZipped := TStreamReadOnlyByBinaryData.Create(AResultOk.Data);
  try
    VUnzipped := TMemoryStream.Create;

    GZDecompressStream(VZipped, VUnzipped);

    // failed to unzip - try to use as plain text
    if (VUnzipped.Memory=nil) or (VUnzipped.Size=0) then
      Abort;

    // unzipped
    SetString(VStrValue, PChar(VUnzipped.Memory), (VUnzipped.Size div SizeOf(Char)));

    VStrValue := PrepareToParseString(VStrValue);

    PrepareStringList(AList);
    AList.DelimitedText := VStrValue;
    Result := (AList.Count>1);
  finally
    VZipped.Free;
    VUnzipped.Free;
  end;
end;

function TAvailPicsKS.PrepareToParseString(const AStrValue: string): String;
begin
  if (0=Length(AStrValue)) then begin
    Result := '';
    Exit;
  end;
  //убираем [ в начале и ] в конце
  Result := System.Copy(AStrValue, 2, Length(AStrValue)-2);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
  Result := StringReplace(Result, '},{', '|*|', [rfReplaceAll]);
  if (0<Length(Result)) then begin
    Result[1] := '|';
    Result[Length(AStrValue)] := '|';
  end;
end;

procedure TAvailPicsKS.PrepareStringList(const AList: Tstrings);
begin
  AList.Clear;
  AList.Delimiter := '*';
  AList.QuoteChar := '"';
  AList.NameValueSeparator := ':';
end;

end.
