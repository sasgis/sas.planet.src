unit u_ImportFromArcGIS;

interface

uses
  SysUtils,
  Types,
  t_GeoTypes,
  i_InetConfig,
  i_CoordConverterFactory,
  i_VectorItemsFactory,
  i_VectorItemSubset,
  i_VectorDataFactory;

// если ещё надо будет использовать - вынести импортилку в отдельный класс
function ImportFromArcGIS(
  const AInetConfig: IInetConfig;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const ALonLatRect: TDoubleRect;
  const ALonLat: TDoublePoint;
  const AZoom: Byte;
  const AMapSize: TPoint
): IVectorItemSubset;

implementation

uses
  Windows,
  Classes,
  ALZLibExGZ,
  c_CoordConverter,
  i_DownloadResultFactory,
  i_DownloadResult,
  i_DownloadRequest,
  i_Downloader,
  i_InterfaceListSimple,
  i_NotifierOperation,
  i_DoublePointsAggregator,
  i_CoordConverter,
  i_VectorItemLonLat,
  u_DownloadResultFactory,
  u_DownloadRequest,
  u_DownloaderHttp,
  u_InterfaceListSimple,
  u_VectorDataItemSubset,
  u_Notifier,
  u_NotifierOperation,
  u_DoublePointsAggregator,
  u_MultiPoligonParser,
  u_GeoToStr;

procedure AddWithBR(var AFullDesc: String; const ACaption, AValue: String);
begin
  if (0=Length(AValue)) then
    Exit;
  if SameText(AValue,'null') then
    Exit;
  if SameText(AValue,'неопр') then
    Exit;
  if (0<Length(AFullDesc)) then
    AFullDesc := AFullDesc + '<br>';
  AFullDesc := AFullDesc + ACaption + ':' + AValue;
end;

function UnQuote(const S: String): String;
begin
  Result := S;
  if (0<Length(Result)) and (Result[1]='"') then
    System.Delete(Result,1,1);
  if (0<Length(Result)) and (Result[Length(Result)]='"') then
    SetLength(Result,(Length(Result)-1));
end;

function _refererxy(const AConverter: ICoordConverter; const ALonLat: TDoublePoint): String;
var
  VMetrPoint: TDoublePoint;
begin
  VMetrPoint := AConverter.LonLat2Metr(ALonLat);
  Result := '&x='+RoundEx(VMetrPoint.X, 4)+'&y='+RoundEx(VMetrPoint.Y, 4);
end;

function _geometry(const AConverter: ICoordConverter; const ALonLat: TDoublePoint): String;
var
  VMetrPoint: TDoublePoint;
begin
  VMetrPoint := AConverter.LonLat2Metr(ALonLat);
  Result := '{"x":'+RoundEx(VMetrPoint.X, 9)+
            ',"y":'+RoundEx(VMetrPoint.Y, 9)+
            ',"spatialReference":{"wkid":102100}}';
end;

function _mapExtent(const AConverter: ICoordConverter; const ALonLatRect: TDoubleRect): String;
var
  VMetrPointMin, VMetrPointMax: TDoublePoint;
begin
  VMetrPointMin.X := ALonLatRect.Left;
  VMetrPointMin.Y := ALonLatRect.Bottom;
  VMetrPointMin := AConverter.LonLat2Metr(VMetrPointMin);
  VMetrPointMax.X := ALonLatRect.Right;
  VMetrPointMax.Y := ALonLatRect.Top;
  VMetrPointMax := AConverter.LonLat2Metr(VMetrPointMax);
  Result := '{"xmin":'+RoundEx(VMetrPointMin.X, 9)+
            ',"ymin":'+RoundEx(VMetrPointMin.Y, 9)+
            ',"xmax":'+RoundEx(VMetrPointMax.X, 9)+
            ',"ymax":'+RoundEx(VMetrPointMax.Y, 9)+
            ',"spatialReference":{"wkid":102100}}';
end;

function _imageDisplay(const AMapSize: TPoint): String;
begin
  // '1314,323,96'
  Result := IntToStr(AMapSize.X)+','+IntToStr(AMapSize.Y)+','+'96';
end;

function _GetCadastreLevel(const AText: String): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(AText) do begin
    if AText[i]=':' then begin
      Inc(Result);
    end;
  end;
end;

function _IsZippedDownloadResult(
  const ADownloadResultOk: IDownloadResultOk;
  var AContentEncodingDetector: TStringList;
  out AInUtf8: Boolean
): Boolean;
begin
  if (nil=AContentEncodingDetector) then begin
    AContentEncodingDetector := TStringList.Create;
    AContentEncodingDetector.NameValueSeparator := ':';
  end;
  AContentEncodingDetector.Text := ADownloadResultOk.RawResponseHeader;
  AInUtf8 := (System.Pos('utf-8', ADownloadResultOk.ContentType)>0);
  Result := SameText(Trim(AContentEncodingDetector.Values['Content-Encoding']), 'gzip');
end;

function _GetPlainResponseFromDownloadResult(
  var ASrcInZip: TMemoryStream;
  var AUnZipped: TMemoryStream;
  var AContentEncodingDetector: TStringList;
  const ADownloadResultOk: IDownloadResultOk
): String;
var
  VZipped: Boolean;
  VUnUtf8: Boolean;
begin
  Result := '';
  VZipped := _IsZippedDownloadResult(ADownloadResultOk, AContentEncodingDetector, VUnUtf8);
  if VZipped then
  try
    FreeAndNil(ASrcInZip);
    ASrcInZip:=TMemoryStream.Create;
    ASrcInZip.SetSize(ADownloadResultOk.Data.Size);
    CopyMemory(ASrcInZip.Memory, ADownloadResultOk.Data.Buffer, ADownloadResultOk.Data.Size);
    if (nil=ASrcInZip.Memory) or (0=ASrcInZip.Size) then
      Exit;
    FreeAndNil(AUnZipped);
    AUnZipped := TMemoryStream.Create;
    GZDecompressStream(ASrcInZip, AUnZipped);
    SetString(Result, PChar(AUnZipped.Memory), AUnZipped.Size div SizeOf(Char));
    if VUnUtf8 then
      Result := Utf8ToAnsi(Result);
    Exit;
  except
  end;

  // as plain text
  if ADownloadResultOk.Data.Size>0 then begin
    SetString(Result, PChar(ADownloadResultOk.Data.Buffer), ADownloadResultOk.Data.Size div SizeOf(Char));
    if VUnUtf8 then
      Result := Utf8ToAnsi(Result);
  end;
end;

procedure _CheckThisIsParcel(
  const AText: String;
  var AMaxCadastreDelimiters: Integer;
  var ACadastreNumber: String
);
var
  VLevel: Integer;
begin
  VLevel := _GetCadastreLevel(AText);
  if (VLevel=3) then begin
    ACadastreNumber := AText;
  end;
  if (AMaxCadastreDelimiters<VLevel) then begin
    AMaxCadastreDelimiters:=VLevel;
  end;
end;

procedure _PrepareJSONParser(var AJSONParams: TStringList);
begin
  if (nil=AJSONParams) then begin
    AJSONParams := TStringList.Create;
    AJSONParams.NameValueSeparator := ':';
    AJSONParams.QuoteChar := #0;
    AJSONParams.Delimiter := ',';
    AJSONParams.StrictDelimiter := TRUE;
  end else begin
    AJSONParams.Clear;
  end;
end;

procedure _ParseSingleJSON(
  const AInetConfig: IInetConfig;
  const AText, AGeometry: String;
  const APointsAggregator: IDoublePointsAggregator;
  const ADownloader: IDownloader;
  const AHeaders: String;
  const AConverter: ICoordConverter;
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  var AJSONParams: TStringList;
  var ASrcInZip: TMemoryStream;
  var AUnZipped: TMemoryStream;
  var AContentEncodingDetector: TStringList;
  out AMarkName, AMarkDesc: String
);
var
  i: Integer;
  VTemp, VLine, VCadastreNumber: String;
  VMaxCadastreDelimiters: Integer;
  VSecondLink: String;
  VSecondRequest: IDownloadRequest;
  VSecondResult: IDownloadResult;
  VSecondDownloadResultOk: IDownloadResultOk;
begin
  VMaxCadastreDelimiters := 0;
  VCadastreNumber := '';
  AMarkName := '';
  AMarkDesc := '';

  _PrepareJSONParser(AJSONParams);

  // parse text
  AJSONParams.DelimitedText := AText;

  // make full description
  for i := 0 to AJSONParams.Count-1 do begin
    VTemp := AJSONParams.Names[i];
    VTemp := UnQuote(VTemp);
    VLine := UnQuote(AJSONParams.ValueFromIndex[i]);
    AddWithBR(AMarkDesc, VTemp, VLine);
    // check for parcels
    if (VMaxCadastreDelimiters<=3) then begin
      // check max count of ':' in values is equal to 3
      // and save this value as cadastre number
      _CheckThisIsParcel(VLine, VMaxCadastreDelimiters, VCadastreNumber);
    end;
    // make name
    if (0=Length(AMarkName)) and SameText(VTemp,'displayFieldName') then begin
      AMarkName := VLine;
    end else if (0<>Length(AMarkName)) and SameText(VTemp,AMarkName) then begin
      AMarkName := VLine;
    end;
  end;

  // secondary request (only for parcels!)
  if (VMaxCadastreDelimiters=3) and (0<Length(VCadastreNumber)) then begin
    VSecondLink :=
      'http://maps.rosreestr.ru/ArcGIS/rest/services/CadastreNew/'+
      'Cadastre/MapServer/exts/GKNServiceExtension/online/parcel/find?'+
      // ['59:39:320001:640'] = %5B%2759%3A39%3A320001%3A640%27%5D
      'cadNums=%5B%27'+StringReplace(VCadastreNumber,':','%3A',[rfReplaceAll])+'%27%5D'+
      '&onlyAttributes=false'+
      '&returnGeometry=false'+ // allow 'true'
      '&f=json';

    VSecondRequest := TDownloadRequest.Create(
      VSecondLink,
      AHeaders,
      AInetConfig.GetStatic
    );

    VSecondResult := ADownloader.DoRequest(
      VSecondRequest,
      ACancelNotifier,
      ACancelNotifier.CurrentOperation
    );

    if Supports(VSecondResult, IDownloadResultOk, VSecondDownloadResultOk) then begin
      // check HTTP status
      if VSecondDownloadResultOk.StatusCode<>200 then begin
        raise Exception.Create('Cannot request parcel information');
      end;

      // add values from this response
      _PrepareJSONParser(AJSONParams);

      // parse second response
      VSecondLink :=
        _GetPlainResponseFromDownloadResult(
          ASrcInZip,
          AUnZipped,
          AContentEncodingDetector,
          VSecondDownloadResultOk
        );

      if (System.Pos('error', VSecondLink)>0) and
         (System.Pos('code', VSecondLink)>0) and
         (System.Pos('Invalid URL', VSecondLink)>0) then begin
        raise Exception.Create('Cannot request parcel information');
      end;

      // delete debug fields
      VMaxCadastreDelimiters :=  System.Pos('"debug":', VSecondLink);
      if (VMaxCadastreDelimiters>0) then begin
        SetLength(VSecondLink, VMaxCadastreDelimiters-1);
      end;

      VSecondLink := StringReplace(VSecondLink, '"features":','',[rfIgnoreCase]);
      VSecondLink := StringReplace(VSecondLink, '"attributes":','',[rfIgnoreCase]);
      VSecondLink := StringReplace(VSecondLink, '}]','',[rfReplaceAll]);
      VSecondLink := StringReplace(VSecondLink, '[{','',[rfReplaceAll]);
      VSecondLink := StringReplace(VSecondLink, '}','',[rfReplaceAll]);
      VSecondLink := StringReplace(VSecondLink, '{','',[rfReplaceAll]);

      AJSONParams.DelimitedText := VSecondLink;

      // just add fields
      for i := 0 to AJSONParams.Count-1 do begin
        VTemp := AJSONParams.Names[i];
        VTemp := UnQuote(VTemp);
        VLine := UnQuote(AJSONParams.ValueFromIndex[i]);
        AddWithBR(AMarkDesc, VTemp, VLine);
      end;

      // done
    end;
  end;

  APointsAggregator.Clear;

  // parse geometry
  ParsePointsToAggregator(
    APointsAggregator,
    AGeometry,
    AConverter,
    True,
    False,
    True
  );
end;

function ImportFromArcGIS(
  const AInetConfig: IInetConfig;
  const ACoordConverterFactory: ICoordConverterFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const AVectorDataFactory: IVectorDataFactory;
  const ALonLatRect: TDoubleRect;
  const ALonLat: TDoublePoint;
  const AZoom: Byte;
  const AMapSize: TPoint
): IVectorItemSubset;

const
  c_JSON_Delimiter = '},{';

var
  VConverter: ICoordConverter;
  VHead: String;
  VDownloader: IDownloader;
  VCancelNotifier: INotifierOperation;
  VContentEncodingDetector, VJSONParams: TStringList;
  VSrcInZip: TMemoryStream;
  VUnZipped: TMemoryStream;
  VResultFactory: IDownloadResultFactory;
  VRequest: IDownloadRequest;
  VDownloadResultOk: IDownloadResultOk;
  VLink: String;
  VResult: IDownloadResult;
  VJSON, VText, VGeometry: String;
  VMarkName, VMarkDesc: String;
  VPos: Integer;
  VPointsAggregator: IDoublePointsAggregator;
  VPolygon: ILonLatPolygon;
  VAllNewMarks: IInterfaceListSimple;
begin
  Result := nil;
  VConverter := ACoordConverterFactory.GetCoordConverterByCode(CGoogleProjectionEPSG, CTileSplitQuadrate256x256);
  VResultFactory := TDownloadResultFactory.Create;
  VDownloader:=TDownloaderHttp.Create(VResultFactory);

  VHead :=
      'User-Agent: Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.17 (KHTML, like Gecko) Chrome/24.0.1312.52 Safari/537.17'+#$D#$A+
      'Host: maps.rosreestr.ru'+#$D#$A+
      'Accept: */*'+#$D#$A+
      'Accept-Language: ru-RU,ru;q=0.8,en-US;q=0.6,en;q=0.4'+#$D#$A+
      'Referer: http://maps.rosreestr.ru/PortalOnline/?l='+IntToStr(AZoom)+_refererxy(VConverter, ALonLat)+'&mls=map|anno&cls=cadastre'+#$D#$A+
      'Connection: Keep-Alive'+#$D#$A+
      'Accept-Charset: windows-1251,utf-8;q=0.7,*;q=0.3'+#$D#$A+
      'Accept-Encoding: gzip, deflate';

  // identify object
  VLink := 'http://maps.rosreestr.ru/ArcGIS/rest/services/CadastreNew/'+
           'CadastreSelected/MapServer/identify?f=json'+
           '&geometry='+_geometry(VConverter, ALonLat)+
           '&tolerance=0'+
           '&returnGeometry=true'+
           '&mapExtent='+_mapExtent(VConverter, ALonLatRect)+
           '&imageDisplay='+_imageDisplay(AMapSize)+
           '&geometryType=esriGeometryPoint'+
           '&sr=102100'+
           '&layers=top,bottom'; // 'top' is ok

  VRequest := TDownloadRequest.Create(
    VLink,
    VHead,
    AInetConfig.GetStatic
  );

  VCancelNotifier := TNotifierOperation.Create(TNotifierBase.Create);

  VResult := VDownloader.DoRequest(
    VRequest,
    VCancelNotifier,
    VCancelNotifier.CurrentOperation
  );

  if not Supports(VResult, IDownloadResultOk, VDownloadResultOk) then
    Exit;

  if (nil=VDownloadResultOk.Data) then
    Exit;
  if (nil=VDownloadResultOk.Data.Buffer) then
    Exit;
  if (0=VDownloadResultOk.Data.Size) then
    Exit;

  // bingo!
  VPointsAggregator := nil;
  VAllNewMarks := nil;
  VSrcInZip := nil;
  VUnzipped := nil;
  VContentEncodingDetector := nil;
  VJSONParams := nil;
  try
    VJSON :=
      _GetPlainResponseFromDownloadResult(
        VSrcInZip,
        VUnZipped,
        VContentEncodingDetector,
        VDownloadResultOk
      );

    // divide by '},{' and parse each part
    repeat
      if (0=Length(VJSON)) then
        break;

      VText := '';
      VPos := System.Pos(c_JSON_Delimiter, VJSON);
      if (VPos>0) then begin
        // has delimiter
        VText := System.Copy(VJSON, 1, VPos-1);
        System.Delete(VJSON, 1, VPos+Length(c_JSON_Delimiter)-1);
      end else begin
        // use all text
        VText := VJSON;
        VJSON := '';
      end;

      if (0<Length(VText)) then begin
        // prepare
        VText := StringReplace(VText, '"results":', '', [rfIgnoreCase]);
        VText := StringReplace(VText, '"attributes":', '', [rfIgnoreCase]);
        VText := StringReplace(VText, '"wkid":', '', [rfIgnoreCase]);
        VText := StringReplace(VText, '"geometry":', '', [rfIgnoreCase]);

        VPos := System.Pos('"rings":', VText);
        if (VPos>0) then begin
          // has geometry
          VGeometry := System.Copy(VText, VPos+8, Length(VText));
          SetLength(VText, VPos-1);

          VText := StringReplace(VText, '}', '', [rfReplaceAll]);
          VText := StringReplace(VText, '{', '', [rfReplaceAll]);
          VText := StringReplace(VText, '[', '', [rfReplaceAll]);

          if (nil=VPointsAggregator) then begin
            VPointsAggregator := TDoublePointsAggregator.Create;
          end;

          _ParseSingleJSON(
            AInetConfig,
            VText,
            VGeometry,
            VPointsAggregator,
            VDownloader,
            VHead,
            VConverter,
            VCancelNotifier,
            VCancelNotifier.CurrentOperation,
            VJSONParams,
            VSrcInZip,
            VUnZipped,
            VContentEncodingDetector,
            VMarkName,
            VMarkDesc
          );

          if (VPointsAggregator.Count>0) then begin
            // create lonlats
            VPolygon := AVectorItemsFactory.CreateLonLatPolygon(VPointsAggregator.Points, VPointsAggregator.Count);
            if (VPolygon <> nil) and (VPolygon.Count > 0) then begin
              // make polygon
              if (nil=VAllNewMarks) then
                VAllNewMarks := TInterfaceListSimple.Create;
              VAllNewMarks.Add(AVectorDataFactory.BuildPoly(nil, VMarkName, VMarkDesc, VPolygon));
            end;
          end;

          if (nil=Result) then begin
            Result := TVectorItemSubset.Create(VAllNewMarks.MakeStaticAndClear);
          end;
        end;
      end;
    until FALSE;
  finally
    FreeAndNil(VSrcInZip);
    FreeAndNil(VUnzipped);
    FreeAndNil(VJSONParams);
    FreeAndNil(VContentEncodingDetector);
  end;
end;

end.