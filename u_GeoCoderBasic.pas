unit u_GeoCoderBasic;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_GeoTypes,
  i_GeoCoder,
  i_InetConfig,
  i_SimpleDownloader;

type
  TGeoCoderBasic = class(TInterfacedObject, IGeoCoder)
  protected
    FInetConfig: IInetConfig;
    FOnGetLocation: TGetLocation;
    FDownloader: ISimpleDownloader;
    function URLEncode(const S: string): string; virtual;
    function PrepareURL(ASearch: WideString): string; virtual; abstract;
    function ParseStringToPlacemarksList(AStr: string; ASearch: WideString): IInterfaceList; virtual; abstract;
    procedure ParseHttpResponse(
      var AResponseCode: Cardinal;
      AResponseBuf: TMemoryStream;
      out AMessage: WideString;
      out AList: IInterfaceList
    );
    procedure OnAsyncDownloadReady(
      Sender: TObject;
      AResponseCode: Cardinal;
      const AContentType: string;
      const AResponseHead: string;
      AResponseBuf: TMemoryStream
    );
    function GetLocations(ASearch: WideString; ACurrentPos: TDoublePoint): IGeoCodeResult; virtual; safecall;
    procedure GetLocationsAsync(ASearch: WideString; ACurrentPos: TDoublePoint; OnGetLocation: TGetLocation); virtual; safecall;
  public
    constructor Create(AInetConfig: IInetConfig);
    destructor Destroy; override;
  end;

implementation

uses
  u_GeoCodeResult,
  u_SimpleDownloader;

{ TGeoCoderBasic }

constructor TGeoCoderBasic.Create(AInetConfig: IInetConfig);
begin
  FInetConfig := AInetConfig;
  FDownloader := TSimpleDownloader.Create(FInetConfig);
end;

destructor TGeoCoderBasic.Destroy;
begin
  FDownloader := nil;
  FInetConfig := nil;
  inherited;
end;

procedure TGeoCoderBasic.ParseHttpResponse(
  var AResponseCode: Cardinal;
  AResponseBuf: TMemoryStream;
  out AMessage: WideString;
  out AList: IInterfaceList
);
var
  VResponseBody: string;
begin
  if AResponseCode = 200 then begin
    if Assigned(AResponseBuf) then begin
      SetLength(VResponseBody, AResponseBuf.Size);
      FillChar(VResponseBody[1], Length(VResponseBody), 0);
      AResponseBuf.Position := 0;
      AResponseBuf.ReadBuffer(VResponseBody[1], Length(VResponseBody));
    end else begin
      VResponseBody := '';
    end;
    AList := ParseStringToPlacemarksList(VResponseBody, '');
    if (AList = nil) or (AList.Count = 0) then begin
      AResponseCode := 404;
      AMessage := 'Not Found';
    end;
  end else begin
    AMessage := 'HTTP Error: ' + IntToStr(AResponseCode);
  end;
end;

procedure TGeoCoderBasic.OnAsyncDownloadReady(
  Sender: TObject;
  AResponseCode: Cardinal;
  const AContentType: string;
  const AResponseHead: string;
  AResponseBuf: TMemoryStream
);
var
  VList: IInterfaceList;
  VMessage: WideString;
begin
  if Assigned(FOnGetLocation) then begin
    ParseHttpResponse(AResponseCode, AResponseBuf, VMessage, VList);
    FOnGetLocation(TGeoCodeResult.Create('', AResponseCode, VMessage, VList));
  end;
end;

procedure TGeoCoderBasic.GetLocationsAsync(
  ASearch: WideString;
  ACurrentPos: TDoublePoint;
  OnGetLocation: TGetLocation
);
begin
  if FDownloader <> nil then begin
    FOnGetLocation := OnGetLocation;
    FDownloader.GetFromInternetAsync(
      PrepareURL(ASearch),
      '',
      '',
      nil,
      Self.OnAsyncDownloadReady
    );
  end;
end;

function TGeoCoderBasic.GetLocations(
  ASearch: WideString;
  ACurrentPos: TDoublePoint
): IGeoCodeResult;
var
  VList: IInterfaceList;
  VResponseCode: Cardinal;
  VResponseBuf: TMemoryStream;
  VContentType: string;
  VResponseHead: string;
  VMessage: WideString;
begin
  if FDownloader <> nil then begin
    VResponseBuf := TMemoryStream.Create;
    try
      VResponseCode := FDownloader.GetFromInternet(
        PrepareURL(ASearch),
        '',
        '',
        nil,
        VResponseBuf,
        VContentType,
        VResponseHead
      );
      ParseHttpResponse(VResponseCode, VResponseBuf, VMessage, VList);
      Result:=TGeoCodeResult.Create(ASearch, VResponseCode, VMessage, VList);
    finally
      FreeAndNil(VResponseBuf);
    end;
  end;
end;

function TGeoCoderBasic.URLEncode(const S: string): string;
  function DigitToHex(Digit: Integer): Char;
  begin
    case Digit of
      0..9:
      begin
        Result := Chr(Digit + Ord('0'));
      end;
      10..15:
      begin
        Result := Chr(Digit - 10 + Ord('A'));
      end;
    else begin
      Result := '0';
    end;
    end;
  end; // DigitToHex
var
  i, idx, len: Integer;
begin
  len := 0;
  for i := 1 to Length(S) do begin
    if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or (S[i] = ' ') or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then begin
      len := len + 1;
    end else begin
      len := len + 3;
    end;
  end;
  SetLength(Result, len);
  idx := 1;
  for i := 1 to Length(S) do begin
    if S[i] = ' ' then begin
      Result[idx] := '+';
      idx := idx + 1;
    end else if ((S[i] >= '0') and (S[i] <= '9')) or
      ((S[i] >= 'A') and (S[i] <= 'Z')) or
      ((S[i] >= 'a') and (S[i] <= 'z')) or
      (S[i] = '_') or (S[i] = '*') or (S[i] = '-') or (S[i] = '.') then begin
      Result[idx] := S[i];
      idx := idx + 1;
    end else begin
      Result[idx] := '%';
      Result[idx + 1] := DigitToHex(Ord(S[i]) div 16);
      Result[idx + 2] := DigitToHex(Ord(S[i]) mod 16);
      idx := idx + 3;
    end;
  end;
end;

end.
