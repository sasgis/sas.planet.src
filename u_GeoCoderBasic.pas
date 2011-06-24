unit u_GeoCoderBasic;

interface

uses
  Windows,
  SysUtils,
  Classes,
  SwinHttp,
  t_GeoTypes,
  u_GeoCodeResult,
  i_ProxySettings,
  i_GeoCoder;

type
  TGeoCoderBasic = class(TInterfacedObject, IGeoCoder)
  protected
    FInetSettings: IProxySettings;
    FOnGetLocation: TGetLocation;
    procedure SendRequest(ASearch: WideString; ACurrentPos: TDoublePoint; OnGetLocation:TGetLocation); virtual; safecall;
    function URLEncode(const S: string): string; virtual;
    function PrepareURL(ASearch: WideString): string; virtual; abstract;
    function ParseStringToPlacemarksList(AStr: string; ASearch: WideString): IInterfaceList; virtual; abstract;
    procedure OnSendRequestEndWork(Sender: TSwinHttp; Request: TSwRequest);
    function GetLocations(ASearch: WideString; ACurrentPos: TDoublePoint): IGeoCodeResult; virtual; safecall;
  public
    constructor Create(AInetSettings: IProxySettings);
    destructor Destroy; override;
  end;

  EInternetOpenError = class(Exception)
  public
    ErrorCode: DWORD;
    constructor Create(Code: DWORD; Msg: String);
  end;

  EProxyAuthError = class(Exception);
  EAnswerParseError = class(Exception);

var
 shttp:TSwinHttp;
implementation

{ TGeoCoderBasic }

constructor TGeoCoderBasic.Create(AInetSettings: IProxySettings);
begin
  FInetSettings := AInetSettings;
  shttp:=TSwinHttp.Create(nil);
end;

destructor TGeoCoderBasic.Destroy;
begin
  FInetSettings := nil;
  freeandnil(shttp);
  inherited;
end;

procedure TGeoCoderBasic.OnSendRequestEndWork(Sender: TSwinHttp; Request: TSwRequest);
var
  VList: IInterfaceList;
  VResultCode: Integer;
  VMessage: WideString;
begin
  VResultCode := Sender.Response.Code;
  if VResultCode=200 then begin
    VList := ParseStringToPlacemarksList(Sender.Response.Body, '');
    if (VList=nil)or(VList.Count = 0) then begin
      VResultCode := 404;
      VMessage := 'Íå íàéäåíî';
    end;
  end else begin
    VMessage := 'Êîä îøèáêè '+inttostr(VResultCode);
  end;
  FOnGetLocation(TGeoCodeResult.Create('', VResultCode, VMessage, VList));
end;

procedure TGeoCoderBasic.SendRequest(ASearch: WideString; ACurrentPos: TDoublePoint; OnGetLocation:TGetLocation);
begin
  FOnGetLocation:=OnGetLocation;
  shttp.InThread:=true;
  shttp.OnWorkEnd:=OnSendRequestEndWork;
  shttp.Request.Agent:='Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  shttp.Request.url.url:=PrepareURL(ASearch);
  shttp.DoRequest;
end;

function TGeoCoderBasic.GetLocations(ASearch: WideString;
  ACurrentPos: TDoublePoint): IGeoCodeResult;
var
  VList: IInterfaceList;
  VResultCode: Integer;
  VMessage: WideString;
begin
  shttp.InThread:=false;
  shttp.OnWorkEnd:=nil;
  shttp.Request.Agent:='Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)';
  shttp.Request.url.url:=PrepareURL(ASearch);
  shttp.DoRequest;
  VResultCode := shttp.Response.Code;
  if VResultCode=200 then begin
    VList := ParseStringToPlacemarksList(shttp.Response.Body, '');
    if (VList=nil)or(VList.Count = 0) then begin
      VResultCode := 404;
      VMessage := 'Íå íàéäåíî';
    end;
  end else begin
    VMessage := 'Êîä îøèáêè '+inttostr(VResultCode);
  end;
  Result:=TGeoCodeResult.Create(ASearch, VResultCode, VMessage, VList);
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

{ EInternetOpenError }

constructor EInternetOpenError.Create(Code: DWORD; Msg: String);
begin
  inherited Create(Msg);
  ErrorCode := Code;
end;

end.
