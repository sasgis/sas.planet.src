unit u_GeoCoderBasic;

interface

uses
  Windows,
  Classes,
  t_GeoTypes,
  i_IProxySettings,
  i_GeoCoder;

type
  TGeoCoderBasic = class(TInterfacedObject, IGeoCoder)
  protected
    FInetSettings: IProxySettings;
    function URLEncode(const S: string): string; virtual;
    function PrepareURL(ASearch: WideString): string; virtual; abstract;
    function GetDataFromInet(ASearch: WideString): string; virtual;
    function ParseStringToPlacemarksList(AStr: string; ASearch: WideString): IInterfaceList; virtual; abstract;
    function GetLocations(ASearch: WideString; ACurrentPos: TDoublePoint): IGeoCodeResult; virtual; safecall;
  public
    constructor Create(AInetSettings: IProxySettings);
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  WinInet,
  u_GeoCodeResult;

{ TGeoCoderBasic }

constructor TGeoCoderBasic.Create(AInetSettings: IProxySettings);
begin
  FInetSettings := AInetSettings;
end;

destructor TGeoCoderBasic.Destroy;
begin
  FInetSettings := nil;
  inherited;
end;

function TGeoCoderBasic.GetDataFromInet(ASearch: WideString): string;
var
  s, par: string;
  err: boolean;
  Buffer: array [1..64535] of char;
  BufferLen: LongWord;
  hSession, hFile: Pointer;
  dwindex, dwcodelen, dwReserv: dword;
  dwtype: array [1..20] of char;
  strr: string;
  VLogin: string;
  VPassword: string;
begin
  hSession := InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(hSession) then begin
    try
      strr := PrepareURL(ASearch);
      hFile := InternetOpenUrl(hSession, PChar(strr), PChar(par), length(par), INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD, 0);
      if Assigned(hFile) then begin
        try
          dwcodelen := SizeOf(dwtype);
          dwReserv := 0;
          dwindex := 0;
          if HttpQueryInfo(hFile, HTTP_QUERY_STATUS_CODE, @dwtype, dwcodelen, dwReserv) then begin
            dwindex := strtoint(pchar(@dwtype));
          end;
          if (dwindex = HTTP_STATUS_PROXY_AUTH_REQ) then begin
            if FInetSettings <> nil then begin
              if (FInetSettings.UseLogin) then begin
                VLogin := FInetSettings.Login;
                VPassword := FInetSettings.Password;
                InternetSetOption(hFile, INTERNET_OPTION_PROXY_USERNAME, PChar(VLogin), length(VLogin));
                InternetSetOption(hFile, INTERNET_OPTION_PROXY_PASSWORD, PChar(VPassword), length(VPassword));
                HttpSendRequest(hFile, nil, 0, Nil, 0);
              end;
            end;
            dwcodelen := SizeOf(dwtype);
            dwReserv := 0;
            dwindex := 0;
            if HttpQueryInfo(hFile, HTTP_QUERY_STATUS_CODE, @dwtype, dwcodelen, dwReserv) then begin
              dwindex := strtoint(pchar(@dwtype));
            end;
            if (dwindex = HTTP_STATUS_PROXY_AUTH_REQ) then //Неверные пароль логин
            begin
              InternetCloseHandle(hFile);
              InternetCloseHandle(hSession);
              exit;
            end;
          end;

          repeat
            err := not (internetReadFile(hFile, @Buffer, SizeOf(Buffer), BufferLen));
            s := s + Buffer;
          until (BufferLen = 0) and (BufferLen < SizeOf(Buffer)) and (err = false);
        finally
          InternetCloseHandle(hFile);
        end;
      end;
    finally
      InternetCloseHandle(hSession);
    end;
  end;
  Result := s;
end;

function TGeoCoderBasic.GetLocations(ASearch: WideString;
  ACurrentPos: TDoublePoint): IGeoCodeResult;
var
  s: string;
  VList: IInterfaceList;
begin
  if not (ASearch = '') then begin
    s := GetDataFromInet(ASearch);
    VList := ParseStringToPlacemarksList(s, ASearch);
  end;
  if VList = nil then begin
    VList := TInterfaceList.Create;
  end;
  Result := TGeoCodeResult.Create(ASearch, VList, '');
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
