unit u_GeoCoderByGoogle;

interface

uses
  Windows,
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCoderByGoogle = class(TInterfacedObject, IGeoCoder)
  private
    function GetKmlFromGoogle(ASearch: WideString): string;
    function GetLocations(ASearch: WideString; ACurrentPos: TDoublePoint): IGeoCodeResult; safecall;
  public

  end;

implementation

uses
  Classes,
  SysUtils,
  StrUtils,
  WinInet,
  u_GlobalState,
  u_GeoToStr,
  u_GeoCodePalcemark,
  u_GeoCodeResult;

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

function URLEncode(const S: string): string;
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
end; // URLEncode

{ TGeoCoderByGoogle }

function TGeoCoderByGoogle.GetKmlFromGoogle(ASearch: WideString): string;
var
  s, par: string;
  i: integer;
  err: boolean;
  Buffer: array [1..64535] of char;
  BufferLen: LongWord;
  hSession, hFile: Pointer;
  dwindex, dwcodelen, dwReserv: dword;
  dwtype: array [1..20] of char;
  strr: string;
  VSearch: String;
begin
  VSearch := ASearch;
  for i := 1 to length(VSearch) do begin
    if VSearch[i] = ' ' then begin
      VSearch[i] := '+';
    end;
  end;
  hSession := InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(hSession) then begin
    try
      strr := 'http://maps.google.com/maps/geo?q=' + URLEncode(AnsiToUtf8(VSearch)) + '&output=xml&hl=ru&key=ABQIAAAA5M1y8mUyWUMmpR1jcFhV0xSHfE-V63071eGbpDusLfXwkeh_OhT9fZIDm0qOTP0Zey_W5qEchxtoeA';
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
            if (not GState.InetConnect.userwinset) and (GState.InetConnect.uselogin) then begin
              InternetSetOption(hFile, INTERNET_OPTION_PROXY_USERNAME, PChar(GState.InetConnect.loginstr), length(GState.InetConnect.loginstr));
              InternetSetOption(hFile, INTERNET_OPTION_PROXY_PASSWORD, PChar(GState.InetConnect.passstr), length(GState.InetConnect.Passstr));
              HttpSendRequest(hFile, nil, 0, Nil, 0);
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
end;

function TGeoCoderByGoogle.GetLocations(ASearch: WideString;
  ACurrentPos: TDoublePoint): IGeoCodeResult;
var
  s, slat, slon: string;
  i, j: integer;
  strr: string;
  VPoint: TDoublePoint;
  VPlace: IGeoCodePalcemark;
  VList: IInterfaceList;
begin
  if not (ASearch = '') then begin

    s := GetKmlFromGoogle(ASearch);

    if not (PosEx(AnsiToUtf8('Placemark'), s) < 1) then begin
      i := PosEx('<address>', s);
      j := PosEx('</address>', s);
      strr := Utf8ToAnsi(Copy(s, i + 9, j - (i + 9)));
      i := PosEx('<coordinates>', s);
      j := PosEx(',', s, i + 13);
      slon := Copy(s, i + 13, j - (i + 13));
      i := PosEx(',0</coordinates>', s, j);
      slat := Copy(s, j + 1, i - (j + 1));
      if slat[1] = '\' then begin
        delete(slat, 1, 1);
      end;
      if slon[1] = '\' then begin
        delete(slon, 1, 1);
      end;
      try
        VPoint.Y := str2r(slat);
        VPoint.X := str2r(slon);
        VPlace := TGeoCodePalcemark.Create(VPoint, strr, 4);
        VList := TInterfaceList.Create;
        VList.Add(VPlace);
        Result := TGeoCodeResult.Create(ASearch, VList);
      except
      end;
    end;
  end;
  if Result = nil then begin
    Result := TGeoCodeResult.Create(ASearch, TInterfaceList.Create);
  end;
end;

end.
