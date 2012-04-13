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

unit u_GeoCoderBasic;

interface

uses
  Windows,
  SysUtils,
  Classes,
  i_OperationNotifier,
  i_ProxySettings,
  i_GeoCoder,
  i_LocalCoordConverter;

type
  TGeoCoderBasic = class(TInterfacedObject, IGeoCoder)
  protected
    FLocalConverter: ILocalCoordConverter;
    FInetSettings: IProxySettings;
    function URLEncode(const S: string): string; virtual;
    function PrepareURL(const ASearch: WideString): string; virtual; abstract;
    function GetDataFromInet(const ASearch: WideString): string; virtual;
    function ParseStringToPlacemarksList(const AStr: string; const ASearch: WideString): IInterfaceList; virtual; abstract;
  protected
    function GetLocations(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      const ASearch: WideString;
      const ALocalConverter: ILocalCoordConverter
    ): IGeoCodeResult; virtual; safecall;
  public
    constructor Create(AInetSettings: IProxySettings);
    destructor Destroy; override;
  end;

  EInternetOpenError = class(Exception)
  public
    ErrorCode: DWORD;
    constructor Create(Code: DWORD; const Msg: String);
  end;

  EProxyAuthError = class(Exception);
  EAnswerParseError = class(Exception);

implementation

uses
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

function TGeoCoderBasic.GetDataFromInet(const ASearch: WideString): string;
var
  s, par: string;
  err: boolean;
  Buffer: array [1..64535] of char;
  BufferLen: LongWord;
  hSession, hFile: Pointer;
  dwindex, dwcodelen, dwReserv: dword;
  dwtype: array [1..20] of char;
  VUrl: string;
  VLogin: string;
  VPassword: string;
  VLastError: DWORD;
begin
  hSession := InternetOpen(pChar('Mozilla/4.0 (compatible; MSIE 7.0; Windows NT 5.1; .NET CLR 2.0.50727)'), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);
  if not Assigned(hSession) then begin
    VLastError := GetLastError;
    raise EInternetOpenError.Create(VLastError, 'Ошибка InternetOpen');
  end;
  try
    VUrl := PrepareURL(ASearch);
    if VUrl = '' then begin
      Result := '';
      exit;
    end;
    hFile := InternetOpenUrl(hSession, PChar(VUrl), PChar(par), length(par), INTERNET_FLAG_DONT_CACHE or INTERNET_FLAG_KEEP_CONNECTION or INTERNET_FLAG_RELOAD, 0);
    if not Assigned(hFile) then begin
      VLastError := GetLastError;
      raise EInternetOpenError.Create(VLastError, 'Ошибка InternetOpenUrl');
    end;
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

            dwcodelen := SizeOf(dwtype);
            dwReserv := 0;
            dwindex := 0;
            if HttpQueryInfo(hFile, HTTP_QUERY_STATUS_CODE, @dwtype, dwcodelen, dwReserv) then begin
              dwindex := strtoint(pchar(@dwtype));
            end;
          end;
        end;
        if (dwindex = HTTP_STATUS_PROXY_AUTH_REQ) then begin
          raise EProxyAuthError.Create('Ошибка уатентификации на Proxy');
        end;
      end;

      repeat
        err := not (internetReadFile(hFile, @Buffer, SizeOf(Buffer), BufferLen));
        s := s + copy(Buffer,1,BufferLen);
      until (BufferLen = 0) and (BufferLen < SizeOf(Buffer)) and (err = false);
    finally
      InternetCloseHandle(hFile);
    end;
  finally
    InternetCloseHandle(hSession);
  end;
  Result := s;
end;

function TGeoCoderBasic.GetLocations(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  const ASearch: WideString;
  const ALocalConverter: ILocalCoordConverter
): IGeoCodeResult;
var
  VServerResult: string;
  VList: IInterfaceList;
  VResultCode: Integer;
  VMessage: WideString;
begin
  VResultCode := 200;
  VMessage := '';
  FLocalConverter:=ALocalConverter;
  try
    if not (ASearch = '') then begin
      VServerResult := GetDataFromInet(ASearch);
      VList := ParseStringToPlacemarksList(VServerResult, ASearch);
    end;
  except
    on E: EInternetOpenError do begin
      VResultCode := 503;
      VMessage := E.Message;
    end;
    on E: EProxyAuthError do begin
      VResultCode := 407;
      VMessage := E.Message;
    end;
    on E: EAnswerParseError do begin
      VResultCode := 416;
      VMessage := E.Message;
    end;
    on E: Exception do begin
      VResultCode := 417;
      VMessage := E.Message;
    end;
  end;
  if VList = nil then begin
    VList := TInterfaceList.Create;
  end;
  if VList.Count = 0 then begin
    VResultCode := 404;
    VMessage := 'Не найдено';
  end;
  Result := TGeoCodeResult.Create(ASearch, VResultCode, VMessage, VList);
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

constructor EInternetOpenError.Create(Code: DWORD; const Msg: String);
begin
  inherited Create(Msg);
  ErrorCode := Code;
end;

end.
