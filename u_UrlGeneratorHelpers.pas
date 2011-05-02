unit u_UrlGeneratorHelpers;

interface

function Rand(X: Integer): Integer;            
function GetUnixTime: Int64;
function StrLength (const Str: string): Integer;
function GetAfter(SubStr, Str: string): string;
function GetBefore(SubStr, Str: string): string;
function GetBetween(Str, After, Before: string): string;
function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer;
function RegExprGetMatchSubStr(const AStr, AMatchExpr: string; AMatchID: Integer): string;
function RegExprReplaceMatchSubStr(const AStr, AMatchExpr, AReplace: string): string;
function SetHeaderValue(AHeaders, AName, AValue: string): string;
function GetHeaderValue(AHeaders, AName: string): string;
function DoRequest(const AHost, ADoc, ARequestHeader, APostData: AnsiString; UseSSL: Boolean; out AResponseHeader, AResponseData: AnsiString): Cardinal;

implementation

uses
  Windows,
  Classes,
  SysUtils,
  DateUtils,
  RegExpr,
  WinInet,
  u_GlobalState;

function Rand(X: Integer): Integer;
begin
  Result := Random(X);
end;

function GetUnixTime: Int64;
begin
  Result := DateTimeToUnix(now);
end;

function StrLength (const Str: string): Integer;
begin
  Result := Length(Str);
end;

function GetAfter(SubStr, Str: string): string;
begin
  if pos(substr,str) > 0 then
    result := copy(str,pos(substr,str)+length(substr),length(str))
  else
    result := '';
end;

function GetBefore(SubStr, Str: string): string;
begin
  if pos(substr,str)>0 then
    result := copy(str,1,pos(substr,str)-1)
  else
    result := '';
end;

function GetBetween(Str, After, Before: string): string;
begin
  result := GetBefore(Before,GetAfter(After,str));
end;

function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer; assembler;
asm
      PUSH EDI
      PUSH ESI
      PUSH EBX
      PUSH EAX
      OR EAX,EAX
      JE @@2
      OR EDX,EDX
      JE @@2
      DEC ECX
      JS @@2

      MOV EBX,[EAX-4]
      SUB EBX,ECX
      JLE @@2
      SUB EBX,[EDX-4]
      JL @@2
      INC EBX

      ADD EAX,ECX
      MOV ECX,EBX
      MOV EBX,[EDX-4]
      DEC EBX
      MOV EDI,EAX
 @@1: MOV ESI,EDX
      LODSB
      REPNE SCASB
      JNE @@2
      MOV EAX,ECX
      PUSH EDI
      MOV ECX,EBX
      REPE CMPSB
      POP EDI
      MOV ECX,EAX
      JNE @@1
      LEA EAX,[EDI-1]
      POP EDX
      SUB EAX,EDX
      INC EAX
      JMP @@3
 @@2: POP EAX
      XOR EAX,EAX
 @@3: POP EBX
      POP ESI
      POP EDI
end;

function RegExprGetMatchSubStr(const AStr, AMatchExpr: string; AMatchID: Integer): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
    begin
      if (AMatchID <= VRegExpr.SubExprMatchCount) and (AMatchID >= 0) then
        Result := VRegExpr.Match[AMatchID]
      else
        Result := '';
    end
    else
      Result := '';
  finally
    FreeAndNil(VRegExpr);
  end;
end;

function RegExprReplaceMatchSubStr(const AStr, AMatchExpr, AReplace: string): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
      Result := VRegExpr.Replace(AStr, AReplace, True)
    else
      Result := AStr;
  finally
    FreeAndNil(VRegExpr);
  end;
end;

function SetHeaderValue(AHeaders, AName, AValue: string): string;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then
  begin
      VRegExpr  := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then
        Result := StringReplace(AHeaders, VRegExpr.Match[2], AValue, [rfIgnoreCase])
      else
        Result := AName + ': ' + AValue + #13#10 + AHeaders;
    finally
      FreeAndNil(VRegExpr);
    end;
  end
  else
    Result := AName + ': ' + AValue + #13#10;
end;

function GetHeaderValue(AHeaders, AName: string): string;
var
  VRegExpr: TRegExpr;
begin
  if AHeaders <> '' then
  begin
      VRegExpr  := TRegExpr.Create;
    try
      VRegExpr.Expression := '(?i)' + AName + ':(\s+|)(.*?)(\r\n|$)';
      if VRegExpr.Exec(AHeaders) then
        Result := VRegExpr.Match[2]
      else
        Result := '';
    finally
      FreeAndNil(VRegExpr);
    end;
  end
  else
    Result := '';
end;

{$WARNINGS OFF}

function DoRequest(const AHost, ADoc, ARequestHeader, APostData: AnsiString; UseSSL: Boolean; out AResponseHeader, AResponseData: AnsiString): Cardinal;
var
  aBuffer     : array [0..4096] of Char;
  BufStream   : TMemoryStream;
  sMethod     : AnsiString;
  BytesRead   : Cardinal;
  pSession    : HINTERNET;
  pConnection : HINTERNET;
  pRequest    : HINTERNET;
  Port        : Integer;
  flags       : DWORD;
  dwIndex     : Cardinal;
  VBufSize    : Cardinal;
  VStatusCode : Cardinal;
  //VTimeOut    : Cardinal; // TODO
  VUseIE      : Boolean;
  VUseProxy   : Boolean;
  VProxyHost  : string;
  VUselogin   : Boolean;
  VLogin      : string;
  VPassword   : string;
begin
  try
    Result := 9; // dtrUnknownError

      GState.InetConfig.LockWrite;
    try
      //VTimeOut := GState.InetConfig.GetTimeOut;
      VUseIE := GState.InetConfig.ProxyConfig.GetUseIESettings;
      if not VUseIE  then
      begin
        VUseProxy := GState.InetConfig.ProxyConfig.GetUseProxy;
        if VUseProxy then
        begin
          VProxyHost := GState.InetConfig.ProxyConfig.GetHost;
          VUselogin := GState.InetConfig.ProxyConfig.GetUseLogin;
          if VUselogin then
          begin
            VLogin := GState.InetConfig.ProxyConfig.GetLogin;
            VPassword := GState.InetConfig.ProxyConfig.GetPassword;
          end;
        end;
      end
      else
        VUselogin := False;
    finally
      GState.InetConfig.UnlockWrite;
    end;

    if VUseIE then
      pSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0)
    else
      if VUseProxy then
      begin
        pSession := InternetOpen(nil, INTERNET_OPEN_TYPE_PROXY, PChar(VProxyHost), nil, 0);
        if Assigned(pSession) and VUselogin then
        begin
          InternetSetOption(pSession, INTERNET_OPTION_PROXY_USERNAME, PChar(VLogin), Length(VLogin));
          InternetSetOption(pSession, INTERNET_OPTION_PROXY_PASSWORD, PChar(VPassword), Length(VPassword));
        end;
      end
      else
        pSession := InternetOpen(nil, INTERNET_OPEN_TYPE_DIRECT, nil, nil, 0);

    if Assigned(pSession) then
    try
      if UseSSL then
        Port := INTERNET_DEFAULT_HTTPS_PORT
      else
        Port := INTERNET_DEFAULT_HTTP_PORT;

      pConnection := InternetConnect(pSession, PChar(AHost), Port, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);

      if Assigned(pConnection) then
      try
        if (APostData = '') then
          sMethod := 'GET'
        else
          sMethod := 'POST';

        if UseSSL then
          flags := INTERNET_FLAG_SECURE or INTERNET_FLAG_NO_COOKIES
        else
          flags := INTERNET_SERVICE_HTTP or INTERNET_FLAG_NO_COOKIES;

        pRequest := HTTPOpenRequest(pConnection, PChar(sMethod), PChar(ADoc), nil, nil, nil, flags, 0);

        if Assigned(pRequest) then
        try
          HttpAddRequestHeaders(pRequest, PChar(ARequestHeader), Length(ARequestHeader), HTTP_ADDREQ_FLAG_ADD);

          if HTTPSendRequest(pRequest, nil, 0, Pointer(APostData), Length(APostData)) then
          begin

            // Response Status Code
            try
              VBufSize := sizeof(VStatusCode);
              dwIndex := 0;
              if HttpQueryInfo(pRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @VStatusCode, VBufSize, dwIndex) then
              begin
                Result := VStatusCode;
              end
              else
                Exit;
            except

            end;

            // Headers
            try
              AResponseHeader := '';
              VBufSize := 1024;
              SetLength(AResponseHeader, VBufSize);
              FillChar(AResponseHeader[1], VBufSize, 0);
              dwIndex := 0;
              if not HttpQueryInfo(pRequest, HTTP_QUERY_RAW_HEADERS_CRLF, @AResponseHeader[1], VBufSize, dwIndex) then
              begin
                if GetLastError = ERROR_INSUFFICIENT_BUFFER then
                begin
                  SetLength(AResponseHeader, VBufSize);
                  FillChar(AResponseHeader[1], VBufSize, 0);
                  dwIndex := 0;
                  if not HttpQueryInfo(pRequest, HTTP_QUERY_RAW_HEADERS_CRLF, @AResponseHeader[1], VBufSize, dwIndex) then
                    AResponseHeader := ''
                  else
                    SetLength(AResponseHeader, VBufSize);
                end
                else
                  AResponseHeader := '';
              end
              else
                SetLength(AResponseHeader, VBufSize);
            except
              AResponseHeader := '';
            end;

            // Body
              BufStream := TMemoryStream.Create;
            try
              while InternetReadFile(pRequest, @aBuffer, SizeOf(aBuffer), BytesRead) do
              begin
                if (BytesRead = 0) then
                  Break;
                BufStream.Write(aBuffer, BytesRead);
              end;
              SetLength(AResponseData, BufStream.Size);
              FillChar(AResponseData[1], Length(AResponseData), 0);
              BufStream.Position := 0;
              BufStream.Read(Pointer(AResponseData)^, Length(AResponseData));
            finally
              BufStream.Free;
            end;

          end;
        finally
          InternetCloseHandle(pRequest);
        end;
      finally
        InternetCloseHandle(pConnection);
      end;
    finally
      InternetCloseHandle(pSession);
    end
    else
      Result := 2; // dtrErrorInternetOpen
  except

  end;
end;

end.
