{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_TileRequestBuilderHelpers;

interface

function Rand(X: Integer): Integer;            
function GetUnixTime: Int64;
function StrLength (const Str: string): Integer;
function GetAfter(SubStr, Str: string): string;
function GetBefore(SubStr, Str: string): string;
function GetBetween(Str, After, Before: string): string;
function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer;
function SetHeaderValue(AHeaders, AName, AValue: string): string;
function GetHeaderValue(AHeaders, AName: string): string;
function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal;
function GetNumberAfter(const ASubStr, AText: String): String;
function GetDiv3Path(const ASource: String): String;
function DownloadFileToLocal(const AFullRemoteUrl, AFullLocalFilename, AContType: String): Integer;

// auxillary
function DoHttpRequestEx(const ARequestUrl, ARequestHeader, APostData: string;
                         out AResponseHeader, AResponseData: string;
                         const ASaveToLocal: Boolean;
                         const ALocalFileName: String;
                         const AContentType: String): Cardinal;

implementation

uses
  SysUtils,
  Classes,
  DateUtils,
  RegExpr,
  ALHTTPCommon,
  ALHttpClient,
  ALWinInetHttpClient,
  i_InetConfig,
  i_ProxySettings,
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

function DoHttpRequestEx(const ARequestUrl, ARequestHeader, APostData: string;
                         out AResponseHeader, AResponseData: string;
                         const ASaveToLocal: Boolean;
                         const ALocalFileName: String;
                         const AContentType: String): Cardinal;
var
  VHttpClient: TALWinInetHTTPClient;
  VHttpResponseHeader: TALHTTPResponseHeader;
  VHttpResponseBody: TMemoryStream;
  VHttpPostData: TMemoryStream;
  VInetConfig: IInetConfigStatic;
  VProxyConfig: IProxyConfigStatic;
  VTmp:TStringList;
  VPath: String;
begin
  try
    VHttpClient := TALWinInetHTTPClient.Create(nil);
    try
      VHttpResponseHeader := TALHTTPResponseHeader.Create;
      try
        // config
        VInetConfig := GState.InetConfig.GetStatic;
        VHttpClient.RequestHeader.RawHeaderText := ARequestHeader;
        VHttpClient.RequestHeader.Accept := '*/*';
        VHttpClient.ConnectTimeout := VInetConfig.TimeOut;
        VHttpClient.SendTimeout := VInetConfig.TimeOut;
        VHttpClient.ReceiveTimeout := VInetConfig.TimeOut;
        VHttpClient.InternetOptions := [  wHttpIo_No_cache_write,
                                          wHttpIo_Pragma_nocache,
                                          wHttpIo_No_cookies,
                                          wHttpIo_Ignore_cert_cn_invalid,
                                          wHttpIo_Ignore_cert_date_invalid
                                       ];
        VProxyConfig := VInetConfig.ProxyConfigStatic;
        if Assigned(VProxyConfig) then begin
          if VProxyConfig.UseIESettings then begin
            VHttpClient.AccessType := wHttpAt_Preconfig
          end else if VProxyConfig.UseProxy then begin
            VHttpClient.AccessType := wHttpAt_Proxy;
            VHttpClient.ProxyParams.ProxyServer :=
              Copy(VProxyConfig.Host, 0, Pos(':', VProxyConfig.Host) - 1);
            VHttpClient.ProxyParams.ProxyPort :=
              StrToInt(Copy(VProxyConfig.Host, Pos(':', VProxyConfig.Host) + 1));
            if VProxyConfig.UseLogin then begin
              VHttpClient.ProxyParams.ProxyUserName := VProxyConfig.Login;
              VHttpClient.ProxyParams.ProxyPassword := VProxyConfig.Password;
            end;
          end else begin
            VHttpClient.AccessType := wHttpAt_Direct;
          end;
        end;
        // request
        VHttpResponseBody := TMemoryStream.Create;
        try
          VTmp := TStringList.Create;
          try
            if APostData <> '' then begin
              VHttpPostData := TMemoryStream.Create;
              try
                VHttpPostData.Position := 0;
                VTmp.Text := APostData;
                VTmp.SaveToStream(VHttpPostData);
                VHttpClient.Post(ARequestUrl, VHttpPostData, VHttpResponseBody, VHttpResponseHeader);
              finally
                VHttpPostData.Free;
              end;
            end else begin
              VHttpClient.Get(ARequestUrl, VHttpResponseBody, VHttpResponseHeader);
            end;
            Result := StrToIntDef(VHttpResponseHeader.StatusCode, 0);
            AResponseHeader := VHttpResponseHeader.RawHeaderText;
            if VHttpResponseBody.Size > 0 then begin
              VHttpResponseBody.Position := 0;
              VTmp.Clear;
              VTmp.LoadFromStream(VHttpResponseBody);
              AResponseData := VTmp.Text;
              // to file
              if ASaveToLocal then
              if (0=Length(AContentType)) or (System.Pos(AContentType,AResponseHeader)>0) then begin
                VPath:=ExtractFilePath(ALocalFileName);
                if (not DirectoryExists(VPath)) then
                  ForceDirectories(VPath);
                VHttpResponseBody.Position:=0;
                VHttpResponseBody.SaveToFile(ALocalFileName);
              end;
            end;
          finally
            VTmp.Free;
          end;
        finally
          VHttpResponseBody.Free;
        end;
      finally
        VHttpResponseHeader.Free;
      end;
    finally
      VHttpClient.Free;
    end;
  except
    on E: EALHTTPClientException do begin
      Result := E.StatusCode;
      AResponseHeader := '';
      AResponseData := E.Message;
    end;
    on E: EOSError do begin
      Result := E.ErrorCode;
      AResponseHeader := '';
      AResponseData := E.Message;
    end;
  end;
end;

function DoHttpRequest(const ARequestUrl, ARequestHeader, APostData: string; out AResponseHeader, AResponseData: string): Cardinal;
begin
  Result := DoHttpRequestEx(ARequestUrl, ARequestHeader, APostData,
                            AResponseHeader, AResponseData,
                            FALSE, '', '');
end;

function GetNumberAfter(const ASubStr, AText: String): String;
var VPos: Integer;
begin
  Result := '';
  VPos:=SYstem.Pos(ASubStr,AText);
  if (VPos>0) then begin
    VPos := VPos + Length(ASubStr);
    while ((VPos<=System.Length(AText)) and (AText[VPos] in ['0','1'..'9'])) do begin
      Result := Result + AText[VPos];
      Inc(VPos);
    end;
  end;
end;

function GetDiv3Path(const ASource: String): String;
var i: Integer;
begin
  Result:='';

  if (0<Length(ASource)) then
  for i := Length(ASource) downto 1 do begin
    if (0 = ((Length(ASource)-i) mod 3)) then
      Result := '\' + Result;
    Result := ASource[i] + Result;
  end;

  if (Length(Result)>0) then
    if ('\'=Result[1]) then
      System.Delete(Result,1,1);

  i := System.Pos('\',Result);
  if (i<4) then
    System.Delete(Result, 1, i);
end;

function DownloadFileToLocal(const AFullRemoteUrl, AFullLocalFilename, AContType: String): Integer;
var VResponseHeader, VResponseData: string;
begin
  Result := DoHttpRequestEx(AFullRemoteUrl, '', '',
                            VResponseHeader, VResponseData,
                            TRUE, AFullLocalFilename, AContType);
end;

end.