{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_DownloaderHttpLog;

interface

{$IFDEF DEBUG}
  {$I Network.inc}
{$ENDIF}

uses
  Windows,
  Classes,
  SysUtils,
  EDBase64;

type
  TDownloaderHttpLogMsgType = (hmtRaw, hmtInfo, hmtError);

  TDownloaderHttpLog = class
  private
    FLogStream: TFileStream;
  public
    procedure Write(const AMsgType: TDownloaderHttpLogMsgType; const AMsg: string); overload;
    procedure Write(const AMsgType: TDownloaderHttpLogMsgType; const AFmt: string; const AArgs: array of const); overload;
    procedure Write(const AData: TMemoryStream); overload;
  public
    constructor Create(const ADownloaderId: string);
    destructor Destroy; override;
  end;

implementation

var
  GLogIdCounter: Integer = 0;

function _GetTimeStamp: string;
begin
  Result := FormatDateTime('hh:nn:ss.zzz', Now);
end;

{ TDownloaderHttpLog }

constructor TDownloaderHttpLog.Create(const ADownloaderId: string);
var
  VLogId: Integer;
  VLogFileName: string;
begin
  VLogId := InterlockedIncrement(GLogIdCounter);
  VLogFileName := Format('%s\log\http\%.4d_%s.txt', [ExtractFileDir(ParamStr(0)), VLogId, ADownloaderId]);
  ForceDirectories(ExtractFileDir(VLogFileName));
  FLogStream := TFileStream.Create(VLogFileName, fmCreate or fmShareDenyWrite);
end;

destructor TDownloaderHttpLog.Destroy;
begin
  FreeAndNil(FLogStream);
  inherited Destroy;
end;

procedure TDownloaderHttpLog.Write(const AMsgType: TDownloaderHttpLogMsgType; const AMsg: string);
var
  VMsg: string;
  VText: AnsiString;
begin
  case AMsgType of
    hmtRaw: begin
      VMsg := AMsg;
    end;
    hmtInfo: begin
      VMsg := Format('[INF] %s tid=%d; %s', [_GetTimeStamp, GetCurrentThreadId, AMsg]);
    end;
    hmtError: begin
      VMsg := Format('[ERR] %s tid=%d; %s', [_GetTimeStamp, GetCurrentThreadId, AMsg]);
    end;
  else
    raise Exception.CreateFmt('Unexpected message type: %d', [Integer(AMsgType)]);
  end;

  VText := UTF8Encode(VMsg) + #13#10;
  FLogStream.WriteBuffer(PAnsiChar(VText)^, Length(VText));
end;

procedure TDownloaderHttpLog.Write(const AMsgType: TDownloaderHttpLogMsgType; const AFmt: string; const AArgs: array of const);
begin
  Write(AMsgType, Format(AFmt, AArgs));
end;

procedure TDownloaderHttpLog.Write(const AData: TMemoryStream);
var
  VData: AnsiString;
  VText: AnsiString;
begin
  {$IFDEF WRITE_HTTP_LOG_WITH_BODY}
  VData := '; data=' + Base64Encode(AData.Memory, AData.Size);
  {$ELSE}
  VData := '';
  {$ENDIF}
  VText := AnsiString(Format('[DAT] %s tid=%d; size=%d', [_GetTimeStamp, GetCurrentThreadId, AData.Size])) + VData + #13#10;
  FLogStream.WriteBuffer(PAnsiChar(VText)^, Length(VText));
end;

end.
