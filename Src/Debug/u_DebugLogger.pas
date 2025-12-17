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

unit u_DebugLogger;

{$DEFINE ENABLE_ASYNC_LOG}

{$IFDEF RELEASE}
  {$UNDEF ENABLE_ASYNC_LOG} // disable this if you need full support in Release build
{$ENDIF}

interface

uses
  SysUtils;

type
  IDebugLogger = interface
  ['{AE1ADC4D-4EC4-46BD-8E03-80B057C92ACC}']
    function ToStr(const AValue: Boolean): string; overload;
    function ToStr(const AValue: Integer): string; overload;

    procedure Write(const AObj: TObject; const AMsg: string); overload;
    procedure Write(const AObj: TObject; const AMsg: string; const AFmt: array of const); overload;
  end;

var
  GLog: IDebugLogger = nil;

function GetLogsPath: string;

implementation

uses
  Windows,
  StrUtils,
  {$IFDEF ENABLE_ASYNC_LOG}
  Classes,
  SyncObjs,
  Generics.Collections,
  {$ENDIF}
  u_BaseInterfacedObject;

type
  {$IFDEF ENABLE_ASYNC_LOG}
  TLogOutputType = (lotOutputDebugString, lotFile);
  {$ENDIF}

  TDebugLogger = class(TBaseInterfacedObject, IDebugLogger)
  private
    {$IFDEF ENABLE_ASYNC_LOG}
    FQueue: TThreadedQueue<string>;
    FThread: TThread;
    FOutputType: TLogOutputType;
    FLogFileName: string;
    FLogFileStream: TFileStream;

    procedure OnLogThreadExecute;
    {$ENDIF}
  private
    { IDebugLogger }
    function ToStr(const AValue: Boolean): string; overload;
    function ToStr(const AValue: Integer): string; overload;

    procedure Write(const AObj: TObject; const AMsg: string); overload;
    procedure Write(const AObj: TObject; const AMsg: string; const AFmt: array of const); overload;
  public
    {$IFDEF ENABLE_ASYNC_LOG}
    constructor Create(const AOutputType: TLogOutputType);
    destructor Destroy; override;
    {$ENDIF}
  end;

{$IFDEF ENABLE_ASYNC_LOG}
const
  CQueueDepth = 10000;

{ TDebugLogger }

constructor TDebugLogger.Create(const AOutputType: TLogOutputType);
var
  VLogPath: string;
begin
  inherited Create;

  FOutputType := AOutputType;

  if FOutputType = lotFile then begin
    VLogPath := GetLogsPath;

    if not ForceDirectories(VLogPath) then begin
      RaiseLastOSError;
    end;

    FLogFileName := VLogPath + 'debug.txt';
  end;

  FQueue := TThreadedQueue<string>.Create(CQueueDepth);

  FThread := TThread.CreateAnonymousThread(Self.OnLogThreadExecute);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

destructor TDebugLogger.Destroy;
begin
  if FThread <> nil then begin
    FThread.Terminate;
    FQueue.DoShutDown;
    FThread.WaitFor;
    FThread := nil;
  end;
  FreeAndNil(FQueue);
  FreeAndNil(FLogFileStream);
  inherited Destroy;
end;

procedure TDebugLogger.OnLogThreadExecute;
var
  VMsg: string;
  VMsgUtf8: RawByteString;
begin
  while not TThread.CheckTerminated do begin
    if FQueue.PopItem(VMsg) = TWaitResult.wrSignaled then begin
      case FOutputType of
        lotOutputDebugString: begin
          OutputDebugString(PChar(VMsg));
        end;
        lotFile: begin
          if FLogFileName <> '' then begin
            if FLogFileStream = nil then begin
              FLogFileStream := TFileStream.Create(FLogFileName, fmCreate or fmShareDenyWrite);
            end;
            VMsgUtf8 := UTF8Encode(VMsg) + sLineBreak;
            FLogFileStream.WriteBuffer(Pointer(VMsgUtf8)^, Length(VMsgUtf8));
          end;
        end;
      end;
    end else begin
      Exit;
    end;
  end;
end;
{$ENDIF}

procedure TDebugLogger.Write(const AObj: TObject; const AMsg: string);
var
  VMsg: string;
begin
  VMsg := '[' + IfThen(AObj <> nil, AObj.ClassName, 'nil') + '] ' + AMsg;

  {$IFDEF ENABLE_ASYNC_LOG}
  if FOutputType = lotFile then begin
    VMsg := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now) + ' ' + VMsg;
  end;
  FQueue.PushItem(VMsg);
  {$ELSE}
  OutputDebugString(PChar(VMsg));
  {$ENDIF}
end;

procedure TDebugLogger.Write(const AObj: TObject; const AMsg: string; const AFmt: array of const);
begin
  Self.Write(AObj, Format(AMsg, AFmt));
end;

function TDebugLogger.ToStr(const AValue: Boolean): string;
begin
  Result := BoolToStr(AValue, True);
end;

function TDebugLogger.ToStr(const AValue: Integer): string;
begin
  Result := IntToStr(AValue);
end;

function GetLogsPath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'log' + PathDelim;
end;

initialization
  GLog := TDebugLogger.Create({$IFDEF ENABLE_ASYNC_LOG} lotOutputDebugString {$ENDIF});

end.
