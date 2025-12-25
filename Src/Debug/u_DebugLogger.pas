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

interface

{$I DebugLog.inc}

type
  TLogItem = record
    TimeStamp: TDateTime;
    ClassName: string;
    MessageText: string;
  end;

  IDebugLoggerConsumer = interface
    ['{0FDB2DFB-782A-4D35-8717-40931D3141B1}']
    procedure Process(const AItem: TLogItem);
  end;

  IDebugLogger = interface
    ['{AE1ADC4D-4EC4-46BD-8E03-80B057C92ACC}']
    procedure Write(const AObj: TObject; const AMsg: string); overload;
    procedure Write(const AObj: TObject; const AMsg: string; const AFmt: array of const); overload;

    function GetConsumer: IDebugLoggerConsumer;
    property Consumer: IDebugLoggerConsumer read GetConsumer;
  end;

  TLog = record
    class function BasePath: string; static;

    class function ToStr(const AValue: Boolean): string; overload; static;
    class function ToStr(const AValue: Integer): string; overload; static;
    class function ToStr(const AValue: TDateTime; const ATimeOnly: Boolean = False): string; overload; static;
    class function ToStr(const AValue: TLogItem; const AIncludeTimestamp: Boolean; const ATimeOnly: Boolean): string; overload; static;

    class function IfThen(const AValue: Boolean; const ATrue: string; const AFalse: string = ''): string; static;

    class function Format(const AFormat: string; const AArgs: array of const): string; static;
  end;

function BuildDebugLogger: IDebugLogger;

var
  GLog: IDebugLogger = nil;

implementation

uses
  Windows,
  SysUtils,
  StrUtils,
  Classes,
  SyncObjs,
  Generics.Collections,
  u_ExceptionManager,
  u_ReadableThreadNames,
  u_BaseInterfacedObject;

type
  TDebugLogger = class(TBaseInterfacedObject, IDebugLogger)
  protected
    FConsumer: IDebugLoggerConsumer;
    procedure WriteItem(const AItem: TLogItem); virtual;
  private
    { IDebugLogger }
    procedure Write(const AObj: TObject; const AMsg: string); overload;
    procedure Write(const AObj: TObject; const AMsg: string; const AFmt: array of const); overload;
    function GetConsumer: IDebugLoggerConsumer;
  public
    constructor Create(const AConsumer: IDebugLoggerConsumer);
  end;

  TDebugLoggerAsync = class(TDebugLogger)
  private
    const CQueueDepth = 10000;
  private
    FQueue: TThreadedQueue<TLogItem>;
    FThread: TThread;
    procedure OnExecute;
  protected
    procedure WriteItem(const AItem: TLogItem); override;
  public
    constructor Create(const AConsumer: IDebugLoggerConsumer);
    destructor Destroy; override;
  end;

  TDebugLoggerConsumerFake = class(TBaseInterfacedObject, IDebugLoggerConsumer)
  private
    { IDebugLoggerConsumer }
    procedure Process(const AItem: TLogItem);
  end;

  TDebugLoggerConsumerSimple = class(TBaseInterfacedObject, IDebugLoggerConsumer)
  private
    { IDebugLoggerConsumer }
    procedure Process(const AItem: TLogItem);
  end;

  TDebugLoggerConsumerToLogFile = class(TBaseInterfacedObject, IDebugLoggerConsumer)
  private
    const CFileName = 'debug.txt';
  private
    FLock: TCriticalSection;
    FStream: TFileStream;
  private
    { IDebugLoggerConsumer }
    procedure Process(const AItem: TLogItem);
  public
    constructor Create;
    destructor Destroy; override;
  end;

function BuildDebugLogger: IDebugLogger;
var
  VConsumer: IDebugLoggerConsumer;
begin
  VConsumer :=
    {$if defined(USE_FAKE_CONSUMER)}
    TDebugLoggerConsumerFake.Create;
    {$elseif defined(USE_LOG_FILE_CONSUMER)}
    TDebugLoggerConsumerToLogFile.Create;
    {$else}
    TDebugLoggerConsumerSimple.Create;
    {$ifend}

  {$ifdef ENABLE_ASYNC_LOGGER}
  Result := TDebugLoggerAsync.Create(VConsumer);
  {$else}
  Result := TDebugLogger.Create(VConsumer);
  {$endif}
end;

{ TLog }

class function TLog.BasePath: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'log' + PathDelim;
end;

class function TLog.Format(const AFormat: string; const AArgs: array of const): string;
begin
  Result := SysUtils.Format(AFormat, AArgs);
end;

class function TLog.IfThen(const AValue: Boolean; const ATrue, AFalse: string): string;
begin
  Result := StrUtils.IfThen(AValue, ATrue, AFalse);
end;

class function TLog.ToStr(const AValue: Boolean): string;
begin
  Result := SysUtils.BoolToStr(AValue, True);
end;

class function TLog.ToStr(const AValue: Integer): string;
begin
  Result := SysUtils.IntToStr(AValue);
end;

class function TLog.ToStr(const AValue: TDateTime; const ATimeOnly: Boolean): string;
begin
  if ATimeOnly then begin
    Result := SysUtils.FormatDateTime('hh:nn:ss.zzz', AValue);
  end else begin
    Result := SysUtils.FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', AValue);
  end;
end;

class function TLog.ToStr(const AValue: TLogItem; const AIncludeTimestamp: Boolean; const ATimeOnly: Boolean): string;
begin
  if AIncludeTimestamp then begin
    Result := TLog.ToStr(AValue.TimeStamp, ATimeOnly) + ' ';
  end else begin
    Result := '';
  end;
  Result := Result + IfThen(AValue.ClassName <> '', '[' + AValue.ClassName + '] ', '') + AValue.MessageText;
end;

{ TDebugLogger }

constructor TDebugLogger.Create(const AConsumer: IDebugLoggerConsumer);
begin
  Assert(AConsumer <> nil);
  inherited Create;
  FConsumer := AConsumer;
end;

function TDebugLogger.GetConsumer: IDebugLoggerConsumer;
begin
  Result := FConsumer;
end;

procedure TDebugLogger.WriteItem(const AItem: TLogItem);
begin
  FConsumer.Process(AItem);
end;

procedure TDebugLogger.Write(const AObj: TObject; const AMsg: string);
var
  VItem: TLogItem;
begin
  VItem.TimeStamp := Now;
  VItem.MessageText := AMsg;

  if AObj <> nil then begin
    VItem.ClassName := AObj.ClassName;
  end;

  Self.WriteItem(VItem);
end;

procedure TDebugLogger.Write(const AObj: TObject; const AMsg: string; const AFmt: array of const);
begin
  Self.Write(AObj, Format(AMsg, AFmt));
end;

{ TDebugLoggerAsync }

constructor TDebugLoggerAsync.Create(const AConsumer: IDebugLoggerConsumer);
begin
  inherited Create(AConsumer);

  FQueue := TThreadedQueue<TLogItem>.Create(CQueueDepth);

  FThread := TThread.CreateAnonymousThread(Self.OnExecute);
  FThread.FreeOnTerminate := False;
  FThread.Start;
end;

destructor TDebugLoggerAsync.Destroy;
begin
  if FThread <> nil then begin
    FThread.Terminate;
    FQueue.DoShutDown;
    FThread.WaitFor;
    FThread := nil;
  end;
  FreeAndNil(FQueue);
  inherited Destroy;
end;

procedure TDebugLoggerAsync.OnExecute;
var
  VItem: TLogItem;
begin
  SetCurrentThreadName(Self.ClassName);
  while not TThread.CheckTerminated do begin
    if FQueue.PopItem(VItem) = TWaitResult.wrSignaled then begin
      if FConsumer <> nil then
      try
        FConsumer.Process(VItem);
      except
        FConsumer := nil;
        FQueue.DoShutDown;
        TExceptionManager.ShowExceptionInfo;
        Exit;
      end;
    end else begin
      Exit;
    end;
  end;
end;

procedure TDebugLoggerAsync.WriteItem(const AItem: TLogItem);
begin
  FQueue.PushItem(AItem);
end;

{ TDebugLoggerConsumerFake }

procedure TDebugLoggerConsumerFake.Process(const AItem: TLogItem);
begin
 // nothing to do
end;

{ TDebugLoggerConsumerSimple }

procedure TDebugLoggerConsumerSimple.Process(const AItem: TLogItem);
begin
  OutputDebugString(PChar(TLog.ToStr(AItem, False, False)));
end;

{ TDebugLoggerConsumerToLogFile }

constructor TDebugLoggerConsumerToLogFile.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TDebugLoggerConsumerToLogFile.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FStream);
  inherited Destroy;
end;

procedure TDebugLoggerConsumerToLogFile.Process(const AItem: TLogItem);
var
  VMsg: RawByteString;
  VPath: string;
begin
  FLock.Acquire;
  try
    if FStream = nil then begin
      VPath := TLog.BasePath;
      if not ForceDirectories(VPath) then begin
        RaiseLastOSError;
      end;
      FStream := TFileStream.Create(VPath + CFileName, fmCreate or fmShareDenyWrite);
    end;
    VMsg := UTF8Encode(TLog.ToStr(AItem, True, False) + sLineBreak);
    FStream.WriteBuffer(Pointer(VMsg)^, Length(VMsg));
  finally
    FLock.Release;
  end;
end;

end.
