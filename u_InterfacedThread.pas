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

unit u_InterfacedThread;

interface

uses
  Classes,
  SysUtils,
  i_Notifier,
  i_Listener,
  i_ThreadConfig,
  i_Thread,
  u_BaseInterfacedObject;

type
  TInterfacedThread = class(TBaseInterfacedObject, IThread)
  private
    FConfig: IThreadConfig;
    FThread: TThread;
    FCS: IReadWriteSync;
    FTerminated: Boolean;
    FStarted: Boolean;
    FFinished: Boolean;
    FConfigListener: IListener;
    procedure OnTerminate(Sender: TObject);
    procedure OnConfigChange;
  protected
    procedure Execute; virtual; abstract;
    property Terminated: Boolean read FTerminated;
  protected
    procedure Start; virtual;
    procedure Terminate; virtual;
  public
    constructor Create(
      const AConfig: IThreadConfig;
      const ADebugName: AnsiString = ''
    );
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF EUREKALOG}
  ExceptionLog,
  {$ENDIF}
  u_ListenerByEvent,
  u_ReadableThreadNames,
  u_Synchronizer;

type
  TThread4InterfacedThread = class(TThread)
  private
    FRef: IInterface;
    FExec: TThreadMethod;
    FDebugName: AnsiString;
  protected
    procedure DoTerminate; override;
    procedure Execute; override;
  public
    constructor Create(
      APriority: TThreadPriority;
      const ADebugName: AnsiString;
      AExec: TThreadMethod
    );
    procedure Start(const ARef: IInterface);
  end;

{ TInterfacedThread }

constructor TInterfacedThread.Create(
  const AConfig: IThreadConfig;
  const ADebugName: AnsiString = ''
);
begin
  inherited Create;
  FConfig := AConfig;
  FCS := MakeSyncRW_Var(Self, False);
  FConfigListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FThread := TThread4InterfacedThread.Create(
    FConfig.Priority,
    ADebugName,
    Self.Execute
  );
  FThread.OnTerminate := Self.OnTerminate;
  FTerminated := False;
  FStarted := False;
  FFinished := False;
  FConfig.ChangeNotifier.Add(FConfigListener);
end;

destructor TInterfacedThread.Destroy;
var
  VNeedResume: Boolean;
begin
  VNeedResume := False;
  if Assigned(FConfig) and Assigned(FConfigListener) then begin
    FConfig.ChangeNotifier.Remove(FConfigListener);
    FConfigListener := nil;
    FConfig := nil;
  end;

  if Assigned(FCS) then begin
    FCS.BeginWrite;
    try
      if not FStarted then begin
        if Assigned(FThread) then begin
          FThread.OnTerminate := nil;
        end;
        VNeedResume := True;
      end;
    finally
      FCS.EndWrite;
    end;
  end;


  FCS := nil;

  if VNeedResume then begin
    if Assigned(FThread) then begin
      FThread.Resume;
    end;
  end;
  inherited;
end;

procedure TInterfacedThread.OnConfigChange;
begin
  FCS.BeginWrite;
  try
    if not FFinished then begin
      FThread.Priority := FConfig.Priority;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TInterfacedThread.OnTerminate(Sender: TObject);
begin
  FCS.BeginWrite;
  try
    FFinished := True
  finally
    FCS.EndWrite;
  end;
end;

procedure TInterfacedThread.Start;
begin
  FCS.BeginWrite;
  try
    if not FStarted then begin
      if not FTerminated then begin
        FStarted := True;
        TThread4InterfacedThread(FThread).Start(Self);
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TInterfacedThread.Terminate;
begin
  FCS.BeginWrite;
  try
    if not FTerminated then begin
      FTerminated := True;
      if not FFinished then begin
        FThread.Terminate;
      end;
    end;
  finally
    FCS.EndWrite;
  end;
end;

{ TThread4InterfacedThread }

constructor TThread4InterfacedThread.Create(
  APriority: TThreadPriority;
  const ADebugName: AnsiString;
  AExec: TThreadMethod
);
begin
  inherited Create(True);
  FDebugName := ADebugName;
  Self.Priority := APriority;
  Self.FreeOnTerminate := True;
  FExec := AExec;
end;

procedure TThread4InterfacedThread.DoTerminate;
begin
  inherited;
  FRef := nil;
end;

procedure TThread4InterfacedThread.Execute;
begin
  {$IFDEF EUREKALOG}
  try
  {$ENDIF}
    inherited;
    SetCurrentThreadName(FDebugName);
    if not Terminated then begin
      FExec;
    end;
  {$IFDEF EUREKALOG}
  except
    ShowLastExceptionData;
  end;
  {$ENDIF}
end;

procedure TThread4InterfacedThread.Start(const ARef: IInterface);
begin
  FRef := ARef;
  if not Terminated then begin
    Resume;
  end;
end;

end.
