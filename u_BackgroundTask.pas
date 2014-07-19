{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BackgroundTask;

interface

uses
  Windows,
  i_Listener,
  i_NotifierOperation,
  i_ThreadConfig,
  i_BackgroundTask,
  u_InterfacedThread;

type
  TBackgroundTaskExecuteEvent =
    procedure(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ) of object;

  TBackgroundTask = class(TInterfacedThread, IBackgroundTask)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FOnExecute: TBackgroundTaskExecuteEvent;
    FCancelNotifierInternal: INotifierOperationInternal;
    FCancelNotifier: INotifierOperation;
    FStopThreadHandle: THandle;
    FAllowExecuteHandle: THandle;
    FAppClosingListener: IListener;
    procedure OnAppClosing;
  protected
    procedure Execute; override;
    procedure Terminate; override;
    property CancelNotifier: INotifierOperation read FCancelNotifier;
  protected
    procedure StartExecute;
    procedure StopExecute;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      AOnExecute: TBackgroundTaskExecuteEvent;
      const AThreadConfig: IThreadConfig;
      const AThreadDebugName: string
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Notifier,
  u_NotifierOperation,
  u_Synchronizer,
  u_ListenerByEvent;

{ TBackgroundTask }

constructor TBackgroundTask.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  AOnExecute: TBackgroundTaskExecuteEvent;
  const AThreadConfig: IThreadConfig;
  const AThreadDebugName: string
);
var
  VOperationNotifier: TNotifierOperation;
begin
  inherited Create(AThreadConfig, AThreadDebugName);
  FOnExecute := AOnExecute;
  FAppClosingNotifier := AAppClosingNotifier;
  Assert(Assigned(FOnExecute));
  FStopThreadHandle := CreateEvent(nil, TRUE, FALSE, nil);
  FAllowExecuteHandle := CreateEvent(nil, TRUE, FALSE, nil);
  VOperationNotifier :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TBackgroundTask.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;

  Terminate;
  CloseHandle(FStopThreadHandle);
  CloseHandle(FAllowExecuteHandle);
  FCancelNotifierInternal := nil;
  FCancelNotifier := nil;

  inherited;
end;

procedure TBackgroundTask.Execute;
var
  VHandles: array [0..1] of THandle;
  VWaitResult: DWORD;
  VOperatonID: Integer;
begin
  inherited;
  VHandles[0] := FAllowExecuteHandle;
  VHandles[1] := FStopThreadHandle;
  while not Terminated do begin
    VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
    case VWaitResult of
      WAIT_OBJECT_0:
      begin
        ResetEvent(FAllowExecuteHandle);
        VOperatonID := FCancelNotifier.CurrentOperation;

        if Assigned(FOnExecute) then begin
          FOnExecute(VOperatonID, FCancelNotifier);
        end;

        if Terminated then begin
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TBackgroundTask.OnAppClosing;
begin
  Terminate;
end;

procedure TBackgroundTask.StartExecute;
begin
  SetEvent(FAllowExecuteHandle);
end;

procedure TBackgroundTask.StopExecute;
begin
  FCancelNotifierInternal.NextOperation;
  ResetEvent(FAllowExecuteHandle);
end;

procedure TBackgroundTask.Terminate;
begin
  StopExecute;
  inherited Terminate;
  SetEvent(FStopThreadHandle);
end;

end.
