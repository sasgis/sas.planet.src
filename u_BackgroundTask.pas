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

unit u_BackgroundTask;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_JclNotify,
  i_OperationNotifier,
  i_BackgroundTask,
  u_OperationNotifier,
  u_InterfacedThread;

type
  TBackgroundTask = class(TInterfacedThread, IBackgroundTask)
  private
    FAppClosingNotifier: IJclNotifier;
    FCancelNotifierInternal: IOperationNotifierInternal;
    FCancelNotifier: IOperationNotifier;
    FStopThread: TEvent;
    FAllowExecute: TEvent;
    FCS: TCriticalSection;
    FAppClosingListener: IJclListener;
    procedure OnAppClosing;
  protected
    procedure ExecuteTask(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ); virtual; abstract;
    procedure Execute; override;
    procedure Terminate; override;
    property CancelNotifier: IOperationNotifier read FCancelNotifier;
  protected
    procedure StartExecute; virtual;
    procedure StopExecute; virtual;
  public
    constructor Create(
      AAppClosingNotifier: IJclNotifier;
      APriority: TThreadPriority = tpLowest);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TBackgroundTask }

constructor TBackgroundTask.Create(
  AAppClosingNotifier: IJclNotifier;
  APriority: TThreadPriority
);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create;
  FAppClosingNotifier := AAppClosingNotifier;
  FStopThread := TEvent.Create;
  FAllowExecute := TEvent.Create(nil, True, False, '');
  FCS := TCriticalSection.Create;
  SetPriority(APriority);
  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
end;

destructor TBackgroundTask.Destroy;
begin
  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingListener := nil;
  FAppClosingNotifier := nil;

  Terminate;
  FreeAndNil(FStopThread);
  FreeAndNil(FAllowExecute);
  FreeAndNil(FCS);
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
  VHandles[0] := FAllowExecute.Handle;
  VHandles[1] := FStopThread.Handle;
  while not Terminated do begin
    VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);
    case VWaitResult of
      WAIT_OBJECT_0:
      begin
        FCS.Acquire;
        try
          VOperatonID := FCancelNotifier.CurrentOperation;
        finally
          FCS.Release;
        end;
        ExecuteTask(VOperatonID, FCancelNotifier);
        FCS.Acquire;
        try
          if not FCancelNotifier.IsOperationCanceled(VOperatonID) then begin
            FAllowExecute.ResetEvent;
          end;
        finally
          FCS.Release;
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
  FCS.Acquire;
  try
    FAllowExecute.SetEvent;
  finally
    FCS.Release;
  end;
end;

procedure TBackgroundTask.StopExecute;
begin
  FCS.Acquire;
  try
    FCancelNotifierInternal.NextOperation;
    FAllowExecute.ResetEvent;
  finally
    FCS.Release;
  end;
end;

procedure TBackgroundTask.Terminate;
begin
  StopExecute;
  inherited Terminate;
  FStopThread.SetEvent;
end;

end.
