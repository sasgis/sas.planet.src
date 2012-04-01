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

unit u_BackgroundTask;

interface

uses
  Windows,
  Classes,
  i_JclNotify,
  i_OperationNotifier,
  i_ThreadConfig,
  i_BackgroundTask,
  u_OperationNotifier,
  u_InterfacedThread;

type
  TBackgroundTask = class(TInterfacedThread, IBackgroundTask)
  private
    FAppClosingNotifier: IJclNotifier;
    FCancelNotifierInternal: IOperationNotifierInternal;
    FCancelNotifier: IOperationNotifier;
    FStopThreadHandle: THandle;
    FAllowExecuteHandle: THandle;
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
      AThreadConfig: IThreadConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TBackgroundTask }

constructor TBackgroundTask.Create(
  AAppClosingNotifier: IJclNotifier;
  AThreadConfig: IThreadConfig
);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create(AThreadConfig);
  FAppClosingNotifier := AAppClosingNotifier;
  FStopThreadHandle := CreateEvent(nil, TRUE, FALSE, nil);
  FAllowExecuteHandle := CreateEvent(nil, TRUE, FALSE, nil);
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
        VOperatonID := FCancelNotifier.CurrentOperation;

        ExecuteTask(VOperatonID, FCancelNotifier);

        if Terminated then
          Exit;

        if not FCancelNotifier.IsOperationCanceled(VOperatonID) then begin
          ResetEvent(FAllowExecuteHandle);
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
