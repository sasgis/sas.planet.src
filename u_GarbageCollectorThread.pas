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

unit u_GarbageCollectorThread;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_InternalPerformanceCounter,
  i_Listener,
  i_NotifierOperation,
  i_NotifierTime;

type
  TGarbageCollectorThread = class(TThread)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FNotifier: INotifierTimeInternal;
    FCounter: IInternalPerformanceCounter;

    FAppClosingListener: IListener;
    FCancelEvent: TEvent;
    FSleepTime: Cardinal;
    procedure SleepCancelable(ATime: Cardinal);
    procedure OnAppClosing;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const ACounter: IInternalPerformanceCounter;
      const ANotifier: INotifierTimeInternal;
      ASleepTime: Cardinal
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ListenerByEvent,
  u_ReadableThreadNames;

constructor TGarbageCollectorThread.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const ACounter: IInternalPerformanceCounter;
  const ANotifier: INotifierTimeInternal;
  ASleepTime: Cardinal
);
begin
  inherited Create(false);
  FAppClosingNotifier := AAppClosingNotifier;
  FCounter := ACounter;
  FNotifier := ANotifier;
  FSleepTime := ASleepTime;

  FCancelEvent := TEvent.Create;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TGarbageCollectorThread.Destroy;
begin
  FCancelEvent.SetEvent;

  if FAppClosingNotifier <> nil then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;

  FreeAndNil(FCancelEvent);
  inherited;
end;

procedure TGarbageCollectorThread.Execute;
var
  VNow: Cardinal;
  VContext: TInternalPerformanceCounterContext;
begin
  SetCurrentThreadName(AnsiString(Self.ClassName));
  while not Terminated do begin
    VContext := FCounter.StartOperation;
    try
      VNow := GetTickCount;
      FNotifier.Notify(VNow);
    finally
      FCounter.FinishOperation(VContext);
    end;
    SleepCancelable(FSleepTime);
  end;
end;

procedure TGarbageCollectorThread.OnAppClosing;
begin
  FCancelEvent.SetEvent;
  Terminate;
end;

procedure TGarbageCollectorThread.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

end.
