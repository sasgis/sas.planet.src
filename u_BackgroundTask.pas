{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_BackgroundTask;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_OperationNotifier,
  i_BackgroundTask,
  u_OperationNotifier,
  u_InterfacedThread;

type
  TBackgroundTask = class(TInterfacedThread, IBackgroundTask)
  private
    FCancelNotifierInternal: IOperationNotifierInternal;
    FCancelNotifier: IOperationNotifier;
    FStopThread: TEvent;
    FAllowExecute: TEvent;
    FCS: TCriticalSection;
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
    constructor Create(APriority: TThreadPriority = tpLowest);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TBackgroundTask }

constructor TBackgroundTask.Create(APriority: TThreadPriority);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create;
  FStopThread := TEvent.Create(nil, True, False, '');
  FAllowExecute := TEvent.Create(nil, True, False, '');
  FCS := TCriticalSection.Create;
  SetPriority(APriority);
  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
end;

destructor TBackgroundTask.Destroy;
begin
  Terminate;
  FreeAndNil(FStopThread);
  FreeAndNil(FAllowExecute);
  FreeAndNil(FCS);
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
