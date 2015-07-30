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

unit u_MarkSystemImplChangeable;

interface

uses
  i_PathConfig,
  i_Listener,
  i_Notifier,
  i_NotifierOperation,
  i_BackgroundTask,
  i_ReadWriteState,
  i_MarkSystemConfig,
  i_MarkSystemImplConfig,
  i_MarkSystemImplFactory,
  i_MarkSystemImpl,
  i_MarkSystemImplChangeable,
  u_ChangeableBase,
  u_ReadWriteStateInternalByOther;

type
  TMarkSystemImplChangeable = class(TChangeableWithSimpleLockBase, IMarkSystemImplChangeable)
  private
    FBasePath: IPathConfig;
    FConfigList: IMarkSystemConfigListChangeable;
    FImplFactoryList: IMarkSystemImplFactoryListStatic;
    FErrorNotifierInternal: INotifierInternal;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FStatic: IMarkSystemImpl;
    FBackgroundTask: IBackgroundTask;

    FState: IReadWriteStateChangeble;
    FStateInternal: IReadWriteStateInternalByOther;

    FConfigChangeListener: IListener;
    FAppStartedListener: IListener;
  private
    procedure OnConfigChanged;
    procedure OnAppStarted;
    procedure OnInitialization(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
  private
    function GetState: IReadWriteStateChangeble;
    function GetStatic: IMarkSystemImpl;
  public
    constructor Create(
      const ABasePath: IPathConfig;
      const AConfig: IMarkSystemConfigListChangeable;
      const AImplFactoryList: IMarkSystemImplFactoryListStatic;
      const AErrorNotifierInternal: INotifierInternal;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  SysUtils,
  i_MarkSystemErrorMsg,
  u_MarkSystemErrorMsg,
  u_BackgroundTask,
  u_ThreadConfig,
  u_ListenerByEvent;

{ TMarksSystemImplChangeable }

constructor TMarkSystemImplChangeable.Create(
  const ABasePath: IPathConfig;
  const AConfig: IMarkSystemConfigListChangeable;
  const AImplFactoryList: IMarkSystemImplFactoryListStatic;
  const AErrorNotifierInternal: INotifierInternal;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation
);
begin
  Assert(Assigned(ABasePath));
  Assert(Assigned(AImplFactoryList));
  Assert(Assigned(AErrorNotifierInternal));
  inherited Create;
  FBasePath := ABasePath;
  FConfigList := AConfig;
  FImplFactoryList := AImplFactoryList;
  FErrorNotifierInternal := AErrorNotifierInternal;
  FAppStartedNotifier := AAppStartedNotifier;
  FAppClosingNotifier := AAppClosingNotifier;
  FStateInternal := TReadWriteStateInternalByOther.Create;
  FState := FStateInternal;

  FBackgroundTask :=
    TBackgroundTask.Create(
      FAppClosingNotifier,
      Self.OnInitialization,
      TThreadConfig.Create(tpNormal),
      Self.ClassName
    );

  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChanged);
  FBasePath.ChangeNotifier.Add(FConfigChangeListener);
  FConfigList.ChangeNotifier.Add(FConfigChangeListener);

  FAppStartedListener := TNotifyNoMmgEventListener.Create(Self.OnAppStarted);
  FAppStartedNotifier.Add(FAppStartedListener);
  OnAppStarted;
end;

destructor TMarkSystemImplChangeable.Destroy;
begin
  if Assigned(FBackgroundTask) then begin
    FBackgroundTask.StopExecute;
    FBackgroundTask.Terminate;
  end;
  if Assigned(FBasePath) and Assigned(FConfigChangeListener) then begin
    FBasePath.ChangeNotifier.Remove(FConfigChangeListener);
    FBasePath := nil;
  end;
  if Assigned(FConfigList) and Assigned(FConfigChangeListener) then begin
    FConfigList.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigList := nil;
  end;
  if Assigned(FAppStartedNotifier) and Assigned(FAppStartedListener) then begin
    FAppStartedNotifier.Remove(FAppStartedListener);
    FAppStartedNotifier := nil;
  end;
  inherited;
end;

procedure TMarkSystemImplChangeable.OnInitialization(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VStatic: IMarkSystemImpl;
  VConfig: IMarkSystemConfigStatic;
  VFactory: IMarkSystemImplFactory;
  VErrorMsg: IMarkSystemErrorMsg;
begin
  if FAppStartedNotifier.IsExecuted then begin

    CS.BeginWrite;
    try
      FStatic := nil;
      FStateInternal.SetOther(nil);
    finally
      CS.EndWrite;
    end;
    DoChangeNotify;

    VConfig := FConfigList.GetActiveConfig;
    if Assigned(VConfig) then begin
      VFactory := FImplFactoryList.Get(VConfig.DatabaseGUID).Factory;
      if Assigned(VFactory) then begin
        try
          VStatic :=
            VFactory.Build(
              AOperationID,
              ACancelNotifier,
              FBasePath.FullPath,
              VConfig.ImplConfig
            );
        except
          on E: Exception do begin
            VStatic := nil;
            VErrorMsg := TMarkSystemErrorMsg.Create(E.ClassName + ': ' + E.Message);
            FErrorNotifierInternal.Notify(VErrorMsg);
            //ToDo: LogError
          end;
        end;
      end;
    end;
  end;
  CS.BeginWrite;
  try
    FStatic := VStatic;
    if Assigned(VStatic) and Assigned(VFactory) then begin
      FStateInternal.SetOther(VStatic.State);
    end else begin
      FStateInternal.SetOther(nil);
    end;
  finally
    CS.EndWrite;
  end;
  DoChangeNotify;
end;

function TMarkSystemImplChangeable.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarkSystemImplChangeable.GetStatic: IMarkSystemImpl;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TMarkSystemImplChangeable.OnAppStarted;
begin
  if (FAppStartedNotifier <> nil) and FAppStartedNotifier.IsExecuted then begin
    FBackgroundTask.Start;
    FBackgroundTask.StartExecute;
  end;
end;

procedure TMarkSystemImplChangeable.OnConfigChanged;
begin
  FBackgroundTask.StopExecute;
  FBackgroundTask.StartExecute;
end;

end.
