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
  i_NotifierOperation,
  i_BackgroundTask,
  i_ReadWriteState,
  i_MarkSystemImplFactory,
  i_MarkSystemImpl,
  i_MarkSystemImplChangeable,
  u_ConfigDataElementBase,
  u_ReadWriteStateInternalByOther;

type
  TMarkSystemImplChangeable = class(TConfigDataElementBaseEmptySaveLoad, IMarkSystemImplChangeable)
  private
    FBasePath: IPathConfig;
    FBaseFactory: IMarkSystemImplFactoryChangeable;
    FAppStartedNotifier: INotifierOneOperation;
    FAppClosingNotifier: INotifierOneOperation;

    FStatic: IMarkSystemImpl;
    FBackgroundTask: IBackgroundTask;

    FState: IReadWriteStateChangeble;
    FStateInternal: IReadWriteStateInternalByOther;

    FPathChangeListener: IListener;
    FAppStartedListener: IListener;
  private
    procedure OnPathChanged;
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
      const ABaseFactory: IMarkSystemImplFactoryChangeable;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation
    );
    destructor Destroy; override;
  end;

implementation

uses
  Classes,
  u_BackgroundTask,
  u_ThreadConfig,
  u_ListenerByEvent;

{ TMarksSystemImplChangeable }

constructor TMarkSystemImplChangeable.Create(
  const ABasePath: IPathConfig;
  const ABaseFactory: IMarkSystemImplFactoryChangeable;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation
);
begin
  Assert(Assigned(ABasePath));
  Assert(Assigned(ABaseFactory));
  inherited Create;
  FBasePath := ABasePath;
  FBaseFactory := ABaseFactory;
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

  FPathChangeListener := TNotifyNoMmgEventListener.Create(Self.OnPathChanged);
  FBasePath.ChangeNotifier.Add(FPathChangeListener);
  FBaseFactory.ChangeNotifier.Add(FPathChangeListener);

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
  if Assigned(FBasePath) and Assigned(FPathChangeListener) then begin
    FBasePath.ChangeNotifier.Remove(FPathChangeListener);
    FBasePath := nil;
  end;
  if Assigned(FBaseFactory) and Assigned(FPathChangeListener) then begin
    FBaseFactory.ChangeNotifier.Remove(FPathChangeListener);
    FBaseFactory := nil;
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
  VFactory: IMarkSystemImplFactory;
  VStatic: IMarkSystemImpl;
begin
  if FAppStartedNotifier.IsExecuted then begin
    VFactory := FBaseFactory.GetStatic;
    if Assigned(VFactory) then begin
      VStatic :=
        VFactory.Build(
          AOperationID,
          ACancelNotifier,
          FBasePath.FullPath,
          False
        );
    end;
  end;
  LockWrite;
  try
    FStatic := VStatic;
    if Assigned(VStatic) and Assigned(VFactory) then begin
      FStateInternal.SetOther(VStatic.State);
    end else begin
      FStateInternal.SetOther(nil);
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TMarkSystemImplChangeable.GetState: IReadWriteStateChangeble;
begin
  Result := FState;
end;

function TMarkSystemImplChangeable.GetStatic: IMarkSystemImpl;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TMarkSystemImplChangeable.OnAppStarted;
begin
  if (FAppStartedNotifier <> nil) and FAppStartedNotifier.IsExecuted then begin
    FBackgroundTask.Start;
    FBackgroundTask.StartExecute;
  end;
end;

procedure TMarkSystemImplChangeable.OnPathChanged;
begin
  FBackgroundTask.StopExecute;
  FBackgroundTask.StartExecute;
end;

end.
