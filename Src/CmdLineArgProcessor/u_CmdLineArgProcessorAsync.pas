{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_CmdLineArgProcessorAsync;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_MarkSystem,
  i_RegionProcess,
  i_ReadWriteState,
  i_Listener,
  i_Notifier,
  i_NotifierOperation,
  i_CmdLineArgProcessor;

type
  TCmdLineArgProcessorAsync = class(TThread)
  private
    FMarkSystem: IMarkSystem;
    FMarkSystemErrorNotifier: INotifier;
    FMarkSystemStateNotifier: INotifier;
    FAppClosingNotifier: INotifierOneOperation;
    FCmdLineArgProcessor: ICmdLineArgProcessor;
    FRegionProcess: IRegionProcessFromFile;

    FAbortListener: IListener;

    FState: IReadWriteStateStatic;
    FStateEvent: TEvent;
    FStateListener: IListener;

    function IsMarkSystemOk: Boolean;

    procedure OnAbort;
    procedure OnStateChange;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AMarkSystem: IMarkSystem;
      const AAppClosingNotifier: INotifierOneOperation;
      const ACmdLineArgProcessor: ICmdLineArgProcessor;
      const ARegionProcess: IRegionProcessFromFile
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_Dialogs,
  u_ListenerByEvent,
  u_CmdLineArgProcessorAPI;

type
  ECmdLineArgProcessorAsync = class(Exception);

{ TCmdLineArgProcessorAsync }

constructor TCmdLineArgProcessorAsync.Create(
  const AMarkSystem: IMarkSystem;
  const AAppClosingNotifier: INotifierOneOperation;
  const ACmdLineArgProcessor: ICmdLineArgProcessor;
  const ARegionProcess: IRegionProcessFromFile
);
begin
  inherited Create(False);

  Self.FreeOnTerminate := True;

  FMarkSystem := AMarkSystem;
  FAppClosingNotifier := AAppClosingNotifier;
  FCmdLineArgProcessor := ACmdLineArgProcessor;
  FRegionProcess := ARegionProcess;

  FAbortListener := TNotifyNoMmgEventListener.Create(Self.OnAbort);

  FMarkSystemErrorNotifier := FMarkSystem.ErrorNotifier;
  FMarkSystemErrorNotifier.Add(FAbortListener);
  FAppClosingNotifier.Add(FAbortListener);

  FState := FMarkSystem.State.GetStatic;
  FStateEvent := TEvent.Create;
  FStateListener := TNotifyNoMmgEventListener.Create(Self.OnStateChange);
  FMarkSystemStateNotifier := FMarkSystem.State.ChangeNotifier;
  FMarkSystemStateNotifier.Add(FStateListener);
end;

destructor TCmdLineArgProcessorAsync.Destroy;
begin
  if (FAppClosingNotifier <> nil) and (FAbortListener <> nil) then begin
    FAppClosingNotifier.Remove(FAbortListener);
  end;

  if (FMarkSystemErrorNotifier <> nil) and (FAbortListener <> nil) then begin
    FMarkSystemErrorNotifier.Remove(FAbortListener);
  end;

  if (FMarkSystemStateNotifier <> nil) and (FStateListener <> nil) then begin
    FMarkSystemStateNotifier.Remove(FStateListener);
  end;

  FreeAndNil(FStateEvent);
  inherited Destroy;
end;

function TCmdLineArgProcessorAsync.IsMarkSystemOk: Boolean;
begin
  Result := FState.ReadAccess or FState.WriteAccess;
end;

procedure TCmdLineArgProcessorAsync.Execute;
var
  VRetCode: Integer;
  VErrorMsg: string;
begin
  VErrorMsg := '';

  try
    if not IsMarkSystemOk then begin
      FStateEvent.WaitFor(INFINITE);
    end;

    if Terminated then begin
      Exit;
    end;

    if not IsMarkSystemOk then begin
      raise ECmdLineArgProcessorAsync.Create('MarkSystem state invalid!');
    end;

    VRetCode := FCmdLineArgProcessor.Process(FRegionProcess);

    if VRetCode <> cCmdLineArgProcessorOk then begin
      VErrorMsg :=
        FCmdLineArgProcessor.GetErrorFromCode(VRetCode) + #13#10 + #13#10 +
        FCmdLineArgProcessor.GetArguments;
    end;
  except
    on E: Exception do begin
      VErrorMsg := E.ClassName + ': ' + E.Message;
    end;
  end;

  if VErrorMsg <> '' then begin
    ShowErrorMessageSync(VErrorMsg);
  end;
end;

procedure TCmdLineArgProcessorAsync.OnAbort;
begin
  Terminate;
  FStateEvent.SetEvent;
end;

procedure TCmdLineArgProcessorAsync.OnStateChange;
begin
  FState := FMarkSystem.State.GetStatic;
  FStateEvent.SetEvent;
end;

end.
