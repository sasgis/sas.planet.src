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

unit u_UpdateCheckerThread;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  t_UpdateChecker,
  i_InetConfig,
  i_DownloaderFactory,
  i_UpdateProgress,
  u_UpdateChecker;

type
  TUpdateCheckerThread = class(TThread)
  private
    FLock: IReadWriteSync;
    FIsFinished: TEvent;

    FOperationID: Integer;
    FUpdateCheckerProgress: IUpdateCheckerProgress;

    FChecker: array [TUpdateSource] of TUpdateChecker;
    FResults: TUpdateCheckerResultArray;

    procedure OnUpdateCheckerResult(
      const AResult: TUpdateCheckerResult
    );
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AUpdateChannel: TUpdateChannel;
      const ADownloaderFactory: IDownloaderFactory;
      const AInetConfig: IInetConfig;
      const AUpdateCheckerProgress: IUpdateCheckerProgress
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  u_UpdateCheckerFunc;

{ TUpdateCheckerThread }

constructor TUpdateCheckerThread.Create(
  const AUpdateChannel: TUpdateChannel;
  const ADownloaderFactory: IDownloaderFactory;
  const AInetConfig: IInetConfig;
  const AUpdateCheckerProgress: IUpdateCheckerProgress
);
var
  VUpdateSource: TUpdateSource;
begin
  inherited Create(False);
  FreeOnTerminate := True;

  FLock := GSync.SyncStd.Make(Self.ClassName);

  FUpdateCheckerProgress := AUpdateCheckerProgress;
  FOperationID := FUpdateCheckerProgress.CurrentOperationID;

  for VUpdateSource := Low(FChecker) to High(FChecker) do begin
    FChecker[VUpdateSource] :=
      TUpdateChecker.Create(
        VUpdateSource,
        AUpdateChannel,
        Self.OnUpdateCheckerResult,
        ADownloaderFactory,
        AInetConfig,
        FUpdateCheckerProgress.CancelNotifier
      );
  end;

  FIsFinished := TEvent.Create;
end;

destructor TUpdateCheckerThread.Destroy;
var
  I: TUpdateSource;
begin
  FreeAndNil(FIsFinished);

  for I := Low(FChecker) to High(FChecker) do begin
    FreeAndNil(FChecker[I]);
  end;

  inherited Destroy;
end;

procedure TUpdateCheckerThread.Execute;
var
  I: TUpdateSource;
  VResult: TUpdateCheckerResult;
begin
  FIsFinished.ResetEvent;

  SetLength(FResults, 0);
  for I := Low(FChecker) to High(FChecker) do begin
    FChecker[I].Perform(FOperationID);
  end;

  FIsFinished.WaitFor(INFINITE);

  VResult := TUpdateCheckerFunc.LatestResultFromArray(FResults);
  FUpdateCheckerProgress.SetResult(FOperationID, VResult);
end;

procedure TUpdateCheckerThread.OnUpdateCheckerResult(
  const AResult: TUpdateCheckerResult
);
var
  I: Integer;
  VIsFinished: Boolean;
begin
  FLock.BeginWrite;
  try
    I := Length(FResults);
    SetLength(FResults, I+1);
    FResults[I] := AResult;

    VIsFinished := Length(FResults) = Length(FChecker);
  finally
    FLock.EndWrite;
  end;

  if VIsFinished then begin
    FIsFinished.SetEvent;
  end;
end;

end.
