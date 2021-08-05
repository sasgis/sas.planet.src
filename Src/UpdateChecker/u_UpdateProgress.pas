{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit u_UpdateProgress;

interface

uses
  SysUtils,
  t_UpdateChecker,
  i_UpdateProgress,
  i_NotifierOperation;

type
  TUpdateProgress = class(TInterfacedObject, IUpdateProgress)
  private
    FLock: IReadWriteSync;
    FCancelNotifierInternal: INotifierOperationInternal;
    FStatus: TUpdateProgressStatus;
  private
    { IUpdateProgress }
    procedure Reset;
    function GetCancelNotifier: INotifierOperation;
    function GetCurrentOperationID: Integer;
  protected
    procedure ResetResult; virtual; abstract;
  public
    constructor Create;
  end;

  TUpdateCheckerProgress = class(TUpdateProgress, IUpdateCheckerProgress)
  private
    FResult: TUpdateCheckerResult;
  private
    { IUpdateCheckerProgress }
    procedure SetResult(
      const AOperationID: Integer;
      const AResult: TUpdateCheckerResult
    );
    function GetResult(
      const AOperationID: Integer;
      out AResult: TUpdateCheckerResult
    ): TUpdateProgressStatus;
  protected
    procedure ResetResult; override;
  end;

  TUpdateDownloaderProgress = class(TUpdateProgress, IUpdateDownloaderProgress)
  private
    FResult: TUpdateDownloaderResult;
  private
    { IUpdateDownloaderProgress }
    procedure SetResult(
      const AOperationID: Integer;
      const AResult: TUpdateDownloaderResult
    );
    function GetResult(
      const AOperationID: Integer;
      out AResult: TUpdateDownloaderResult
    ): TUpdateProgressStatus;
  protected
    procedure ResetResult; override;
  end;

implementation

uses
  u_Notifier,
  u_NotifierOperation,
  u_Synchronizer;

{ TUpdateProgress }

constructor TUpdateProgress.Create;
begin
  inherited Create;

  FCancelNotifierInternal :=
    TNotifierOperation.Create(
      TNotifierBase.Create(GSync.SyncVariable.Make(Self.ClassName + 'Notifier'))
    );

  FLock := GSync.SyncVariable.Make(Self.ClassName + 'Lock');

  FStatus := psBusy;
  ResetResult;
end;

procedure TUpdateProgress.Reset;
begin
  FLock.BeginWrite;
  try
    FCancelNotifierInternal.NextOperation;
    FStatus := psBusy;
    ResetResult;
  finally
    FLock.EndWrite;
  end;
end;

function TUpdateProgress.GetCancelNotifier: INotifierOperation;
begin
  Result := FCancelNotifierInternal;
end;

function TUpdateProgress.GetCurrentOperationID: Integer;
begin
  Result := FCancelNotifierInternal.CurrentOperation;
end;

{ TUpdateCheckerProgress }

procedure TUpdateCheckerProgress.ResetResult;
begin
  FResult.IsFound := False;
end;

procedure TUpdateCheckerProgress.SetResult(
  const AOperationID: Integer;
  const AResult: TUpdateCheckerResult
);
begin
  FLock.BeginWrite;
  try
    if not FCancelNotifierInternal.IsOperationCanceled(AOperationID) then begin
      FResult := AResult;
      FStatus := psFinished;
    end;
  finally
    FLock.EndWrite;
  end;
end;

function TUpdateCheckerProgress.GetResult(
  const AOperationID: Integer;
  out AResult: TUpdateCheckerResult
): TUpdateProgressStatus;
begin
  FLock.BeginRead;
  try
    if FCancelNotifierInternal.IsOperationCanceled(AOperationID) then begin
      Result := psCanceled;
      Exit;
    end;

    Result := FStatus;
    if Result = psFinished then begin
      AResult := FResult;
    end;
  finally
    FLock.EndRead;
  end;
end;

{ TUpdateDownloaderProgress }

procedure TUpdateDownloaderProgress.ResetResult;
begin
  FResult.IsFinished := False;
  FResult.IsError := False;

  FResult.BytesDownloaded := 0;
  FResult.BytesTotal := 0;

  FResult.Text := '';
end;

procedure TUpdateDownloaderProgress.SetResult(
  const AOperationID: Integer;
  const AResult: TUpdateDownloaderResult
);
begin
  FLock.BeginWrite;
  try
    if not FCancelNotifierInternal.IsOperationCanceled(AOperationID) then begin
      FResult := AResult;
      if FResult.IsFinished or FResult.IsError then begin
        FStatus := psFinished;
      end;
    end;
  finally
    FLock.EndWrite;
  end;
end;

function TUpdateDownloaderProgress.GetResult(
  const AOperationID: Integer;
  out AResult: TUpdateDownloaderResult
): TUpdateProgressStatus;
begin
  FLock.BeginRead;
  try
    if FCancelNotifierInternal.IsOperationCanceled(AOperationID) then begin
      Result := psCanceled;
      Exit;
    end;

    Result := FStatus;
    AResult := FResult;
  finally
    FLock.EndRead;
  end;
end;

end.
