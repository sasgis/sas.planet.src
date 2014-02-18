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

unit u_TileDownloaderUIOneTile;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  Types,
  i_Listener,
  i_TileError,
  i_ThreadConfig,
  i_MapVersionInfo,
  i_TileRequestTask,
  i_TileRequestResult,
  i_NotifierOperation,
  i_DownloadInfoSimple,
  i_MapTypes,
  i_GlobalInternetState;

type
  TTileDownloaderUIOneTile = class(TThread)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FErrorLogger: ITileErrorLogger;
    FDownloadInfo: IDownloadInfoSimple;
    FGlobalInternetState: IGlobalInternetState;
    FMapType: IMapType;
    FVersion: IMapVersionInfo;
    FTile: TPoint;
    FZoom: Byte;
    FTileRequestResult: ITileRequestResult;

    FCancelNotifier: INotifierOperation;
    FCancelNotifierInternal: INotifierOperationInternal;
    FFinishEvent: TEvent;
    FTaskFinishNotifier: ITileRequestTaskFinishNotifier;

    FAppClosingListener: IListener;

    procedure OnTileDownloadFinish(
      const ATask: ITileRequestTask;
      const AResult: ITileRequestResult
    );
    procedure OnAppClosing;
    procedure ProcessResult(const AResult: ITileRequestResult);
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const AXY: TPoint;
      AZoom: byte;
      const AMapType: IMapType;
      const AVersion: IMapVersionInfo;
      const ADownloadInfo: IDownloadInfoSimple;
      const AGlobalInternetState: IGlobalInternetState;
      const AErrorLogger: ITileErrorLogger
    ); overload;
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_DownloadResult,
  u_Notifier,
  u_NotifierOperation,
  u_TileRequestTask,
  u_ReadableThreadNames,
  u_ListenerByEvent,
  u_TileErrorInfo;

constructor TTileDownloaderUIOneTile.Create(
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const AXY: TPoint;
  AZoom: byte;
  const AMapType: IMapType;
  const AVersion: IMapVersionInfo;
  const ADownloadInfo: IDownloadInfoSimple;
  const AGlobalInternetState: IGlobalInternetState;
  const AErrorLogger: ITileErrorLogger
);
var
  VOperationNotifier: TNotifierOperation;
begin
  inherited Create(False);
  FDownloadInfo := ADownloadInfo;
  FGlobalInternetState := AGlobalInternetState;
  FErrorLogger := AErrorLogger;
  FAppClosingNotifier := AAppClosingNotifier;
  FTile := AXY;
  FZoom := AZoom;
  FMapType := AMapType;
  FVersion := AVersion;
  Priority := AThreadConfig.Priority;
  FreeOnTerminate := True;

  VOperationNotifier := TNotifierOperation.Create(TNotifierBase.Create);
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
  FFinishEvent := TEvent.Create;

  FTaskFinishNotifier := TTileRequestTaskFinishNotifier.Create(Self.OnTileDownloadFinish);

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TTileDownloaderUIOneTile.Destroy;
begin
  if Assigned(FTaskFinishNotifier) then begin
    FTaskFinishNotifier.Enabled := False;
    FTaskFinishNotifier := nil;
  end;

  if FFinishEvent <> nil then begin
    FTileRequestResult := nil;
    FFinishEvent.SetEvent;
  end;

  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;

  FreeAndNil(FFinishEvent);

  inherited;
end;

procedure TTileDownloaderUIOneTile.Execute;
var
  VOperationID: Integer;
  VTask: ITileRequestTask;
  VSoftCancelNotifier: INotifierOneOperation;
begin
  SetCurrentThreadName(Self.ClassName);
  Randomize;
  if FMapType.TileDownloadSubsystem.State.GetStatic.Enabled then begin
    VOperationID := FCancelNotifier.CurrentOperation;
    VSoftCancelNotifier := TNotifierOneOperationByNotifier.Create(FCancelNotifier, VOperationID);
    VTask :=
      FMapType.TileDownloadSubsystem.GetRequestTask(
        VSoftCancelNotifier,
        FCancelNotifier,
        VOperationID,
        FTaskFinishNotifier,
        FTile,
        FZoom,
        FVersion,
        False
      );
    if VTask <> nil then begin
      FGlobalInternetState.IncQueueCount;
      FTileRequestResult := nil;
      FMapType.TileDownloadSubsystem.Download(VTask);
      FFinishEvent.WaitFor(INFINITE);
      ProcessResult(FTileRequestResult);
    end;
  end;
end;

procedure TTileDownloaderUIOneTile.OnAppClosing;
begin
  if Assigned(FTaskFinishNotifier) then begin
    FTaskFinishNotifier.Enabled := False;
  end;
  Terminate;
  FTileRequestResult := nil;
  if Assigned(FFinishEvent) then begin
    FFinishEvent.SetEvent;
  end;
end;

procedure TTileDownloaderUIOneTile.OnTileDownloadFinish(
  const ATask: ITileRequestTask;
  const AResult: ITileRequestResult
);
begin
  FTileRequestResult := AResult;;
  if Assigned(FFinishEvent) then begin
    FFinishEvent.SetEvent;
  end;
end;

procedure TTileDownloaderUIOneTile.ProcessResult(const AResult: ITileRequestResult);
var
  VResultWithDownload: ITileRequestResultWithDownloadResult;
  VDownloadResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
  VResultNotNecessary: IDownloadResultNotNecessary;
  VResultDataNotExists: IDownloadResultDataNotExists;
  VRequestError: ITileRequestResultError;
  VError: ITileErrorInfo;
begin
  FGlobalInternetState.DecQueueCount;
  if AResult <> nil then begin
    VError := nil;
    if Supports(AResult, ITileRequestResultError, VRequestError) then begin
      VError :=
        TTileErrorInfoByTileRequestResult.Create(
          FMapType.Zmp.GUID,
          VRequestError
        );
    end else if Supports(AResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
      if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VDownloadResultOk) then begin
        if FDownloadInfo <> nil then begin
          FDownloadInfo.Add(1, VDownloadResultOk.Data.Size);
        end;
      end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultDataNotExists, VResultDataNotExists) then begin
        VError :=
          TTileErrorInfoByDataNotExists.Create(
            FMapType.Zmp.GUID,
            AResult.Request.Zoom,
            AResult.Request.Tile,
            VResultDataNotExists
          );
      end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
        VError :=
          TTileErrorInfoByDownloadResultError.Create(
            FMapType.Zmp.GUID,
            AResult.Request.Zoom,
            AResult.Request.Tile,
            VResultDownloadError
          );
      end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary, VResultNotNecessary) then begin
        VError :=
          TTileErrorInfoByNotNecessary.Create(
            FMapType.Zmp.GUID,
            AResult.Request.Zoom,
            AResult.Request.Tile,
            VResultNotNecessary
          );
      end else begin
        VError :=
          TTileErrorInfo.Create(
            FMapType.Zmp.GUID,
            AResult.Request.Zoom,
            AResult.Request.Tile,
            'Unexpected error'
          );
      end;
    end;

    if VError <> nil then begin
      if FErrorLogger <> nil then begin
        FErrorLogger.LogError(VError);
      end;
    end;
  end;
end;

end.
