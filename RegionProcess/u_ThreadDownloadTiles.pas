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

unit u_ThreadDownloadTiles;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  i_Listener,
  i_LogSimple,
  i_MapVersionInfo,
  i_NotifierOperation,
  i_GlobalDownloadConfig,
  i_TileRequestTask,
  i_TileRequestResult,
  i_VectorItemProjected,
  i_DownloadInfoSimple,
  i_RegionProcessProgressInfoDownload,
  u_MapType;

type
  TThreadDownloadTiles = class(TThread)
  private
    FProgressInfo: IRegionProcessProgressInfoDownloadInternal;

    FAppClosingNotifier: INotifierOneOperation;
    FMapType: TMapType;
    FZoom: Byte;
    FVersionForCheck: IMapVersionInfo;
    FVersionForDownload: IMapVersionInfo;
    FDownloadInfo: IDownloadInfoSimple;
    FPolyProjected: IProjectedPolygon;
    FSecondLoadTNE: boolean;
    FReplaceExistTiles: boolean;
    FCheckExistTileSize: boolean;
    FCheckExistTileDate: boolean;
    FDownloadConfig: IGlobalDownloadConfig;
    FCheckTileDate: TDateTime;
    FLastProcessedPoint: TPoint;

    FPausedSleepTime: Cardinal;
    FBanSleepTime: Cardinal;
    FProxyAuthErrorSleepTime: Cardinal;
    FDownloadErrorSleepTime: Cardinal;

    FTileRequestResult: ITileRequestResult;

    FRES_UserStop: string;
    FRES_ProcessedFile: string;
    FRES_LoadProcessRepl: string;
    FRES_LoadProcess: string;
    FRES_FileBeCreateTime: string;
    FRES_FileBeCreateLen: string;
    FRES_Authorization: string;
    FRES_WaitTime: string;
    FRES_Ban: string;
    FRES_BadMIME: string;
    FRES_TileNotExists: string;
    FRES_Noconnectionstointernet: string;
    FRES_FileExistsShort: string;
    FRES_ProcessFilesComplete: string;


    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FFinishEvent: TEvent;
    FTaskFinishNotifier: ITileRequestTaskFinishNotifier;

    FAppClosingListener: IListener;

    procedure OnTileDownloadFinish(
      const ATask: ITileRequestTask;
      const AResult: ITileRequestResult
    );
    procedure OnAppClosing;
    function ProcessResultAndCheckGotoNextTile(const AResult: ITileRequestResult): Boolean;
    procedure SleepCancelable(ATime: Cardinal);

    procedure PrepareStrings;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer;
      const AProgressInfo: IRegionProcessProgressInfoDownloadInternal;
      const AAppClosingNotifier: INotifierOneOperation;
      AMapType: TMapType;
      const AVersionForCheck: IMapVersionInfo;
      const AVersionForDownload: IMapVersionInfo;
      AZoom: byte;
      const APolyProjected: IProjectedPolygon;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      AReplaceExistTiles: Boolean;
      ACheckExistTileSize: Boolean;
      ACheckExistTileDate: Boolean;
      const AReplaceOlderDate: TDateTime;
      ASecondLoadTNE: Boolean;
      const ALastProcessedPoint: TPoint;
      const AElapsedTime: TDateTime
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  Types,
  i_DownloadResult,
  i_TileInfoBasic,
  i_TileIterator,
  i_TileDownloaderState,
  i_TileStorage,
  u_NotifierOperation,
  u_TileIteratorByPolygon,
  u_TileRequestTask,
  u_ListenerByEvent,
  u_ReadableThreadNames,
  u_ResStrings;

constructor TThreadDownloadTiles.Create(
  const ACancelNotifier: INotifierOperation;
  AOperationID: Integer;
  const AProgressInfo: IRegionProcessProgressInfoDownloadInternal;
  const AAppClosingNotifier: INotifierOneOperation;
  AMapType: TMapType;
  const AVersionForCheck: IMapVersionInfo;
  const AVersionForDownload: IMapVersionInfo;
  AZoom: byte;
  const APolyProjected: IProjectedPolygon;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  AReplaceExistTiles, ACheckExistTileSize, ACheckExistTileDate: Boolean;
  const AReplaceOlderDate: TDateTime;
  ASecondLoadTNE: Boolean;
  const ALastProcessedPoint: TPoint;
  const AElapsedTime: TDateTime
);
var
  VState: ITileDownloaderStateStatic;
begin
  Assert(ACancelNotifier <> nil);
  Assert(AProgressInfo <> nil);
  Assert(AAppClosingNotifier <> nil);
  Assert(AMapType <> nil);
  Assert(APolyProjected <> nil);
  Assert(ADownloadConfig <> nil);
  Assert(ADownloadInfo <> nil);
  inherited Create(False);
  Priority := tpLower;
  FreeOnTerminate := True;

  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FProgressInfo := AProgressInfo;
  FAppClosingNotifier := AAppClosingNotifier;
  FDownloadInfo := ADownloadInfo;
  FDownloadConfig := ADownloadConfig;

  FPausedSleepTime := 100;
  FBanSleepTime := 5000;
  FProxyAuthErrorSleepTime := 10000;
  FDownloadErrorSleepTime := 5000;
  Priority := tpLower;

  FReplaceExistTiles := AReplaceExistTiles;
  FZoom := AZoom;
  FCheckExistTileSize := ACheckExistTileSize;
  FMapType := AMapType;
  FVersionForCheck := AVersionForCheck;
  FVersionForDownload := AVersionForDownload;
  FCheckTileDate := AReplaceOlderDate;
  FCheckExistTileDate := ACheckExistTileDate;
  FSecondLoadTNE := ASecondLoadTNE;
  FPolyProjected := APolyProjected;
  FLastProcessedPoint := ALastProcessedPoint;

  if FMapType = nil then begin
    Terminate;
    Exit;
  end;
  if not FMapType.Abilities.UseDownload then begin
    FProgressInfo.Log.WriteText(Format('Download of map %s disabled by map params', [FMapType.GUIConfig.Name.Value]), 10);
    Terminate;
    Exit;
  end;
  VState := FMapType.TileDownloadSubsystem.State.GetStatic;
  if not VState.Enabled then begin
    FProgressInfo.Log.WriteText(Format('Download of map %s disabled. Reason: %s', [FMapType.GUIConfig.Name.Value, VState.DisableReason]), 10);
    Terminate;
    Exit;
  end;
  if FPolyProjected = nil then begin
    FProgressInfo.Log.WriteText('Polygon does not exist', 10);
    Terminate;
    Exit;
  end;
  if FPolyProjected.Count < 1 then begin
    FProgressInfo.Log.WriteText('Empty Polygon', 10);
    Terminate;
    Exit;
  end;

  PrepareStrings;

  FFinishEvent := TEvent.Create;

  FTaskFinishNotifier := TTileRequestTaskFinishNotifier.Create(Self.OnTileDownloadFinish);

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  FCancelNotifier.AddListener(FAppClosingListener);

  if FAppClosingNotifier.IsExecuted or FCancelNotifier.IsOperationCanceled(FOperationID) then begin
    OnAppClosing;
  end;
end;

destructor TThreadDownloadTiles.Destroy;
begin
  if Assigned(FTaskFinishNotifier) then begin
    FTaskFinishNotifier.Enabled := False;
    FTaskFinishNotifier := nil;
  end;

  if FFinishEvent <> nil then begin
    FTileRequestResult := nil;
    FFinishEvent.SetEvent;
  end;

  if Assigned(FCancelNotifier) and Assigned(FAppClosingListener) then begin
    FCancelNotifier.RemoveListener(FAppClosingListener);
    FCancelNotifier := nil;
  end;

  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingNotifier := nil;
    FAppClosingListener := nil;
  end;

  FreeAndNil(FFinishEvent);
  inherited;
end;

procedure TThreadDownloadTiles.SleepCancelable(ATime: Cardinal);
begin
  FFinishEvent.WaitFor(ATime);
end;

procedure TThreadDownloadTiles.Execute;
var
  VTileInfo: ITileInfoBasic;
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VTask: ITileRequestTask;
  VGotoNextTile: Boolean;
  VLastSkipped: TPoint;
  VCntSkipped: Cardinal;
  VSoftCancelNotifier: INotifierOneOperation;
begin
  try
    if Terminated then begin
      Exit;
    end;

    SetCurrentThreadName(Self.ClassName);
    Randomize;

    VSoftCancelNotifier := TNotifierOneOperationByNotifier.Create(FCancelNotifier, FOperationID);

    VTileIterator := TTileIteratorByPolygon.Create(FPolyProjected);
    FProgressInfo.SetTotalToProcess(VTileIterator.TilesTotal);
    if (FLastProcessedPoint.X >= 0) and (FLastProcessedPoint.Y >= 0) then begin
      VCntSkipped := 0;
      while VTileIterator.Next(VTile) do begin
        if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
          Break;
        end;
        if (VTile.X = FLastProcessedPoint.X) and (VTile.Y = FLastProcessedPoint.Y) then begin
          Break;
        end;
        Inc(VCntSkipped);
        VLastSkipped := VTile;
        if VCntSkipped > 100 then begin
          FProgressInfo.AddManyProcessedTile(VLastSkipped, VCntSkipped);
          VCntSkipped := 0;
        end;
      end;
      if VCntSkipped > 0 then begin
        FProgressInfo.AddManyProcessedTile(VLastSkipped, VCntSkipped);
      end;
    end;
    if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
      Exit;
    end;
    while VTileIterator.Next(VTile) do begin
      VGotoNextTile := false;
      while not VGotoNextTile do begin
        FFinishEvent.ResetEvent;
        if FProgressInfo.NeedPause then begin
          FProgressInfo.SetPaused;
          FProgressInfo.Log.WriteText(FRES_UserStop, 10);
          While FProgressInfo.NeedPause do begin
            if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
              Exit;
            end;
            SleepCancelable(FPausedSleepTime);
          end;
          FProgressInfo.SetStarted;
        end;
        if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
          Exit;
        end;

        // notify about current tile
        FProgressInfo.Log.WriteText(Format(FRES_ProcessedFile, [FMapType.GetTileShowName(VTile, FZoom, FVersionForDownload)]), 0);
        // if gtimWithData - tile will be loaded, so we use gtimAsIs
        VTileInfo := FMapType.TileStorage.GetTileInfo(VTile, FZoom, FVersionForCheck, gtimAsIs);
        
        if (FReplaceExistTiles) or not (VTileInfo.IsExists) then begin
          // what to do
          if VTileInfo.IsExists then begin
            FProgressInfo.Log.WriteText(FRES_LoadProcessRepl, 0);
          end else begin
            FProgressInfo.Log.WriteText(FRES_LoadProcess, 0);
          end;
          if (FCheckExistTileDate) and (VTileInfo.IsExists or VTileInfo.IsExistsTNE) and (VTileInfo.LoadDate >= FCheckTileDate) then begin
            // skip existing newer tile
            FProgressInfo.Log.WriteText(FRES_FileBeCreateTime, 0);
            FLastProcessedPoint := VTile;
            VGotoNextTile := True;
          end else begin
            try
              if (not (FSecondLoadTNE)) and
                (VTileInfo.IsExistsTNE) and
                (FDownloadConfig.IsSaveTileNotExists) then begin
                // tne found - skip downloading tile
                FProgressInfo.Log.WriteText('(tne exists)', 0);
                FLastProcessedPoint := VTile;
                VGotoNextTile := True;
              end else begin
                // download tile
                VTask := FMapType.TileDownloadSubsystem.GetRequestTask(
                  VSoftCancelNotifier,
                  FCancelNotifier,
                  FOperationID,
                  FTaskFinishNotifier,
                  VTile,
                  FZoom,
                  FVersionForDownload,
                  FCheckExistTileSize
                );
                if VTask <> nil then begin
                  FTileRequestResult := nil;
                  FMapType.TileDownloadSubsystem.Download(VTask);
                  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
                    Exit;
                  end;
                  FFinishEvent.WaitFor(INFINITE);
                  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
                    Exit;
                  end;
                  VGotoNextTile := ProcessResultAndCheckGotoNextTile(FTileRequestResult);
                  if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
                    Exit;
                  end;
                  FLastProcessedPoint := VTile;
                end else begin
                  FProgressInfo.Log.WriteText('Download disabled', 0);
                  VGotoNextTile := False;
                  FProgressInfo.NeedPause := True;
                end;
              end;
            except
              on E: Exception do begin
                FProgressInfo.Log.WriteText(E.Message, 0);
                VGotoNextTile := True;
              end;
            end;
          end;
        end else begin
          FProgressInfo.Log.WriteText(FRES_FileExistsShort, 0);
          VGotoNextTile := True;
        end;
        if VGotoNextTile then begin
          FProgressInfo.AddProcessedTile(VTile);
        end;
        if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
          Exit;
        end;
      end;
      if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
        Exit;
      end;
    end;
    if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
      Exit;
    end;
    FProgressInfo.Log.WriteText(FRES_ProcessFilesComplete, 0);
  finally
    FProgressInfo.Finish;
  end;
end;

procedure TThreadDownloadTiles.OnAppClosing;
begin
  if Assigned(FTaskFinishNotifier) then begin
    FTaskFinishNotifier.Enabled := False;
  end;
  Terminate;
  if Assigned(FFinishEvent) then begin
    FFinishEvent.SetEvent;
  end;
end;

procedure TThreadDownloadTiles.OnTileDownloadFinish(
  const ATask: ITileRequestTask;
  const AResult: ITileRequestResult
);
begin
  FTileRequestResult := AResult;
  if Assigned(FFinishEvent) then begin
    FFinishEvent.SetEvent;
  end;
end;

procedure TThreadDownloadTiles.PrepareStrings;
begin
  FRES_UserStop := SAS_STR_UserStop;
  FRES_ProcessedFile := SAS_STR_ProcessedFile;
  FRES_LoadProcessRepl := SAS_STR_LoadProcessRepl;
  FRES_LoadProcess := SAS_STR_LoadProcess;
  FRES_FileBeCreateTime := SAS_MSG_FileBeCreateTime;
  FRES_FileBeCreateLen := SAS_MSG_FileBeCreateLen;
  FRES_Authorization := SAS_ERR_Authorization;
  FRES_WaitTime := SAS_ERR_WaitTime;
  FRES_Ban := SAS_ERR_Ban;
  FRES_BadMIME := SAS_ERR_BadMIME;
  FRES_TileNotExists := SAS_ERR_TileNotExists;
  FRES_Noconnectionstointernet := SAS_ERR_Noconnectionstointernet;
  FRES_FileExistsShort := SAS_ERR_FileExistsShort;
  FRES_ProcessFilesComplete := SAS_MSG_ProcessFilesComplete;
end;

function TThreadDownloadTiles.ProcessResultAndCheckGotoNextTile(
  const AResult: ITileRequestResult
): Boolean;
var
  VResultError: ITileRequestResultError;
  VResultOk: IDownloadResultOk;
  VResultBadContentType: IDownloadResultBadContentType;
  VResultDownloadError: IDownloadResultError;
  VResultWithDownload: ITileRequestResultWithDownloadResult;
begin
  if Supports(AResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
    FFinishEvent.ResetEvent;
    if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VResultOk) then begin
      if Supports(AResult, ITileRequestResultError, VResultError) then begin
        // tile downloaded successfully downloaded, but not saved
        FProgressInfo.Log.WriteText('Error: ' + VResultError.ErrorText, 0);
        FProgressInfo.NeedPause := True;
        Result := False;
      end else begin
        // tile downloaded successfully
        FProgressInfo.Log.WriteText('(Ok!)', 0);
        FProgressInfo.AddDownloadedTile(AResult.Request.Tile, VResultOk.Data.Size);
        Result := True;
      end;
      FDownloadInfo.Add(1, VResultOk.Data.Size);
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary) then begin
      // same file size - assuming file the same
      FProgressInfo.Log.WriteText(FRES_FileBeCreateLen, 0);
      FProgressInfo.AddNotNecessaryTile(AResult.Request.Tile);
      Result := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultProxyError) then begin
      FProgressInfo.Log.WriteText(FRES_Authorization + #13#10 + Format(FRES_WaitTime, [FProxyAuthErrorSleepTime div 1000]), 10);
      SleepCancelable(FProxyAuthErrorSleepTime);
      Result := false;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultBanned) then begin
      FProgressInfo.Log.WriteText(FRES_Ban + #13#10 + Format(FRES_WaitTime, [FBanSleepTime div 1000]), 10);
      SleepCancelable(FBanSleepTime);
      Result := false;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultBadContentType, VResultBadContentType) then begin
      FProgressInfo.Log.WriteText(Format(FRES_BadMIME, [VResultBadContentType.ContentType]), 1);
      Result := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultDataNotExists) then begin
      FProgressInfo.Log.WriteText(FRES_TileNotExists, 1);
      Result := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
      if Supports(VResultWithDownload.DownloadResult, IDownloadResultNoConnetctToServer) then begin
        FProgressInfo.Log.WriteText(VResultDownloadError.ErrorText + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
        SleepCancelable(FDownloadErrorSleepTime);
        Result := false;
      end else begin
        FProgressInfo.Log.WriteText(FRES_Noconnectionstointernet + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
        SleepCancelable(FDownloadErrorSleepTime);
        if FDownloadConfig.IsGoNextTileIfDownloadError then begin
          Result := True;
        end else begin
          Result := False;
        end;
      end;
    end else begin
      FProgressInfo.Log.WriteText('Unknown download result', 10);
      Result := False;
    end;
  end else begin
    if Supports(AResult, ITileRequestResultCanceled) then begin
      Result := False;
    end else begin
      Result := True;
    end;
  end;
end;

end.
