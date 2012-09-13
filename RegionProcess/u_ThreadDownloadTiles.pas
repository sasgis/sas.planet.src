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
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_NotifierOperation,
  i_CoordConverterFactory,
  i_GlobalDownloadConfig,
  i_TileRequestResult,
  i_VectorItemLonLat,
  i_VectorItemProjected,
  i_VectorItmesFactory,
  i_DownloadInfoSimple,
  i_MapTypes,
  u_MapType,
  u_BaseTileDownloaderThread,
  u_NotifierOperation;

type
  TThreadDownloadTiles = class(TBaseTileDownloaderThread)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FMapType: TMapType;
    FDownloadInfo: IDownloadInfoSimple;
    FPolygLL: ILonLatPolygon;
    FPolyProjected: IProjectedPolygon;
    FSecondLoadTNE: boolean;
    FReplaceExistTiles: boolean;
    FCheckExistTileSize: boolean;
    FCheckExistTileDate: boolean;
    FDownloadConfig: IGlobalDownloadConfig;
    FCheckTileDate: TDateTime;
    FLastProcessedPoint: TPoint;
    FLastSuccessfulPoint: TPoint;

    FTotalInRegion: Int64;
    FProcessed: Int64;

    FElapsedTime: TDateTime;
    FStartTime: TDateTime;

    FLog: ILogSimple;
    //FDownloadPause: Boolean;
    FFinished: Boolean;
    FZoom: Byte;
    FGotoNextTile: Boolean;
    FPausedSleepTime: Cardinal;
    FBanSleepTime: Cardinal;
    FProxyAuthErrorSleepTime: Cardinal;
    FDownloadErrorSleepTime: Cardinal;

    FRES_UserStop: string;
    FRES_ProcessedFile: string;
    FRES_LoadProcessRepl: string;
    FRES_LoadProcess: string;
    FRES_LoadAttachmentsBegin: string;
    FRES_LoadAttachmentsEnd_Downloaded: string;
    FRES_LoadAttachmentsEnd_Skipped: string;
    FRES_LoadAttachmentsEnd_Failed: string;
    FRES_LoadAttachmentsEnd_Cancelled: string;
    FRES_LoadAttachmentsEnd_Nothing: string;
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


    FCancelNotifier: INotifierOperation;
    FCancelNotifierInternal: INotifierOperationInternal;
    FFinishEvent: TEvent;
    FTileDownloadFinishListener: IListenerDisconnectable;

    FAppClosingListener: IListener;

    procedure OnTileDownloadFinish(const AMsg: IInterface);
    procedure OnAppClosing;
    procedure ProcessResult(const AResult: ITileRequestResult);
    procedure SleepCancelable(ATime: Cardinal);

    procedure PrepareStrings;

    function GetElapsedTime: TDateTime;
    function GetDownloaded: Int64;
    function GetDownloadSize: Double;

    constructor CreateInternal(
      ACreatePaused: Boolean;
      const AAppClosingNotifier: INotifierOneOperation;
      const ALog: ILogSimple;
      AMapType: TMapType;
      AZoom: byte;
      const APolygon: ILonLatPolygon;
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
  protected
    procedure Execute; override;
  public
    constructor Create(
      ACreatePaused: Boolean;
      const AAppClosingNotifier: INotifierOneOperation;
      const ALog: ILogSimple;
      const APolygon: ILonLatPolygon;
      const APolyProjected: IProjectedPolygon;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple;
      Azamena: boolean;
      ACheckExistTileSize: boolean;
      Azdate: boolean;
      ASecondLoadTNE: boolean;
      AZoom: byte;
      AMapType: TMapType;
      const AReplaceOlderDate: TDateTime
    );
    constructor CreateFromSls(
      const AAppClosingNotifier: INotifierOneOperation;
      const AVectorItmesFactory: IVectorItmesFactory;
      const ALog: ILogSimple;
      const AFullMapsSet: IMapTypeSet;
      const AProjectionFactory: IProjectionInfoFactory;
      const ASLSSection: IConfigDataProvider;
      const ADownloadConfig: IGlobalDownloadConfig;
      const ADownloadInfo: IDownloadInfoSimple
    );
    destructor Destroy; override;

    procedure SaveToFile(const ASLSSection: IConfigDataWriteProvider);
    procedure DownloadPause;
    procedure DownloadResume;
    property TotalInRegion: Int64 read FTotalInRegion;
    property Downloaded: Int64 read GetDownloaded;
    property Processed: Int64 read FProcessed;
    property DownloadSize: Double read GetDownloadSize;
    property ElapsedTime: TDateTime read GetElapsedTime;
    property Zoom: Byte read FZoom;
    property Finished: Boolean read FFinished;
  end;

implementation

uses
  SysUtils,
  Types,
  i_DownloadResult,
  i_TileRequestTask,
  i_TileInfoBasic,
  i_TileIterator,
  i_TileDownloaderState,
  i_TileStorage,
  u_DownloadInfoSimple,
  u_TileIteratorByPolygon,
  u_ListenerByEvent,
  u_ReadableThreadNames,
  u_ConfigProviderHelpers,
  u_ResStrings;

constructor TThreadDownloadTiles.CreateInternal(
  ACreatePaused: Boolean;
  const AAppClosingNotifier: INotifierOneOperation;
  const ALog: ILogSimple;
  AMapType: TMapType;
  AZoom: byte;
  const APolygon: ILonLatPolygon;
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
  VOperationNotifier: TNotifierOperation;
  VState: ITileDownloaderStateStatic;
begin
  inherited Create(False);
  FPausedByUser := ACreatePaused;
  FAppClosingNotifier := AAppClosingNotifier;
  FDownloadInfo := ADownloadInfo;
  FDownloadConfig := ADownloadConfig;
  FLog := ALog;

  FProcessed := 0;
  FPausedSleepTime := 100;
  FBanSleepTime := 5000;
  FProxyAuthErrorSleepTime := 10000;
  FDownloadErrorSleepTime := 5000;
  Priority := tpLower;

  FReplaceExistTiles := AReplaceExistTiles;
  FZoom := AZoom;
  FCheckExistTileSize := ACheckExistTileSize;
  FMapType := AMapType;
  FCheckTileDate := AReplaceOlderDate;
  FCheckExistTileDate := ACheckExistTileDate;
  FSecondLoadTNE := ASecondLoadTNE;
  FPolygLL := APolygon;
  FPolyProjected := APolyProjected;
  FElapsedTime := AElapsedTime;
  FLastProcessedPoint := ALastProcessedPoint;
  if Terminated then begin
    FFinished := true;
    Exit;
  end;
  if FMapType = nil then begin
    Terminate;
    FFinished := true;
    Exit;
  end;
  if not FMapType.Abilities.UseDownload then begin
    ALog.WriteText(Format('Download of map %s disabled by map params', [FMapType.GUIConfig.Name.Value]), 10);
    Terminate;
    FFinished := true;
    Exit;
  end;
  VState := FMapType.TileDownloadSubsystem.State.GetStatic;
  if not VState.Enabled then begin
    ALog.WriteText(Format('Download of map %s disabled. Reason: %s', [FMapType.GUIConfig.Name.Value, VState.DisableReason]), 10);
    Terminate;
    FFinished := true;
    Exit;
  end;
  if FPolygLL = nil then begin
    ALog.WriteText('Polygon does not exist', 10);
    Terminate;
    FFinished := true;
    Exit;
  end;
  if FPolygLL.Count < 1 then begin
    ALog.WriteText('Empty Polygon', 10);
    Terminate;
    FFinished := true;
    Exit;
  end;

  if FPolyProjected = nil then begin
    ALog.WriteText('Polygon does not exist', 10);
    Terminate;
    FFinished := true;
    Exit;
  end;
  if FPolyProjected.Count < 1 then begin
    ALog.WriteText('Empty Polygon', 10);
    Terminate;
    FFinished := true;
    Exit;
  end;

  PrepareStrings;

  VOperationNotifier := TNotifierOperation.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
  FFinishEvent := TEvent.Create;

  FTileDownloadFinishListener := TNotifyEventListener.Create(Self.OnTileDownloadFinish);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

constructor TThreadDownloadTiles.Create(
  ACreatePaused: Boolean;
  const AAppClosingNotifier: INotifierOneOperation;
  const ALog: ILogSimple;
  const APolygon: ILonLatPolygon;
  const APolyProjected: IProjectedPolygon;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple;
  Azamena, ACheckExistTileSize, Azdate, ASecondLoadTNE: boolean;
  AZoom: byte;
  AMapType: TMapType;
  const AReplaceOlderDate: TDateTime
);
begin
  CreateInternal(
    ACreatePaused,
    AAppClosingNotifier,
    ALog,
    AMapType,
    AZoom,
    APolygon,
    APolyProjected,
    ADownloadConfig,
    TDownloadInfoSimple.Create(ADownloadInfo),
    Azamena,
    ACheckExistTileSize,
    Azdate,
    AReplaceOlderDate,
    ASecondLoadTNE,
    Point(-1, -1),
    0
  );
end;

constructor TThreadDownloadTiles.CreateFromSls(
  const AAppClosingNotifier: INotifierOneOperation;
  const AVectorItmesFactory: IVectorItmesFactory;
  const ALog: ILogSimple;
  const AFullMapsSet: IMapTypeSet;
  const AProjectionFactory: IProjectionInfoFactory;
  const ASLSSection: IConfigDataProvider;
  const ADownloadConfig: IGlobalDownloadConfig;
  const ADownloadInfo: IDownloadInfoSimple
);
var
  VGuids: string;
  VGuid: TGUID;
  VMap: IMapType;
  VZoom: Byte;
  VReplaceExistTiles: Boolean;
  VCheckExistTileSize: Boolean;
  VCheckExistTileDate: Boolean;
  VCheckTileDate: TDateTime;
  VProcessedTileCount: Int64;
  VProcessedSize: Int64;
  VSecondLoadTNE: Boolean;
  VLastProcessedPoint: TPoint;
  VElapsedTime: TDateTime;
  VMapType: TMapType;
  VPolygon: ILonLatPolygon;
  VProjectedPolygon: IProjectedPolygon;
begin
  VMapType := nil;
  VReplaceExistTiles := False;
  VCheckExistTileSize := False;
  VCheckExistTileDate := False;
  VCheckTileDate := Now;
  VSecondLoadTNE := False;
  VElapsedTime := 0;
  VProcessedTileCount := 0;
  VProcessedSize := 0;
  try
    if ASLSSection = nil then begin
      ALog.WriteText('No SLS data', 10);
      Terminate;
      FFinished := true;
      Exit;
    end;
    VGuids := ASLSSection.ReadString('MapGUID', '');
    if VGuids = '' then begin
      ALog.WriteText('Map GUID is empty', 10);
      Terminate;
      FFinished := true;
      Exit;
    end;
    VGuid := StringToGUID(VGuids);
    VZoom := ASLSSection.ReadInteger('Zoom', 0);
    if VZoom > 0 then begin
      Dec(VZoom);
    end else begin
      ALog.WriteText('Unknown zoom', 10);
      Terminate;
      FFinished := true;
      Exit;
    end;
    VReplaceExistTiles := ASLSSection.ReadBool('ReplaceExistTiles', VReplaceExistTiles);
    VCheckExistTileSize := ASLSSection.ReadBool('CheckExistTileSize', VCheckExistTileSize);
    VCheckExistTileDate := ASLSSection.ReadBool('CheckExistTileDate', VCheckExistTileDate);
    VCheckTileDate := ASLSSection.ReadDate('CheckTileDate', VCheckTileDate);
    VProcessedTileCount := ASLSSection.ReadInteger('ProcessedTileCount', VProcessedTileCount);
    VProcessedSize := trunc(ASLSSection.ReadFloat('ProcessedSize', 0) * 1024);

    VSecondLoadTNE := ASLSSection.ReadBool('SecondLoadTNE', VSecondLoadTNE);
    VElapsedTime := ASLSSection.ReadFloat('ElapsedTime', VElapsedTime);
    if ADownloadConfig.IsUseSessionLastSuccess then begin
      VLastProcessedPoint.X := ASLSSection.ReadInteger('LastSuccessfulStartX', -1);
      VLastProcessedPoint.Y := ASLSSection.ReadInteger('LastSuccessfulStartY', -1);
    end else begin
      VLastProcessedPoint.X := ASLSSection.ReadInteger('StartX', -1);
      VLastProcessedPoint.Y := ASLSSection.ReadInteger('StartY', -1);
    end;
    VPolygon := ReadPolygon(ASLSSection, AVectorItmesFactory);
    VMap := AFullMapsSet.GetMapTypeByGUID(VGuid);
    if VMap = nil then begin
      ALog.WriteText(Format('Map with GUID = %s not found', [VGuids]), 10);
    end else begin
      VMapType := VMap.MapType;
      if not VMapType.GeoConvert.CheckZoom(VZoom) then begin
        ALog.WriteText('Unknown zoom', 10);
        Terminate;
        FFinished := true;
        Exit;
      end;
    end;

    if VPolygon.Count > 0 then begin
      VProjectedPolygon :=
        AVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
          AProjectionFactory.GetByConverterAndZoom(
          VMapType.GeoConvert,
          VZoom
        ),
          VPolygon
        );
    end;
  finally
    CreateInternal(
      False,
      AAppClosingNotifier,
      ALog,
      VMapType,
      VZoom,
      VPolygon,
      VProjectedPolygon,
      ADownloadConfig,
      TDownloadInfoSimple.Create(ADownloadInfo, VProcessedTileCount, VProcessedSize),
      VReplaceExistTiles,
      VCheckExistTileSize,
      VCheckExistTileDate,
      VCheckTileDate,
      VSecondLoadTNE,
      VLastProcessedPoint,
      VElapsedTime
    );
  end;
end;

destructor TThreadDownloadTiles.Destroy;
begin
  if FCancelNotifierInternal <> nil then begin
    FCancelNotifierInternal.NextOperation;
    FCancelNotifierInternal := nil;
  end;
  if FTileDownloadFinishListener <> nil then begin
    FTileDownloadFinishListener.Disconnect;
    FTileDownloadFinishListener := nil;
  end;

  if FAppClosingNotifier <> nil then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
  end;
  FAppClosingNotifier := nil;
  FAppClosingListener := nil;

  if FFinishEvent <> nil then begin
    FFinishEvent.SetEvent;
    FreeAndNil(FFinishEvent);
  end;

  FLog := nil;
  inherited;
end;

procedure TThreadDownloadTiles.SaveToFile(const ASLSSection: IConfigDataWriteProvider);
var
  VElapsedTime: TDateTime;
begin
  ASLSSection.WriteString('MapGUID', GUIDToString(FMapType.Zmp.GUID));
  ASLSSection.WriteInteger('Zoom', FZoom + 1);
  ASLSSection.WriteBool('ReplaceExistTiles', FReplaceExistTiles);
  ASLSSection.WriteBool('CheckExistTileSize', FCheckExistTileSize);
  ASLSSection.WriteBool('CheckExistTileDate', FCheckExistTileDate);
  ASLSSection.WriteDate('CheckTileDate', FCheckTileDate);
  ASLSSection.WriteBool('SecondLoadTNE', FSecondLoadTNE);
  ASLSSection.WriteInteger('ProcessedTileCount', FDownloadInfo.TileCount);
  ASLSSection.WriteInteger('Processed', FProcessed);
  ASLSSection.WriteFloat('ProcessedSize', FDownloadInfo.Size / 1024);
  ASLSSection.WriteInteger('StartX', FLastProcessedPoint.X);
  ASLSSection.WriteInteger('StartY', FLastProcessedPoint.Y);
  ASLSSection.WriteInteger('LastSuccessfulStartX', FLastSuccessfulPoint.X);
  ASLSSection.WriteInteger('LastSuccessfulStartY', FLastSuccessfulPoint.Y);
  WritePolygon(ASLSSection, FPolygLL);
  if (FPausedByUser) then begin
    VElapsedTime := FElapsedTime;
  end else begin
    VElapsedTime := FElapsedTime + (Now - FStartTime);
  end;
  ASLSSection.WriteFloat('ElapsedTime', VElapsedTime);
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
  VOperationID: Integer;
  VTask: ITileRequestTask;
begin
  SetCurrentThreadName(AnsiString(Self.ClassName));
  Randomize;
  FStartTime := Now;
  VTileIterator := TTileIteratorByPolygon.Create(FPolyProjected);
  try
    FTotalInRegion := VTileIterator.TilesTotal;
    FLastSuccessfulPoint := Point(-1, -1);
    if (FLastProcessedPoint.X >= 0) and (FLastProcessedPoint.Y >= 0) then begin
      while VTileIterator.Next(VTile) do begin
        Inc(FProcessed);
        if Terminated then begin
          Break;
        end;
        if (VTile.X = FLastProcessedPoint.X) and (VTile.Y = FLastProcessedPoint.Y) then begin
          Break;
        end;
      end;
    end;

    if not Terminated then begin
      while VTileIterator.Next(VTile) do begin
        if Terminated then begin
          Break;
        end;
        FGotoNextTile := false;
        while not FGotoNextTile do begin
          FFinishEvent.ResetEvent;
          if (FPausedByUser) then begin
            FElapsedTime := FElapsedTime + (Now - FStartTime);
            FLog.WriteText(FRES_UserStop, 10);
            While (FPausedByUser) and (not Terminated) do begin
              SleepCancelable(FPausedSleepTime);
            end;
            FStartTime := now;
          end;
          if Terminated then begin
            Break;
          end;

          // notify about current tile
          FLog.WriteText(Format(FRES_ProcessedFile, [FMapType.GetTileShowName(VTile, FZoom)]), 0);
          VTileInfo := FMapType.TileStorage.GetTileInfo(VTile, FZoom, FMapType.VersionConfig.Version, gtimWithData);

          // for attachments need base tile - but even for existing tile some attachments may not exist
          if (FReplaceExistTiles) or not (VTileInfo.IsExists) then begin
            // what to do
            if VTileInfo.IsExists then begin
              FLog.WriteText(FRES_LoadProcessRepl, 0);
            end else begin
              FLog.WriteText(FRES_LoadProcess, 0);
            end;
            if (FCheckExistTileDate) and (VTileInfo.IsExists) and (VTileInfo.LoadDate >= FCheckTileDate) then begin
              // skip existing newer tile (but download attachments)
              if (FLog <> nil) then begin
                FLog.WriteText(FRES_FileBeCreateTime, 0);
              end;
              FLastSuccessfulPoint := VTile;
              FLastProcessedPoint := VTile;
              FGotoNextTile := True;
            end else begin
              try
                if (not (FSecondLoadTNE)) and
                  (VTileInfo.IsExistsTNE) and
                  (FDownloadConfig.IsSaveTileNotExists) then begin
                  // tne found - skip downloading tile
                  if (FLog <> nil) then begin
                    FLog.WriteText('(tne exists)', 0);
                  end;
                  FLastProcessedPoint := VTile;
                  FLastSuccessfulPoint := VTile;
                  FGotoNextTile := True;
                end else begin
                  // download tile
                  VOperationID := FCancelNotifier.CurrentOperation;
                  VTask := FMapType.TileDownloadSubsystem.GetRequestTask(FCancelNotifier, VOperationID, VTile, FZoom, FCheckExistTileSize);
                  if VTask <> nil then begin
                    VTask.FinishNotifier.Add(FTileDownloadFinishListener);
                    FMapType.TileDownloadSubsystem.Download(VTask);
                    if Terminated then begin
                      Break;
                    end;
                    if not VTask.FinishNotifier.IsExecuted then begin
                      FFinishEvent.WaitFor(INFINITE);
                    end;
                    if Terminated then begin
                      Break;
                    end;
                    ProcessResult(VTask.Result);
                    if Terminated then begin
                      Break;
                    end;
                    FLastProcessedPoint := VTile;
                  end else begin
                    FLog.WriteText('Download disabled', 0);
                    FGotoNextTile := False;
                    FPausedByUser := True;
                    Break;
                  end;
                end;
              except
                on E: Exception do begin
                  FLog.WriteText(E.Message, 0);
                  FGotoNextTile := True;
                end;
              end;
            end;
          end else begin
            FLog.WriteText(FRES_FileExistsShort, 0);
            FGotoNextTile := True;
          end;
          if FGotoNextTile then begin
            inc(FProcessed);
          end;
          if Terminated then begin
            Break;
          end;
        end;
        if Terminated then begin
          Break;
        end;
      end;
    end;
  finally
    VTileIterator := nil;
  end;
  if not Terminated then begin
    FLog.WriteText(FRES_ProcessFilesComplete, 0);
    FFinished := true;
  end;
end;

procedure TThreadDownloadTiles.DownloadPause;
begin
  FPausedByUser := True;
end;

procedure TThreadDownloadTiles.DownloadResume;
begin
  FPausedByUser := False;
end;

function TThreadDownloadTiles.GetDownloaded: Int64;
begin
  Result := FDownloadInfo.TileCount;
end;

function TThreadDownloadTiles.GetDownloadSize: Double;
begin
  Result := FDownloadInfo.Size / 1024;
end;

function TThreadDownloadTiles.GetElapsedTime: TDateTime;
begin
  if FFinished or FPausedByUser then begin
    Result := FElapsedTime;
  end else begin
    Result := FElapsedTime + (Now - FStartTime);
  end;
end;

procedure TThreadDownloadTiles.OnAppClosing;
begin
  Terminate;
  FCancelNotifierInternal.NextOperation;
  FFinishEvent.SetEvent;
end;

procedure TThreadDownloadTiles.OnTileDownloadFinish(const AMsg: IInterface);
begin
  FFinishEvent.SetEvent;
end;

procedure TThreadDownloadTiles.PrepareStrings;
begin
  FRES_UserStop := SAS_STR_UserStop;
  FRES_ProcessedFile := SAS_STR_ProcessedFile;
  FRES_LoadProcessRepl := SAS_STR_LoadProcessRepl;
  FRES_LoadProcess := SAS_STR_LoadProcess;
  FRES_LoadAttachmentsBegin := SAS_STR_LoadAttachmentsBegin;
  FRES_LoadAttachmentsEnd_Downloaded := SAS_STR_load;
  FRES_LoadAttachmentsEnd_Skipped := SAS_STR_LoadAttachmentsEnd_Skipped;
  FRES_LoadAttachmentsEnd_Failed := SAS_STR_LoadAttachmentsEnd_Failed;
  FRES_LoadAttachmentsEnd_Cancelled := SAS_STR_LoadAttachmentsEnd_Cancelled;
  FRES_LoadAttachmentsEnd_Nothing := SAS_STR_LoadAttachmentsEnd_Nothing;
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

procedure TThreadDownloadTiles.ProcessResult(const AResult: ITileRequestResult);
var
  VResultOk: IDownloadResultOk;
  VResultBadContentType: IDownloadResultBadContentType;
  VResultDownloadError: IDownloadResultError;
  VResultWithDownload: ITileRequestResultWithDownloadResult;
begin
  if Supports(AResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
    FFinishEvent.ResetEvent;
    if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VResultOk) then begin
      // tile downloaded successfully (try to download attachments)
      if (FLog <> nil) then begin
        FLog.WriteText('(Ok!)', 0);
      end;
      FLastSuccessfulPoint := AResult.Request.Tile;
      FGotoNextTile := True;
      if FDownloadInfo <> nil then begin
        FDownloadInfo.Add(1, VResultOk.Data.Size);
      end;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary) then begin
      // same file size - assuming file the same (but download attachments)
      if (FLog <> nil) then begin
        FLog.WriteText(FRES_FileBeCreateLen, 0);
      end;
      FLastSuccessfulPoint := AResult.Request.Tile;
      FGotoNextTile := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultProxyError) then begin
      FLog.WriteText(FRES_Authorization + #13#10 + Format(FRES_WaitTime, [FProxyAuthErrorSleepTime div 1000]), 10);
      SleepCancelable(FProxyAuthErrorSleepTime);
      FGotoNextTile := false;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultBanned) then begin
      FLog.WriteText(FRES_Ban + #13#10 + Format(FRES_WaitTime, [FBanSleepTime div 1000]), 10);
      SleepCancelable(FBanSleepTime);
      FGotoNextTile := false;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultBadContentType, VResultBadContentType) then begin
      FLog.WriteText(Format(FRES_BadMIME, [VResultBadContentType.ContentType]), 1);
      FGotoNextTile := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultDataNotExists) then begin
      FLog.WriteText(FRES_TileNotExists, 1);
      FGotoNextTile := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
      if Supports(VResultWithDownload.DownloadResult, IDownloadResultNoConnetctToServer) then begin
        FLog.WriteText(VResultDownloadError.ErrorText + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
        SleepCancelable(FDownloadErrorSleepTime);
        FGotoNextTile := false;
      end else begin
        FLog.WriteText(FRES_Noconnectionstointernet + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
        SleepCancelable(FDownloadErrorSleepTime);
        if FDownloadConfig.IsGoNextTileIfDownloadError then begin
          FGotoNextTile := True;
        end else begin
          FGotoNextTile := False;
        end;
      end;
    end;
  end else begin
    if Supports(AResult, ITileRequestResultCanceled) then begin
      FGotoNextTile := False;
    end else begin
      FGotoNextTile := True;
    end;
  end;
end;

end.
