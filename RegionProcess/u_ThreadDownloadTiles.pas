unit u_ThreadDownloadTiles;

interface

uses
  Windows,
  SyncObjs,
  Classes,
  t_GeoTypes,
  i_JclNotify,
  i_LogSimple,
  i_OperationNotifier,
  i_GlobalDownloadConfig,
  i_TileRequestResult,
  i_VectorItemLonLat,
  i_VectorItmesFactory,
  i_DownloadInfoSimple,
  i_MapTypes,
  u_MapType,
  u_OperationNotifier;

type
  TThreadDownloadTiles = class(TThread)
  private
    FAppClosingNotifier: IJclNotifier;
    FMapType: TMapType;
    FDownloadInfo: IDownloadInfoSimple;
    FPolygLL: ILonLatPolygonLine;
    FSecondLoadTNE:boolean;
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
    FDownloadPause: Boolean;
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


    FCancelNotifier: IOperationNotifier;
    FCancelNotifierInternal: IOperationNotifierInternal;
    FFinishEvent: TEvent;
    FTileDownloadFinishListener: IJclListenerDisconnectable;

    FAppClosingListener: IJclListener;
    FResult: ITileRequestResult;

    procedure OnTileDownloadFinish(AMsg: IInterface);
    procedure OnAppClosing;
    procedure ProcessResult(AResult: ITileRequestResult);
    procedure SleepCancelable(ATime: Cardinal);

    procedure PrepareStrings;

    function GetElapsedTime: TDateTime;
    function GetDownloaded: Int64;
    function GetDownloadSize: Double;

    constructor CreateInternal(
      AAppClosingNotifier: IJclNotifier;
      ALog: ILogSimple;
      AMapType: TMapType;
      AZoom: byte;
      APolygon: ILonLatPolygonLine;
      ADownloadConfig: IGlobalDownloadConfig;
      ADownloadInfo: IDownloadInfoSimple;
      AReplaceExistTiles: Boolean;
      ACheckExistTileSize: Boolean;
      ACheckExistTileDate: Boolean;
      AReplaceOlderDate: TDateTime;
      ASecondLoadTNE: Boolean;
      ALastProcessedPoint: TPoint;
      AProcessed: Int64;
      AElapsedTime: TDateTime
    );
  protected
    procedure Execute; override;
  public
    constructor Create(
      AAppClosingNotifier: IJclNotifier;
      ALog: ILogSimple;
      APolygon: ILonLatPolygonLine;
      ADownloadConfig: IGlobalDownloadConfig;
      ADownloadInfo: IDownloadInfoSimple;
      Azamena: boolean;
      ACheckExistTileSize: boolean;
      Azdate: boolean;
      ASecondLoadTNE: boolean;
      AZoom: byte;
      Atypemap: TMapType;
      AReplaceOlderDate: TDateTime
    );
    constructor CreateFromSls(
      AAppClosingNotifier: IJclNotifier;
      AVectorItmesFactory: IVectorItmesFactory;
      ALog: ILogSimple;
      AFullMapsSet: IMapTypeSet;
      FileName: string;
      ADownloadConfig: IGlobalDownloadConfig;
      ADownloadInfo: IDownloadInfoSimple;
      AZoom: Byte
    );
    destructor Destroy; override;

    procedure SaveToFile(AFileName: string);
    procedure DownloadPause;
    procedure DownloadResume;
    property TotalInRegion: Int64 read FTotalInRegion;
    property Downloaded: Int64 read GetDownloaded;
    property Processed: Int64 read FProcessed;
    property DownloadSize: Double read GetDownloadSize;
    property ElapsedTime: TDateTime read GetElapsedTime;
    property StartTime: TDateTime read FStartTime;
    property Zoom: Byte read FZoom;
    property Finished: Boolean read FFinished;
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils,
  IniFiles,
  Types,
  i_DownloadResult,
  i_EnumDoublePoint,
  i_TileRequest,
  i_TileIterator,
  i_TileDownloaderState,
  u_DownloadInfoSimple,
  u_TileIteratorStuped,
  u_NotifyEventListener,
  u_ResStrings;

constructor TThreadDownloadTiles.CreateInternal(
  AAppClosingNotifier: IJclNotifier;
  ALog: ILogSimple;
  AMapType: TMapType;
  AZoom: byte;
  APolygon: ILonLatPolygonLine;
  ADownloadConfig: IGlobalDownloadConfig;
  ADownloadInfo: IDownloadInfoSimple;
  AReplaceExistTiles, ACheckExistTileSize, ACheckExistTileDate: Boolean;
  AReplaceOlderDate: TDateTime; ASecondLoadTNE: Boolean;
  ALastProcessedPoint: TPoint;
  AProcessed: Int64;
  AElapsedTime: TDateTime
);
var
  VOperationNotifier: TOperationNotifier;
  VState: ITileDownloaderStateStatic;
begin
  inherited Create(False);

  FAppClosingNotifier := AAppClosingNotifier;
  FDownloadInfo := ADownloadInfo;
  FPausedSleepTime := 100;
  FBanSleepTime := 5000;
  FProxyAuthErrorSleepTime := 10000;
  FDownloadErrorSleepTime := 5000;
  PrepareStrings;

  FDownloadConfig := ADownloadConfig;
  FLog := ALog;
  Priority := tpLower;
  FReplaceExistTiles := AReplaceExistTiles;
  Fzoom := AZoom;
  FCheckExistTileSize := ACheckExistTileSize;
  FMapType := AMapType;
  FCheckTileDate := AReplaceOlderDate;
  FCheckExistTileDate := ACheckExistTileDate;
  FSecondLoadTNE := ASecondLoadTNE;
  FPolygLL := APolygon;
  FProcessed := AProcessed;
  FElapsedTime := AElapsedTime;
  FLastProcessedPoint := ALastProcessedPoint;

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
  if FPolygLL.Count < 3 then begin
    ALog.WriteText('Empty Polygon', 10);
    Terminate;
    FFinished := true;
    Exit;
  end;

  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
  FFinishEvent := TEvent.Create;

  FTileDownloadFinishListener := TNotifyEventListener.Create(Self.OnTileDownloadFinish);
  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
end;

constructor TThreadDownloadTiles.Create(
  AAppClosingNotifier: IJclNotifier;
  ALog: ILogSimple;
  APolygon: ILonLatPolygonLine;
  ADownloadConfig: IGlobalDownloadConfig;
  ADownloadInfo: IDownloadInfoSimple;
  Azamena, ACheckExistTileSize, Azdate, ASecondLoadTNE: boolean;
  AZoom: byte;
  Atypemap: TMapType;
  AReplaceOlderDate: TDateTime
);
begin
  CreateInternal(
    AAppClosingNotifier,
    ALog,
    Atypemap,
    AZoom,
    APolygon,
    ADownloadConfig,
    TDownloadInfoSimple.Create(ADownloadInfo),
    Azamena,
    ACheckExistTileSize,
    Azdate,
    AReplaceOlderDate,
    ASecondLoadTNE,
    Point(-1,-1),
    0,
    0
  );
end;

constructor TThreadDownloadTiles.CreateFromSls(
  AAppClosingNotifier: IJclNotifier;
  AVectorItmesFactory: IVectorItmesFactory;
  ALog: ILogSimple;
  AFullMapsSet: IMapTypeSet;
  FileName:string;
  ADownloadConfig: IGlobalDownloadConfig;
  ADownloadInfo: IDownloadInfoSimple;
  AZoom: Byte
);
var
  Ini: Tinifile;
  i: integer;
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
  VProcessed: Int64;
  VSecondLoadTNE: Boolean;
  VLastProcessedPoint: TPoint;
  VPolygLL: TArrayOfDoublePoint;
  VElapsedTime: TDateTime;
  VMapType: TMapType;
begin
  Ini:=TiniFile.Create(FileName);
  try
    VGuids:=Ini.ReadString('Session','MapGUID','');
    VGuid := StringToGUID(VGuids);
    VZoom := Ini.ReadInteger('Session', 'zoom', AZoom + 1) - 1;
    VReplaceExistTiles := Ini.ReadBool('Session', 'zamena', false);
    VCheckExistTileSize := Ini.ReadBool('Session','raz', false);
    VCheckExistTileDate := Ini.ReadBool('Session','zdate', false);
    VCheckTileDate := Ini.ReadDate('Session', 'FDate', now);
    VProcessedTileCount := Ini.ReadInteger('Session', 'scachano', 0);
    VProcessedSize := trunc(Ini.ReadFloat('Session','dwnb', 0)*1024);

    VProcessed := Ini.ReadInteger('Session', 'obrab', 0);
    VSecondLoadTNE:=Ini.ReadBool('Session', 'SecondLoadTNE', false);
    if FDownloadConfig.IsUseSessionLastSuccess then begin
      VLastProcessedPoint.X:=Ini.ReadInteger('Session','LastSuccessfulStartX',-1);
      VLastProcessedPoint.Y:=Ini.ReadInteger('Session','LastSuccessfulStartY',-1);
    end else begin
      VLastProcessedPoint.X:=Ini.ReadInteger('Session','StartX',-1);
      VLastProcessedPoint.Y:=Ini.ReadInteger('Session','StartY',-1);
    end;
    i:=1;
    while Ini.ReadFloat('Session','LLPointX_'+inttostr(i),-10000)>-10000 do begin
      setlength(VPolygLL, i);
      VPolygLL[i-1].x := Ini.ReadFloat('Session','LLPointX_'+inttostr(i),-10000);
      VPolygLL[i-1].y := Ini.ReadFloat('Session','LLPointY_'+inttostr(i),-10000);
      inc(i);
    end;
    VElapsedTime := Ini.ReadFloat('Session', 'ElapsedTime', 0);
  finally
    ini.Free;
  end;
  VMapType := nil;
  VMap := AFullMapsSet.GetMapTypeByGUID(VGuid);
  if VMap = nil then begin
    ALog.WriteText(Format('Map with GUID = %s not found', [VGuids]), 10);
  end else begin
    VMapType := VMap.MapType;
  end;

  CreateInternal(
    AAppClosingNotifier,
    ALog,
    VMapType,
    VZoom,
    AVectorItmesFactory.CreateLonLatPolygon(@VPolygLL[0], Length(VPolygLL)).Item[0],
    ADownloadConfig,
    TDownloadInfoSimple.Create(ADownloadInfo, VProcessedTileCount, VProcessedSize),
    VReplaceExistTiles,
    VCheckExistTileSize,
    VCheckExistTileDate,
    VCheckTileDate,
    VSecondLoadTNE,
    VLastProcessedPoint,
    VProcessed,
    VElapsedTime
  );
end;

destructor TThreadDownloadTiles.Destroy;
begin
  FCancelNotifierInternal.NextOperation;
  FTileDownloadFinishListener.Disconnect;

  FAppClosingNotifier.Remove(FAppClosingListener);
  FAppClosingNotifier := nil;
  FAppClosingListener := nil;

  FFinishEvent.SetEvent;
  FreeAndNil(FFinishEvent);

  FLog := nil;
  inherited;
end;

procedure TThreadDownloadTiles.SaveToFile(AFileName: string);
var
  Ini: Tinifile;
  i:integer;
  VElapsedTime: TDateTime;
  VEnum: IEnumDoublePoint;
  VPoint: TDoublePoint;
begin
  Ini:=TiniFile.Create(AFileName);
  try
    Ini.WriteString('Session', 'MapGUID', GUIDToString(FMapType.Zmp.GUID));
    Ini.WriteInteger('Session', 'zoom', Fzoom + 1);
    Ini.WriteBool('Session', 'zamena', FReplaceExistTiles);
    Ini.WriteBool('Session', 'raz', FCheckExistTileSize);
    Ini.WriteBool('Session', 'zdate', FCheckExistTileDate);
    Ini.WriteDate('Session', 'FDate', FCheckTileDate);
    Ini.WriteBool('Session', 'SecondLoadTNE', FSecondLoadTNE);
    Ini.WriteInteger('Session', 'scachano', FDownloadInfo.TileCount);
    Ini.WriteInteger('Session', 'obrab', FProcessed);
    Ini.WriteFloat('Session', 'dwnb', FDownloadInfo.Size / 1024);
    Ini.WriteInteger('Session', 'StartX', FLastProcessedPoint.X);
    Ini.WriteInteger('Session', 'StartY', FLastProcessedPoint.Y);
    Ini.WriteInteger('Session', 'LastSuccessfulStartX', FLastSuccessfulPoint.X);
    Ini.WriteInteger('Session', 'LastSuccessfulStartY', FLastSuccessfulPoint.Y);
    VEnum := FPolygLL.GetEnum;
    i := 0;
    while VEnum.Next(VPoint) do begin
      Ini.WriteFloat('Session', 'LLPointX_'+inttostr(i), VPoint.x);
      Ini.WriteFloat('Session', 'LLPointY_'+inttostr(i), VPoint.y);
      Inc(i);
    end;
    if (FDownloadPause) then begin
      VElapsedTime := FElapsedTime;
    end else begin
      VElapsedTime := FElapsedTime + (Now - FStartTime);
    end;
    Ini.WriteFloat('Session', 'ElapsedTime', VElapsedTime);
  finally
    ini.Free;
  end;
end;

procedure TThreadDownloadTiles.SleepCancelable(ATime: Cardinal);
begin
  FFinishEvent.WaitFor(ATime);
end;

procedure TThreadDownloadTiles.Execute;
var
  VTileExists: boolean;
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VOperationID: Integer;
  VRequest: ITileRequest;
begin
  Randomize;
  FStartTime := Now;
  if (FMapType.TileDownloaderConfig.IteratorSubRectSize.X=1)and
     (FMapType.TileDownloaderConfig.IteratorSubRectSize.Y=1) then begin
    VTileIterator := TTileIteratorStuped.Create(FZoom, FPolygLL, FMapType.GeoConvert);
  end else begin
    VTileIterator := TTileIteratorBySubRect.Create(FZoom, FPolygLL, FMapType.GeoConvert,
                      FMapType.TileDownloaderConfig.IteratorSubRectSize);
  end;
  try
    FTotalInRegion := VTileIterator.TilesTotal;
    FLastSuccessfulPoint := Point(-1,-1);
    if (FLastProcessedPoint.X >= 0) and (FLastProcessedPoint.Y >= 0)  then begin
      while VTileIterator.Next(VTile) do begin
        if Terminated then begin
          Break;;
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
        FGoToNextTile := false;
        while not FGoToNextTile do begin
          FFinishEvent.ResetEvent;
          if (FDownloadPause) then begin
            FElapsedTime := FElapsedTime + (Now - FStartTime);
            FLog.WriteText(FRES_UserStop, 10);
            While (FDownloadPause)and (not Terminated) do SleepCancelable(FPausedSleepTime);
            FStartTime := now;
          end;

          FLog.WriteText(Format(FRES_ProcessedFile, [FMapType.GetTileShowName(VTile, Fzoom)]), 0);
          VTileExists := FMapType.TileExists(VTile, Fzoom);
          if (FReplaceExistTiles) or not(VTileExists) then begin
            if VTileExists then begin
              FLog.WriteText(FRES_LoadProcessRepl, 0);
            end else begin
              FLog.WriteText(FRES_LoadProcess+'...', 0);
            end;
            if (FCheckExistTileDate)
              and (VTileExists)
              and (FMapType.TileLoadDate(VTile, Fzoom) >= FCheckTileDate) then
            begin
              FLog.WriteText(FRES_FileBeCreateTime, 0);
              FLastSuccessfulPoint := VTile;
              FLastProcessedPoint := VTile;
              FGoToNextTile := True;
            end else begin
              try
                if (not(FSecondLoadTNE))and(FMapType.TileNotExistsOnServer(VTile, Fzoom))and(FDownloadConfig.IsSaveTileNotExists) then begin
                  FLog.WriteText('(tne exists)', 0);
                  FLastProcessedPoint := VTile;
                  FLastSuccessfulPoint := VTile;
                  FGoToNextTile := True;
                end else begin
                  VOperationID := FCancelNotifier.CurrentOperation;
                  VRequest := FMapType.TileDownloadSubsystem.GetRequest(FCancelNotifier, VOperationID, VTile, FZoom, FCheckExistTileSize);
                  VRequest.FinishNotifier.Add(FTileDownloadFinishListener);
                  FMapType.TileDownloadSubsystem.Download(VRequest);
                  if Terminated then begin
                    Break;
                  end;
                  FFinishEvent.WaitFor(INFINITE);
                  if Terminated then begin
                    Break;
                  end;
                  ProcessResult(FResult);
                  if Terminated then begin
                    Break;
                  end;
                  FLastProcessedPoint := VTile;
                end;
              except
                on E: Exception do begin
                  FLog.WriteText(E.Message, 0);
                  FGoToNextTile := True;
                end;
              end;
            end;
          end else begin
            FLog.WriteText(FRES_FileExistsShort, 0);
            FGoToNextTile := True;
          end;
          if FGoToNextTile then begin
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
  FDownloadPause := True;
end;

procedure TThreadDownloadTiles.DownloadResume;
begin
  FDownloadPause := False;
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
  if FFinished or FDownloadPause then begin
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

procedure TThreadDownloadTiles.OnTileDownloadFinish(AMsg: IInterface);
var
  VResult: ITileRequestResult;
begin
  VResult := AMsg as ITileRequestResult;
  FResult := VResult;
  FFinishEvent.SetEvent;
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

procedure TThreadDownloadTiles.ProcessResult(AResult: ITileRequestResult);
var
  VResultOk: IDownloadResultOk;
  VResultBadContentType: IDownloadResultBadContentType;
  VResultDownloadError: IDownloadResultError;
  VResultWithDownload: ITileRequestResultWithDownloadResult;
begin
  if Supports(AResult, ITileRequestResultWithDownloadResult, VResultWithDownload) then begin
    FFinishEvent.ResetEvent;
    if Supports(VResultWithDownload.DownloadResult, IDownloadResultOk, VResultOk) then begin
      FLastSuccessfulPoint := AResult.Request.Tile;
      if FDownloadInfo <> nil then begin
        FDownloadInfo.Add(1, VResultOk.Size);
      end;
      FGoToNextTile := True;
      FLog.WriteText('(Ok!)', 0);
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultNotNecessary) then begin
      FLastSuccessfulPoint := AResult.Request.Tile;
      FLog.WriteText(FRES_FileBeCreateLen, 0);
      FGoToNextTile := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultProxyError) then begin
      FLog.WriteText(FRES_Authorization + #13#10 + Format(FRES_WaitTime,[FProxyAuthErrorSleepTime div 1000]), 10);
      SleepCancelable(FProxyAuthErrorSleepTime);
      FGoToNextTile := false;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultBanned) then begin
      FLog.WriteText(FRES_Ban + #13#10 + Format(FRES_WaitTime, [FBanSleepTime div 1000]), 10);
      SleepCancelable(FBanSleepTime);
      FGoToNextTile := false;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultBadContentType, VResultBadContentType) then begin
      FLog.WriteText(Format(FRES_BadMIME, [VResultBadContentType.ContentType]), 1);
      FGoToNextTile := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultDataNotExists) then begin
      FLog.WriteText(FRES_TileNotExists, 1);
      FGoToNextTile := True;
    end else if Supports(VResultWithDownload.DownloadResult, IDownloadResultError, VResultDownloadError) then begin
      if Supports(VResultWithDownload.DownloadResult, IDownloadResultNoConnetctToServer) then begin
        FLog.WriteText(VResultDownloadError.ErrorText + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
        SleepCancelable(FDownloadErrorSleepTime);
        FGoToNextTile := false;
      end else begin
        FLog.WriteText(FRES_Noconnectionstointernet + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
        SleepCancelable(FDownloadErrorSleepTime);
        if FDownloadConfig.IsGoNextTileIfDownloadError then begin
          FGoToNextTile := True;
        end else begin
          FGoToNextTile := False;
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

