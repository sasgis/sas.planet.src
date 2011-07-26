unit u_ThreadDownloadTiles;

interface

uses
  Windows,
  Classes,
  i_LogSimple,
  t_GeoTypes,
  i_DownloadInfoSimple,
  u_MapType,
  u_TileDownloaderThreadBase;

type
  TThreadDownloadTiles = class(TTileDownloaderThreadBase)
  private
    FPolygLL: TArrayOfDoublePoint;
    FSecondLoadTNE:boolean;
    FReplaceExistTiles: boolean;
    FCheckExistTileSize: boolean;
    FCheckExistTileDate: boolean;
    FCheckTileDate: TDateTime;
    FLastProcessedPoint: TPoint;
    FLastSuccessfulPoint: TPoint;

    FTotalInRegion: Int64;
    FDownloadInfo: IDownloadInfoSimple;
    FProcessed: Int64;

    FElapsedTime: TDateTime;
    FStartTime: TDateTime;

    FLog: ILogSimple;
    FDownloadPause: Boolean;
    FFinished: Boolean;
    FZoom: Byte;

    FPausedSleepTime: Cardinal;
    FBanSleepTime: Cardinal;
    FProxyAuthErrorSleepTime: Cardinal;
    FDownloadErrorSleepTime: Cardinal;
    FIsGoNextTileIfDownloadError: Boolean;

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
    procedure PrepareStrings;

    function GetElapsedTime: TDateTime;
    function GetDownloaded: Int64;
    function GetDownloadSize: Double;

  protected
    procedure Execute; override;
  public
    constructor Create(
      ALog: ILogSimple;
      APolygon: TArrayOfDoublePoint;
      Azamena: boolean;
      ACheckExistTileSize: boolean;
      Azdate: boolean;
      ASecondLoadTNE: boolean;
      AZoom: byte;
      Atypemap: TMapType;
      AReplaceOlderDate: TDateTime
    );overload;
    constructor Create(ALog: ILogSimple; FileName:string;LastSuccessful:boolean; AZoom: Byte); overload;
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
  end;

implementation

uses
  SysUtils,
  IniFiles,
  Types,
  i_DownloadResult,
  u_GlobalState,
  i_TileIterator,
  u_DownloadInfoSimple,
  u_TileIteratorStuped,
  u_ResStrings;

constructor TThreadDownloadTiles.Create(
  ALog: ILogSimple;
  APolygon: TArrayOfDoublePoint;
  Azamena, ACheckExistTileSize, Azdate, ASecondLoadTNE: boolean;
  AZoom: byte;
  Atypemap: TMapType;
  AReplaceOlderDate: TDateTime
);
begin
  inherited Create(false);

  FPausedSleepTime := 100;
  FBanSleepTime := 5000;
  FProxyAuthErrorSleepTime := 10000;
  FDownloadErrorSleepTime := 5000;
  PrepareStrings;

  FIsGoNextTileIfDownloadError := GState.DownloadConfig.IsGoNextTileIfDownloadError;
  FDownloadInfo := TDownloadInfoSimple.Create(GState.DownloadInfo);
  FLog := ALog;
  Priority := tpLower;
  FReplaceExistTiles:=Azamena;
  Fzoom:=AZoom;
  FCheckExistTileSize := ACheckExistTileSize;
  FMapType := Atypemap;
  FCheckTileDate := AReplaceOlderDate;
  FCheckExistTileDate := AzDate;
  FSecondLoadTNE := ASecondLoadTNE;
  FPolygLL := copy(APolygon);
  FProcessed := 0;
  FElapsedTime := 0;
  FLastProcessedPoint := Point(-1,-1);
  randomize;
end;

constructor TThreadDownloadTiles.Create(ALog: ILogSimple; FileName:string; LastSuccessful:boolean; AZoom: Byte);
var
  Ini: Tinifile;
  i: integer;
  VGuids: string;
  VGuid: TGUID;
begin
  inherited Create(false);
  FPausedSleepTime := 100;
  FBanSleepTime := 5000;
  FProxyAuthErrorSleepTime := 10000;
  FDownloadErrorSleepTime := 5000;
  PrepareStrings;

  FLog := ALog;
  Priority := tpLower;
  Ini:=TiniFile.Create(FileName);
  try
    VGuids:=Ini.ReadString('Session','MapGUID','');
    VGuid := StringToGUID(VGuids);
    Fzoom := Ini.ReadInteger('Session', 'zoom', AZoom + 1) - 1;
    FReplaceExistTiles := Ini.ReadBool('Session', 'zamena', false);
    FCheckExistTileSize := Ini.ReadBool('Session','raz', false);
    FCheckExistTileDate := Ini.ReadBool('Session','zdate', false);
    FCheckTileDate := Ini.ReadDate('Session', 'FDate', now);
    FDownloadInfo :=
      TDownloadInfoSimple.Create(
        GState.DownloadInfo,
        Ini.ReadInteger('Session', 'scachano', 0),
        trunc(Ini.ReadFloat('Session','dwnb', 0)*1024)
      );
    FProcessed := Ini.ReadInteger('Session', 'obrab', 0);
    FSecondLoadTNE:=Ini.ReadBool('Session', 'SecondLoadTNE', false);
    if LastSuccessful then begin
      FLastProcessedPoint.X:=Ini.ReadInteger('Session','LastSuccessfulStartX',-1);
      FLastProcessedPoint.Y:=Ini.ReadInteger('Session','LastSuccessfulStartY',-1);
    end else begin
      FLastProcessedPoint.X:=Ini.ReadInteger('Session','StartX',-1);
      FLastProcessedPoint.Y:=Ini.ReadInteger('Session','StartY',-1);
    end;
    i:=1;
    while Ini.ReadFloat('Session','LLPointX_'+inttostr(i),-10000)>-10000 do begin
      setlength(FPolygLL, i);
      FPolygLL[i-1].x := Ini.ReadFloat('Session','LLPointX_'+inttostr(i),-10000);
      FPolygLL[i-1].y := Ini.ReadFloat('Session','LLPointY_'+inttostr(i),-10000);
      inc(i);
    end;
    FElapsedTime := Ini.ReadFloat('Session', 'ElapsedTime', 0);
  finally
    ini.Free;
  end;
  FMapType := GState.MapType.GetMapFromID(VGuid);
  if FMapType = nil then Terminate;
  if length(FPolygLL) = 0 then Terminate;
  if Terminated then begin
    FFinished := true;
  end else begin
    randomize;
  end;
end;

destructor TThreadDownloadTiles.Destroy;
begin
  FLog := nil;
  inherited;
end;

procedure TThreadDownloadTiles.SaveToFile(AFileName: string);
var
  Ini: Tinifile;
  i:integer;
  VElapsedTime: TDateTime;
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
    for i := 1 to length(FPolygLL) do begin
      Ini.WriteFloat('Session', 'LLPointX_'+inttostr(i), FPolygLL[i-1].x);
      Ini.WriteFloat('Session', 'LLPointY_'+inttostr(i), FPolygLL[i-1].y);
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

procedure TThreadDownloadTiles.Execute;
var
  VTileExists: boolean;
  VResult: IDownloadResult;
  VGotoNextTile: Boolean;
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VResultOk: IDownloadResultOk;
  VResultBadContentType: IDownloadResultBadContentType;
  VResultDownloadError: IDownloadResultError;
begin
  FStartTime := Now;

  VTileIterator := TTileIteratorStuped.Create(FZoom, FPolygLL, FMapType.GeoConvert);
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
        VGotoNextTile := false;
        while not VGotoNextTile do begin
          if (FDownloadPause) then begin
            FElapsedTime := FElapsedTime + (Now - FStartTime);
            FLog.WriteText(FRES_UserStop, 10);
            While (FDownloadPause)and (not Terminated) do SleepCancelable(FPausedSleepTime);
            FStartTime := now;
          end;
          FLoadXY := VTile;

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
              VGotoNextTile := True;
            end else begin
                try
                  if (not(FSecondLoadTNE))and(FMapType.TileNotExistsOnServer(VTile, Fzoom))and(GState.DownloadConfig.IsSaveTileNotExists) then begin
                    FLog.WriteText('(tne exists)', 0);
                    VGotoNextTile := True;
                    FLastProcessedPoint := FLoadXY;
                  end else begin
                    VResult:=FMapType.DownloadTile(FCancelNotifier, VTile, FZoom, FCheckExistTileSize);
                    if Terminated then begin
                      Break;
                    end;
                    FLastProcessedPoint := FLoadXY;
                    if Supports(VResult, IDownloadResultOk, VResultOk) then begin
                      FDownloadInfo.Add(1, VResultOk.Size);
                      FLog.WriteText('(Ok!)', 0);
                      VGotoNextTile := True;
                    end else if Supports(VResult, IDownloadResultNotNecessary) then begin
                      FLastSuccessfulPoint := FLoadXY;
                      FLog.WriteText(FRES_FileBeCreateLen, 0);
                      VGotoNextTile := True;
                    end else if Supports(VResult, IDownloadResultProxyError) then begin
                      FLog.WriteText(FRES_Authorization + #13#10 + Format(FRES_WaitTime,[FProxyAuthErrorSleepTime div 1000]), 10);
                      SleepCancelable(FProxyAuthErrorSleepTime);
                      VGotoNextTile := false;
                    end else if Supports(VResult, IDownloadResultBanned) then begin
                      FLog.WriteText(FRES_Ban + #13#10 + Format(FRES_WaitTime, [FBanSleepTime div 1000]), 10);
                      SleepCancelable(FBanSleepTime);
                      VGotoNextTile := false;
                    end else if Supports(VResult, IDownloadResultBadContentType, VResultBadContentType) then begin
                      FLog.WriteText(Format(FRES_BadMIME, [VResultBadContentType.ContentType]), 1);
                      VGotoNextTile := True;
                    end else if Supports(VResult, IDownloadResultDataNotExists) then begin
                      FLog.WriteText(FRES_TileNotExists, 1);
                      VGotoNextTile := True;
                    end else if Supports(VResult, IDownloadResultError, VResultDownloadError) then begin
                      if Supports(VResult, IDownloadResultNoConnetctToServer) then begin
                        FLog.WriteText(VResultDownloadError.ErrorText + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
                        SleepCancelable(FDownloadErrorSleepTime);
                        VGotoNextTile := false;
                      end else begin
                        FLog.WriteText(FRES_Noconnectionstointernet + #13#10 + Format(FRES_WaitTime, [FDownloadErrorSleepTime div 1000]), 10);
                        SleepCancelable(FDownloadErrorSleepTime);
                        if FIsGoNextTileIfDownloadError then begin
                          VGotoNextTile := True;
                        end else begin
                          VGotoNextTile := false;
                        end;
                      end;
                    end;
                  end;
                except
                  on E: Exception do begin
                    FLog.WriteText(E.Message, 0);
                    VGotoNextTile := True;
                  end;
                end;
            end;
          end else begin
            FLog.WriteText(FRES_FileExistsShort, 0);
            VGotoNextTile := True;
          end;
          if VGotoNextTile then begin
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

end.

