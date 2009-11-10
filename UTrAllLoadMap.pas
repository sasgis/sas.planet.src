unit UTrAllLoadMap;
interface

uses
  Windows,
  Classes,
  Wininet,
  i_ILogSimple,
  t_GeoTypes,
  UMapType,
  u_TileDownloaderBase,
  u_TileDownloaderThreadBase;

type
  ThreadAllLoadMap = class(TTileDownloaderThreadBase)
  private
    FRegionPoly: TPointArray;
    FSecondLoadTNE:boolean;
    FReplaceExistTiles: boolean;
    FCheckExistTileSize: boolean;
    FCheckExistTileDate: boolean;
    FCheckTileDate: TDateTime;
    FLastProcessedPoint: TPoint;
    FLastSuccessfulPoint: TPoint;
    FRegionRect: TRect;

    FTotalInRegion: Cardinal;
    FDownloaded: Cardinal;
    FDownloadSize: Double;
    FProcessed: Cardinal;

    FElapsedTime: TDateTime;
    FStartTime: TDateTime;

    FLog: ILogSimple;
    FZoom: Byte;
    FDownloadPause: Boolean;
    FFinished: Boolean;

  protected
    procedure Execute; override;
  public
    constructor Create(ALog: ILogSimple; APolygon_:TPointArray; Azamena,ACheckExistTileSize,Azdate,ASecondLoadTNE:boolean;AZoom:byte;Atypemap:TMapType;AFDate:TDateTime);overload;
    constructor Create(ALog: ILogSimple; FileName:string;LastSuccessful:boolean); overload;
    destructor Destroy; override;

    procedure SaveToFile(AFileName: string);
    procedure DownloadPause;
    procedure DownloadResume;

    property TotalInRegion: Cardinal read FTotalInRegion;
    property Downloaded: Cardinal read FDownloaded;
    property Processed: Cardinal read FProcessed;
    property DownloadSize: Double read FDownloadSize;
    property ElapsedTime: TDateTime read FElapsedTime;
    property StartTime: TDateTime read FStartTime;
    property Zoom: Byte read FZoom;
    property Finished: Boolean read FFinished;
  end;

implementation
uses
  SysUtils,
  IniFiles,
  DateUtils,
  StrUtils,
  Math,
  Types,
  u_GlobalState,
  UGeoFun,
  UResStrings;

constructor ThreadAllLoadMap.Create(ALog: ILogSimple; APolygon_:TPointArray;Azamena,ACheckExistTileSize,Azdate,ASecondLoadTNE:boolean;AZoom:byte;Atypemap:TMapType;AFDate:TDateTime);
var
  i: integer;
begin
  inherited Create(false);
  FLog := ALog;
  Priority := tpLower;
  FReplaceExistTiles:=Azamena;
  Fzoom:=AZoom;
  FCheckExistTileSize := ACheckExistTileSize;
  FTypeMap := Atypemap;
  FCheckTileDate := AFDate;
  FCheckExistTileDate := AzDate;
  FSecondLoadTNE := ASecondLoadTNE;
  SetLength(FRegionPoly, length(APolygon_));
  for i := 0 to length(APolygon_) - 1 do begin
    FRegionPoly[i] := Apolygon_[i];
  end;
  FTotalInRegion := GetDwnlNum(FRegionRect.TopLeft, FRegionRect.BottomRight, FRegionPoly, true);
  FDownloaded := 0;
  FProcessed := 0;
  FDownloadSize := 0;
  FElapsedTime := 0;
  randomize;
end;

constructor ThreadAllLoadMap.Create(ALog: ILogSimple; FileName:string;LastSuccessful:boolean);
var
  Ini: Tinifile;
  i: integer;
  Guids: string;
begin
  inherited Create(false);
  FLog := ALog;
  Priority := tpLower;
  Ini:=TiniFile.Create(FileName);
  try
    Guids:=Ini.ReadString('Session','MapGUID','');
    Fzoom := Ini.ReadInteger('Session', 'zoom', GState.zoom_size);
    FReplaceExistTiles := Ini.ReadBool('Session', 'zamena', false);
    FCheckExistTileSize := Ini.ReadBool('Session','raz', false);
    FCheckExistTileDate := Ini.ReadBool('Session','zdate', false);
    FCheckTileDate := Ini.ReadDate('Session', 'FDate', now);
    FDownloaded := Ini.ReadInteger('Session', 'scachano', 0);
    FProcessed := Ini.ReadInteger('Session', 'obrab', 0);
    FDownloadSize := Ini.ReadFloat('Session','dwnb', 0);
    FSecondLoadTNE:=Ini.ReadBool('Session', 'SecondLoadTNE', false);
    if LastSuccessful then begin
      FLastProcessedPoint.X:=Ini.ReadInteger('Session','LastSuccessfulStartX',-1);
      FLastProcessedPoint.Y:=Ini.ReadInteger('Session','LastSuccessfulStartY',-1);
    end else begin
      FLastProcessedPoint.X:=Ini.ReadInteger('Session','StartX',-1);
      FLastProcessedPoint.Y:=Ini.ReadInteger('Session','StartY',-1);
    end;
    i:=1;
    while Ini.ReadInteger('Session','PointX_'+inttostr(i),2147483647)<>2147483647 do begin
      setlength(FRegionPoly, i);
      FRegionPoly[i-1].x := Ini.ReadInteger('Session','PointX_'+inttostr(i),2147483647);
      FRegionPoly[i-1].y := Ini.ReadInteger('Session','PointY_'+inttostr(i),2147483647);
      inc(i);
    end;
    FElapsedTime := Ini.ReadFloat('Session', 'ElapsedTime', 0);
  finally
    ini.Free;
  end;
  For i:=0 to length(MapType)-1 do begin
    if MapType[i].guids=Guids then begin
      FTypeMap := MapType[i];
    end;
  end;
  if FTypeMap = nil then Terminate;
  if length(FRegionPoly) = 0 then Terminate;
  if Terminated then begin
    FFinished := true;
  end else begin
    FTotalInRegion := GetDwnlNum(FRegionRect.TopLeft, FRegionRect.BottomRight, FRegionPoly, true);
    randomize;
  end;
end;

destructor ThreadAllLoadMap.Destroy;
begin
  FRegionPoly := nil;
  FLog := nil;
  inherited;
end;

procedure ThreadAllLoadMap.SaveToFile(AFileName: string);
var
  Ini: Tinifile;
  i:integer;
  VElapsedTime: TDateTime;
begin
  Ini:=TiniFile.Create(AFileName);
  try
    Ini.WriteString('Session', 'MapGUID', FTypeMap.guids);
    Ini.WriteInteger('Session', 'zoom', Fzoom);
    Ini.WriteBool('Session', 'zamena', FReplaceExistTiles);
    Ini.WriteBool('Session', 'raz', FCheckExistTileSize);
    Ini.WriteBool('Session', 'zdate', FCheckExistTileDate);
    Ini.WriteDate('Session', 'FDate', FCheckTileDate);
    Ini.WriteBool('Session', 'SecondLoadTNE', FSecondLoadTNE);
    Ini.WriteInteger('Session', 'scachano', FDownloaded);
    Ini.WriteInteger('Session', 'obrab', FProcessed);
    Ini.WriteFloat('Session', 'dwnb', FDownloadSize);
    Ini.WriteInteger('Session', 'StartX', FLastProcessedPoint.X);
    Ini.WriteInteger('Session', 'StartY', FLastProcessedPoint.Y);
    Ini.WriteInteger('Session', 'LastSuccessfulStartX', FLastSuccessfulPoint.X);
    Ini.WriteInteger('Session', 'LastSuccessfulStartY', FLastSuccessfulPoint.Y);
    for i := 1 to length(FRegionPoly) do begin
      Ini.WriteInteger('Session', 'PointX_'+inttostr(i), FRegionPoly[i-1].x);
      Ini.WriteInteger('Session', 'PointY_'+inttostr(i), FRegionPoly[i-1].y);
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

procedure ThreadAllLoadMap.Execute;
var
  p_x,p_y: integer;
  ty: string;
  VTileExists: boolean;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
  razlen: integer;
  VGotoNextTile: Boolean;
begin
  FStartTime := Now;
  FLastSuccessfulPoint := Point(-1,-1);
  if FRegionRect.Left < FLastProcessedPoint.x then begin
    p_x := FLastProcessedPoint.x;
  end else begin
    p_x := FRegionRect.Left;
  end;
  if FRegionRect.Top < FLastProcessedPoint.y then begin
    p_y := FLastProcessedPoint.Y
  end else begin
    p_y := FRegionRect.Top;
  end;
  while p_x < FRegionRect.Right do begin
    while p_y < FRegionRect.Bottom do begin
      sleep(1);
      if (FDownloadPause) then begin
        FElapsedTime := FElapsedTime + (Now - FStartTime);
        FLog.WriteText(SAS_STR_UserStop, 10);
        While (FDownloadPause)and (not Terminated) do sleep(100);
        FStartTime := now;
      end;
      if Terminated then exit;
      if RgnAndRgn(FRegionPoly, p_x, p_y, false) then begin
        FLoadXY.X := p_x;
        FLoadXY.Y := p_y;
        FLog.WriteText(SAS_STR_ProcessedFile + ': ' + FTypeMap.GetTileShowName(FLoadXY.X, FLoadXY.y, Fzoom) + '...', 0);
        VTileExists := FTypeMap.TileExists(FLoadXY.x, FLoadXY.y, Fzoom);
        if (FReplaceExistTiles) or not(VTileExists) then begin
          if VTileExists then begin
            FLog.WriteText(SAS_STR_LoadProcessRepl+' ...', 0);
          end else begin
            FLog.WriteText(SAS_STR_LoadProcess+'...', 0);
          end;
          if (FCheckExistTileDate)
            and (VTileExists)
            and (FTypeMap.TileLoadDate(FLoadXY.x, FLoadXY.y, Fzoom) >= FCheckTileDate) then
          begin
            FLog.WriteText(SAS_MSG_FileBeCreateTime, 0);
            VGotoNextTile := True;
          end else begin
            razlen := FTypeMap.TileSize(FLoadXY.x, FLoadXY.y, Fzoom);

            FileBuf:=TMemoryStream.Create;
            try
              if (not(FSecondLoadTNE))and(FTypeMap.TileNotExistsOnServer(FLoadXY.x, FLoadXY.y, Fzoom)) then begin
                res := dtrTileNotExists;
              end else begin
                sleep(FTypeMap.Sleep);
                res:=FTypeMap.DownloadTile(FLoadXY, FZoom, FCheckExistTileSize,  razlen, FLoadUrl, ty, fileBuf);
                case res of
                  dtrOK,
                  dtrSameTileSize,
                  dtrErrorMIMEType,
                  dtrTileNotExists,
                  dtrBanError: begin
                    if FTypeMap.IncDownloadedAndCheckAntiBan then begin
                      Synchronize(FTypeMap.addDwnforban);
                    end;
                  end;
                end;
              end;

              case res of
                dtrSameTileSize: begin
                  GState.IncrementDownloaded(razlen/1024, 1);
                  FDownloadSize := FDownloadSize + (razlen / 1024);
                  inc(FDownloaded);
                  FLastSuccessfulPoint := FLoadXY;
                  FLog.WriteText(SAS_MSG_FileBeCreateLen, 0);
                  VGotoNextTile := True;
                end;
                dtrProxyAuthError: begin
                  FLog.WriteText(SAS_ERR_Authorization+#13#13+SAS_STR_Wite+'5'+SAS_UNITS_Secund+'...', 10);
                  sleep(5000);
                  VGotoNextTile := false;
                end;
                dtrBanError: begin
                  Synchronize(Ban);
                  FLog.WriteText(SAS_ERR_Ban+#13#13+SAS_STR_Wite+' 10 '+SAS_UNITS_Secund+'...', 10);
                  sleep(10000);
                  VGotoNextTile := false;
                end;
                dtrTileNotExists: begin
                  FLog.WriteText(SAS_ERR_TileNotExists, 1);
                  if (GState.SaveTileNotExists) then begin
                    FTypeMap.SaveTileNotExists(FLoadXY.X, FLoadXY.Y, FZoom);
                  end;
                  VGotoNextTile := True;
                end;
                dtrDownloadError: begin
                  FLog.WriteText(SAS_ERR_Noconnectionstointernet, 10);
                  if GState.GoNextTileIfDownloadError then begin
                    VGotoNextTile := True;
                  end else begin
                    FLog.WriteText(SAS_STR_Wite+' 5 '+SAS_UNITS_Secund+'...', 10);
                    sleep(5000);
                    VGotoNextTile := false;
                  end;
                  continue;
                end;
                dtrOK : begin
                  FTypeMap.SaveTileDownload(FLoadXY.x, FLoadXY.y, FZoom, fileBuf, ty);
                  FLastSuccessfulPoint := FLoadXY;
                  GState.IncrementDownloaded(fileBuf.Size/1024, 1);
                  FDownloadSize := FDownloadSize + (fileBuf.Size / 1024);
                  inc(FDownloaded);
                  FLog.WriteText('(Ok!)', 0);
                  VGotoNextTile := True;
                end;
                else begin
                  FLog.WriteText(GetErrStr(res), 10);
                  FLog.WriteText(SAS_STR_Wite+' 5 '+SAS_UNITS_Secund+'...', 10);
                  sleep(5000);
                  VGotoNextTile := false;
                end;
              end;
            finally
              FileBuf.Free;
            end;
          end;
        end else begin
          FLog.WriteText(SAS_ERR_FileExistsShort+';', 0);
          VGotoNextTile := True;
        end;
        if VGotoNextTile then begin
          inc(FProcessed);
        end;
      end else begin
        VGotoNextTile := True;
      end;
      if VGotoNextTile then begin
        inc(p_y,256);
      end else begin
        FLog.WriteText(SAS_ERR_RepeatProcess, 0);
      end;
      FLastProcessedPoint.Y := p_y;
    end;
    p_y := FRegionRect.Top;
    inc(p_x,256);
    FLastProcessedPoint.X := p_x;
  end;
  if not Terminated then begin
    FLog.WriteText(SAS_MSG_ProcessFilesComplete, 0);
    FFinished := true;
  end;
end;

procedure ThreadAllLoadMap.DownloadPause;
begin
  FDownloadPause := True;
end;

procedure ThreadAllLoadMap.DownloadResume;
begin
  FDownloadPause := False;
end;

end.
