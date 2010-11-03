unit u_ThreadDownloadTiles;
interface

uses
  Windows,
  Classes,
  i_ILogSimple,
  t_GeoTypes,
  UMapType,
  u_TileDownloaderThreadBase;

type
  TThreadDownloadTiles = class(TTileDownloaderThreadBase)
  private
    FPolygLL: TDoublePointArray;
    FSecondLoadTNE:boolean;
    FReplaceExistTiles: boolean;
    FCheckExistTileSize: boolean;
    FCheckExistTileDate: boolean;
    FCheckTileDate: TDateTime;
    FLastProcessedPoint: TPoint;
    FLastSuccessfulPoint: TPoint;

    FTotalInRegion: Int64;
    FDownloaded: Int64;
    FDownloadSize: Double;
    FProcessed: Int64;

    FElapsedTime: TDateTime;
    FStartTime: TDateTime;

    FLog: ILogSimple;
    FDownloadPause: Boolean;
    FFinished: Boolean;
    function GetElapsedTime: TDateTime;

  protected
    procedure Execute; override;
  public
    constructor Create(
      ALog: ILogSimple;
      APolygon: TDoublePointArray;
      Azamena: boolean;
      ACheckExistTileSize: boolean;
      Azdate: boolean;
      ASecondLoadTNE: boolean;
      AZoom: byte;
      Atypemap: TMapType;
      AReplaceOlderDate: TDateTime
    );overload;
    constructor Create(ALog: ILogSimple; FileName:string;LastSuccessful:boolean); overload;
    destructor Destroy; override;

    procedure SaveToFile(AFileName: string);
    procedure DownloadPause;
    procedure DownloadResume;

    property TotalInRegion: Int64 read FTotalInRegion;
    property Downloaded: Int64 read FDownloaded;
    property Processed: Int64 read FProcessed;
    property DownloadSize: Double read FDownloadSize;
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
  i_ITileDownlodSession,
  u_GlobalState,
  u_TileIteratorAbstract,
  u_TileIteratorStuped,
  UResStrings;

constructor TThreadDownloadTiles.Create(
  ALog: ILogSimple;
  APolygon: TDoublePointArray;
  Azamena, ACheckExistTileSize, Azdate, ASecondLoadTNE: boolean;
  AZoom: byte;
  Atypemap: TMapType;
  AReplaceOlderDate: TDateTime
);
begin
  inherited Create(false);
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
  FDownloaded := 0;
  FProcessed := 0;
  FDownloadSize := 0;
  FElapsedTime := 0;
  FLastProcessedPoint := Point(-1,-1);
  randomize;
end;

constructor TThreadDownloadTiles.Create(ALog: ILogSimple; FileName:string;LastSuccessful:boolean);
var
  Ini: Tinifile;
  i: integer;
  VGuids: string;
  VGuid: TGUID;
begin
  inherited Create(false);
  FLog := ALog;
  Priority := tpLower;
  Ini:=TiniFile.Create(FileName);
  try
    VGuids:=Ini.ReadString('Session','MapGUID','');
    VGuid := StringToGUID(VGuids);
    Fzoom := Ini.ReadInteger('Session', 'zoom', GState.ViewState.GetCurrentZoom) - 1;
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
    while Ini.ReadFloat('Session','LLPointX_'+inttostr(i),-10000)>-10000 do begin
      setlength(FPolygLL, i);
      FPolygLL[i-1].x := Ini.ReadInteger('Session','LLPointX_'+inttostr(i),-10000);
      FPolygLL[i-1].y := Ini.ReadInteger('Session','LLPointY_'+inttostr(i),-10000);
      inc(i);
    end;
    FElapsedTime := Ini.ReadFloat('Session', 'ElapsedTime', 0);
  finally
    ini.Free;
  end;
  FMapType := GState.GetMapFromID(VGuid);
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
    Ini.WriteString('Session', 'MapGUID', FMapType.GUIDString);
    Ini.WriteInteger('Session', 'zoom', Fzoom + 1);
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
  ty: string;
  VTileExists: boolean;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
  razlen: integer;
  VGotoNextTile: Boolean;
  VTile: TPoint;
  VTileIterator: TTileIteratorAbstract;
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
            FLog.WriteText(SAS_STR_UserStop, 10);
            While (FDownloadPause)and (not Terminated) do sleep(100);
            FStartTime := now;
          end;
          FLoadXY := VTile;

          FLog.WriteText(SAS_STR_ProcessedFile + ': ' + FMapType.GetTileShowName(VTile, Fzoom) + '...', 0);
          VTileExists := FMapType.TileExists(VTile, Fzoom);
          if (FReplaceExistTiles) or not(VTileExists) then begin
            if VTileExists then begin
              FLog.WriteText(SAS_STR_LoadProcessRepl+' ...', 0);
            end else begin
              FLog.WriteText(SAS_STR_LoadProcess+'...', 0);
            end;
            if (FCheckExistTileDate)
              and (VTileExists)
              and (FMapType.TileLoadDate(VTile, Fzoom) >= FCheckTileDate) then
            begin
              FLog.WriteText(SAS_MSG_FileBeCreateTime, 0);
              VGotoNextTile := True;
            end else begin
              razlen := FMapType.TileSize(VTile, Fzoom);

              FileBuf:=TMemoryStream.Create;
              try
                try
                  if (not(FSecondLoadTNE))and(FMapType.TileNotExistsOnServer(VTile, Fzoom))and(GState.SaveTileNotExists) then begin
                    res := dtrTileNotExists;
                  end else begin
                    res:=FMapType.DownloadTile(Self, VTile, FZoom, FCheckExistTileSize,  razlen, FLoadUrl, ty, fileBuf);
                  end;

                  case res of
                    dtrOK : begin
                      FLastSuccessfulPoint := FLoadXY;
                      GState.IncrementDownloaded(fileBuf.Size/1024, 1);
                      FDownloadSize := FDownloadSize + (fileBuf.Size / 1024);
                      inc(FDownloaded);
                      FLog.WriteText('(Ok!)', 0);
                      VGotoNextTile := True;
                    end;
                    dtrSameTileSize: begin
                      FLastSuccessfulPoint := FLoadXY;
                      GState.IncrementDownloaded(razlen/1024, 1);
                      FDownloadSize := FDownloadSize + (razlen / 1024);
                      inc(FDownloaded);
                      FLog.WriteText(SAS_MSG_FileBeCreateLen, 0);
                      VGotoNextTile := True;
                    end;
                    dtrProxyAuthError: begin
                      FLog.WriteText(SAS_ERR_Authorization+#13#13+SAS_STR_Wite+'5'+SAS_UNITS_Secund+'...', 10);
                      sleep(5000);
                      VGotoNextTile := false;
                    end;
                    dtrBanError: begin
                      FLog.WriteText(SAS_ERR_Ban+#13#13+SAS_STR_Wite+' 10 '+SAS_UNITS_Secund+'...', 10);
                      sleep(10000);
                      VGotoNextTile := false;
                    end;
                    dtrErrorMIMEType: begin
                      FLog.WriteText(Format(SAS_ERR_BadMIME, [ty]), 1);
                      VGotoNextTile := True;
                    end;
                    dtrTileNotExists: begin
                      FLog.WriteText(SAS_ERR_TileNotExists, 1);
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
                    else begin
                      FLog.WriteText(GetErrStr(res), 10);
                      FLog.WriteText(SAS_STR_Wite+' 5 '+SAS_UNITS_Secund+'...', 10);
                      sleep(5000);
                      VGotoNextTile := false;
                    end;
                  end;
                except
                  on E: Exception do begin
                    FLog.WriteText(E.Message, 0);
                    VGotoNextTile := True;
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
        end;
        if Terminated then begin
          Break;
        end;
      end;
    end;
  finally
    FreeAndNil(VTileIterator);
  end;
  if not Terminated then begin
    FLog.WriteText(SAS_MSG_ProcessFilesComplete, 0);
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

function TThreadDownloadTiles.GetElapsedTime: TDateTime;
begin
  if FFinished or FDownloadPause then begin
    Result := FElapsedTime;
  end else begin
    Result := FElapsedTime + (Now - FStartTime);
  end;
end;

end.

