unit UTrAllLoadMap;
interface

uses
  Windows,
  Forms,
  Classes,
  IniFiles,
  Wininet,
  Dialogs,
  Uprogress,
  t_GeoTypes,
  t_LoadEvent,
  UMapType,
  u_TileDownloaderBase,
  u_TileDownloaderThreadBase,
  UResStrings;

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
    _FProgress:TFProgress;
    AddToMemo:string;
    procedure UpdateMemoProgressForm;
    procedure UpdateMemoAddProgressForm;
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure CloseProgressForm;
    function GetTimeEnd(loadAll,load:integer):String;
    function GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
  protected
    procedure Execute; override;
  public
    constructor Create(APolygon_:TPointArray; Azamena,ACheckExistTileSize,Azdate,ASecondLoadTNE:boolean;AZoom:byte;Atypemap:TMapType;AFDate:TDateTime);overload;
    constructor Create(FileName:string;LastSuccessful:boolean); overload;
    destructor Destroy; override;

    procedure ButtonSaveClick(Sender: TObject);
    procedure SaveSessionToFile;

    property TotalInRegion: Cardinal read FTotalInRegion;
    property Downloaded: Cardinal read FDownloaded;
    property Processed: Cardinal read FProcessed;
    property DownloadSize: Double read FDownloadSize;
    property ElapsedTime: TDateTime read FElapsedTime;
    property StartTime: TDateTime read FStartTime;
  end;

implementation
uses
  SysUtils,
  Types,
  DateUtils,
  StrUtils,
  Math,
  u_GlobalState,
  u_GeoToStr,
  Unit1,
  UImgfun,
  UGeoFun,
  Usaveas;

constructor ThreadAllLoadMap.Create(APolygon_:TPointArray;Azamena,ACheckExistTileSize,Azdate,ASecondLoadTNE:boolean;AZoom:byte;Atypemap:TMapType;AFDate:TDateTime);
var
  i: integer;
begin
  inherited Create(false);
  OnTerminate:=Fmain.ThreadDone;
  Priority := tpLower;
  FreeOnTerminate:=true;
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
  Application.CreateForm(TFProgress, _FProgress);
  _FProgress.ButtonSave.OnClick:=ButtonSaveClick;
  SetProgressForm;
  _FProgress.Visible:=true;

  randomize;
end;

constructor ThreadAllLoadMap.Create(FileName:string;LastSuccessful:boolean);
var
  Ini: Tinifile;
  i: integer;
  Guids: string;
begin
  inherited Create(false);
  OnTerminate:=Fmain.ThreadDone;
  Priority := tpLower;
  FreeOnTerminate:=true;

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
  FTotalInRegion := GetDwnlNum(FRegionRect.TopLeft, FRegionRect.BottomRight, FRegionPoly, true);

  Application.CreateForm(TFProgress, _FProgress);
  _FProgress.ButtonSave.OnClick:=ButtonSaveClick;
  SetProgressForm;
  _FProgress.Visible:=true;

  randomize;
end;

destructor ThreadAllLoadMap.Destroy;
begin
  inherited;
end;

procedure ThreadAllLoadMap.SaveSessionToFile;
var
  Ini: Tinifile;
  i:integer;
  VElapsedTime: TDateTime;
begin
  if (_FProgress.SaveSessionDialog.Execute)and(_FProgress.SaveSessionDialog.FileName<>'') then begin
    Ini:=TiniFile.Create(_FProgress.SaveSessionDialog.FileName);
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
      if (_FProgress.stop) then begin
        VElapsedTime := FElapsedTime;
      end else begin
        VElapsedTime := FElapsedTime + (Now - FStartTime);
      end;
      Ini.WriteFloat('Session', 'ElapsedTime', VElapsedTime);
    finally
      ini.Free;
    end;
  end;
end;

procedure ThreadAllLoadMap.ButtonSaveClick(Sender: TObject);
begin
 Synchronize(SaveSessionToFile);
end;

procedure ThreadAllLoadMap.SetProgressForm;
begin
 _FProgress.RProgr.Max := FTotalInRegion;
 _FProgress.RProgr.Progress1 := FDownloaded;
 _FProgress.RProgr.Progress2 := FProcessed;
 _FProgress.LabelName0.Caption := SAS_STR_ProcessedNoMore+':';
 _FProgress.LabelValue0.Caption := inttostr(FTotalInRegion)+' '+SAS_STR_files+' (х'+inttostr(Fzoom)+')';
 _FProgress.LabelName1.Caption := SAS_STR_AllProcessed;
 _FProgress.LabelName2.Caption := SAS_STR_AllLoad;
 _FProgress.LabelName3.Caption := SAS_STR_TimeRemained;
 _FProgress.LabelName4.Caption := SAS_STR_LoadRemained;
end;

procedure ThreadAllLoadMap.UpdateProgressForm;
var
  path: string;
begin
 if (_FProgress.stop) then
   begin
    _FProgress.Memo1.Lines.Add(SAS_STR_UserStop);
    _FProgress.Caption:=SAS_STR_Stop1+'... ('+inttostr(round(FProcessed/FTotalInRegion*100))+'%)';
    exit;
   end;
 if _FProgress.Memo1.Lines.Count>5000 then _FProgress.Memo1.Lines.Clear;
 _FProgress.Caption:=SAS_STR_LoadProcess+'... ('+inttostr(round(FProcessed/FTotalInRegion*100))+'%)';
 Application.ProcessMessages;
 _FProgress.LabelValue1.Caption:=inttostr(FProcessed)+' '+SAS_STR_files;
 _FProgress.LabelValue2.Caption:=inttostr(FDownloaded)+' ('+kb2KbMbGb(FDownloadSize)+') '+SAS_STR_Files;
 _FProgress.LabelValue3.Caption:=GetTimeEnd(FTotalInRegion,FProcessed);
 _FProgress.LabelValue4.Caption:=GetLenEnd(FTotalInRegion, FProcessed, FDownloaded, FDownloadSize);
 //Имя файла для вывода в сообщении. Заменить на обобобщенное имя тайла
  path := FTypeMap.GetTileFileName(FLoadXY.X, FLoadXY.y, Fzoom);
 _FProgress.Memo1.Lines.Add(SAS_STR_ProcessedFile+': '+path+'...');
 Application.ProcessMessages;
 if (FProcessed mod 10 = 0)or(FTotalInRegion<100) then
  begin
   _FProgress.RProgr.Progress1 := FProcessed;
   _FProgress.RProgr.Progress2 := FDownloaded;
  end;
end;

procedure ThreadAllLoadMap.UpdateMemoProgressForm;
begin
 _FProgress.Memo1.Lines.Add(AddToMemo);
 Fmain.toSh;
end;

procedure ThreadAllLoadMap.UpdateMemoAddProgressForm;
begin
 _FProgress.Memo1.Lines.strings[_FProgress.Memo1.Lines.Count-1]:=
   _FProgress.Memo1.Lines.strings[_FProgress.Memo1.Lines.Count-1]+AddToMemo;
end;

procedure ThreadAllLoadMap.CloseProgressForm;
begin
 _FProgress.Memo1.Lines.Add(SAS_MSG_ProcessFilesComplete);
 _FProgress.Caption := SAS_MSG_LoadComplete+' ('+inttostr(round(FProcessed/FTotalInRegion*100))+'%)';
 _FProgress.LabelValue1.Caption := inttostr(FProcessed)+' '+SAS_STR_files;
 _FProgress.LabelValue2.Caption := inttostr(FDownloaded)+' ('+kb2KbMbGb(FDownloadSize)+') '+SAS_STR_Files;
 _FProgress.LabelValue3.Caption := GetTimeEnd(FTotalInRegion, FProcessed);
 _FProgress.LabelValue4.Caption := GetLenEnd(FTotalInRegion, FProcessed, FDownloaded, FDownloadSize);
 _FProgress.RProgr.Progress1 := FProcessed;
 _FProgress.RProgr.Progress2 := FDownloaded;
 _FProgress.Repaint;
end;

function ThreadAllLoadMap.GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
begin
  if loaded=0 then begin
    result:='~ Кб';
    exit;
  end;
  Result:=kb2KbMbGb((len/loaded)*(loadAll-obrab));
end;

function ThreadAllLoadMap.GetTimeEnd(loadAll,load:integer):String;
var dd:integer;
    VElapsedTime: TDateTime;
begin
  if load=0 then begin
    result:='~';
    exit;
  end;
  VElapsedTime := FElapsedTime + (Now - FStartTime);;
  dd:=DaysBetween(VElapsedTime,(VElapsedTime*(loadAll/load)));
  Result:='';
  if dd>0 then Result:=inttostr(dd)+' дней, ';
  Result:=Result+TimeToStr((VElapsedTime*(loadAll / load))-VElapsedTime);
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
      if (_FProgress.stop) then begin
        FElapsedTime := FElapsedTime + (Now - FStartTime);
        Synchronize(UpdateProgressForm);
        While (_FProgress.stop)and(_FProgress.Visible) do sleep(100);
        FStartTime := now;
      end;
      if not(_FProgress.Visible) then exit;
      if RgnAndRgn(FRegionPoly, p_x, p_y, false) then begin
        FLoadXY.X := p_x;
        FLoadXY.Y := p_y;

        Synchronize(UpdateProgressForm);
        VTileExists := FTypeMap.TileExists(FLoadXY.x, FLoadXY.y, Fzoom);
        if (FReplaceExistTiles) or not(VTileExists) then begin
          if VTileExists then begin
            AddToMemo:=SAS_STR_LoadProcessRepl+' ...';
          end else begin
            AddToMemo:=SAS_STR_LoadProcess+'...';
          end;
          Synchronize(UpdateMemoProgressForm);
          if (FCheckExistTileDate)
            and (VTileExists)
            and (FTypeMap.TileLoadDate(FLoadXY.x, FLoadXY.y, Fzoom) >= FCheckTileDate) then
          begin
            AddToMemo:=AddToMemo+#13#10+SAS_MSG_FileBeCreateTime;
            Synchronize(UpdateMemoProgressForm);
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
                  AddToMemo:=SAS_MSG_FileBeCreateLen;
                  Synchronize(UpdateMemoProgressForm);
                  VGotoNextTile := True;
                end;
                dtrProxyAuthError: begin
                  AddToMemo:=SAS_ERR_Authorization+#13#13+SAS_STR_Wite+'5'+SAS_UNITS_Secund+'...';
                  sleep(5000);
                  AddToMemo:=AddToMemo+#13#10+SAS_ERR_RepeatProcess;
                  Synchronize(UpdateMemoProgressForm);
                  VGotoNextTile := false;
                end;
                dtrBanError: begin
                  Synchronize(Ban);
                  AddToMemo:=SAS_ERR_Ban+#13#13+SAS_STR_Wite+' 10 '+SAS_UNITS_Secund+'...';
                  sleep(10000);
                  AddToMemo:=AddToMemo+#13#10+SAS_ERR_RepeatProcess;
                  Synchronize(UpdateMemoProgressForm);
                  VGotoNextTile := false;
                end;
                dtrTileNotExists,
                dtrErrorMIMEType: begin
                  AddToMemo:=SAS_ERR_TileNotExists;
                  Synchronize(UpdateMemoProgressForm);
                  if (GState.SaveTileNotExists) then begin
                    FTypeMap.SaveTileNotExists(FLoadXY.X, FLoadXY.Y, FZoom);
                  end;
                  VGotoNextTile := True;
                end;
                dtrDownloadError: begin
                  AddToMemo:=SAS_ERR_Noconnectionstointernet;
                  if GState.GoNextTileIfDownloadError then begin
                    VGotoNextTile := True;
                  end else begin
                    AddToMemo:=AddToMemo+#13#10+SAS_STR_Wite+' 5 '+SAS_UNITS_Secund+'...';
                    sleep(5000);
                    AddToMemo:=AddToMemo+#13#10+SAS_ERR_RepeatProcess;
                    VGotoNextTile := false;
                  end;
                  Synchronize(UpdateMemoProgressForm);
                  continue;
                end;
                dtrOK : begin
                  FTypeMap.SaveTileDownload(FLoadXY.x, FLoadXY.y, FZoom, fileBuf, ty);
                  FLastSuccessfulPoint := FLoadXY;
                  GState.IncrementDownloaded(fileBuf.Size/1024, 1);
                  FDownloadSize := FDownloadSize + (fileBuf.Size / 1024);
                  inc(FDownloaded);
                  AddToMemo:='(Ok!)';
                  Synchronize(UpdateMemoAddProgressForm);
                  VGotoNextTile := True;
                end;
                else begin
                  AddToMemo:=GetErrStr(res);
                  AddToMemo:=AddToMemo+#13#10+SAS_STR_Wite+' 5 '+SAS_UNITS_Secund+'...';
                  Synchronize(UpdateMemoProgressForm);
                  sleep(5000);
                  VGotoNextTile := false;
                end;
              end;
            finally
              FileBuf.Free;
            end;
          end;
        end else begin
          AddToMemo:=SAS_ERR_FileExistsShort+';';
          Synchronize(UpdateMemoProgressForm);
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
      end;
      FLastProcessedPoint.Y := p_y;
    end;
    p_y := FRegionRect.Top;
    inc(p_x,256);
    FLastProcessedPoint.X := p_x;
  end;
  Synchronize(CloseProgressForm);
end;

end.
