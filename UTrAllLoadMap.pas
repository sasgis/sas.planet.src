unit UTrAllLoadMap;
interface

uses
  Windows,
  Forms,
  Classes,
  IniFiles,
  Wininet,
  Dialogs,
  Jpeg,
  GR32,
  GR32_Resamplers,
  Uprogress,
  t_GeoTypes,
  UMapType,
  u_TileDownloaderBase,
  UResStrings;

type
  TlastLoad_ = record
    x,y:longint;
    z:byte;
    mt:TMapType;
    use:boolean;
  end;
  ThreadAllLoadMap = class(TThread)
  private
    Poly:TPointArray;
    Zoom:byte;
    typemap:TMapType;
    zamena:boolean;
    StartPoint:TPoint;
    LastSuccessfulPoint:TPoint;
    LastLoadTileSize: integer;
    CheckExistTileSize: boolean;
    Zdate:boolean;
    mapsload:boolean;
    SecondLoadTNE:boolean;
    url_ifban,ErrorString:string;
    LoadXY: TPoint;

    FDate:TDateTime;
    OperBegin:TDateTime;
    UPos:TPoint;
    FDownloader: TTileDownloaderBase;
    _FProgress:TFProgress;
    lastLoad:TlastLoad_;
    max,min:TPoint;
    scachano,num_dwn,obrab,vsego:integer;
    dwnb:real;
    AddToMemo,TimeEnd,LenEnd:string;
  private
    function GetErrStr(Aerr: TDownloadTileResult): string;
  protected
    procedure UpdateMemoProgressForm;
    procedure UpdateMemoAddProgressForm;
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure CloseProgressForm;
    function DownloadTile(AXY: TPoint; AZoom: byte;MT:TMapType; AOldTileSize: Integer; out ty: string; fileBuf:TMemoryStream): TDownloadTileResult;
    procedure Execute; override;
    procedure dwnReg;
    procedure dwnOne;
    procedure addDwnforban;
    procedure AfterWriteToFile;
    procedure addDwnTiles;
    procedure ban;
    function GetTimeEnd(loadAll,load:integer):String;
    function GetLenEnd(loadAll,obrab,loaded:integer;len:real):string;
    procedure GetCurrentMapAndPos;
    procedure DwnInFon;
  public
    typeRect:1..4;
    procedure ButtonSaveClick(Sender: TObject);
    procedure SaveSessionToFile;
    constructor Create(CrSusp:Boolean;APolygon_:TPointArray; Atyperect:byte;Azamena,ACheckExistTileSize,Azdate,ASecondLoadTNE:boolean;AZoom:byte;Atypemap:TMapType;AFDate:TDateTime);overload;
    constructor Create(CrSusp:Boolean;FileName:string;LastSuccessful:boolean); overload;
    destructor Destroy; override;
  end;

implementation
uses
  SysUtils,
  Graphics,
  DateUtils,
  StrUtils,
  Math,
  u_GlobalState,
  u_GeoToStr,
  Unit1,
  UImgfun,
  UWikilayer,
  UGeoFun,
  Usaveas;

procedure ThreadAllLoadMap.ButtonSaveClick(Sender: TObject);
begin
 Synchronize(SaveSessionToFile);
end;

constructor ThreadAllLoadMap.Create(CrSusp:Boolean;FileName:string;LastSuccessful:boolean);
var Ini: Tinifile;
    i:integer;
    Guids:string;
begin
 Application.CreateForm(TFProgress, _FProgress);
 _FProgress.ButtonSave.OnClick:=ButtonSaveClick;
 typeRect:=3;
  begin
   Ini:=TiniFile.Create(FileName);
   Guids:=Ini.ReadString('Session','MapGUID','');
   For i:=0 to length(MapType)-1 do
    if MapType[i].guids=Guids then
     begin
      typemap:=MapType[i];
     end;
   if typemap=nil then Terminate;
   zoom:=Ini.ReadInteger('Session','zoom',GState.zoom_size);
   zamena:=Ini.ReadBool('Session','zamena',false);
   CheckExistTileSize := Ini.ReadBool('Session','raz',false);
   zdate:=Ini.ReadBool('Session','zdate',false);
   Fdate:=Ini.ReadDate('Session','FDate',now);
   scachano:=Ini.ReadInteger('Session','scachano',0);
   obrab:=Ini.ReadInteger('Session','obrab',0);
   dwnb:=Ini.ReadFloat('Session','dwnb',0);
   SecondLoadTNE:=Ini.ReadBool('Session','SecondLoadTNE',false);
   mapsload:=false;
   if LastSuccessful then
         begin
          StartPoint.X:=Ini.ReadInteger('Session','LastSuccessfulStartX',-1);
          StartPoint.Y:=Ini.ReadInteger('Session','LastSuccessfulStartY',-1);
         end
    else begin
          StartPoint.X:=Ini.ReadInteger('Session','StartX',-1);
          StartPoint.Y:=Ini.ReadInteger('Session','StartY',-1);
         end;
   i:=1;
   while Ini.ReadInteger('Session','PointX_'+inttostr(i),2147483647)<>2147483647 do
    begin
     setlength(poly,i);
     poly[i-1].x:=Ini.ReadInteger('Session','PointX_'+inttostr(i),2147483647);
     poly[i-1].y:=Ini.ReadInteger('Session','PointY_'+inttostr(i),2147483647);
     inc(i);
    end;
   if length(poly)=0 then Terminate;
   ini.Free;
  end;
 num_dwn:=GetDwnlNum(min,max,poly,true);
 vsego:=num_dwn;
 Synchronize(SetProgressForm);
 _FProgress.Visible:=true;
 Synchronize(addDwnforban);
 randomize;
 inherited Create(CrSusp);
end;

procedure ThreadAllLoadMap.SaveSessionToFile;
var Ini: Tinifile;
    i:integer;
begin
 if (_FProgress.SaveSessionDialog.Execute)and(_FProgress.SaveSessionDialog.FileName<>'') then
  begin
   Ini:=TiniFile.Create(_FProgress.SaveSessionDialog.FileName);
   Ini.WriteString('Session','MapGUID',typemap.guids);
   Ini.WriteInteger('Session','zoom',zoom);
   Ini.WriteBool('Session','zamena',zamena);
   Ini.WriteBool('Session','raz', CheckExistTileSize);
   Ini.WriteBool('Session','zdate',zdate);
   Ini.WriteDate('Session','FDate',FDate);
   Ini.WriteBool('Session','SecondLoadTNE',SecondLoadTNE);
   Ini.WriteInteger('Session','scachano',scachano);
   Ini.WriteInteger('Session','obrab',obrab);
   Ini.WriteFloat('Session','dwnb',dwnb);
   Ini.WriteInteger('Session','StartX',StartPoint.X);
   Ini.WriteInteger('Session','StartY',StartPoint.Y);
   Ini.WriteInteger('Session','LastSuccessfulStartX',LastSuccessfulPoint.X);
   Ini.WriteInteger('Session','LastSuccessfulStartY',LastSuccessfulPoint.Y);
   for i:=1 to length(Poly) do
    begin
     Ini.WriteInteger('Session','PointX_'+inttostr(i),Poly[i-1].x);
     Ini.WriteInteger('Session','PointY_'+inttostr(i),Poly[i-1].y);
    end;
   ini.Free;
  end;
end;

procedure ThreadAllLoadMap.SetProgressForm;
begin
 _FProgress.RProgr.Max:=vsego;
 _FProgress.RProgr.Progress1:=scachano;
 _FProgress.RProgr.Progress2:=obrab;
 _FProgress.LabelName0.Caption:=SAS_STR_ProcessedNoMore+':';
 _FProgress.LabelValue0.Caption:=inttostr(num_dwn)+' '+SAS_STR_files+' (х'+inttostr(zoom)+')';
 _FProgress.LabelName1.Caption:=SAS_STR_AllProcessed;
 _FProgress.LabelName2.Caption:=SAS_STR_AllLoad;
 _FProgress.LabelName3.Caption:=SAS_STR_TimeRemained;
 _FProgress.LabelName4.Caption:=SAS_STR_LoadRemained;
end;

procedure ThreadAllLoadMap.UpdateProgressForm;
var
  path: string;
begin
 if (_FProgress.stop) then
   begin
    _FProgress.Memo1.Lines.Add(SAS_STR_UserStop);
    _FProgress.Caption:=SAS_STR_Stop1+'... ('+inttostr(round(obrab/vsego*100))+'%)';
    exit;
   end;
 if _FProgress.Memo1.Lines.Count>5000 then _FProgress.Memo1.Lines.Clear;
 _FProgress.Caption:=SAS_STR_LoadProcess+'... ('+inttostr(round(obrab/vsego*100))+'%)';
 Application.ProcessMessages;
 _FProgress.LabelValue1.Caption:=inttostr(obrab)+' '+SAS_STR_files;
 _FProgress.LabelValue2.Caption:=inttostr(scachano)+' ('+kb2KbMbGb(dwnb)+') '+SAS_STR_Files;
 _FProgress.LabelValue3.Caption:=TimeEnd;
 _FProgress.LabelValue4.Caption:=LenEnd;
 //Имя файла для вывода в сообщении. Заменить на обобобщенное имя тайла
  path:=typemap.GetTileFileName(LoadXY.X,LoadXY.y,zoom);
 _FProgress.Memo1.Lines.Add(SAS_STR_ProcessedFile+': '+path+'...');
 Application.ProcessMessages;
 if (obrab mod 10 = 0)or(num_dwn<100) then
  begin
   _FProgress.RProgr.Progress1:=obrab;
   _FProgress.RProgr.Progress2:=scachano;
  end;
end;

procedure ThreadAllLoadMap.UpdateMemoProgressForm;
begin
 _FProgress.Memo1.Lines.Add(AddToMemo);
end;

procedure ThreadAllLoadMap.UpdateMemoAddProgressForm;
begin
 _FProgress.Memo1.Lines.strings[_FProgress.Memo1.Lines.Count-1]:=
   _FProgress.Memo1.Lines.strings[_FProgress.Memo1.Lines.Count-1]+AddToMemo;
end;

procedure ThreadAllLoadMap.CloseProgressForm;
begin
 _FProgress.Memo1.Lines.Add(SAS_MSG_ProcessFilesComplete);
 _FProgress.Caption:=SAS_MSG_LoadComplete+' ('+inttostr(round(obrab/vsego*100))+'%)';
 _FProgress.LabelValue1.Caption:=inttostr(obrab)+' '+SAS_STR_files;
 _FProgress.LabelValue2.Caption:=inttostr(scachano)+' ('+kb2KbMbGb(dwnb)+') '+SAS_STR_Files;
 _FProgress.LabelValue3.Caption:=GetTimeEnd(num_dwn,obrab);
 _FProgress.LabelValue4.Caption:=GetLenEnd(num_dwn,obrab,scachano,dwnb);
 _FProgress.RProgr.Progress1:=obrab;
 _FProgress.RProgr.Progress2:=scachano;
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
    Time1:TDateTime;
begin
  if load=0 then begin
                    result:='~';
                    exit;
                 end;
  Time1:=now-OperBegin;
  dd:=DaysBetween(Time1,(Time1*(loadAll/load)));
  Result:='';
  if dd>0 then Result:=inttostr(dd)+' дней, ';
  Result:=Result+TimeToStr((Time1*(loadAll / load))-Time1);
end;

procedure ThreadAllLoadMap.ban;
begin
 if typemap.ban_pg_ld then
  begin
   Fmain.ShowCaptcha(url_ifban);
   typemap.ban_pg_ld:=false;
  end;
end;

procedure ThreadAllLoadMap.addDwnforban;
var
  VExpectedMIMETypes: string;
  VDownloadTryCount: Integer;
begin
  if FDownloader=nil then begin
    if GState.TwoDownloadAttempt then begin
      VDownloadTryCount := 2;
    end else begin
      VDownloadTryCount := 1;
    end;
    if typemap <> nil then begin
      VExpectedMIMETypes := typemap.CONTENT_TYPE;
    end;
    FDownloader := TTileDownloaderBase.Create(VExpectedMIMETypes, VDownloadTryCount, GState.InetConnect);
  end;
  if (mapsload=false)and(typemap.UseAntiBan>0)and(typeRect<>1) then begin
    Fmain.WebBrowser1.Navigate('http://maps.google.com/?ie=UTF8&ll='+inttostr(random(100)-50)+','+inttostr(random(300)-150)+'&spn=1,1&t=k&z=8');
    mapsload:=true;
  end;
end;

procedure ThreadAllLoadMap.addDwnTiles;
begin
 inc(GState.all_dwn_tiles);
 GState.all_dwn_kb := GState.all_dwn_kb + (LastLoadTileSize/1024);
end;

constructor ThreadAllLoadMap.Create(CrSusp:Boolean;APolygon_:TPointArray;Atyperect:byte;Azamena,ACheckExistTileSize,Azdate,ASecondLoadTNE:boolean;AZoom:byte;Atypemap:TMapType;AFDate:TDateTime);
var i:integer;
begin
  inherited Create(CrSusp);
  mapsload:=false;
  zamena:=Azamena;
  zoom:=AZoom;
  CheckExistTileSize := ACheckExistTileSize;
  typemap:=Atypemap;
  typeRect:=AtypeRect;
  FDate:=AFDate;
  Zdate:=AzDate;
  SecondLoadTNE:=ASecondLoadTNE;
  for i:=1 to length(APolygon_) do
   begin
    setlength(Poly,i);
    poly[i-1]:=Apolygon_[i-1];
   end;
  if AtypeRect in [2,3] then
   begin
    Application.CreateForm(TFProgress, _FProgress);
    _FProgress.ButtonSave.OnClick:=ButtonSaveClick;
    num_dwn:=GetDwnlNum(min,max,poly,true);
    vsego:=num_dwn;
    scachano:=0;
    obrab:=0;
    dwnb:=0;
    Synchronize(SetProgressForm);
    _FProgress.Visible:=true;
   end;
  Synchronize(addDwnforban);
  randomize;
end;
function ThreadAllLoadMap.DownloadTile(AXY: TPoint; AZoom: byte;
  MT: TMapType; AOldTileSize: Integer; out ty: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  StatusCode: Cardinal;
begin
  Result := dtrUnknownError;
  if terminated then exit;
  url_ifban := MT.GetLink(AXY.X, AXY.Y, AZoom);
  FDownloader.ExpectedMIMETypes := MT.CONTENT_TYPE;
  FDownloader.SleepOnResetConnection := MT.Sleep;
  Result := FDownloader.DownloadTile(url_ifban, CheckExistTileSize, AOldTileSize, fileBuf, StatusCode, ty);
  if (ty <> MT.Content_type)
    and(fileBuf.Size <> 0)
    and(MT.BanIfLen <> 0)
    and(fileBuf.Size < (MT.BanIfLen + 50))
    and(fileBuf.Size >(MT.BanIfLen-50)) then
  begin
    result := dtrBanError;
  end;

  if Result = dtrBanError  then begin
    Synchronize(Ban);
  end;
end;

procedure ThreadAllLoadMap.dwnOne;
var i:integer;
  ty: string;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
begin
 for i:=0 to length(poly)-1 do begin
  lastload.X:=poly[i].X-(abs(poly[i].X) mod 256);
  lastload.Y:=poly[i].Y-(abs(poly[i].Y) mod 256);
  lastload.z:=zoom; lastLoad.mt:=typemap; lastLoad.use:=true;
  LoadXY.X := poly[i].X;
  LoadXY.Y := poly[i].Y;
  if typemap.UseDwn then begin
    FileBuf:=TMemoryStream.Create;
    try
      res :=DownloadTile(LoadXY, Zoom, typemap, 0, ty, fileBuf);
      ErrorString:=GetErrStr(res);
      if (res = dtrOK) or (res = dtrSameTileSize) then begin
        LastLoadTileSize := fileBuf.Size;
        Synchronize(addDwnTiles);
      end;
      if (res = dtrTileNotExists) and (GState.SaveTileNotExists) then begin
        typemap.SaveTileNotExists(LoadXY.X, LoadXY.Y, Zoom);
      end;
      if res = dtrOK then begin
        typemap.SaveTileDownload(LoadXY.X, LoadXY.Y, Zoom, fileBuf, ty);
      end;
      Synchronize(AfterWriteToFile);
    finally
      FileBuf.Free;
    end;
  end else begin
    ErrorString:=SAS_ERR_NotLoads;
  end;
 end;
end;

function ThreadAllLoadMap.GetErrStr(Aerr: TDownloadTileResult): string;
begin
 case Aerr of
  dtrProxyAuthError: result:=SAS_ERR_Authorization;
  dtrBanError: result:=SAS_ERR_Ban;
  dtrTileNotExists: result:=SAS_ERR_TileNotExists;
  dtrDownloadError,
  dtrErrorInternetOpen,
  dtrErrorInternetOpenURL: result:=SAS_ERR_Noconnectionstointernet;
  dtrErrorMIMEType: result := 'Ошибочный тип данных'; //TODO: Заменить на ресурсную строку
  dtrUnknownError: Result := 'Неизвестная ошибка при скачивании'
  else result:='';
 end;
end;

procedure ThreadAllLoadMap.DwnInFon;
var i,j,ii,k,r,XX,YY,g,x,y,m1,num_dwn:integer;
    Bpos:TPoint;
    ty: string;
    fileBuf:TMemoryStream;
    VMap: TMapType;
    res: TDownloadTileResult;
begin
  num_dwn:=0;
  repeat
    if(not FMain.change_scene)then begin
      sleep(100);
      continue;
    end;
    FMain.change_scene:=false;
    Synchronize(GetCurrentMapAndPos);
    Synchronize(addDwnforban);
    j:=0;
    i:=-1;
    for r:=1 to (hg_x div 2)+2 do begin
      g:=(r*2-2);
      if r=1 then m1:=0 else m1:=1;
      for k:=0 to g*4-m1 do begin
        if (k=0) then inc(i);
        if (k>0)and(k<g) then inc(j);
        if (k>=g)and(k<g*2) then dec(i);
        if (k>=g*2)and(k<g*3) then dec(j);
        if (k>=g*3) then inc(i);
        if g=0 then i:=0;
        x:=(hg_x div 2)+i;
        y:=(hg_y div 2)+j;
        if(FMain.change_scene) then continue;
        Synchronize(GetCurrentMapAndPos);
        for ii:=0 to length(MapType)-1 do begin
          VMap := MapType[ii];
          if VMap.active then begin
            BPos:=UPos;
            BPos := typemap.GeoConvert.Pos2OtherMap(Upos, (zoom - 1) + 8, VMap.GeoConvert);
            xx:=Fmain.X2AbsX(BPos.x-pr_x+(x shl 8),zoom);
            yy:=Fmain.X2AbsX(BPos.y-pr_y+(y shl 8),zoom);
            LoadXY.X := xx;
            LoadXY.Y := yy;

            lastload.X:=XX-(abs(XX) mod 256);
            lastload.Y:=YY-(abs(YY) mod 256);
            lastload.z:=zoom;
            lastLoad.mt:=MapType[ii];
            lastLoad.use:=true;
            if (FMain.TileSource=tsInternet)or((FMain.TileSource=tsCacheInternet)and(not(VMap.TileExists(xx,yy,zoom)))) then begin
              If (VMap.UseAntiBan>1) then begin
                inc(num_dwn);
                If ((num_dwn>0)and((num_dwn mod VMap.UseAntiBan)=0)) then begin
                  mapsload:=false;
                end;
              end;
              if VMap.UseDwn then begin
                FileBuf:=TMemoryStream.Create;
                try
                  res:=DownloadTile(LoadXY, Zoom, VMap, 0, ty, fileBuf);
                  ErrorString:=GetErrStr(res);
                  if (res = dtrOK) or (res = dtrSameTileSize) then begin
                    LastLoadTileSize := fileBuf.Size;
                    Synchronize(addDwnTiles);
                  end;
                  if (res = dtrTileNotExists)and(GState.SaveTileNotExists) then begin
                    VMap.SaveTileNotExists(LoadXY.X, LoadXY.Y, Zoom);
                  end;
                  if res = dtrOK then begin
                    VMap.SaveTileDownload(xx, yy, zoom, fileBuf, ty);
                  end;
                  Synchronize(AfterWriteToFile);
                finally
                  FileBuf.Free;
                end;
              end else begin
                ErrorString:=SAS_ERR_NotLoads;
              end;
              sleep(typemap.Sleep);
              While (FMain.MapMoving)or(FMain.MapZoomAnimtion=1) do Sleep(10);
            end;
          end;
        end;
      end;
    end;
  until Terminated;
end;

procedure ThreadAllLoadMap.dwnReg;
var
  p_x,p_y: integer;
  ty: string;
  VTileExists: boolean;
  fileBuf:TMemoryStream;
  res: TDownloadTileResult;
  razlen: integer;
  VGotoNextTile: Boolean;
begin
  OperBegin:=now;
  LastSuccessfulPoint:=Point(-1,-1);
  if min.x<StartPoint.x then begin
    p_x:=StartPoint.x;
  end else begin
    p_x:=min.x;
  end;
  if min.y<StartPoint.y then begin
    p_y:=StartPoint.Y
  end else begin
    p_y:=min.Y;
  end;
  while p_x<max.X do begin
    while p_y<max.y do begin
      sleep(1);
      if (_FProgress.stop) then begin
        Synchronize(UpdateProgressForm);
        While (_FProgress.stop)and(_FProgress.Visible) do sleep(100);
      end;
      if not(_FProgress.Visible) then exit;
      if RgnAndRgn(Poly,p_x,p_y,false) then begin
        LoadXY.X := p_x;
        LoadXY.Y := p_y;
        lastload.X:=p_x-(abs(p_x) mod 256);
        lastload.Y:=p_y-(abs(p_y) mod 256);
        lastload.z:=zoom;
        lastLoad.mt:=typemap;
        lastLoad.use:=true;
        TimeEnd:=GetTimeEnd(num_dwn,obrab);
        LenEnd:=GetLenEnd(num_dwn,obrab,scachano,dwnb);
        Synchronize(UpdateProgressForm);
        VTileExists := typeMap.TileExists(p_x,p_y,zoom);
        if (zamena) or not(VTileExists) then begin
          if VTileExists then begin
            AddToMemo:=SAS_STR_LoadProcessRepl+' ...';
          end else begin
            AddToMemo:=SAS_STR_LoadProcess+'...';
          end;
          Synchronize(UpdateMemoProgressForm);
          if (zDate)and(VTileExists)and(typeMap.TileLoadDate(p_x,p_y,zoom)>=FDate) then   begin
            AddToMemo:=AddToMemo+#13#10+SAS_MSG_FileBeCreateTime;
            Synchronize(UpdateMemoProgressForm);
            VGotoNextTile := True;
            inc(obrab);
          end else begin
            razlen:=typeMap.TileSize(p_x,p_y,zoom);

            FileBuf:=TMemoryStream.Create;
            try
              if (not(SecondLoadTNE))and(typemap.TileNotExistsOnServer(p_x,p_y,zoom)) then begin
                res := dtrTileNotExists;
              end else begin
                sleep(typemap.Sleep);
                res:=DownloadTile(LoadXY, Zoom, typemap, razlen, ty, fileBuf);
              end;

              If (typemap.UseAntiBan>1)and((scachano>0)and((scachano mod typemap.UseAntiBan)=0)) then begin
                mapsload:=false;
                Synchronize(addDwnforban);
              end;
              case res of
                dtrSameTileSize: begin
                  LastLoadTileSize := razlen;
                  Synchronize(addDwnTiles);
                  dwnb := dwnb + (LastLoadTileSize / 1024);
                  AddToMemo:=SAS_MSG_FileBeCreateLen;
                  Synchronize(UpdateMemoProgressForm);
                  inc(obrab);
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
                  AddToMemo:=SAS_ERR_Ban+#13#13+SAS_STR_Wite+' 10 '+SAS_UNITS_Secund+'...';
                  sleep(10000);
                  AddToMemo:=AddToMemo+#13#10+SAS_ERR_RepeatProcess;
                  Synchronize(UpdateMemoProgressForm);
                  VGotoNextTile := false;
                end;
                dtrTileNotExists: begin
                  AddToMemo:=SAS_ERR_TileNotExists;
                  Synchronize(UpdateMemoProgressForm);
                  inc(obrab);
                  if (GState.SaveTileNotExists) then begin
                    typemap.SaveTileNotExists(LoadXY.X, LoadXY.Y, Zoom);
                  end;
                  VGotoNextTile := True;
                end;
                dtrDownloadError: begin
                  AddToMemo:=SAS_ERR_Noconnectionstointernet;
                  if GState.GoNextTileIfDownloadError then begin
                    inc(obrab);
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
                  typemap.SaveTileDownload(p_x, p_y, Zoom, fileBuf, ty);
                  LastLoadTileSize := fileBuf.Size;
                  Synchronize(addDwnTiles);
                  inc(obrab);
                  Synchronize(AfterWriteToFile);
                  dwnb := dwnb + (LastLoadTileSize / 1024);
                  inc(scachano);
                  AddToMemo:='(Ok!)';
                  LastSuccessfulPoint:=Point(p_x,p_y);
                  Synchronize(UpdateMemoAddProgressForm);
                  VGotoNextTile := True;
                end;
                else begin
                  ErrorString:=GetErrStr(res);
                  AddToMemo:=ErrorString;
                  AddToMemo:=AddToMemo+#13#10+SAS_STR_Wite+' 5 '+SAS_UNITS_Secund+'...';
                  Synchronize(UpdateMemoProgressForm);
                  sleep(5000);
                end;
              end;
            finally
              FileBuf.Free;
            end;
          end;
        end else begin
          inc(obrab);
          AddToMemo:=SAS_ERR_FileExistsShort+';';
          Synchronize(UpdateMemoProgressForm);
          VGotoNextTile := True;
        end;
      end else begin
        VGotoNextTile := True;
      end;
      if VGotoNextTile then begin
        inc(p_y,256);
      end;
      StartPoint.Y:=p_y;
    end;
    p_y:=min.Y;
    inc(p_x,256);
    StartPoint.X:=p_x;
  end;
  Synchronize(CloseProgressForm);
end;

procedure ThreadAllLoadMap.Execute;
begin
 if typeRect=1 then dwnOne;
 if typeRect in [2,3] then dwnReg;
 if typeRect=4 then DwnInFon;
end;

procedure ThreadAllLoadMap.AfterWriteToFile;
begin
 if (not(typeRect in [2,3]))and(Fmain.Enabled)and(not(Fmain.MapMoving))and(not(FMain.MapZoomAnimtion=1)) then
  begin
   Fmain.generate_im(TLastLoad(lastload),ErrorString);
  end
 else Fmain.toSh;
end;

procedure ThreadAllLoadMap.GetCurrentMapAndPos;
begin
 TypeMap:=Sat_map_Both;
 Upos:= FMain.pos;
 Zoom:= GState.zoom_size;
end;

destructor ThreadAllLoadMap.Destroy;
begin
  FreeAndNil(FDownloader);
  inherited;
end;

end.
