unit UThreadExport;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  Graphics,
  gifimage,
  VCLZIp,
  PNGImage,
  JPEG,
  GR32,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  t_GeoTypes;

type
  TThreadExport = class(TThread)
  private
    PolygLL:TExtendedPointArray;
    Zoomarr:array [0..23] of boolean;
    typemaparr:array of TMapType;
    format:byte;
    Fprogress: TFprogress2;
    Move,ziped:boolean;
    Replace:boolean;
    Path:string;
    Zip:TVCLZip;
    Zippu:boolean;
    procedure savefilesREG(APolyLL:TExtendedPointArray);
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Execute; override;
  public
    constructor Create(
      APath: string;
      APolygon_: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean;
      Aziped: boolean;
      Aformat: byte
    );
  end;

implementation

uses
  u_GeoToStr,
  unit1,
  i_ITileFileNameGenerator,
  u_GlobalState;

procedure TThreadExport.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if Zippu then Zip.CancelTheOperation;
end;

constructor TThreadExport.Create(
  APath: string;
  APolygon_: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Amove, Areplace, Aziped: boolean;
  Aformat: byte
);
var i:integer;
begin
  inherited Create(false);
  Priority := tpLowest;
  FreeOnTerminate:=true;
  Application.CreateForm(TFProgress2, FProgress);
  Zippu:=false;
  FProgress.OnClose:=CloseFProgress;
  FProgress.Visible:=true;
  Path:=APath;
  Move:=AMove;
  format:=AFormat+1;
  ziped:=Aziped;
  Replace:=AReplace;
  setlength(PolygLL,length(APolygon_));
  for i:=1 to length(APolygon_) do
    PolygLL[i-1]:=Apolygon_[i-1];
  for i:=0 to 23 do
    zoomarr[i]:=Azoomarr[i];
  setlength(typemaparr,length(Atypemaparr));
  for i:=1 to length(Atypemaparr) do
    typemaparr[i-1]:=Atypemaparr[i-1];
end;


procedure TThreadExport.Execute;
begin
  Zippu:=false;
  savefilesREG(PolygLL);
  FProgress.Close;
end;

function RetDate(inDate: TDateTime): string;
var xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  Result := inttostr(xDay)+'.'+inttostr(xMonth)+'.'+inttostr(xYear);
end;

procedure TThreadExport.savefilesREG(APolyLL:TExtendedPointArray);
var p_x,p_y,i,j:integer;
    num_dwn,obrab:integer;
    polyg:TPointArray;
    pathfrom,pathto,persl,perzoom,kti,datestr:string;
    max,min:TPoint;
    VTileNameGen: ITileFileNameGenerator;
    VExt: string;
    VPath: string;
begin
 num_dwn:=0;
 SetLength(polyg,length(APolyLL));
 persl:='';
 kti:='';
 datestr:=RetDate(now);
 for i:=0 to length(TypeMapArr)-1 do
  begin
   persl:=persl+ TypeMapArr[i].GetShortFolderName+'_';
   perzoom:='';
   for j:=0 to 23 do
    if zoomarr[j] then
     begin
      polyg := TypeMapArr[i].GeoConvert.PoligonProject(j + 8, APolyLL);
      num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      kti:=RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(min,j + 8).y,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).x,4);
      kti:=kti+'_'+RoundEx(TypeMapArr[i].GeoConvert.Pos2LonLat(max,j + 8).y,4);
     end;
  end;
 persl:=copy(persl,1,length(persl)-1);
 perzoom:=copy(perzoom,1,length(perzoom)-1);
 if ziped then begin
                fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles+' '+SAS_STR_CreateArhList;
                Zip:=TVCLZip.Create(Fmain);
                Zippu:=true;
                Zip.Recurse := False;
                Zip.StorePaths := true; // Путь не сохраняем
                Zip.PackLevel := 0; // Уровень сжатия
                Zip.ZipName := path+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
               end
          else fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_Files;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 FProgress.ProgressBar1.Max:=100;
 FProgress.ProgressBar1.Progress1:=0;
 obrab:=0;
 for i:=0 to 23 do //по масштабу
  if zoomarr[i] then
   for j:=0 to length(TypeMapArr)-1 do //по типу
     begin
      polyg := TypeMapArr[j].GeoConvert.PoligonProject(i + 8, APolyLL);
      VExt := TypeMapArr[j].TileFileExt;
      VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(PATH) + TypeMapArr[j].GetShortFolderName);
      VTileNameGen := GState.TileNameGenerator.GetGenerator(format);
      GetDwnlNum(min,max,Polyg,false);
      p_x:=min.x;
      while p_x<max.x do
       begin
        p_y:=min.Y;
        while p_y<max.Y do
         begin
          if FProgress.Visible=false then
           begin
            Fmain.generate_im(nilLastLoad,'');
            exit;
           end;
          if not(RgnAndRgn(Polyg,p_x,p_y,false)) then begin
                                                 inc(p_y,256);
                                                 CONTINUE;
                                                end;
          if TypeMapArr[j].TileExists(p_x,p_y,i+1) then
           begin
            if ziped then begin
//TODO: Разобраться и избавиться от путей. Нужно предусмотреть вариант, что тайлы хранятся не в файлах, а перед зипованием сохраняются в файлы.
                            pathfrom:=TypeMapArr[j].GetTileFileName(p_x,p_y,i+1);
                            Zip.FilesList.Add(pathfrom);
                          end
                     else begin
                           pathto:= VPath + VTileNameGen.GetTileFileName(Point(p_x shr 8,p_y shr 8), i) + VExt;
                           if TypeMapArr[j].TileExportToFile(p_x,p_y,i+1, pathto, replace) then begin
                             if move then TypeMapArr[j].DeleteTile(p_x,p_y,i+1);
                           end;
                          end;
           end;
          inc(obrab);
          if obrab mod 100 = 0 then
           begin
            FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
            fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
           end;
          inc(p_y,256);
         end;
        inc(p_x,256);
       end;
     end;
 if ziped then
  begin
   fprogress.MemoInfo.Lines[0]:=SAS_STR_Pack+' '+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
   if FileExists(Zip.ZipName) then DeleteFile(Zip.ZipName);
   If Zip.Zip=0 then
    Application.MessageBox(PChar(SAS_ERR_CreateArh),PChar(SAS_MSG_coution),48);
   Zip.free;
   Zippu:=false;
  end;
 FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
 VTileNameGen := nil;
 FProgress.Close;
end;

end.
