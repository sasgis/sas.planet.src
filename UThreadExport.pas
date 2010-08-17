unit UThreadExport;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  Graphics,
  VCLZIp,
  GR32,
  i_ITileFileNameGenerator,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  t_GeoTypes;

type
  TThreadExport = class(TThread)
  private
    FPolygLL:TExtendedPointArray;
    FZoomArr:array [0..23] of boolean;
    FMapTypeArr:array of TMapType;
    FTileNameGen: ITileFileNameGenerator;
    Fprogress: TFprogress2;
    FIsMove:boolean;
    FIsZiped:boolean;
    FIsReplace:boolean;
    FPathExport:string;
    FZip:TVCLZip;
    Zippu:boolean;
    procedure savefilesREG;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Execute; override;
  public
    constructor Create(
      APath: string;
      APolygon: TExtendedPointArray;
      Azoomarr: array of boolean;
      Atypemaparr: array of TMapType;
      Amove: boolean;
      Areplace: boolean;
      Aziped: boolean;
      ATileNameGen: ITileFileNameGenerator
    );
  end;

implementation

uses
  u_GeoToStr,
  unit1;

procedure TThreadExport.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if Zippu then FZip.CancelTheOperation;
end;

constructor TThreadExport.Create(
  APath: string;
  APolygon: TExtendedPointArray;
  Azoomarr: array of boolean;
  Atypemaparr: array of TMapType;
  Amove, Areplace, Aziped: boolean;
  ATileNameGen: ITileFileNameGenerator
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
  FPathExport:=APath;
  FIsMove:=AMove;
  FTileNameGen := ATileNameGen;
  FIsZiped:=Aziped;
  FIsReplace:=AReplace;
  setlength(FPolygLL,length(APolygon));
  for i:=1 to length(APolygon) do
    FPolygLL[i-1]:=APolygon[i-1];
  for i:=0 to 23 do
    FZoomArr[i]:=Azoomarr[i];
  setlength(FMapTypeArr,length(Atypemaparr));
  for i:=1 to length(Atypemaparr) do
    FMapTypeArr[i-1]:=Atypemaparr[i-1];
end;


procedure TThreadExport.Execute;
begin
  Zippu:=false;
  savefilesREG;
  FProgress.Close;
end;

function RetDate(inDate: TDateTime): string;
var xYear, xMonth, xDay: word;
begin
  DecodeDate(inDate, xYear, xMonth, xDay);
  Result := inttostr(xDay)+'.'+inttostr(xMonth)+'.'+inttostr(xYear);
end;

procedure TThreadExport.savefilesREG;
var p_x,p_y,i,j:integer;
    num_dwn,obrab:integer;
    polyg:TPointArray;
    pathfrom,pathto,persl,perzoom,kti,datestr:string;
    max,min:TPoint;
    VExt: string;
    VPath: string;
    VMinLonLat, VMaxLonLat: TExtendedPoint;
    VTile: TPoint;
begin
 num_dwn:=0;
 SetLength(polyg,length(FPolygLL));
 persl:='';
 kti:='';
 datestr:=RetDate(now);
 for i:=0 to length(FMapTypeArr)-1 do
  begin
   persl:=persl+ FMapTypeArr[i].GetShortFolderName+'_';
   perzoom:='';
   for j:=0 to 23 do
    if FZoomArr[j] then
     begin
      polyg := FMapTypeArr[i].GeoConvert.PoligonProject(j + 8, FPolygLL);
      num_dwn:=num_dwn+GetDwnlNum(min,max,Polyg,true);
      perzoom:=perzoom+inttostr(j+1)+'_';
      VMinLonLat := FMapTypeArr[i].GeoConvert.PixelPos2LonLat(min,j);
      VMaxLonLat := FMapTypeArr[i].GeoConvert.PixelPos2LonLat(min,j);
      kti:=RoundEx(VMinLonLat.x,4);
      kti:=kti+'_'+RoundEx(VMinLonLat.y,4);
      kti:=kti+'_'+RoundEx(VMaxLonLat.x,4);
      kti:=kti+'_'+RoundEx(VMaxLonLat.y,4);
     end;
  end;
 persl:=copy(persl,1,length(persl)-1);
 perzoom:=copy(perzoom,1,length(perzoom)-1);
 if FIsZiped then begin
                fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles+' '+SAS_STR_CreateArhList;
                FZip:=TVCLZip.Create(Fmain);
                Zippu:=true;
                FZip.Recurse := False;
                FZip.StorePaths := true; // Путь не сохраняем
                FZip.PackLevel := 0; // Уровень сжатия
                FZip.ZipName := FPathExport+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
               end
          else fprogress.MemoInfo.Lines[0]:=SAS_STR_ExportTiles;
 fprogress.Caption:=SAS_STR_AllSaves+' '+inttostr(num_dwn)+' '+SAS_STR_Files;
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
 FProgress.ProgressBar1.Max:=100;
 FProgress.ProgressBar1.Progress1:=0;
 obrab:=0;
 for i:=0 to 23 do //по масштабу
  if FZoomArr[i] then
   for j:=0 to length(FMapTypeArr)-1 do //по типу
     begin
      polyg := FMapTypeArr[j].GeoConvert.PoligonProject(i + 8, FPolygLL);
      VExt := FMapTypeArr[j].TileFileExt;
      VPath := IncludeTrailingPathDelimiter(IncludeTrailingPathDelimiter(FPathExport) + FMapTypeArr[j].GetShortFolderName);
      GetDwnlNum(min,max,Polyg,false);
      p_x:=min.x;
      while p_x<max.x do
       begin
        VTile.X := p_x shr 8;
        p_y:=min.Y;
        while p_y<max.Y do
         begin
          VTile.Y := p_y shr 8;
          if FProgress.Visible=false then
           begin
            Fmain.generate_im;
            exit;
           end;
          if not(RgnAndRgn(Polyg,p_x,p_y,false)) then begin
                                                 inc(p_y,256);
                                                 CONTINUE;
                                                end;
          if FMapTypeArr[j].TileExists(VTile, i) then
           begin
            if FIsZiped then begin
//TODO: Разобраться и избавиться от путей. Нужно предусмотреть вариант, что тайлы хранятся не в файлах, а перед зипованием сохраняются в файлы.
                            pathfrom:=FMapTypeArr[j].GetTileFileName(VTile, i);
                            FZip.FilesList.Add(pathfrom);
                          end
                     else begin
                           pathto:= VPath + FTileNameGen.GetTileFileName(VTile, i) + VExt;
                           if FMapTypeArr[j].TileExportToFile(VTile, i, pathto, FIsReplace) then begin
                             if FIsMove then FMapTypeArr[j].DeleteTile(VTile, i);
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
 if FIsZiped then
  begin
   fprogress.MemoInfo.Lines[0]:=SAS_STR_Pack+' '+'SG-'+persl+'-'+perzoom+'-'+kti+'-'+datestr+'.ZIP';
   if FileExists(FZip.ZipName) then DeleteFile(FZip.ZipName);
   If FZip.FZip=0 then
    Application.MessageBox(PChar(SAS_ERR_CreateArh),PChar(SAS_MSG_coution),48);
   FZip.free;
   Zippu:=false;
  end;
 FProgress.ProgressBar1.Progress1:=round((obrab/num_dwn)*100);
 fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(obrab);
 FTileNameGen := nil;
 FProgress.Close;
end;

end.
