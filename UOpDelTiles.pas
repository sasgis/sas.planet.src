unit UOpDelTiles;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  t_GeoTypes,
  UMapType,
  UImgFun,
  unit4,
  UResStrings;

type
  TOpDelTiles = class(TThread)
    Zoom:byte;
    typemap:TMapType;
    polyg:TPointArray;
    max,min:TPoint;
    ProcessTiles:integer;
  private
    Fprogress: TFprogress2;
    TileInProc:integer;
    Fx, Fy: Integer;
  protected
    procedure DeleteTiles;
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure CloseProgressForm;
    procedure Execute; override;
    procedure DelTileOp;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  public
    destructor destroy; override;
    constructor Create(CrSusp:Boolean;Azoom:byte;Atypemap:TMapType);
  end;

implementation

uses
  unit1,
  Ugeofun;

constructor TOpDelTiles.Create(CrSusp:Boolean;Azoom:byte;Atypemap:TMapType);
begin
  TileInProc:=0;
  zoom:=Azoom;
  typemap:=Atypemap;
  inherited Create(CrSusp);
end;

destructor TOpDelTiles.destroy;
begin
 Synchronize(CloseProgressForm);
 inherited ;
end;

procedure TOpDelTiles.Execute;
begin
 Synchronize(SetProgressForm);
 DeleteTiles;
end;

procedure TOpDelTiles.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if not(Terminated) then Terminate;
end;

procedure TOpDelTiles.CloseProgressForm;
begin
 fprogress.Free;
 MainFileCache.Clear;
 Fmain.generate_im(nilLastLoad,'');
end;

procedure TOpDelTiles.UpdateProgressForm;
begin
  fprogress.MemoInfo.Lines[0]:=SAS_STR_AllDelete+' '+inttostr(TileInProc)+' '+SAS_STR_files;
  FProgress.ProgressBar1.Progress1:=FProgress.ProgressBar1.Progress1+1;
  fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgress.ProgressBar1.Progress1);
end;

procedure TOpDelTiles.SetProgressForm;
begin
  Application.CreateForm(TFProgress2, FProgress);
  FProgress.OnClose:=CloseFProgress;
  FProgress.Visible:=true;
  fprogress.Caption:=SAS_STR_Deleted+' '+inttostr(ProcessTiles)+' '+SAS_STR_files+' (x'+inttostr(zoom)+')';
  fprogress.MemoInfo.Lines[0]:=SAS_STR_AllDelete+' 0';
  fprogress.MemoInfo.Lines[1]:=SAS_STR_Processed+' 0';
  FProgress.ProgressBar1.Progress1:=0;
  FProgress.ProgressBar1.Max:=ProcessTiles;
end;

procedure TOpDelTiles.DelTileOp;
begin
  typemap.DeleteTile(fx, fy, Zoom)
end;

procedure TOpDelTiles.DeleteTiles;
var i,j:integer;
begin
 i:=min.x;
 while (i<max.X)and(not Terminated) do
  begin
   j:=min.Y;
   while (j<max.y)and(not Terminated) do
    begin
     if not(RgnAndRgn(Polyg,i,j,false))
        then begin
              inc(J,256);
              CONTINUE;
             end;
     if typemap.TileExists(i,j,zoom) then begin
                               Fx := i;
                               FY := j;
                               Synchronize(DelTileOp);
                               inc(TileInProc);
                              end;
     Synchronize(UpdateProgressForm);
     inc(j,256);
    end;
   inc(i,256);
  end;
end;

end.
