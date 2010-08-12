unit UOpDelTiles;

interface

uses
  Windows,
  Forms,
  SysUtils,
  Classes,
  t_GeoTypes,
  UMapType,
  unit4,
  UResStrings;

type
  TOpDelTiles = class(TThread)
  private
    Zoom:byte;
    typemap:TMapType;
    polyg:TPointArray;
    max,min:TPoint;
    ProcessTiles:integer;
    Fprogress: TFprogress2;
    TileInProc:integer;
    DelBytes:boolean;
    procedure DeleteTiles;
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure CloseProgressForm;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Execute; override;
  public
    DelBytesNum:integer;
    constructor Create(
      CrSusp:Boolean;
      APolyLL: TExtendedPointArray;
      Azoom: byte;
      Atypemap: TMapType;
      ADelByte:boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_GlobalState,
  unit1,
  Ugeofun;

constructor TOpDelTiles.Create(CrSusp:Boolean; APolyLL: TExtendedPointArray; Azoom:byte;Atypemap:TMapType; ADelByte:boolean);
begin
  inherited Create(CrSusp);
  TileInProc:=0;
  zoom:=Azoom;
  typemap:=Atypemap;
  polyg := typemap.GeoConvert.PoligonProject((Zoom - 1) + 8, APolyLL);
  ProcessTiles:=GetDwnlNum(min,max,Polyg,true);
  Priority := tpLowest;
  FreeOnTerminate:=true;
  DelBytes:=ADelByte;
end;

destructor TOpDelTiles.Destroy;
begin
 Synchronize(CloseProgressForm);
 inherited;
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
 GState.MainFileCache.Clear;
 Fmain.generate_im;
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

procedure TOpDelTiles.DeleteTiles;
var i,j:integer;
  VTile: TPoint;
begin
  i:=min.x;
  while (i<max.X)and(not Terminated) do begin
    VTile.X := i shr 8;
    j:=min.Y;
    while (j<max.y)and(not Terminated) do  begin
      VTile.Y := j shr 8;
      if RgnAndRgn(Polyg,i,j,false) then begin
        if (not DelBytes or (DelBytesNum=typemap.TileSize(VTile, zoom - 1))) then begin
          if typemap.DeleteTile(VTile, Zoom - 1) then begin
            inc(TileInProc);
          end;
          Synchronize(UpdateProgressForm);
        end;
      end;
      inc(j,256);
    end;
    inc(i,256);
  end;
end;

end.
