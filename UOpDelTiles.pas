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
    FPolygLL: TExtendedPointArray;
    polyg:TPointArray;
    max,min:TPoint;
    FTilesToProcess:integer;
    FProgressForm: TFprogress2;
    FDeletedCount:integer;
    DelBytes:boolean;
    DelBytesNum:integer;
    procedure DeleteTiles;
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure CloseProgressForm;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Execute; override;
  public
    constructor Create(
      APolyLL: TExtendedPointArray;
      Azoom: byte;
      Atypemap: TMapType;
      ADelByte:boolean;
      ADelBytesNum:integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_GlobalState,
  unit1,
  Ugeofun;

constructor TOpDelTiles.Create(
  APolyLL: TExtendedPointArray;
  Azoom:byte;
  Atypemap:TMapType;
  ADelByte:boolean;
  ADelBytesNum:integer
);
begin
  inherited Create(False);
  FDeletedCount:=0;
  zoom:=Azoom;
  typemap:=Atypemap;
  FPolygLL := APolyLL;
  Priority := tpLowest;
  FreeOnTerminate:=true;
  DelBytes:=ADelByte;
  DelBytesNum := ADelBytesNum;
end;

destructor TOpDelTiles.Destroy;
begin
 inherited;
end;

procedure TOpDelTiles.Execute;
begin
 DeleteTiles;
 Synchronize(CloseProgressForm);
end;

procedure TOpDelTiles.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if not(Terminated) then Terminate;
end;

procedure TOpDelTiles.CloseProgressForm;
begin
 FProgressForm.Free;
 GState.MainFileCache.Clear;
 Fmain.generate_im;
end;

procedure TOpDelTiles.UpdateProgressForm;
begin
  FProgressForm.MemoInfo.Lines[0]:=SAS_STR_AllDelete+' '+inttostr(FDeletedCount)+' '+SAS_STR_files;
  FProgressForm.ProgressBar1.Progress1:=FProgressForm.ProgressBar1.Progress1+1;
  FProgressForm.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(FProgressForm.ProgressBar1.Progress1);
end;

procedure TOpDelTiles.SetProgressForm;
begin
  Application.CreateForm(TFProgress2, FProgressForm);
  FProgressForm.OnClose:=CloseFProgress;
  FProgressForm.Visible:=true;
  FProgressForm.Caption:=SAS_STR_Deleted+' '+inttostr(FTilesToProcess)+' '+SAS_STR_files+' (x'+inttostr(zoom)+')';
  FProgressForm.MemoInfo.Lines[0]:=SAS_STR_AllDelete+' 0';
  FProgressForm.MemoInfo.Lines[1]:=SAS_STR_Processed+' 0';
  FProgressForm.ProgressBar1.Progress1:=0;
  FProgressForm.ProgressBar1.Max:=FTilesToProcess;
end;

procedure TOpDelTiles.DeleteTiles;
var i,j:integer;
  VTile: TPoint;
begin
  polyg := typemap.GeoConvert.LonLatArray2PixelArray(FPolygLL, (Zoom - 1));
  FTilesToProcess:=GetDwnlNum(min,max,Polyg,true);
  Synchronize(SetProgressForm);
  i:=min.x;
  while (i<max.X)and(not Terminated) do begin
    VTile.X := i shr 8;
    j:=min.Y;
    while (j<max.y)and(not Terminated) do  begin
      VTile.Y := j shr 8;
      if RgnAndRgn(Polyg,i,j,false) then begin
        if (not DelBytes or (DelBytesNum=typemap.TileSize(VTile, zoom - 1))) then begin
          if typemap.DeleteTile(VTile, Zoom - 1) then begin
            inc(FDeletedCount);
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
