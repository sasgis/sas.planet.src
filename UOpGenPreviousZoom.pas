unit UOpGenPreviousZoom;

interface

uses
  Windows,
  Types,
  Forms,
  SysUtils,
  Classes,
  math,
  Graphics,
  Dialogs,
  GR32,
  GR32_Resamplers,
  UMapType,
  UGeoFun,
  unit4,
  UResStrings,
  Uimgfun,
  t_GeoTypes;

type
  TOpGenPreviousZoom = class(TThread)
  private
    Replace:boolean;
    savefull:boolean;
    GenFormPrev:boolean;
    PolygLL: TExtendedPointArray;
    FromZoom:byte;
    InZooms: TArrayOfByte;
    typemap:TMapType;
    max,min:TPoint;
    ProcessTiles:integer;

    Resampler:TTileResamplingType;
    polyg:TPointArray;
    FProgressForm: TFprogress2;
    TileInProc:integer;
    CurrentTile:integer;
    procedure GenPreviousZoom;
    procedure SetProgressForm;
    procedure UpdateProgressForm;
    procedure CloseProgressForm;
    procedure SyncShowMessage;
    procedure CloseFProgress(Sender: TObject; var Action: TCloseAction);
  protected
    procedure Execute; override;
  public
    constructor Create(
      Azoom: byte;
      AInZooms: TArrayOfByte;
      APolygLL: TExtendedPointArray;
      Atypemap: TMapType;
      AReplace: boolean;
      Asavefull: boolean;
      AGenFormPrev: boolean;
      AResampler:TTileResamplingType
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_GlobalState,
  unit1;

constructor TOpGenPreviousZoom.Create(Azoom:byte; AInZooms: TArrayOfByte; APolygLL: TExtendedPointArray; Atypemap:TMapType; AReplace:boolean; Asavefull:boolean; AGenFormPrev:boolean; AResampler:TTileResamplingType);
begin
  inherited Create(False);
  Priority := tpLowest;
  FreeOnTerminate:=true;
  Replace := AReplace;
  savefull := Asavefull;
  GenFormPrev := AGenFormPrev;
  InZooms := AInZooms;
  PolygLL := APolygLL;
  TileInProc:=0;
  FromZoom:=Azoom;
  typemap:=Atypemap;
  Resampler := AResampler;
end;

destructor TOpGenPreviousZoom.Destroy;
begin
 Synchronize(CloseProgressForm);
 inherited ;
end;

procedure TOpGenPreviousZoom.Execute;
var i:integer;
begin
 setlength(polyg,length(PolygLL));
 ProcessTiles:=0;
 for i:=0 to length(InZooms)-1 do
   begin
    polyg := typemap.GeoConvert.LonLatArray2PixelArray(PolygLL, InZooms[i] - 1);
    if (not GenFormPrev)or(i=0) then
                  inc(ProcessTiles,GetDwnlNum(min,max,Polyg,true)*Round(IntPower(4,FromZoom-InZooms[i])))
             else inc(ProcessTiles,GetDwnlNum(min,max,Polyg,true)*Round(IntPower(4,InZooms[i-1]-InZooms[i])));
   end;
 Synchronize(SetProgressForm);
 GenPreviousZoom;
end;

procedure TOpGenPreviousZoom.CloseFProgress(Sender: TObject; var Action: TCloseAction);
begin
 if not(Terminated) then Terminate;
end;

procedure TOpGenPreviousZoom.CloseProgressForm;
begin
 FProgressForm.Free;
 GState.MainFileCache.Clear;
 Fmain.generate_im;
end;

procedure TOpGenPreviousZoom.UpdateProgressForm;
begin
  FProgressForm.MemoInfo.Lines[0]:=SAS_STR_Saves+': '+inttostr(TileInProc)+' '+SAS_STR_files;
  FProgressForm.ProgressBar1.Progress1:=CurrentTile;
  FProgressForm.MemoInfo.Lines[1]:=SAS_STR_Processed+' '+inttostr(CurrentTile);
end;

procedure TOpGenPreviousZoom.SetProgressForm;
begin
  Application.CreateForm(TFProgress2, FProgressForm);
  FProgressForm.OnClose:=CloseFProgress;
  FProgressForm.Visible:=true;
  FProgressForm.Caption:=SAS_STR_ProcessedNoMore+': '+inttostr(ProcessTiles)+' '+SAS_STR_files;
  FProgressForm.MemoInfo.Lines[0]:=SAS_STR_Processed+' 0';
  FProgressForm.MemoInfo.Lines[1]:=SAS_STR_Saves+': 0';
  FProgressForm.ProgressBar1.Progress1:=0;
  FProgressForm.ProgressBar1.Max:=ProcessTiles;
end;

procedure TOpGenPreviousZoom.GenPreviousZoom;
var
  bmp_ex:TCustomBitmap32;
  bmp:TCustomBitmap32;
  i,c_d,p_x,p_y,d2562,p_i,p_j,p_x_x,p_y_y:integer;
  save_len_tile:integer;
  VZoom: Integer;
  VTile: TPoint;
  VSubTile: TPoint;
begin
  bmp_ex:=TCustomBitmap32.Create;
  bmp:=TCustomBitmap32.Create;
  try
    bmp.Resampler := CreateResampler(Resampler);

    TileInProc:=0;
    CurrentTile:=0;
    for i:=0 to length(InZooms)-1 do begin
     if Terminated then continue;
     polyg := typemap.GeoConvert.LonLatArray2PixelArray(PolygLL, InZooms[i] - 1);
     if (not GenFormPrev)or(i=0) then
                   c_d:=round(power(2,FromZoom-InZooms[i]))
              else c_d:=round(power(2,InZooms[i-1]-InZooms[i]));
     if (not GenFormPrev)or(i=0) then
                  VZoom := FromZoom
             else VZoom := InZooms[i-1];
     GetDwnlNum(min,max,Polyg,false);
     p_x:=min.x;
     while (p_x<max.X)and(not Terminated) do
      begin
       VTile.X := p_x shr 8;
       p_y:=min.y;
       while (p_y<max.y)and(not Terminated) do
        begin
         VTile.Y := p_y shr 8;
         if RgnAndRgn(Polyg,p_x,p_y,false) then begin
           if typemap.TileExists(VTile, InZooms[i] - 1)then begin
                                    if not(Replace)
                                     then begin
                                           Synchronize(UpdateProgressForm);
                                           inc(p_y,256);
                                           continue;
                                          end;
                                    typemap.LoadTile(bmp_Ex, VTile, InZooms[i] - 1, false);
                                   end
                              else begin
                                    bmp_ex.SetSize(256, 256);
                                    bmp_ex.Clear(Color32(GState.BGround));
                                   end;
           d2562:=256 div c_d;
           save_len_tile:=0;
           for p_i:=1 to c_d do begin
            if Terminated then continue;
            for p_j:=1 to c_d do begin
              if Terminated then continue;
              p_x_x:=((p_x-128) * c_d)+((p_i-1)*256);
              p_y_y:=((p_y-128) * c_d)+((p_j-1)*256);
              VSubTile := Point(p_x_x shr 8, p_y_y shr 8);

              if typemap.TileExists(VSubTile, VZoom - 1) then
               begin
                if (typemap.LoadTile(bmp, VSubTile, VZoom - 1, false)) then begin
                  bmp_ex.Draw(bounds((p_i-1)*d2562,(p_j-1)*d2562,256 div c_d,256 div c_d),bounds(0,0,256,256),bmp);
                  inc(save_len_tile);
                end else begin
                  Assert(False, 'Ошибка чтения тайла.');
                end;
               end;
              inc(CurrentTile);
              if (CurrentTile mod 30 = 0) then Synchronize(UpdateProgressForm);
            end;
           end;
           if Terminated then continue;
           if ((not savefull)or(save_len_tile=c_d*c_d))and(save_len_tile > 0) then
             try
              typemap.SaveTileSimple(VTile, InZooms[i] - 1, bmp_ex);
              inc(TileInProc);
             except
              Synchronize(SyncShowMessage);
              Terminate;
             end;
         end;
         inc(p_y,256);
        end;
       inc(p_x,256);
      end;
    end;
  finally
    bmp_ex.Free;
    bmp.Free;
  end;
end;

procedure TOpGenPreviousZoom.SyncShowMessage;
begin
  ShowMessage(SAS_ERR_Write);
end;

end.
