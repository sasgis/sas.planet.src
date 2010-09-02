unit u_ThreadGenPrevZoom;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  math,
  GR32,
  GR32_Resamplers,
  UMapType,
  UGeoFun,
  u_ThreadRegionProcessAbstract,
  UResStrings,
  Uimgfun,
  t_GeoTypes;

type
  TThreadGenPrevZoom = class(TThreadRegionProcessAbstract)
  private
    Replace:boolean;
    savefull:boolean;
    GenFormPrev:boolean;
    FromZoom:byte;
    InZooms: TArrayOfByte;
    typemap:TMapType;

    Resampler:TTileResamplingType;
    TileInProc:integer;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress;
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
  end;

implementation

uses
  u_GlobalState;

constructor TThreadGenPrevZoom.Create(Azoom:byte; AInZooms: TArrayOfByte; APolygLL: TExtendedPointArray; Atypemap:TMapType; AReplace:boolean; Asavefull:boolean; AGenFormPrev:boolean; AResampler:TTileResamplingType);
begin
  inherited Create(APolygLL);
  Replace := AReplace;
  savefull := Asavefull;
  GenFormPrev := AGenFormPrev;
  InZooms := AInZooms;
  FPolygLL := APolygLL;
  TileInProc:=0;
  FromZoom:=Azoom;
  typemap:=Atypemap;
  Resampler := AResampler;
end;

procedure TThreadGenPrevZoom.ProcessRegion;
var
  bmp_ex:TCustomBitmap32;
  bmp:TCustomBitmap32;
  i,c_d,p_x,p_y,d2562,p_i,p_j,p_x_x,p_y_y:integer;
  save_len_tile:integer;
  VZoom: Integer;
  VTile: TPoint;
  VSubTile: TPoint;
  max,min:TPoint;
  polyg:TPointArray;
begin
  FTilesToProcess:=0;
  for i:=0 to length(InZooms)-1 do begin
    polyg := typemap.GeoConvert.LonLatArray2PixelArray(FPolygLL, InZooms[i] - 1);
    if (not GenFormPrev)or(i=0) then
                  inc(FTilesToProcess,GetDwnlNum(min,max,Polyg,true)*Round(IntPower(4,FromZoom-InZooms[i])))
             else inc(FTilesToProcess,GetDwnlNum(min,max,Polyg,true)*Round(IntPower(4,InZooms[i-1]-InZooms[i])));
  end;
  ProgressFormUpdateCaption(
    '',
    SAS_STR_ProcessedNoMore+': '+inttostr(FTilesToProcess)+' '+SAS_STR_files
  );

  bmp_ex:=TCustomBitmap32.Create;
  bmp:=TCustomBitmap32.Create;
  try
    bmp.Resampler := CreateResampler(Resampler);

    TileInProc:=0;
    FTilesProcessed:=0;
    for i:=0 to length(InZooms)-1 do begin
     if Terminated then continue;
     polyg := typemap.GeoConvert.LonLatArray2PixelArray(FPolygLL, InZooms[i] - 1);
     if (not GenFormPrev)or(i=0) then
                   c_d:=round(power(2,FromZoom-InZooms[i]))
              else c_d:=round(power(2,InZooms[i-1]-InZooms[i]));
     if (not GenFormPrev)or(i=0) then
                  VZoom := FromZoom
             else VZoom := InZooms[i-1];
     GetDwnlNum(min,max,Polyg,false);
     p_x:=min.x;
     while (p_x<max.X)and(not IsCancel) do
      begin
       VTile.X := p_x shr 8;
       p_y:=min.y;
       while (p_y<max.y)and(not IsCancel) do
        begin
         VTile.Y := p_y shr 8;
         if RgnAndRgn(Polyg,p_x,p_y,false) then begin
           if typemap.TileExists(VTile, InZooms[i] - 1)then begin
                                    if not(Replace)
                                     then begin
                                           ProgressFormUpdateOnProgress;
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
              inc(FTilesProcessed);
              if (FTilesProcessed mod 30 = 0) then ProgressFormUpdateOnProgress;
            end;
           end;
           if Terminated then continue;
           if ((not savefull)or(save_len_tile=c_d*c_d))and(save_len_tile > 0) then begin
              typemap.SaveTileSimple(VTile, InZooms[i] - 1, bmp_ex);
              inc(TileInProc);
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
  GState.MainFileCache.Clear;
end;

procedure TThreadGenPrevZoom.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressLine0AndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_Saves+': '+inttostr(TileInProc)+' '+SAS_STR_files,
    SAS_STR_Processed+' '+inttostr(FTilesProcessed)
  );
end;

end.
