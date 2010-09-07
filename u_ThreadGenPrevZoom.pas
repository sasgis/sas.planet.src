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
    FIsReplace: boolean;
    FIsSaveFullOnly: boolean;
    FGenFormPrevZoom: boolean;
    FSourceZoom: byte;
    FZooms: TArrayOfByte;
    FMapType: TMapType;

    FResamplerType: TTileResamplingType;
    FTileInProc: integer;
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
      AResampler: TTileResamplingType
    );
  end;

implementation

uses
  i_ICoordConverter,
  u_TileIteratorAbstract,
  u_TileIteratorStuped,
  u_GlobalState;

constructor TThreadGenPrevZoom.Create(Azoom: byte; AInZooms: TArrayOfByte; APolygLL: TExtendedPointArray; Atypemap: TMapType; AReplace: boolean; Asavefull: boolean; AGenFormPrev: boolean; AResampler: TTileResamplingType);
begin
  inherited Create(APolygLL);
  FIsReplace := AReplace;
  FIsSaveFullOnly := Asavefull;
  FGenFormPrevZoom := AGenFormPrev;
  FZooms := AInZooms;
  FPolygLL := APolygLL;
  FTileInProc := 0;
  FSourceZoom := Azoom;
  FMapType := Atypemap;
  FResamplerType := AResampler;
end;

procedure TThreadGenPrevZoom.ProcessRegion;
var
  bmp_ex: TCustomBitmap32;
  bmp: TCustomBitmap32;
  i, c_d, p_x, p_y, d2562, p_i, p_j, p_x_x, p_y_y: integer;
  save_len_tile: integer;
  VZoom: Integer;
  VTile: TPoint;
  VSubTile: TPoint;
  max, min: TPoint;
  polyg: TPointArray;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of TTileIteratorAbstract;
  VTileIterator: TTileIteratorAbstract;
  VZoomDelta: Integer;
begin
  inherited;
  FTilesToProcess := 0;
  SetLength(VTileIterators, Length(FZooms));
//  for i := 0 to Length(FZooms) - 1 do begin
//    VZoom := FZooms[i] - 1;
//    VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
//    FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal;
//  end;
//  try
    for i := 0 to length(FZooms) - 1 do begin
      polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, FZooms[i]);
      if (not FGenFormPrevZoom) or (i = 0) then begin
        inc(FTilesToProcess, GetDwnlNum(min, max, Polyg, true) * Round(IntPower(4, FSourceZoom - FZooms[i])));
      end else begin
        inc(FTilesToProcess, GetDwnlNum(min, max, Polyg, true) * Round(IntPower(4, FZooms[i - 1] - FZooms[i])));
      end;
    end;
    ProgressFormUpdateCaption(
      '',
      SAS_STR_ProcessedNoMore + ': ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files
      );

    bmp_ex := TCustomBitmap32.Create;
    bmp := TCustomBitmap32.Create;
    try
      bmp.Resampler := CreateResampler(FResamplerType);

      FTileInProc := 0;
      FTilesProcessed := 0;
      for i := 0 to length(FZooms) - 1 do begin
        if Terminated then begin
          continue;
        end;
        polyg := FMapType.GeoConvert.LonLatArray2PixelArray(FPolygLL, FZooms[i]);
        if (not FGenFormPrevZoom) or (i = 0) then begin
          c_d := round(power(2, FSourceZoom - FZooms[i]));
        end else begin
          c_d := round(power(2, FZooms[i - 1] - FZooms[i]));
        end;
        if (not FGenFormPrevZoom) or (i = 0) then begin
          VZoom := FSourceZoom;
        end else begin
          VZoom := FZooms[i - 1];
        end;
        GetDwnlNum(min, max, Polyg, false);
        p_x := min.x;
        while (p_x < max.X) and (not IsCancel) do begin
          VTile.X := p_x shr 8;
          p_y := min.y;
          while (p_y < max.y) and (not IsCancel) do begin
            VTile.Y := p_y shr 8;
            if RgnAndRgn(Polyg, p_x, p_y, false) then begin
              if FMapType.TileExists(VTile, FZooms[i]) then begin
                if not (FIsReplace) then begin
                  ProgressFormUpdateOnProgress;
                  inc(p_y, 256);
                  continue;
                end;
                FMapType.LoadTile(bmp_Ex, VTile, FZooms[i], false);
              end else begin
                bmp_ex.SetSize(256, 256);
                bmp_ex.Clear(Color32(GState.BGround));
              end;
              d2562 := 256 div c_d;
              save_len_tile := 0;
              for p_i := 1 to c_d do begin
                if Terminated then begin
                  continue;
                end;
                for p_j := 1 to c_d do begin
                  if Terminated then begin
                    continue;
                  end;
                  p_x_x := ((p_x - 128) * c_d) + ((p_i - 1) * 256);
                  p_y_y := ((p_y - 128) * c_d) + ((p_j - 1) * 256);
                  VSubTile := Point(p_x_x shr 8, p_y_y shr 8);

                  if FMapType.TileExists(VSubTile, VZoom) then begin
                    if (FMapType.LoadTile(bmp, VSubTile, VZoom, false)) then begin
                      bmp_ex.Draw(bounds((p_i - 1) * d2562, (p_j - 1) * d2562, 256 div c_d, 256 div c_d), bounds(0, 0, 256, 256), bmp);
                      inc(save_len_tile);
                    end else begin
                      Assert(False, 'Ошибка чтения тайла.');
                    end;
                  end;
                  inc(FTilesProcessed);
                  if (FTilesProcessed mod 30 = 0) then begin
                    ProgressFormUpdateOnProgress;
                  end;
                end;
              end;
              if Terminated then begin
                continue;
              end;
              if ((not FIsSaveFullOnly) or (save_len_tile = c_d * c_d)) and (save_len_tile > 0) then begin
                FMapType.SaveTileSimple(VTile, FZooms[i], bmp_ex);
                inc(FTileInProc);
              end;
            end;
            inc(p_y, 256);
          end;
          inc(p_x, 256);
        end;
      end;
    finally
      bmp_ex.Free;
      bmp.Free;
    end;
  GState.MainFileCache.Clear;
//  finally
//    for i := 0 to Length(VTileIterators) - 1 do begin
//      VTileIterators[i].Free;
//    end;
//    VTileIterators := nil;
//  end;
end;

procedure TThreadGenPrevZoom.ProgressFormUpdateOnProgress;
begin
  ProgressFormUpdateProgressLine0AndLine1(
    round((FTilesProcessed / FTilesToProcess) * 100),
    SAS_STR_Saves + ': ' + inttostr(FTileInProc) + ' ' + SAS_STR_files,
    SAS_STR_Processed + ' ' + inttostr(FTilesProcessed)
  );
end;

end.
