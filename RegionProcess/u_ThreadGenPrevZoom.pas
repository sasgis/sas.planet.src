unit u_ThreadGenPrevZoom;

interface

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  GR32,
  u_MapType,
  u_ThreadRegionProcessAbstract,
  i_ImageResamplerFactory,
  u_ResStrings,
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
    FResamplerFactory: IImageResamplerFactory;

    FTileInProc: integer;
    FBackGroundColor: TColor32;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress;
  public
    constructor Create(
      Azoom: byte;
      AInZooms: TArrayOfByte;
      APolygLL: TArrayOfDoublePoint;
      Atypemap: TMapType;
      AReplace: boolean;
      Asavefull: boolean;
      AGenFormPrev: boolean;
      ABackGroundColor: TColor32;
      AResamplerFactory: IImageResamplerFactory
    );
  end;

implementation

uses
  i_CoordConverter,
  i_TileIterator,
  u_TileIteratorStuped,
  u_TileIteratorByRect;

constructor TThreadGenPrevZoom.Create(
  Azoom: byte;
  AInZooms: TArrayOfByte;
  APolygLL: TArrayOfDoublePoint;
  Atypemap: TMapType;
  AReplace: boolean;
  Asavefull: boolean;
  AGenFormPrev: boolean;
  ABackGroundColor: TColor32;
  AResamplerFactory: IImageResamplerFactory
);
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
  FResamplerFactory := AResamplerFactory;
  FBackGroundColor := ABackGroundColor;
end;

procedure TThreadGenPrevZoom.ProcessRegion;
var
  bmp_ex: TCustomBitmap32;
  bmp: TCustomBitmap32;
  i, VSubTileCount: integer;
  VSubTilesSavedCount: integer;
  VZoomPrev: Byte;
  VZoom: Byte;
  VTile: TPoint;
  VSubTile: TPoint;
  VGeoConvert: ICoordConverter;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VZoomDelta: Integer;
  VRectOfSubTiles: TRect;
  VCurrentTilePixelRect: TRect;
  VRelativeRect: TDoubleRect;
  VSubTileBounds: TRect;
  VSubTileInTargetBounds: TRect;
  VSubTileIterator: ITileIterator;
begin
  inherited;
  FTilesToProcess := 0;
  VGeoConvert := FMapType.GeoConvert;
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];
    VTileIterators[i] := TTileIteratorStuped.Create(VZoom, FPolygLL, VGeoConvert);
    if (not FGenFormPrevZoom) or (i = 0) then begin
      VZoomDelta := FSourceZoom - FZooms[i];
    end else begin
      VZoomDelta := FZooms[i - 1] - FZooms[i];
    end;
    FTilesToProcess := FTilesToProcess + VTileIterators[i].TilesTotal * (1 shl (2*VZoomDelta));
  end;
  try
    ProgressFormUpdateCaption(
      '',
      SAS_STR_ProcessedNoMore + ': ' + inttostr(FTilesToProcess) + ' ' + SAS_STR_files
      );

    bmp_ex := TCustomBitmap32.Create;
    bmp := TCustomBitmap32.Create;
    try
      bmp.Resampler := FResamplerFactory.CreateResampler;

      FTileInProc := 0;
      FTilesProcessed := 0;
      for i := 0 to length(FZooms) - 1 do begin
        if (not FGenFormPrevZoom) or (i = 0) then begin
          VZoomPrev := FSourceZoom;
        end else begin
          VZoomPrev := FZooms[i - 1];
        end;
        VZoom := FZooms[i];
        VTileIterator := VTileIterators[i];
        while VTileIterator.Next(VTile) do begin
          if CancelNotifier.IsOperationCanceled(OperationID) then begin
            exit;
          end;
          VCurrentTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);
          if FMapType.TileExists(VTile, VZoom) then begin
            if not (FIsReplace) then begin
              continue;
            end;
            if not FMapType.LoadTile(bmp_Ex, VTile, VZoom, True) then begin
              bmp_ex.SetSize(
                VCurrentTilePixelRect.Right - VCurrentTilePixelRect.Left,
                VCurrentTilePixelRect.Bottom - VCurrentTilePixelRect.Top
              );
              bmp_ex.Clear(FBackGroundColor);
            end;
          end else begin
            bmp_ex.SetSize(
              VCurrentTilePixelRect.Right - VCurrentTilePixelRect.Left,
              VCurrentTilePixelRect.Bottom - VCurrentTilePixelRect.Top
            );
            bmp_ex.Clear(FBackGroundColor);
          end;
          VRelativeRect := VGeoConvert.TilePos2RelativeRect(VTile, VZoom);
          VRectOfSubTiles := VGeoConvert.RelativeRect2TileRect(VRelativeRect, VZoomPrev);
          VSubTileIterator := TTileIteratorByRect.Create(VRectOfSubTiles);
          try
            VSubTileCount := VSubTileIterator.TilesTotal;
            VSubTilesSavedCount := 0;
            while VSubTileIterator.Next(VSubTile) do begin
              if FMapType.LoadTile(bmp, VSubTile, VZoomPrev, True) then begin
                VSubTileBounds := VGeoConvert.TilePos2PixelRect(VSubTile, VZoomPrev);
                VSubTileBounds.Right := VSubTileBounds.Right - VSubTileBounds.Left;
                VSubTileBounds.Bottom := VSubTileBounds.Bottom - VSubTileBounds.Top;
                VSubTileBounds.Left := 0;
                VSubTileBounds.Top := 0;
                VRelativeRect := VGeoConvert.TilePos2RelativeRect(VSubTile, VZoomPrev);
                VSubTileInTargetBounds := VGeoConvert.RelativeRect2PixelRect(VRelativeRect, VZoom);
                VSubTileInTargetBounds.Left := VSubTileInTargetBounds.Left - VCurrentTilePixelRect.Left;
                VSubTileInTargetBounds.Top := VSubTileInTargetBounds.Top - VCurrentTilePixelRect.Top;
                VSubTileInTargetBounds.Right := VSubTileInTargetBounds.Right - VCurrentTilePixelRect.Left;
                VSubTileInTargetBounds.Bottom := VSubTileInTargetBounds.Bottom - VCurrentTilePixelRect.Top;
                bmp.DrawTo(bmp_ex, VSubTileInTargetBounds, VSubTileBounds);
                inc(VSubTilesSavedCount);
              end;
              inc(FTilesProcessed);
              if (FTilesProcessed mod 30 = 0) then begin
                ProgressFormUpdateOnProgress;
              end;
            end;
          finally
            VSubTileIterator := nil;
          end;
          if ((not FIsSaveFullOnly) or (VSubTilesSavedCount = VSubTileCount)) and (VSubTilesSavedCount > 0) then begin
            FMapType.SaveTileSimple(VTile, VZoom, bmp_ex);
            inc(FTileInProc);
          end;
        end;
      end;
    finally
      bmp_ex.Free;
      bmp.Free;
    end;
    FMapType.CacheBitmap.Clear;
  finally
    for i := 0 to Length(VTileIterators) - 1 do begin
      VTileIterators[i] := nil;
    end;
    VTileIterators := nil;
  end;
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
