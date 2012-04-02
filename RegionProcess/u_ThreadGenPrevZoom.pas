unit u_ThreadGenPrevZoom;

interface

uses
  SysUtils,
  Classes,
  GR32,
  i_OperationNotifier,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_VectorItmesFactory,
  i_VectorItemLonLat,
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
    FUsePrevTiles: boolean;
    FSourceZoom: byte;
    FZooms: TArrayOfByte;
    FMapType: TMapType;
    FResamplerFactory: IImageResamplerFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FVectorItmesFactory: IVectorItmesFactory;

    FTileInProc: integer;
    FBackGroundColor: TColor32;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      AProgressInfo: IRegionProcessProgressInfo;
      AProjectionFactory: IProjectionInfoFactory;
      AVectorItmesFactory: IVectorItmesFactory;
      Azoom: byte;
      AInZooms: TArrayOfByte;
      APolygLL: ILonLatPolygon;
      Atypemap: TMapType;
      AReplace: boolean;
      Asavefull: boolean;
      AGenFormPrev: boolean;
      AUsePrevTiles: boolean;
      ABackGroundColor: TColor32;
      AResamplerFactory: IImageResamplerFactory
    );
  end;

implementation

uses
  GR32_Resamplers,
  i_CoordConverter,
  i_Bitmap32Static,
  i_VectorItemProjected,
  i_TileIterator,
  u_Bitmap32Static,
  u_TileIteratorByPolygon,
  u_TileIteratorByRect;

constructor TThreadGenPrevZoom.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  AProgressInfo: IRegionProcessProgressInfo;
  AProjectionFactory: IProjectionInfoFactory;
  AVectorItmesFactory: IVectorItmesFactory;
  Azoom: byte;
  AInZooms: TArrayOfByte;
  APolygLL: ILonLatPolygon;
  Atypemap: TMapType;
  AReplace: boolean;
  Asavefull: boolean;
  AGenFormPrev: boolean;
  AUsePrevTiles: boolean;
  ABackGroundColor: TColor32;
  AResamplerFactory: IImageResamplerFactory
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    AProgressInfo,
    APolygLL
  );
  FProjectionFactory := AProjectionFactory;
  FVectorItmesFactory := AVectorItmesFactory;
  FIsReplace := AReplace;
  FIsSaveFullOnly := Asavefull;
  FGenFormPrevZoom := AGenFormPrev;
  FUsePrevTiles:=AUsePrevTiles;
  FZooms := AInZooms;
  FTileInProc := 0;
  FSourceZoom := Azoom;
  FMapType := Atypemap;
  FResamplerFactory := AResamplerFactory;
  FBackGroundColor := ABackGroundColor;
end;

procedure TThreadGenPrevZoom.ProcessRegion;
var
  bmp_ex: TCustomBitmap32;
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
  VProjectedPolygon: IProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VResampler: TCustomResampler;
  VBitmapSourceTile: IBitmap32Static;
  VBitmap: IBitmap32Static;
begin
  inherited;
  VTilesToProcess := 0;
  VGeoConvert := FMapType.GeoConvert;
  SetLength(VTileIterators, Length(FZooms));
  for i := 0 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];

    VProjectedPolygon :=
      FVectorItmesFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(
          VGeoConvert,
          VZoom
        ),
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjectedPolygon);
    if (not FGenFormPrevZoom) or (i = 0) then begin
      VZoomDelta := FSourceZoom - FZooms[i];
    end else begin
      VZoomDelta := FZooms[i - 1] - FZooms[i];
    end;
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal * (1 shl (2*VZoomDelta));
  end;
  try
    ProgressInfo.Caption :=
      SAS_STR_ProcessedNoMore + ': ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_files;

      VResampler := FResamplerFactory.CreateResampler;
      try
        FTileInProc := 0;
        VTilesProcessed := 0;
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
            end;
            VBitmapSourceTile := nil;
            if FUsePrevTiles then begin
              VBitmapSourceTile := FMapType.LoadTileUni(VTile, VZoom, VGeoConvert, True, True, True);
            end;
            bmp_ex := TCustomBitmap32.Create;
            try
              if VBitmapSourceTile = nil then begin
                bmp_ex.SetSize(
                  VCurrentTilePixelRect.Right - VCurrentTilePixelRect.Left,
                  VCurrentTilePixelRect.Bottom - VCurrentTilePixelRect.Top
                );
                bmp_ex.Clear(FBackGroundColor);
              end else begin
                bmp_ex.Assign(VBitmapSourceTile.Bitmap);
              end;

              VRelativeRect := VGeoConvert.TilePos2RelativeRect(VTile, VZoom);
              VRectOfSubTiles := VGeoConvert.RelativeRect2TileRect(VRelativeRect, VZoomPrev);
              VSubTileIterator := TTileIteratorByRect.Create(VRectOfSubTiles);
              VSubTileCount := VSubTileIterator.TilesTotal;
              VSubTilesSavedCount := 0;
              while VSubTileIterator.Next(VSubTile) do begin
                VBitmapSourceTile := FMapType.LoadTile(VSubTile, VZoomPrev, True);
                if VBitmapSourceTile <> nil  then begin
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
                  StretchTransfer(
                    bmp_ex,
                    VSubTileInTargetBounds,
                    bmp_ex.ClipRect,
                    VBitmapSourceTile.Bitmap,
                    VSubTileBounds,
                    VResampler,
                    dmOpaque
                  );
                  inc(VSubTilesSavedCount);
                end else begin
                  if FIsSaveFullOnly then begin
                    Break;
                  end;
                end;
                inc(VTilesProcessed);
                if (VTilesProcessed mod 30 = 0) then begin
                  ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
                end;
              end;
              VBitmap := nil;
              if ((not FIsSaveFullOnly) or (VSubTilesSavedCount = VSubTileCount)) and (VSubTilesSavedCount > 0) then begin
                VBitmap := TBitmap32Static.CreateWithOwn(bmp_ex);
                bmp_ex := nil;
              end;
            finally
              bmp_ex.Free;
            end;
            if VBitmap <> nil then begin
              FMapType.SaveTileSimple(VTile, VZoom, VBitmap);
              inc(FTileInProc);
              VBitmap := nil;
            end;
          end;
        end;
      finally
        VResampler.Free;
      end;
  finally
    for i := 0 to Length(VTileIterators) - 1 do begin
      VTileIterators[i] := nil;
    end;
    VTileIterators := nil;
  end;
end;

procedure TThreadGenPrevZoom.ProgressFormUpdateOnProgress(AProcessed, AToProcess: Int64);
begin
  ProgressInfo.Processed := AProcessed/AToProcess;
  ProgressInfo.SecondLine := SAS_STR_Processed + ' ' + inttostr(AProcessed)
end;

end.
