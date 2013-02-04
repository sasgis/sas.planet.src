unit u_ThreadGenPrevZoom;

interface

uses
  Types,
  SysUtils,
  Classes,
  GR32,
  i_NotifierOperation,
  i_RegionProcessProgressInfo,
  i_CoordConverterFactory,
  i_Bitmap32StaticFactory,
  i_VectorItemsFactory,
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
    FGenFormFirstZoom: boolean;
    FUsePrevTiles: boolean;
    FZooms: TByteDynArray;
    FMapType: TMapType;
    FResamplerFactory: IImageResamplerFactory;
    FProjectionFactory: IProjectionInfoFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorItemsFactory: IVectorItemsFactory;

    FTileInProc: integer;
    FBackGroundColor: TColor32;
  protected
    procedure ProcessRegion; override;
    procedure ProgressFormUpdateOnProgress(const AProcessed, AToProcess: Int64);
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AProjectionFactory: IProjectionInfoFactory;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AZooms: TByteDynArray;
      const APolygLL: ILonLatPolygon;
      AMapType: TMapType;
      AReplace: boolean;
      Asavefull: boolean;
      AGenFormFirstZoom: boolean;
      AUsePrevTiles: boolean;
      ABackGroundColor: TColor32;
      const AResamplerFactory: IImageResamplerFactory
    );
  end;

implementation

uses
  i_CoordConverter,
  i_Bitmap32Static,
  i_VectorItemProjected,
  i_TileIterator,
  u_GeoFun,
  u_BitmapFunc,
  u_TileIteratorByPolygon,
  u_TileIteratorByRect;

constructor TThreadGenPrevZoom.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AProjectionFactory: IProjectionInfoFactory;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AZooms: TByteDynArray;
  const APolygLL: ILonLatPolygon;
  AMapType: TMapType;
  AReplace: boolean;
  Asavefull: boolean;
  AGenFormFirstZoom: boolean;
  AUsePrevTiles: boolean;
  ABackGroundColor: TColor32;
  const AResamplerFactory: IImageResamplerFactory
);
begin
  inherited Create(
    AProgressInfo,
    APolygLL,
    AnsiString(Self.ClassName)
  );
  if Length(AZooms) <= 1 then begin
    raise Exception.Create('Не выбрано целевых масштабов');
  end;
  FProjectionFactory := AProjectionFactory;
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
  FIsReplace := AReplace;
  FIsSaveFullOnly := Asavefull;
  FGenFormFirstZoom := AGenFormFirstZoom;
  FUsePrevTiles := AUsePrevTiles;
  FZooms := AZooms;
  FTileInProc := 0;
  FMapType := AMapType;
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
  SetLength(VTileIterators, Length(FZooms) - 1);
  for i := 1 to Length(FZooms) - 1 do begin
    VZoom := FZooms[i];

    VProjectedPolygon :=
      FVectorItemsFactory.CreateProjectedPolygonByLonLatPolygon(
        FProjectionFactory.GetByConverterAndZoom(VGeoConvert, VZoom),
        PolygLL
      );
    VTileIterator := TTileIteratorByPolygon.Create(VProjectedPolygon);
    VTileIterators[i - 1] := VTileIterator;
    if FGenFormFirstZoom then begin
      VZoomDelta := FZooms[0] - VZoom;
    end else begin
      VZoomDelta := FZooms[i - 1] - VZoom;
    end;
    VTilesToProcess := VTilesToProcess + VTileIterator.TilesTotal * (1 shl (2 * VZoomDelta));
  end;
  try
    ProgressInfo.SetCaption(
      SAS_STR_ProcessedNoMore + ': ' + inttostr(VTilesToProcess) + ' ' + SAS_STR_Files
    );

    bmp_ex := TCustomBitmap32.Create;
    VResampler := FResamplerFactory.CreateResampler;
    try
      FTileInProc := 0;
      VTilesProcessed := 0;
      for i := 1 to Length(FZooms) - 1 do begin
        if FGenFormFirstZoom then begin
          VZoomPrev := FZooms[0];
        end else begin
          VZoomPrev := FZooms[i - 1];
        end;
        VZoom := FZooms[i];
        VTileIterator := VTileIterators[i - 1];
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
            if VBitmapSourceTile = nil then begin
              bmp_ex.SetSize(
                VCurrentTilePixelRect.Right - VCurrentTilePixelRect.Left,
                VCurrentTilePixelRect.Bottom - VCurrentTilePixelRect.Top
              );
              bmp_ex.Clear(FBackGroundColor);
            end else begin
              AssignStaticToBitmap32(bmp_ex, VBitmapSourceTile);
            end;

            VRelativeRect := VGeoConvert.TilePos2RelativeRect(VTile, VZoom);
            VRectOfSubTiles :=
              RectFromDoubleRect(
                VGeoConvert.RelativeRect2TileRectFloat(VRelativeRect, VZoomPrev),
                rrToTopLeft
              );
            VSubTileIterator := TTileIteratorByRect.Create(VRectOfSubTiles);
            VSubTileCount := VSubTileIterator.TilesTotal;
            VSubTilesSavedCount := 0;
            while VSubTileIterator.Next(VSubTile) do begin
              VBitmapSourceTile := FMapType.LoadTile(VSubTile, VZoomPrev, True);
              if VBitmapSourceTile <> nil then begin
                VSubTileBounds := VGeoConvert.TilePos2PixelRect(VSubTile, VZoomPrev);
                VSubTileBounds.Right := VSubTileBounds.Right - VSubTileBounds.Left;
                VSubTileBounds.Bottom := VSubTileBounds.Bottom - VSubTileBounds.Top;
                VSubTileBounds.Left := 0;
                VSubTileBounds.Top := 0;
                VRelativeRect := VGeoConvert.TilePos2RelativeRect(VSubTile, VZoomPrev);
                VSubTileInTargetBounds :=
                  RectFromDoubleRect(
                    VGeoConvert.RelativeRect2PixelRectFloat(VRelativeRect, VZoom),
                    rrToTopLeft
                  );
                VSubTileInTargetBounds.Left := VSubTileInTargetBounds.Left - VCurrentTilePixelRect.Left;
                VSubTileInTargetBounds.Top := VSubTileInTargetBounds.Top - VCurrentTilePixelRect.Top;
                VSubTileInTargetBounds.Right := VSubTileInTargetBounds.Right - VCurrentTilePixelRect.Left;
                VSubTileInTargetBounds.Bottom := VSubTileInTargetBounds.Bottom - VCurrentTilePixelRect.Top;
                StretchTransfer(
                  bmp_ex,
                  VSubTileInTargetBounds,
                  VBitmapSourceTile,
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
              VBitmap :=
                FBitmapFactory.Build(
                  Point(bmp_ex.Width, bmp_ex.Height),
                  bmp_ex.Bits
              );
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
      bmp_ex.Free;
    end;
  finally
    for i := 0 to Length(VTileIterators) - 1 do begin
      VTileIterators[i] := nil;
    end;
    VTileIterators := nil;
  end;
end;

procedure TThreadGenPrevZoom.ProgressFormUpdateOnProgress(
  const AProcessed, AToProcess: Int64
);
begin
  ProgressInfo.SetProcessedRatio(AProcessed / AToProcess);
  ProgressInfo.SetSecondLine(SAS_STR_Processed + ' ' + inttostr(AProcessed));
end;

end.
