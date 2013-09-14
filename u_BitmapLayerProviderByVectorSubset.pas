unit u_BitmapLayerProviderByVectorSubset;

interface

uses
  Types,
  SysUtils,
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_ProjectedGeometryProvider,
  i_LocalCoordConverter,
  i_NotifierOperation,
  i_VectorItemProjected,
  i_VectorItemsFactory,
  i_VectorItemSubsetChangeable,
  i_DoublePointsAggregator,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByVectorSubset = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FColorMain: TColor32;
    FColorBG: TColor32;
    FPointColor: TColor32;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorItems: IVectorItemSubsetChangeable;
    FProjectedCache: IProjectedGeometryProvider;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
    FFixedPointArray: TArrayOfFixedPoint;

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      APointColor: TColor32;
      APointColorBG: TColor32;
      const AData: IVectorDataItemPoint;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      const AData: IVectorDataItemLine;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      const AData: IVectorDataItemPoly;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawWikiElement(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      const AData: IVectorDataItemSimple;
      const ALocalConverter: ILocalCoordConverter
    ): Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProjectedCache: IProjectedGeometryProvider;
      const AVectorItems: IVectorItemSubsetChangeable
    );
  end;

implementation

uses
  GR32_Polygons,
  i_EnumDoublePoint,
  i_LonLatRect,
  u_Bitmap32ByStaticBitmap,
  u_DoublePointsAggregator,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeoFun;

{ TBitmapLayerProviderByVectorSubset }

constructor TBitmapLayerProviderByVectorSubset.Create(
  AColorMain: TColor32;
  AColorBG: TColor32;
  APointColor: TColor32;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProjectedCache: IProjectedGeometryProvider;
  const AVectorItems: IVectorItemSubsetChangeable
);
begin
  inherited Create;
  FColorMain := AColorMain;
  FColorBG := AColorBG;
  FPointColor := APointColor;
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
  FProjectedCache := AProjectedCache;
  FVectorItems := AVectorItems;

  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

function TBitmapLayerProviderByVectorSubset.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  AColorMain, AColorBG: TColor32;
  const AData: IVectorDataItemLine;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  i: integer;
  VPoint: TDoublePoint;
  VLineIndex: Integer;
  VProjected: IProjectedPath;
  VLine: IProjectedPathLine;
  VEnum: IEnumLocalPoint;
  VPolygon: TPolygon32;
  VMapRect: TDoubleRect;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VPointsProcessedCount: Integer;
  VIndex: Integer;
begin
  Result := False;
  if AData.Line.Count > 0 then begin
    VProjected := FProjectedCache.GetProjectedPath(ALocalConverter.ProjectionInfo, AData.Line);
    if VProjected.Count > 0 then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IsIntersecProjectedRect(VMapRect, VProjected.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        VPolygon := nil;
        try
          for VLineIndex := 0 to VProjected.Count - 1 do begin
            VLine := VProjected.Item[VLineIndex];
            if IsIntersecProjectedRect(VMapRect, VLine.Bounds) then begin
              FPreparedPointsAggreagtor.Clear;
              VEnum :=
                TEnumDoublePointMapPixelToLocalPixel.Create(
                  ALocalConverter,
                  VLine.GetEnum
                );
              VEnum :=
                TEnumLocalPointClipByRect.Create(
                  False,
                  VRectWithDelta,
                  VEnum
                );
              VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
              while VEnum.Next(VPoint) do begin
                FPreparedPointsAggreagtor.Add(VPoint);
              end;
              VPointsProcessedCount := FPreparedPointsAggreagtor.Count;
              if VPointsProcessedCount > 0 then begin
                if VPolygon = nil then begin
                  VPolygon := TPolygon32.Create;
                  VPolygon.Antialiased := true;
                  VPolygon.AntialiasMode := am4times;
                  VPolygon.Closed := False;
                end else begin
                  VPolygon.NewLine;
                end;
                if Length(FFixedPointArray) < VPointsProcessedCount then begin
                  SetLength(FFixedPointArray, VPointsProcessedCount);
                end;
                VIndex := 0;
                for i := 0 to VPointsProcessedCount - 1 do begin
                  VPoint := FPreparedPointsAggreagtor.Points[i];
                  if PointIsEmpty(VPoint) then begin
                    VPolygon.AddPoints(FFixedPointArray[0], VIndex);
                    VPolygon.NewLine;
                    VIndex := 0;
                  end else begin
                    FFixedPointArray[VIndex] := FixedPoint(VPoint.X, VPoint.Y);
                    Inc(VIndex);
                  end;
                end;
                VPolygon.AddPoints(FFixedPointArray[0], VIndex);
              end;
            end;
          end;
          if VPolygon <> nil then begin
            if not ABitmapInited then begin
              InitBitmap(ATargetBmp, ALocalConverter);
              ABitmapInited := True;
            end;

            with VPolygon.Outline do try
              with Grow(GR32.Fixed(0.5), 0.5) do try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, FColorBG);
              finally
                free;
              end;
            finally
              free;
            end;
            VPolygon.DrawEdge(ATargetBmp, FColorMain);

            Result := True;
          end;
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

function TBitmapLayerProviderByVectorSubset.DrawPoint(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  APointColor, APointColorBG: TColor32;
  const AData: IVectorDataItemPoint;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VConverter: ICoordConverter;
  VPointLL: TDoublePoint;
  VRect: TRect;
begin
  Result := False;
  VConverter := ALocalConverter.GetGeoConverter;
  VPointLL := AData.Point;
  VConverter.CheckLonLatPos(VPointLL);
  VRect.TopLeft := ALocalConverter.LonLat2LocalPixel(VPointLL, prToTopLeft);
  VRect.BottomRight := VRect.TopLeft;
  if Types.PtInRect(ALocalConverter.GetLocalRect, VRect.TopLeft) then begin
    if not ABitmapInited then begin
      InitBitmap(ATargetBmp, ALocalConverter);
      ABitmapInited := True;
    end;
    Dec(VRect.Left, 3);
    Dec(VRect.Top, 3);
    Inc(VRect.Right, 3);
    Inc(VRect.Bottom, 3);
    ATargetBmp.FillRectS(VRect, APointColorBG);
    Inc(VRect.Left);
    Inc(VRect.Top);
    Dec(VRect.Right);
    Dec(VRect.Bottom);
    ATargetBmp.FillRectS(VRect, APointColor);
    Result := True;
  end;
end;

function TBitmapLayerProviderByVectorSubset.DrawPoly(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  AColorMain, AColorBG: TColor32;
  const AData: IVectorDataItemPoly;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VPolygon: TPolygon32;
  i: Integer;
  VPointsProcessedCount: Integer;
  VEnum: IEnumLocalPoint;
  VRectWithDelta: TDoubleRect;
  VLocalRect: TDoubleRect;
  VPoint: TDoublePoint;
  VProjected: IProjectedPolygon;
  VMapRect: TDoubleRect;
  VLineIndex: Integer;
  VLine: IProjectedPolygonLine;
begin
  Result := False;
  VProjected := FProjectedCache.GetProjectedPolygon(ALocalConverter.ProjectionInfo, AData.Line);
  if VProjected <> nil then begin
    if VProjected.Count > 0 then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IsIntersecProjectedRect(VMapRect, VProjected.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        VPolygon := nil;
        try
          for VLineIndex := 0 to VProjected.Count - 1 do begin
            VLine := VProjected.Item[VLineIndex];
            if IsIntersecProjectedRect(VMapRect, VLine.Bounds) then begin
              FPreparedPointsAggreagtor.Clear;
              VEnum :=
                TEnumDoublePointMapPixelToLocalPixel.Create(
                  ALocalConverter,
                  VLine.GetEnum
                );
              VEnum :=
                TEnumLocalPointClipByRect.Create(
                  True,
                  VRectWithDelta,
                  VEnum
                );
              VEnum := TEnumLocalPointFilterEqual.Create(VEnum);
              VEnum := TEnumLocalPointClosePoly.Create(VEnum);
              while VEnum.Next(VPoint) do begin
                FPreparedPointsAggreagtor.Add(VPoint);
              end;
              VPointsProcessedCount := FPreparedPointsAggreagtor.Count;
              if VPointsProcessedCount > 0 then begin
                if VPolygon = nil then begin
                  VPolygon := TPolygon32.Create;
                  VPolygon.Antialiased := true;
                  VPolygon.AntialiasMode := am4times;
                  VPolygon.Closed := True;
                end else begin
                  VPolygon.NewLine;
                end;
                if Length(FFixedPointArray) < VPointsProcessedCount then begin
                  SetLength(FFixedPointArray, VPointsProcessedCount);
                end;
                for i := 0 to VPointsProcessedCount - 1 do begin
                  VPoint := FPreparedPointsAggreagtor.Points[i];
                  FFixedPointArray[i] := FixedPoint(VPoint.X, VPoint.Y);
                end;
                VPolygon.AddPoints(FFixedPointArray[0], VPointsProcessedCount);
              end;
            end;
          end;
          if VPolygon <> nil then begin
            if not ABitmapInited then begin
              InitBitmap(ATargetBmp, ALocalConverter);
              ABitmapInited := True;
            end;
            with VPolygon.Outline do try
              with Grow(GR32.Fixed(0.5), 0.5) do try
                FillMode := pfWinding;
                DrawFill(ATargetBmp, FColorBG);
              finally
                free;
              end;
            finally
              free;
            end;
            VPolygon.DrawEdge(ATargetBmp, FColorMain);
            Result := True;
          end;
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

function TBitmapLayerProviderByVectorSubset.DrawWikiElement(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  AColorMain, AColorBG, APointColor: TColor32;
  const AData: IVectorDataItemSimple;
  const ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VItemPoint: IVectorDataItemPoint;
  VItemLine: IVectorDataItemLine;
  VItemPoly: IVectorDataItemPoly;
begin
  if Supports(AData, IVectorDataItemPoint, VItemPoint) then begin
    Result := DrawPoint(ABitmapInited, ATargetBmp, APointColor, AColorBG, VItemPoint, ALocalConverter);
  end else if Supports(AData, IVectorDataItemLine, VItemLine) then begin
    Result := DrawPath(ABitmapInited, ATargetBmp, AColorMain, AColorBG, VItemLine, ALocalConverter);
  end else if Supports(AData, IVectorDataItemPoly, VItemPoly) then begin
    Result := DrawPoly(ABitmapInited, ATargetBmp, AColorMain, AColorBG, VItemPoly, ALocalConverter);
  end else begin
    Result := False;
  end;
end;

function TBitmapLayerProviderByVectorSubset.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  i: Integer;
  VItem: IVectorDataItemSimple;
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLLRect: TDoubleRect;
  VBitmapInited: Boolean;
  VBitmap: TBitmap32ByStaticBitmap;
  VIsEmpty: Boolean;
  VItems: IVectorItemSubset;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  VBitmapInited := False;
  Result := nil;
  VItems := FVectorItems.GetStatic;
  if (VItems <> nil) and (VItems.Count > 0) then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
    try
      VIsEmpty := True;
      for i := 0 to VItems.Count - 1 do begin
        VItem := VItems.GetItem(i);
        if VItem.LLRect.IsIntersecWithRect(VLLRect) then begin
          if DrawWikiElement(VBitmapInited, VBitmap, FColorMain, FColorBG, FPointColor, VItem, ALocalConverter) then begin
            VIsEmpty := False;
          end;
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            Break;
          end;
        end;
      end;
      if not VIsEmpty then begin
        Result := VBitmap.BitmapStatic;
      end;
    finally
      VBitmap.Free;
    end;
  end;
end;

procedure TBitmapLayerProviderByVectorSubset.InitBitmap(
  ATargetBmp: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter
);
var
  VSize: TPoint;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  ATargetBmp.SetSize(VSize.X, VSize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

end.
