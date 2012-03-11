unit u_BitmapLayerProviderByVectorSubset;

interface

uses
  Classes,
  Types,
  GR32,
  t_GeoTypes,
  i_CoordConverter,
  i_ProjectionInfo,
  i_VectorDataItemSimple,
  i_IdCacheSimple,
  i_LocalCoordConverter,
  i_OperationNotifier,
  i_VectorItemProjected,
  i_VectorItmesFactory,
  i_DoublePointsAggregator,
  i_BitmapLayerProvider;

type
  TBitmapLayerProviderByVectorSubset = class(TInterfacedObject, IBitmapLayerProvider)
  private
    FColorMain: TColor32;
    FColorBG: TColor32;
    FPointColor: TColor32;
    FVectorItmesFactory: IVectorItmesFactory;
    FVectorItems: IInterfaceList;
    FProjectionInfo: IProjectionInfo;
    FProjectedCache: IIdCacheSimple;
    FLinesClipRect: TDoubleRect;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
    FFixedPointArray: TArrayOfFixedPoint;

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    );
    function GetProjectedPath(
      AData: IVectorDataItemLine;
      AProjectionInfo: IProjectionInfo;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function GetProjectedPolygon(
      AData: IVectorDataItemPoly;
      AProjectionInfo: IProjectionInfo;
      ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

    function DrawPoint(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      APointColor: TColor32;
      APointColorBG: TColor32;
      AData: IVectorDataItemPoint;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPath(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      AData: IVectorDataItemLine;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawPoly(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      AData: IVectorDataItemPoly;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
    function DrawWikiElement(
      var ABitmapInited: Boolean;
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      AData: IVectorDataItemSimple;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter
    ): Boolean;
  public
    constructor Create(
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      AVectorItmesFactory: IVectorItmesFactory;
      AProjectionInfo: IProjectionInfo;
      AProjectedCache: IIdCacheSimple;
      ALinesClipRect: TDoubleRect;
      AVectorItems: IInterfaceList
    );
  end;

implementation

uses
  SysUtils,
  GR32_Polygons,
  i_EnumDoublePoint,
  u_DoublePointsAggregator,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_GeoFun;

{ TBitmapLayerProviderByVectorSubset }

constructor TBitmapLayerProviderByVectorSubset.Create(
  AColorMain, AColorBG, APointColor: TColor32;
  AVectorItmesFactory: IVectorItmesFactory;
  AProjectionInfo: IProjectionInfo;
  AProjectedCache: IIdCacheSimple;
  ALinesClipRect: TDoubleRect;
  AVectorItems: IInterfaceList
);
begin
  FColorMain := AColorMain;
  FColorBG := AColorBG;
  FPointColor := APointColor;
  FVectorItmesFactory := AVectorItmesFactory;
  FProjectionInfo := AProjectionInfo;
  FProjectedCache := AProjectedCache;
  FLinesClipRect := ALinesClipRect;
  FVectorItems := AVectorItems;

  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

function TBitmapLayerProviderByVectorSubset.DrawPath(
  var ABitmapInited: Boolean;
  ATargetBmp: TCustomBitmap32;
  AColorMain, AColorBG: TColor32;
  AData: IVectorDataItemLine;
  ALocalConverter: ILocalCoordConverter
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
  VIntersectRect: TDoubleRect;
  VLocalRect: TDoubleRect;
  VRectWithDelta: TDoubleRect;
  VPointsProcessedCount: Integer;
  VIndex: Integer;
begin
  Result := False;
  if AData.Line.Count > 0 then begin
    VProjected := GetProjectedPath(AData, ALocalConverter.ProjectionInfo, FPreparedPointsAggreagtor);
    if VProjected.Count > 0 then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IntersecProjectedRect(VIntersectRect, VMapRect, VProjected.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        VPolygon := nil;
        try
          for VLineIndex := 0 to VProjected.Count - 1 do begin
            VLine := VProjected.Item[VLineIndex];
            if IntersecProjectedRect(VIntersectRect, VMapRect, VLine.Bounds) then begin
              FPreparedPointsAggreagtor.Clear;
              VEnum :=
                TEnumLocalPointFilterEqual.Create(
                  TEnumLocalPointClipByRect.Create(
                    False,
                    VRectWithDelta,
                    TEnumDoublePointMapPixelToLocalPixel.Create(
                      ALocalConverter,
                      VLine.GetEnum
                    )
                  )
                );
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
  AData: IVectorDataItemPoint;
  ALocalConverter: ILocalCoordConverter
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
  VRect.TopLeft := ALocalConverter.LonLat2LocalPixel(VPointLL);
  VRect.BottomRight := VRect.TopLeft;
  if PtInRect(ALocalConverter.GetLocalRect, VRect.TopLeft) then begin
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
  AData: IVectorDataItemPoly;
  ALocalConverter: ILocalCoordConverter
): Boolean;
var
  VPolygon: TPolygon32;
  i: Integer;
  VPointsProcessedCount: Integer;
  VEnum: IEnumDoublePoint;
  VRectWithDelta: TDoubleRect;
  VLocalRect: TDoubleRect;
  VPoint: TDoublePoint;
  VProjected: IProjectedPolygon;
  VIntersectRect: TDoubleRect;
  VMapRect: TDoubleRect;
  VLineIndex: Integer;
  VLine: IProjectedPolygonLine;
begin
  Result := False;
  VProjected := GetProjectedPolygon(AData, FProjectionInfo);
  if VProjected <> nil then begin
    if VProjected.Count > 0 then begin
      VMapRect := ALocalConverter.GetRectInMapPixelFloat;
      if IntersecProjectedRect(VIntersectRect, VMapRect, VProjected.Bounds) then begin
        VLocalRect := DoubleRect(ALocalConverter.GetLocalRect);
        VRectWithDelta.Left := VLocalRect.Left - 10;
        VRectWithDelta.Top := VLocalRect.Top - 10;
        VRectWithDelta.Right := VLocalRect.Right + 10;
        VRectWithDelta.Bottom := VLocalRect.Bottom + 10;
        VPolygon := nil;
        try
          for VLineIndex := 0 to VProjected.Count - 1 do begin
            VLine := VProjected.Item[VLineIndex];
            if IntersecProjectedRect(VIntersectRect, VMapRect, VLine.Bounds) then begin
              FPreparedPointsAggreagtor.Clear;
              VEnum :=
                TEnumDoublePointClosePoly.Create(
                  TEnumLocalPointFilterEqual.Create(
                    TEnumLocalPointClipByRect.Create(
                      True,
                      VRectWithDelta,
                      TEnumDoublePointMapPixelToLocalPixel.Create(
                        ALocalConverter,
                        VLine.GetEnum
                      )
                    )
                  )
                );
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
  AData: IVectorDataItemSimple;
  ALocalConverter: ILocalCoordConverter
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

function TBitmapLayerProviderByVectorSubset.GetBitmapRect(AOperationID: Integer;
  ACancelNotifier: IOperationNotifier; ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter): Boolean;
var
  i: Integer;
  VItem: IVectorDataItemSimple;
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLLRect: TDoubleRect;
  VMarkLonLatRect: TDoubleRect;
  VBitmapInited: Boolean;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  VBitmapInited := False;
  Result := False;
  for i := 0 to FVectorItems.Count - 1 do begin
    VItem := IVectorDataItemSimple(FVectorItems[i]);
    VMarkLonLatRect := VItem.LLRect;
    if(
      (VLLRect.Right >= VMarkLonLatRect.Left)and
      (VLLRect.Left <= VMarkLonLatRect.Right)and
      (VLLRect.Bottom <= VMarkLonLatRect.Top)and
      (VLLRect.Top >= VMarkLonLatRect.Bottom))
    then begin
      if DrawWikiElement(VBitmapInited, ATargetBmp, FColorMain, FColorBG, FPointColor, VItem, ALocalConverter) then begin
        Result := True;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Break;
      end;
    end;
  end;
end;

function TBitmapLayerProviderByVectorSubset.GetProjectedPath(
  AData: IVectorDataItemLine;
  AProjectionInfo: IProjectionInfo;
  ATemp: IDoublePointsAggregator
): IProjectedPath;
var
  VID: Integer;
begin
  VID := Integer(AData);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItmesFactory.CreateProjectedPathWithClipByLonLatPath(
        AProjectionInfo,
        AData.Line,
        FLinesClipRect,
        ATemp
      );
    FProjectedCache.Add(VID, Result);
  end;
end;

function TBitmapLayerProviderByVectorSubset.GetProjectedPolygon(
  AData: IVectorDataItemPoly;
  AProjectionInfo: IProjectionInfo;
  ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VID: Integer;
begin
  VID := Integer(AData);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItmesFactory.CreateProjectedPolygonWithClipByLonLatPolygon(
        AProjectionInfo,
        AData.Line,
        FLinesClipRect,
        ATemp
      );
    FProjectedCache.Add(VID, Result);
  end;
end;

procedure TBitmapLayerProviderByVectorSubset.InitBitmap(
  ATargetBmp: TCustomBitmap32; ALocalConverter: ILocalCoordConverter);
var
  VSize: TPoint;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  ATargetBmp.SetSize(VSize.X, VSize.Y);
  ATargetBmp.Clear(0);
end;

end.
