unit u_BitmapLayerProviderByVectorSubset;

interface

uses
  Types,
  SysUtils,
  GR32,
  t_GeoTypes,
  i_Listener,
  i_MapTypes,
  i_CoordConverter,
  i_ProjectionInfo,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_VectorDataItemSimple,
  i_VectorItemSubset,
  i_IdCacheSimple,
  i_LocalCoordConverter,
  i_NotifierOperation,
  i_BitmapLayerProviderWithListener,
  i_VectorItemProjected,
  i_VectorItemsFactory,
  i_DoublePointsAggregator,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByVectorSubset = class(TBaseInterfacedObject, IBitmapLayerProvider, IObjectWithListener)
  private
    FVectorMapsSet: IMapTypeSet;
    FColorMain: TColor32;
    FColorBG: TColor32;
    FPointColor: TColor32;
    FVectorItemsFactory: IVectorItemsFactory;
    FBitmapFactory: IBitmap32StaticFactory;
    FVectorItems: IVectorItemSubset;
    FProjectionInfo: IProjectionInfo;
    FProjectedCache: IIdCacheSimple;
    FLinesClipRect: TDoubleRect;

    FPreparedPointsAggreagtor: IDoublePointsAggregator;
    FFixedPointArray: TArrayOfFixedPoint;

    FListener: IListener;
    FListenLocalConverter: ILocalCoordConverter;
    FListenLonLatRect: TDoubleRect;
    FListenerCS: IReadWriteSync;

    FLayerListeners: array of IListenerDisconnectable;
    FVersionListener: IListener;

    procedure OnMapVersionChange;
    procedure OnTileUpdate(const AMsg: IInterface);

    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
    function GetProjectedPath(
      const AData: IVectorDataItemLine;
      const AProjectionInfo: IProjectionInfo;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPath;
    function GetProjectedPolygon(
      const AData: IVectorDataItemPoly;
      const AProjectionInfo: IProjectionInfo;
      const ATemp: IDoublePointsAggregator = nil
    ): IProjectedPolygon;

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
  private
    procedure SetListener(
      const AListener: IListener;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure RemoveListener;
  public
    constructor Create(
      const AVectorMapsSet: IMapTypeSet;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      const AVectorItemsFactory: IVectorItemsFactory;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProjectionInfo: IProjectionInfo;
      const AProjectedCache: IIdCacheSimple;
      const ALinesClipRect: TDoubleRect;
      const AVectorItems: IVectorItemSubset
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  GR32_Polygons,
  i_EnumDoublePoint,
  i_LonLatRect,
  i_NotifierTilePyramidUpdate,
  u_Bitmap32ByStaticBitmap,
  u_DoublePointsAggregator,
  u_EnumDoublePointClosePoly,
  u_EnumDoublePointMapPixelToLocalPixel,
  u_EnumDoublePointWithClip,
  u_EnumDoublePointFilterEqual,
  u_TileUpdateListenerToLonLat,
  u_ListenerByEvent,
  u_Synchronizer,
  u_GeoFun;

{ TBitmapLayerProviderByVectorSubset }

constructor TBitmapLayerProviderByVectorSubset.Create(
  const AVectorMapsSet: IMapTypeSet;
  AColorMain, AColorBG, APointColor: TColor32;
  const AVectorItemsFactory: IVectorItemsFactory;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProjectionInfo: IProjectionInfo;
  const AProjectedCache: IIdCacheSimple;
  const ALinesClipRect: TDoubleRect;
  const AVectorItems: IVectorItemSubset
);
begin
  inherited Create;
  FVectorMapsSet := AVectorMapsSet;
  FColorMain := AColorMain;
  FColorBG := AColorBG;
  FPointColor := APointColor;
  FVectorItemsFactory := AVectorItemsFactory;
  FBitmapFactory := ABitmapFactory;
  FProjectionInfo := AProjectionInfo;
  FProjectedCache := AProjectedCache;
  FLinesClipRect := ALinesClipRect;
  FVectorItems := AVectorItems;

  FListenerCS := MakeSyncRW_Var(Self, False);
  FPreparedPointsAggreagtor := TDoublePointsAggregator.Create;
end;

destructor TBitmapLayerProviderByVectorSubset.Destroy;
var
  i: Integer;
  VCnt: Integer;
begin
  if Assigned(FListenerCS) then begin
    RemoveListener;
    FListenerCS.BeginWrite;
    try
      VCnt := Length(FLayerListeners);
      for i := 0 to VCnt - 1 do begin
        if Assigned(FLayerListeners[i]) then begin
          FLayerListeners[i].Disconnect;
        end;
      end;
    finally
      FListenerCS.EndWrite;
    end;
  end;
  inherited;
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
    VProjected := GetProjectedPath(AData, ALocalConverter.ProjectionInfo, FPreparedPointsAggreagtor);
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
  VProjected := GetProjectedPolygon(AData, FProjectionInfo);
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
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  VBitmapInited := False;
  Result := nil;
  if FVectorItems.Count > 0 then begin
    VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
    try
      VIsEmpty := True;
      for i := 0 to FVectorItems.Count - 1 do begin
        VItem := FVectorItems.GetItem(i);
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

function TBitmapLayerProviderByVectorSubset.GetProjectedPath(
  const AData: IVectorDataItemLine;
  const AProjectionInfo: IProjectionInfo;
  const ATemp: IDoublePointsAggregator
): IProjectedPath;
var
  VID: Integer;
begin
  VID := Integer(AData);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItemsFactory.CreateProjectedPathWithClipByLonLatPath(
        AProjectionInfo,
        AData.Line,
        FLinesClipRect,
        ATemp
      );
    FProjectedCache.Add(VID, Result);
  end;
end;

function TBitmapLayerProviderByVectorSubset.GetProjectedPolygon(
  const AData: IVectorDataItemPoly;
  const AProjectionInfo: IProjectionInfo;
  const ATemp: IDoublePointsAggregator
): IProjectedPolygon;
var
  VID: Integer;
begin
  VID := Integer(AData);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItemsFactory.CreateProjectedPolygonWithClipByLonLatPolygon(
        AProjectionInfo,
        AData.Line,
        FLinesClipRect,
        ATemp
      );
    FProjectedCache.Add(VID, Result);
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

procedure TBitmapLayerProviderByVectorSubset.OnMapVersionChange;
var
  VListener: IListener;
begin
  FListenerCS.BeginRead;
  try
    VListener := FListener;
  finally
    FListenerCS.EndRead;
  end;
  if VListener <> nil then begin
    VListener.Notification(nil);
  end;
end;

procedure TBitmapLayerProviderByVectorSubset.OnTileUpdate(
  const AMsg: IInterface);
var
  VListener: IListener;
  VLonLatRect: ILonLatRect;
  VListenLonLatRect: TDoubleRect;
begin
  FListenerCS.BeginRead;
  try
    VListener := FListener;
    VListenLonLatRect := FListenLonLatRect;
  finally
    FListenerCS.EndRead;
  end;
  if VListener <> nil then begin
    if Supports(AMsg, ILonLatRect, VLonLatRect) then begin
      if VLonLatRect.IsIntersecWithRect(VListenLonLatRect) then begin
        VListener.Notification(nil);
      end;
    end else begin
      VListener.Notification(nil);
    end;
  end;
end;

procedure TBitmapLayerProviderByVectorSubset.RemoveListener;
var
  VNotifier: INotifierTilePyramidUpdate;
  i: Integer;
  VMap: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
begin
  if FVectorMapsSet <> nil then begin
    FListenerCS.BeginWrite;
    try
      if (FListener <> nil) and (FListenLocalConverter <> nil) then begin
        VEnum := FVectorMapsSet.GetIterator;
        i := 0;
        while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
          VMap := FVectorMapsSet.GetMapTypeByGUID(VGUID);
          if VMap <> nil then begin
            VNotifier := VMap.MapType.TileNotifier;
            if VNotifier <> nil then begin
              VNotifier.Remove(FLayerListeners[i]);
            end;
            VMap.MapType.VersionConfig.ChangeNotifier.Remove(FVersionListener);
          end;
          Inc(i);
        end;
      end;
      FListener := nil;
      FListenLocalConverter := nil;
    finally
      FListenerCS.EndWrite;
    end;
  end;
end;

procedure TBitmapLayerProviderByVectorSubset.SetListener(
  const AListener: IListener; const ALocalConverter: ILocalCoordConverter);
var
  VNotifier: INotifierTilePyramidUpdate;
  i: Integer;
  VMap: IMapType;
  VZoom: Byte;
  VTileRect: TRect;
  VLonLatRect: TDoubleRect;
  VMapRect: TRect;
  VConverter: ICoordConverter;
  VMapLonLatRect: TDoubleRect;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
begin
  if FVectorMapsSet <> nil then begin
    FListenerCS.BeginWrite;
    try
      if (AListener = nil) or (ALocalConverter = nil) then begin
        RemoveListener;
      end else begin
        if (FListener <> nil) and (FListenLocalConverter <> nil) then begin
          VZoom := FListenLocalConverter.Zoom;
          if (VZoom <> ALocalConverter.Zoom) then begin
            VEnum := FVectorMapsSet.GetIterator;
            i := 0;
            while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
              VMap := FVectorMapsSet.GetMapTypeByGUID(VGUID);
              if VMap <> nil then begin
                VNotifier := VMap.MapType.TileNotifier;
                if VNotifier <> nil then begin
                  VNotifier.Remove(FLayerListeners[i]);
                end;
              end;
              Inc(i);
            end;
          end;
        end;
        if Length(FLayerListeners) = 0 then begin
          SetLength(FLayerListeners, FVectorMapsSet.GetCount);
          VEnum := FVectorMapsSet.GetIterator;
          i := 0;
          while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
            VMap := FVectorMapsSet.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              FLayerListeners[i] := TTileUpdateListenerToLonLat.Create(VMap.MapType.GeoConvert, Self.OnTileUpdate);
            end;
            Inc(i);
          end;
        end;
        if FVersionListener = nil then begin
          FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);
        end;
        if (FListener = nil) or (FListenLocalConverter = nil) then begin
          VEnum := FVectorMapsSet.GetIterator;
          while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
            VMap := FVectorMapsSet.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              VMap.MapType.VersionConfig.ChangeNotifier.Add(FVersionListener);
            end;
          end;
        end;
        if not ALocalConverter.GetIsSameConverter(FListenLocalConverter) then begin
          VZoom := ALocalConverter.Zoom;
          VConverter := ALocalConverter.GeoConverter;
          VMapRect := ALocalConverter.GetRectInMapPixel;
          VConverter.CheckPixelRect(VMapRect, VZoom);
          VLonLatRect := VConverter.PixelRect2LonLatRect(VMapRect, VZoom);
          FListenLonLatRect := VLonLatRect;
          VEnum := FVectorMapsSet.GetIterator;
          i := 0;
          while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
            VMap := FVectorMapsSet.GetMapTypeByGUID(VGUID);
            if VMap <> nil then begin
              VNotifier := VMap.MapType.TileNotifier;
              if VNotifier <> nil then begin
                VConverter := VMap.MapType.GeoConvert;
                VMapLonLatRect := VLonLatRect;
                VConverter.CheckLonLatRect(VMapLonLatRect);
                VTileRect :=
                  RectFromDoubleRect(
                    VConverter.LonLatRect2TileRectFloat(VMapLonLatRect, VZoom),
                    rrToTopLeft
                  );
                VNotifier.AddListenerByRect(FLayerListeners[i], VZoom, VTileRect);
              end;
            end;
            Inc(i);
          end;
        end;
        FListener := AListener;
        FListenLocalConverter := ALocalConverter;
      end;
    finally
      FListenerCS.EndWrite;
    end;
  end;
end;

end.
