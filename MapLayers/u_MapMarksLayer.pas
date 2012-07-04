{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapMarksLayer;

interface

uses
  SysUtils,
  GR32,
  GR32_Image,
  i_Notifier,
  t_GeoTypes,
  i_NotifierOperation,
  i_ImageResamplerConfig,
  i_LayerBitmapClearStrategy,
  i_UsedMarksConfig,
  i_MarksDrawConfig,
  i_MarksLayerConfig,
  i_MarksSimple,
  i_ViewPortState,
  i_ProjectionInfo,
  i_CoordConverter,
  i_VectorItmesFactory,
  i_VectorItemProjected,
  i_IdCacheSimple,
  i_DoublePointsAggregator,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_InternalPerformanceCounter,
  u_MarksSystem,
  u_MapLayerWithThreadDraw;

type
  TMapMarksLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IMarksLayerConfig;
    FConfigStatic: IUsedMarksConfigStatic;
    FDrawConfigStatic: IMarksDrawConfigStatic;
    FVectorItmesFactory: IVectorItmesFactory;
    FMarkDB: TMarksSystem;

    FGetMarksCounter: IInternalPerformanceCounter;
    FMouseOnRegCounter: IInternalPerformanceCounter;
    FProjectedCache: IIdCacheSimple;
    FPointsAgregator: IDoublePointsAggregator;

    FMarksSubset: IMarksSubset;
    FMarksSubsetCS: IReadWriteSync;
    FLinesClipRect: TDoubleRect;

    function GetProjectedPath(
      const AMarkPath: IMarkLine;
      const AProjectionInfo: IProjectionInfo
    ): IProjectedPath;
    function GetProjectedPolygon(
      const AMarkPoly: IMarkPoly;
      const AProjectionInfo: IProjectionInfo
    ): IProjectedPolygon;

    procedure OnConfigChange;
    procedure OnMarksDbChange;
    function GetMarksSubset(const ALocalConverter: ILocalCoordConverter): IMarksSubset;
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const AVectorItmesFactory: IVectorItmesFactory;
      const AResamplerConfig: IImageResamplerConfig;
      const AConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const ATimerNoifier: INotifier;
      const AConfig: IMarksLayerConfig;
      AMarkDB: TMarksSystem
    );

    function MouseOnReg(
      const AVisualConverter: ILocalCoordConverter;
      const xy: TPoint;
      out AMarkS: Double
    ): IMark; overload;
    function MouseOnReg(
      const AVisualConverter: ILocalCoordConverter;
      const xy: TPoint
    ): IMark; overload;

    function GetIntersection(
      const ACurrLonLat: TDoublePoint;
      var AIntersectionLonLat: TDoublePoint;
      const AMarkPoly: IMarkPoly;
      const AProjection: IProjectionInfo
    ): boolean; overload;
    function GetIntersection(
      const ACurrLonLat: TDoublePoint;
      var AIntersectionLonLat: TDoublePoint;
      const AMarkLine: IMarkLine;
      const AProjection: IProjectionInfo
    ): boolean; overload;
  end;

implementation

uses
  ActiveX,
  Types,
  Classes,
  u_Synchronizer,
  GR32_Resamplers,
  i_TileIterator,
  i_Bitmap32Static,
  i_EnumDoublePoint,
  i_BitmapLayerProvider,
  u_IdCacheSimpleThreadSafe,
  u_DoublePointsAggregator,
  u_BitmapLayerProviderByMarksSubset,
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const AVectorItmesFactory: IVectorItmesFactory;
  const AResamplerConfig: IImageResamplerConfig;
  const AConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const ATimerNoifier: INotifier;
  const AConfig: IMarksLayerConfig;
  AMarkDB: TMarksSystem
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    AClearStrategyFactory,
    ATimerNoifier,
    AConfig.ThreadConfig
  );
  FMarksSubsetCS := MakeSyncRW_Var(Self);
  FVectorItmesFactory := AVectorItmesFactory;
  FGetMarksCounter := PerfList.CreateAndAddNewCounter('GetMarks');
  FMouseOnRegCounter := PerfList.CreateAndAddNewCounter('MouseOnReg');

  FConfig := AConfig;
  FMarkDB := AMarkDB;

  FProjectedCache := TIdCacheSimpleThreadSafe.Create;
  FPointsAgregator := TDoublePointsAggregator.Create;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.MarksShowConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.MarksDrawConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FMarkDB.MarksDb.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMarksDbChange),
    FMarkDB.CategoryDB.ChangeNotifier
  );
end;

procedure TMapMarksLayer.DrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VBitmapTile: IBitmap32Static;

  VGeoConvert: ICoordConverter;
  VBitmapConverter: ILocalCoordConverter;
  VTileIterator: ITileIterator;
  VZoom: Byte;
  { Прямоугольник пикселей растра в координатах основного конвертера }
  VBitmapOnMapPixelRect: TRect;
  { Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    основного конвертера }
  VTileSourceRect: TRect;
  { Текущий тайл в кооординатах основного конвертера }
  VTile: TPoint;
  { Прямоугольник пикслов текущего тайла в кооординатах основного конвертера }
  VCurrTilePixelRect: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
  VProv: IBitmapLayerProvider;
  VMarksSubset: IMarksSubset;
  VMapRect: TDoubleRect;
  VCounterContext: TInternalPerformanceCounterContext;
  VTileConverter: ILocalCoordConverter;
begin
  VBitmapConverter := LayerCoordConverter;
  if VBitmapConverter <> nil then begin
    VCounterContext := FGetMarksCounter.StartOperation;
    try
      VMarksSubset := GetMarksSubset(VBitmapConverter);
      FMarksSubsetCS.BeginWrite;
      try
        FMarksSubset := VMarksSubset;
      finally
        FMarksSubsetCS.EndWrite;
      end;
    finally
      FGetMarksCounter.FinishOperation(VCounterContext);
    end;
    FProjectedCache.Clear;
    if (VMarksSubset <> nil) and (not VMarksSubset.IsEmpty) then begin
      VMapRect := VBitmapConverter.GetRectInMapPixelFloat;
      FLinesClipRect.Left := VMapRect.Left - 10;
      FLinesClipRect.Top := VMapRect.Top - 10;
      FLinesClipRect.Right := VMapRect.Right + 10;
      FLinesClipRect.Bottom := VMapRect.Bottom + 10;
      VProv :=
        TBitmapLayerProviderByMarksSubset.Create(
          FDrawConfigStatic,
          FVectorItmesFactory,
          VBitmapConverter.ProjectionInfo,
          FProjectedCache,
          FLinesClipRect,
          VMarksSubset
        );
      VGeoConvert := VBitmapConverter.GetGeoConverter;
      VZoom := VBitmapConverter.GetZoom;

      VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
      VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

      VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
      VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);

      if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        while VTileIterator.Next(VTile) do begin
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            break;
          end;
          VTileConverter := ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);

          VCurrTilePixelRect := VTileConverter.GetRectInMapPixel;
          VCurrTileOnBitmapRect := VBitmapConverter.MapRect2LocalRect(VCurrTilePixelRect);

          VBitmapTile :=
            VProv.GetBitmapRect(
              AOperationID,
              ACancelNotifier,
              VTileConverter
            );
          Layer.Bitmap.Lock;
          try
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              break;
            end;
            if VBitmapTile <> nil then begin
              BlockTransfer(
                Layer.Bitmap,
                VCurrTileOnBitmapRect.Left,
                VCurrTileOnBitmapRect.Top,
                Layer.Bitmap.ClipRect,
                VBitmapTile.Bitmap,
                VBitmapTile.Bitmap.BoundsRect,
                dmOpaque
              );
            end else begin
              Layer.Bitmap.FillRectS(
                VCurrTileOnBitmapRect.Left,
                VCurrTileOnBitmapRect.Top,
                VCurrTileOnBitmapRect.Right,
                VCurrTileOnBitmapRect.Bottom,
                0
              );
            end;
            SetBitmapChanged;
          finally
            Layer.Bitmap.UnLock;
          end;
        end;
      end;
    end else begin
      Layer.Bitmap.Lock;
      try
        if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Layer.Bitmap.Clear(0);
          SetBitmapChanged;
        end;
      finally
        Layer.Bitmap.UnLock;
      end;
    end;
  end;
end;

function TMapMarksLayer.MouseOnReg(
  const AVisualConverter: ILocalCoordConverter;
  const xy: TPoint;
  out AMarkS: Double
): IMark;
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMark: IMark;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VMarksSubset: IMarksSubset;
  VMarksEnum: IEnumUnknown;
  VSquare: Double;
  i: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
  VProjectdPath: IProjectedPath;
  VProjectdPolygon: IProjectedPolygon;
begin
  Result := nil;
  VCounterContext := FMouseOnRegCounter.StartOperation;
  try
    AMarkS := 0;

    FMarksSubsetCS.BeginRead;
    try
      VMarksSubset := FMarksSubset;
    finally
      FMarksSubsetCS.EndRead;
    end;

    if VMarksSubset <> nil then begin
      if not VMarksSubset.IsEmpty then begin
        VRect.Left := xy.X - 8;
        VRect.Top := xy.Y - 16;
        VRect.Right := xy.X + 8;
        VRect.Bottom := xy.Y + 16;
        VLocalConverter := LayerCoordConverter;
        VConverter := VLocalConverter.GetGeoConverter;
        VZoom := VLocalConverter.GetZoom;
        VMapRect := AVisualConverter.LocalRect2MapRectFloat(VRect);
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VPixelPos := AVisualConverter.LocalPixel2MapPixelFloat(xy);
        VMarksEnum := VMarksSubset.GetEnum;
        while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
          if VMark.LLRect.IsIntersecWithRect(VLonLatRect) then begin
            if Supports(VMark, IMarkPoint) then begin
              Result := VMark;
              AMarkS := 0;
              exit;
            end else begin
              if Supports(VMark, IMarkLine, VMarkLine) then begin
                VProjectdPath := GetProjectedPath(VMarkLine, VLocalConverter.ProjectionInfo);
                if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                  Result := VMark;
                  AMarkS := 0;
                  exit;
                end;
              end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
                VProjectdPolygon := GetProjectedPolygon(VMarkPoly, VLocalConverter.ProjectionInfo);
                if VProjectdPolygon.IsPointInPolygon(VPixelPos) or
                  VProjectdPolygon.IsPointOnBorder(VPixelPos, (VMarkPoly.LineWidth / 2) + 3) then begin
                  VSquare := VProjectdPolygon.CalcArea;
                  if (Result = nil) or (VSquare < AMarkS) then begin
                    Result := VMark;
                    AMarkS := VSquare;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    FMouseOnRegCounter.FinishOperation(VCounterContext);
  end;
end;

function TMapMarksLayer.GetMarksSubset(
  const ALocalConverter: ILocalCoordConverter
): IMarksSubset;
var
  VList: IInterfaceList;
  VZoom: Byte;
  VMapPixelRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VGeoConverter: ICoordConverter;
begin
  VList := nil;
  Result := nil;
  if FConfigStatic.IsUseMarks then begin
    VZoom := ALocalConverter.GetZoom;
    if not FConfigStatic.IgnoreCategoriesVisible then begin
      VList := FMarkDB.GetVisibleCategories(VZoom);
    end;
    try
      if (VList <> nil) and (VList.Count = 0) then begin
        Result := nil;
      end else begin
        VGeoConverter := ALocalConverter.GetGeoConverter;
        VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
        VGeoConverter.CheckPixelRectFloat(VMapPixelRect, VZoom);
        VLonLatRect := VGeoConverter.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);
        Result := FMarkDB.MarksDb.GetMarksSubset(VLonLatRect, VList, FConfigStatic.IgnoreMarksVisible);
      end;
    finally
      VList := nil;
    end;
  end;
end;

function TMapMarksLayer.GetProjectedPath(
  const AMarkPath: IMarkLine;
  const AProjectionInfo: IProjectionInfo
): IProjectedPath;
var
  VID: Integer;
begin
  VID := Integer(AMarkPath);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItmesFactory.CreateProjectedPathWithClipByLonLatPath(
        AProjectionInfo,
        AMarkPath.Line,
        FLinesClipRect,
        FPointsAgregator
      );
  end;
end;

function TMapMarksLayer.GetProjectedPolygon(
  const AMarkPoly: IMarkPoly;
  const AProjectionInfo: IProjectionInfo
): IProjectedPolygon;
var
  VID: Integer;
begin
  VID := Integer(AMarkPoly);
  if not Supports(FProjectedCache.GetByID(VID), IProjectedPath, Result) then begin
    Result :=
      FVectorItmesFactory.CreateProjectedPolygonWithClipByLonLatPolygon(
        AProjectionInfo,
        AMarkPoly.Line,
        FLinesClipRect,
        FPointsAgregator
      );
  end;
end;

function TMapMarksLayer.MouseOnReg(
  const AVisualConverter: ILocalCoordConverter;
  const xy: TPoint
): IMark;
var
  VMarkS: Double;
begin
  Result := MouseOnReg(AVisualConverter, xy, VMarkS);
end;

function TMapMarksLayer.GetIntersection(
  const ACurrLonLat: TDoublePoint;
  var AIntersectionLonLat: TDoublePoint;
  const AMarkPoly: IMarkPoly;
  const AProjection: IProjectionInfo
): boolean;
var
  r: double;
  VConverter: ICoordConverter;
  VZoom: byte;
  VEnum: IEnumLonLatPoint;
  VLonLatPoint: TDoublePoint;
  VMapPoint: TDoublePoint;
  VCurrPixel: TDoublePoint;
begin
  VZoom := AProjection.Zoom;
  VConverter := AProjection.GeoConverter;
  Result := False;
  r := (AMarkPoly.LineWidth / 2) + 3;
  VCurrPixel := VConverter.LonLat2PixelPosFloat(ACurrLonLat, VZoom);
  VEnum := AMarkPoly.Line.GetEnum;
  while VEnum.Next(VLonLatPoint) do begin
    VConverter.CheckLonLatPos(VLonLatPoint);
    VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
    if (VCurrPixel.x >= VMapPoint.X - r) and (VCurrPixel.x <= VMapPoint.X + r) and
      (VCurrPixel.y >= VMapPoint.Y - r) and (VCurrPixel.y <= VMapPoint.Y + r) then begin
      AIntersectionLonLat := VLonLatPoint;
      Result := true;
      exit;
    end;
  end;
end;

function TMapMarksLayer.GetIntersection(
  const ACurrLonLat: TDoublePoint;
  var AIntersectionLonLat: TDoublePoint;
  const AMarkLine: IMarkLine;
  const AProjection: IProjectionInfo
): Boolean;
var
  r: double;
  VConverter: ICoordConverter;
  VZoom: byte;
  VEnum: IEnumLonLatPoint;
  VLonLatPoint: TDoublePoint;
  VMapPoint: TDoublePoint;
  VCurrPixel: TDoublePoint;
begin
  VZoom := AProjection.Zoom;
  VConverter := AProjection.GeoConverter;
  Result := False;
  r := (AMarkLine.LineWidth / 2) + 3;
  VCurrPixel := VConverter.LonLat2PixelPosFloat(ACurrLonLat, VZoom);
  VEnum := AMarkLine.Line.GetEnum;
  while VEnum.Next(VLonLatPoint) do begin
    VConverter.CheckLonLatPos(VLonLatPoint);
    VMapPoint := VConverter.LonLat2PixelPosFloat(VLonLatPoint, VZoom);
    if (VCurrPixel.x >= VMapPoint.X - r) and (VCurrPixel.x <= VMapPoint.X + r) and
      (VCurrPixel.y >= VMapPoint.Y - r) and (VCurrPixel.y <= VMapPoint.Y + r) then begin
      AIntersectionLonLat := VLonLatPoint;
      Result := true;
      exit;
    end;
  end;
end;

procedure TMapMarksLayer.OnConfigChange;
begin
  ViewUpdateLock;
  try
    FConfigStatic := FConfig.MarksShowConfig.GetStatic;
    FDrawConfigStatic := FConfig.MarksDrawConfig.GetStatic;
    SetVisible(FConfigStatic.IsUseMarks);
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMapMarksLayer.OnMarksDbChange;
begin
  Redraw;
end;

procedure TMapMarksLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
