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
  i_JclNotify,
  t_GeoTypes,
  i_OperationNotifier,
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
  i_ConfigDataProvider,
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
      AMarkPath: IMarkLine;
      AProjectionInfo: IProjectionInfo
    ): IProjectedPath;
    function GetProjectedPolygon(
      AMarkPoly: IMarkPoly;
      AProjectionInfo: IProjectionInfo
    ): IProjectedPolygon;

    procedure OnConfigChange;
    function GetMarksSubset(ALocalConverter: ILocalCoordConverter): IMarksSubset;
  protected
    procedure DrawBitmap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    ); override;
  public
    procedure StartThreads; override;
  public
    constructor Create(
      AThreadPriorityByClass: IConfigDataProvider;
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AVectorItmesFactory: IVectorItmesFactory;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      ATimerNoifier: IJclNotifier;
      AConfig: IMarksLayerConfig;
      AMarkDB: TMarksSystem
    );
    destructor Destroy; override;

    procedure MouseOnReg(xy: TPoint; out AMark: IMark; out AMarkS: Double); overload;
    procedure MouseOnReg(xy: TPoint; out AMark: IMark); overload;

    function GetIntersection(
      const ACurrLonLat: TDoublePoint;
      var AIntersectionLonLat: TDoublePoint;
      AMarkPoly: IMarkPoly;
      AProjection: IProjectionInfo
    ):boolean; overload;
    function GetIntersection(
      const ACurrLonLat: TDoublePoint;
      var AIntersectionLonLat: TDoublePoint;
      AMarkLine: IMarkLine;
      AProjection: IProjectionInfo
    ):boolean; overload;
  end;

implementation

uses
  ActiveX,
  Types,
  Classes,
  u_Synchronizer,
  GR32_Resamplers,
  i_TileIterator,
  i_EnumDoublePoint,
  i_BitmapLayerProvider,
  u_IdCacheSimpleThreadSafe,
  u_DoublePointsAggregator,
  u_BitmapLayerProviderByMarksSubset,
  u_NotifyEventListener,
  u_ThreadPriorityByClass,
  u_TileIteratorSpiralByRect;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(
  AThreadPriorityByClass: IConfigDataProvider;
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AVectorItmesFactory: IVectorItmesFactory;
  AResamplerConfig: IImageResamplerConfig;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  ATimerNoifier: IJclNotifier;
  AConfig: IMarksLayerConfig;
  AMarkDB: TMarksSystem
);
begin
  inherited Create(
    APerfList,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState,
    AResamplerConfig,
    AConverterFactory,
    AClearStrategyFactory,
    ATimerNoifier,
    GetThreadPriorityByClass(AThreadPriorityByClass, Self)
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
end;

destructor TMapMarksLayer.Destroy;
begin
  FMarksSubsetCS := nil;
  inherited;
end;

procedure TMapMarksLayer.DrawBitmap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier
);
var
  VTileToDrawBmp: TCustomBitmap32;

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
  VResult: Boolean;
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

      VTileToDrawBmp := TCustomBitmap32.Create;
      VTileToDrawBmp.CombineMode:=cmMerge;
      try
        if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          while VTileIterator.Next(VTile) do begin
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              break;
            end;
            VTileConverter := ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert);

            VCurrTilePixelRect := VTileConverter.GetRectInMapPixel;
            VCurrTileOnBitmapRect := VBitmapConverter.MapRect2LocalRect(VCurrTilePixelRect);

            VResult :=
              VProv.GetBitmapRect(
                AOperationID,
                ACancelNotifier,
                VTileToDrawBmp,
                VTileConverter
              );
            Layer.Bitmap.Lock;
            try
              if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                break;
              end;
              if VResult then begin
                VTileToDrawBmp.CombineMode := cmMerge;
                BlockTransfer(
                  Layer.Bitmap,
                  VCurrTileOnBitmapRect.Left,
                  VCurrTileOnBitmapRect.Top,
                  Layer.Bitmap.ClipRect,
                  VTileToDrawBmp,
                  VTileToDrawBmp.BoundsRect,
                  dmOpaque
                );
              end else begin
                Layer.Bitmap.FillRect(
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
      finally
        VTileToDrawBmp.Free;
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

procedure TMapMarksLayer.MouseOnReg(xy: TPoint; out AMark: IMark; out AMarkS: Double);
var
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMark: IMark;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
  VMarksSubset: IMarksSubset;
  VMarksEnum: IEnumUnknown;
  VSquare:Double;
  i: Cardinal;
  VCounterContext: TInternalPerformanceCounterContext;
  VMarkLine: IMarkLine;
  VMarkPoly: IMarkPoly;
  VProjectdPath: IProjectedPath;
  VProjectdPolygon: IProjectedPolygon;
begin
  VCounterContext := FMouseOnRegCounter.StartOperation;
  try
    AMark := nil;
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
        VVisualConverter := ViewCoordConverter;
        VMapRect := VVisualConverter.LocalRect2MapRectFloat(VRect);
        VConverter.CheckPixelRectFloat(VMapRect, VZoom);
        VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
        VPixelPos := VVisualConverter.LocalPixel2MapPixelFloat(xy);
        VMarksEnum := VMarksSubset.GetEnum;
        while VMarksEnum.Next(1, VMark, @i) = S_OK do begin
          VMarkLonLatRect := VMark.LLRect;
          if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
          (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
            if Supports(VMark, IMarkPoint) then begin
              AMark := VMark;
              AMarkS := 0;
              exit;
            end else begin
              if Supports(VMark, IMarkLine, VMarkLine) then begin
                VProjectdPath := GetProjectedPath(VMarkLine, VLocalConverter.ProjectionInfo);
                if VProjectdPath.IsPointOnPath(VPixelPos, 2) then begin
                  AMark := VMark;
                  AMarkS := 0;
                  exit;
                end;
              end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
                VProjectdPolygon := GetProjectedPolygon(VMarkPoly, VLocalConverter.ProjectionInfo);
                if
                  VProjectdPolygon.IsPointInPolygon(VPixelPos) or
                  VProjectdPolygon.IsPointOnBorder(VPixelPos, (VMarkPoly.LineWidth / 2) + 3)
                then begin
                  VSquare := VProjectdPolygon.CalcArea;
                  if (AMark = nil) or (VSquare<AMarkS) then begin
                    AMark := VMark;
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
  ALocalConverter: ILocalCoordConverter
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

function TMapMarksLayer.GetProjectedPath(AMarkPath: IMarkLine;
  AProjectionInfo: IProjectionInfo): IProjectedPath;
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

function TMapMarksLayer.GetProjectedPolygon(AMarkPoly: IMarkPoly;
  AProjectionInfo: IProjectionInfo): IProjectedPolygon;
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

procedure TMapMarksLayer.MouseOnReg(xy: TPoint; out AMark: IMark);
var
  VMarkS: Double;
begin
  MouseOnReg(xy, AMark, VMarkS);
end;

function TMapMarksLayer.GetIntersection(
  const ACurrLonLat: TDoublePoint;
  var AIntersectionLonLat: TDoublePoint;
  AMarkPoly: IMarkPoly;
  AProjection: IProjectionInfo
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
    if
      (VCurrPixel.x>=VMapPoint.X-r)and(VCurrPixel.x<=VMapPoint.X+r)and
      (VCurrPixel.y>=VMapPoint.Y-r)and(VCurrPixel.y<=VMapPoint.Y+r)
    then begin
      AIntersectionLonLat := VLonLatPoint;
      Result := true;
      exit;
    end;
  end;
end;

function TMapMarksLayer.GetIntersection(
  const ACurrLonLat: TDoublePoint;
  var AIntersectionLonLat: TDoublePoint;
  AMarkLine: IMarkLine;
  AProjection: IProjectionInfo
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
    if
      (VCurrPixel.x>=VMapPoint.X-r)and(VCurrPixel.x<=VMapPoint.X+r)and
      (VCurrPixel.y>=VMapPoint.Y-r)and(VCurrPixel.y<=VMapPoint.Y+r)
    then begin
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
  ViewUpdate;
end;

procedure TMapMarksLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
end;

end.
