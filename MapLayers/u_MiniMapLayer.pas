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

unit u_MiniMapLayer;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Controls,
  TBX,
  TB2Item,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_Notifier,
  t_GeoTypes,
  i_NotifierOperation,
  i_MapTypes,
  i_MapTypeIconsList,
  i_ActiveMapsConfig,
  i_BackgroundTask,
  i_InternalPerformanceCounter,
  i_LayerBitmapClearStrategy,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_GlobalViewMainConfig,
  i_ViewPortState,
  i_SimpleFlag,
  i_TileError,
  i_BitmapLayerProvider,
  i_MapTypeGUIConfigList,
  i_MiniMapLayerConfig,
  i_BitmapPostProcessingConfig,
  u_MapType,
  u_WindowLayerWithPos;

type
  TMiniMapLayer = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: IMiniMapLayerConfig;
    FViewConfig: IGlobalViewMainConfig;
    FErrorLogger: ITileErrorLogger;
    FParentMap: TImage32;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FGUIConfigList: IMapTypeGUIConfigList;
    FPostProcessingConfig: IBitmapPostProcessingConfig;
    FViewPortState: IViewPortState;

    FPopup: TTBXPopupMenu;
    FIconsList: IMapTypeIconsList;
    FPlusButton: TBitmapLayer;
    FPlusButtonPressed: Boolean;
    FMinusButton: TBitmapLayer;
    FMinusButtonPressed: Boolean;
    FViewRectDrawLayer: TBitmapLayer;
    FPosMoved: Boolean;
    FViewRectMoveDelta: TDoublePoint;

    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;

    FMainMap: IMapType;
    FMainMapCS: IReadWriteSync;
    FLayersSet: IMapTypeSet;
    FLayersSetCS: IReadWriteSync;

    FBitmapProvider: IBitmapLayerProvider;
    FBitmapProviderCS: IReadWriteSync;

    FDrawTask: IBackgroundTask;
    FUpdateFlag: ISimpleFlag;
    FBgDrawCounter: IInternalPerformanceCounter;
    FClearStrategy: ILayerBitmapClearStrategy;
    FClearStrategyFactory: ILayerBitmapClearStrategyFactory;
    procedure CreateBitmapProvider;

    procedure DrawMainViewRect;

    procedure PlusButtonMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure PlusButtonMouseUP(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );

    procedure MinusButtonMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure MinusButtonMouseUP(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );

    procedure LayerMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure LayerMouseUP(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer
    );
    procedure LayerMouseMove(
      Sender: TObject;
      Shift: TShiftState;
      X, Y: Integer
    );


    function GetActualZoom(const AVisualCoordConverter: ILocalCoordConverter): Byte;

    procedure BuildPopUpMenu;
    procedure BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
    procedure CreateLayers(AParentMap: TImage32);
    procedure OnClickMapItem(Sender: TObject);
    procedure OnClickLayerItem(Sender: TObject);
    procedure OnConfigChange;

    procedure OnMainMapChange;
    procedure OnLayerSetChange;
    procedure OnDrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure OnTimer;

    procedure SetBitmapChanged;
    procedure DrawBitmap(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation
    );
    procedure ClearLayerBitmap;
    function GetLayersSet: IMapTypeSet;
    function GetMainMap: IMapType;
    procedure SetLayersSet(const Value: IMapTypeSet);
    procedure SetMainMap(const Value: IMapType);
    property MainMap: IMapType read GetMainMap write SetMainMap;
    property LayersSet: IMapTypeSet read GetLayersSet write SetLayersSet;
    property ConverterFactory: ILocalCoordConverterFactorySimpe read FConverterFactory;
  protected
    function GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure DoRedraw; override;
    procedure DoUpdateLayerSize(const ANewSize: TPoint); override;
    procedure DoUpdateLayerLocation(const ANewLocation: TFloatRect); override;
    function GetLayerSizeForView(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): TPoint; override;
    function GetLayerCoordConverterByViewConverter(
      const ANewVisualCoordConverter: ILocalCoordConverter
    ): ILocalCoordConverter; override;
    procedure SetLayerCoordConverter(const AValue: ILocalCoordConverter); override;
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AAppStartedNotifier: INotifierOneOperation;
      const AAppClosingNotifier: INotifierOneOperation;
      AParentMap: TImage32;
      const AViewPortState: IViewPortState;
      const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      const AConfig: IMiniMapLayerConfig;
      const AViewConfig: IGlobalViewMainConfig;
      const APostProcessingConfig: IBitmapPostProcessingConfig;
      const AGUIConfigList: IMapTypeGUIConfigList;
      const AIconsList: IMapTypeIconsList;
      const AErrorLogger: ITileErrorLogger;
      const ATimerNoifier: INotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  Types,
  GR32_Polygons,
  GR32_Resamplers,
  c_ZeroGUID,
  u_Synchronizer,
  u_GeoFun,
  u_ResStrings,
  u_SimpleFlagWithInterlock,
  i_TileIterator,
  i_Bitmap32Static,
  u_ListenerByEvent,
  u_MapTypeListStatic,
  u_BitmapLayerProviderForViewMaps,
  u_BackgroundTask,
  u_TileIteratorSpiralByRect,
  u_MapTypeMenuItemsGeneratorBasic;

{ TMapMainLayer }

constructor TMiniMapLayer.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AAppStartedNotifier: INotifierOneOperation;
  const AAppClosingNotifier: INotifierOneOperation;
  AParentMap: TImage32;
  const AViewPortState: IViewPortState;
  const ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  const AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  const AConfig: IMiniMapLayerConfig;
  const AViewConfig: IGlobalViewMainConfig;
  const APostProcessingConfig: IBitmapPostProcessingConfig;
  const AGUIConfigList: IMapTypeGUIConfigList;
  const AIconsList: IMapTypeIconsList;
  const AErrorLogger: ITileErrorLogger;
  const ATimerNoifier: INotifier
);
begin
  inherited Create(
    APerfList,
    AAppStartedNotifier,
    AAppClosingNotifier,
    AParentMap,
    AViewPortState
  );
  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  FViewPortState := AViewPortState;
  FConfig := AConfig;
  FGUIConfigList := AGUIConfigList;
  FErrorLogger := AErrorLogger;
  FClearStrategyFactory := AClearStrategyFactory;
  FViewConfig := AViewConfig;
  FConverterFactory := ACoordConverterFactory;
  FPostProcessingConfig := APostProcessingConfig;
  FParentMap := AParentMap;
  FIconsList := AIconsList;

  FMainMapCS := MakeSyncRW_Var(Self);
  FLayersSetCS := MakeSyncRW_Var(Self);
  FBitmapProviderCS := MakeSyncRW_Var(Self);

  FViewRectMoveDelta := DoublePoint(0, 0);

  FPopup := TTBXPopupMenu.Create(AParentMap);
  FPopup.Name := 'PopupMiniMap';
  FPopup.Images := FIconsList.GetImageList;

  Layer.Bitmap.BeginUpdate;
  CreateLayers(AParentMap);

  FDrawTask :=
    TBackgroundTask.Create(
      AAppClosingNotifier,
      OnDrawBitmap,
      FConfig.ThreadConfig,
      Self.ClassName
    );
  FUpdateFlag := TSimpleFlagWithInterlock.Create;

  BuildPopUpMenu;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnMainMapChange),
    FConfig.MapsConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnLayerSetChange),
    FConfig.MapsConfig.GetActiveLayersSet.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FViewConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    APostProcessingConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );
end;

procedure TMiniMapLayer.CreateBitmapProvider;
var
  VMainMap: IMapType;
  VLayersSet: IMapTypeSet;
  VUsePrevZoomAtMap, VUsePrevZoomAtLayer: Boolean;
  VPostProcessingConfig: IBitmapPostProcessingConfigStatic;

  VLayers: array of IMapType;
  VLayersList: IMapTypeListStatic;
  VProvider: IBitmapLayerProvider;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  VCnt: Cardinal;
  i: Integer;
  VLayersCount: Integer;
  VZOrder: Integer;
  VIndex: Integer;
begin
  VMainMap := MainMap;
  VLayersSet := LayersSet;
  VUsePrevZoomAtMap := FUsePrevZoomAtMap;
  VUsePrevZoomAtLayer := FUsePrevZoomAtLayer;
  VPostProcessingConfig := FPostProcessingConfig.GetStatic;

  VLayersCount := 0;
  try
    if VLayersSet <> nil then begin
      VEnum := VLayersSet.GetIterator;
      while VEnum.Next(1, VGUID, VCnt) = S_OK do begin
        VItem := VLayersSet.GetMapTypeByGUID(VGUID);
        if VItem.MapType.IsBitmapTiles then begin
          VZOrder := VItem.MapType.LayerDrawConfig.LayerZOrder;
          Inc(VLayersCount);
          SetLength(VLayers, VLayersCount);
          VIndex := 0;
          if VLayersCount > 1 then begin
            for i := VLayersCount - 2 downto 0 do begin
              if VLayers[i].MapType.LayerDrawConfig.LayerZOrder > VZOrder then begin
                VLayers[i + 1] := VLayers[i];
              end else begin
                VIndex := i + 1;
                Break;
              end;
            end;
          end;
          VLayers[VIndex] := VItem;
        end;
      end;
    end;
    VLayersList := TMapTypeListStatic.Create(VLayers);
  finally
    for i := 0 to Length(VLayers) - 1 do begin
      VLayers[i] := nil;
    end;
    VLayers := nil;
  end;
  VProvider :=
    TBitmapLayerProviderForViewMaps.Create(
      VMainMap,
      VLayersList,
      VUsePrevZoomAtMap,
      VUsePrevZoomAtLayer,
      True,
      VPostProcessingConfig,
      FErrorLogger
    );
  FBitmapProviderCS.BeginWrite;
  try
    FBitmapProvider := VProvider;
  finally
    FBitmapProviderCS.EndWrite;
  end;
end;

destructor TMiniMapLayer.Destroy;
begin
  FMainMapCS := nil;
  FLayersSetCS := nil;

  FConverterFactory := nil;
  FDrawTask := nil;
  inherited;
end;

procedure TMiniMapLayer.ClearLayerBitmap;
begin
  if Visible then begin
    Layer.Bitmap.Lock;
    try
      if FClearStrategy <> nil then begin
        FClearStrategy.Clear(Layer.Bitmap);
        FClearStrategy := nil;
      end;
    finally
      Layer.Bitmap.UnLock;
    end;
  end;
end;

function TMiniMapLayer.GetLayerCoordConverterByViewConverter(
  const ANewVisualCoordConverter: ILocalCoordConverter
): ILocalCoordConverter;
var
  VVisualMapCenter: TDoublePoint;
  VZoom: Byte;
  VSourceZoom: Byte;
  VConverter: ICoordConverter;
  VVisualMapCenterInRelative: TDoublePoint;
  VVisualMapCenterInLayerMap: TDoublePoint;
  VLocalTopLeftAtMap: TPoint;
  VLocalTopLeftAtMapFloat: TDoublePoint;
  VLayerSize: TPoint;
begin
  VVisualMapCenter := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VSourceZoom := ANewVisualCoordConverter.GetZoom;
  VConverter := ANewVisualCoordConverter.GetGeoConverter;
  VConverter.CheckPixelPosFloatStrict(VVisualMapCenter, VSourceZoom, True);
  VVisualMapCenterInRelative := VConverter.PixelPosFloat2Relative(VVisualMapCenter, VSourceZoom);
  VZoom := GetActualZoom(ANewVisualCoordConverter);
  VVisualMapCenterInLayerMap := VConverter.Relative2PixelPosFloat(VVisualMapCenterInRelative, VZoom);
  VLayerSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  VLocalTopLeftAtMapFloat :=
    DoublePoint(
      VVisualMapCenterInLayerMap.X - VLayerSize.X / 2,
      VVisualMapCenterInLayerMap.Y - VLayerSize.Y / 2
    );
  VLocalTopLeftAtMap := PointFromDoublePoint(VLocalTopLeftAtMapFloat, prToTopLeft);

  Result := ConverterFactory.CreateConverterNoScale(
    Rect(0, 0, VLayerSize.X, VLayerSize.Y),
    VZoom,
    VConverter,
    VLocalTopLeftAtMap
  );
end;

function TMiniMapLayer.GetLayerSizeForView(
  const ANewVisualCoordConverter: ILocalCoordConverter
): TPoint;
var
  VWidth: Integer;
begin
  VWidth := FConfig.Width;
  Result := Point(VWidth, VWidth);
end;

function TMiniMapLayer.GetLayersSet: IMapTypeSet;
begin
  FLayersSetCS.BeginRead;
  try
    Result := FLayersSet;
  finally
    FLayersSetCS.EndRead;
  end;
end;

procedure TMiniMapLayer.CreateLayers(AParentMap: TImage32);
begin
  FViewRectDrawLayer := TBitmapLayer.Create(AParentMap.Layers);
  FViewRectDrawLayer.Visible := False;
  FViewRectDrawLayer.MouseEvents := false;
  FViewRectDrawLayer.Bitmap.DrawMode := dmBlend;
  FViewRectDrawLayer.OnMouseDown := LayerMouseDown;
  FViewRectDrawLayer.OnMouseUp := LayerMouseUP;
  FViewRectDrawLayer.OnMouseMove := LayerMouseMove;

  FPlusButton := TBitmapLayer.Create(AParentMap.Layers);
  FPlusButton.Visible := False;
  FPlusButton.MouseEvents := false;
  FPlusButton.Bitmap.DrawMode := dmBlend;
  FPlusButton.OnMouseDown := PlusButtonMouseDown;
  FPlusButton.OnMouseUp := PlusButtonMouseUP;
  FPlusButton.Cursor := crHandPoint;
  FPlusButtonPressed := False;

  FMinusButton := TBitmapLayer.Create(AParentMap.Layers);
  FMinusButton.Visible := False;
  FMinusButton.MouseEvents := false;
  FMinusButton.Bitmap.DrawMode := dmBlend;
  FMinusButton.OnMouseDown := MinusButtonMouseDown;
  FMinusButton.OnMouseUp := MinusButtonMouseUP;
  FMinusButton.Cursor := crHandPoint;
  FMinusButtonPressed := False;
end;

procedure TMiniMapLayer.BuildPopUpMenu;
var
  VSubMenuItem: TTBXSubmenuItem;
  VLayersSubMenu: TTBXSubmenuItem;
begin
  VSubMenuItem := TTBXSubmenuItem.Create(FPopup);
  VSubMenuItem.Name := 'MiniMapLayers';
  VSubMenuItem.Caption := SAS_STR_Layers;
  VSubMenuItem.Hint := '';
  VSubMenuItem.SubMenuImages := FPopup.Images;
  FPopup.Items.Add(VSubMenuItem);
  VLayersSubMenu := VSubMenuItem;

  BuildMapsListUI(FPopup.Items, VLayersSubMenu);
end;

procedure TMiniMapLayer.BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create(
    FGUIConfigList,
    FConfig.MapsConfig.GetActiveMapsSet,
    AMapssSubMenu,
    Self.OnClickMapItem,
    FIconsList
  );
  try
    VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
  VGenerator := TMapMenuGeneratorBasic.Create(
    FGUIConfigList,
    FConfig.MapsConfig.GetActiveLayersSet,
    ALayersSubMenu,
    Self.OnClickLayerItem,
    FIconsList
  );
  try
    VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
end;

procedure TMiniMapLayer.DoRedraw;
begin
  FDrawTask.StopExecute;
  CreateBitmapProvider;
  inherited;
  ClearLayerBitmap;
  DrawMainViewRect;
  if Visible then begin
    FDrawTask.StartExecute;
  end;
end;

procedure TMiniMapLayer.DrawMainViewRect;
var
  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VLoadedRect: TDoubleRect;
  VZoomSource: Byte;
  VZoom: Byte;
  VMiniMapRect: TDoubleRect;
  VBitmapRect: TDoubleRect;
  VRelRect: TDoubleRect;
  VPolygon: TPolygon32;
  VBitmapSize: TPoint;
  VVisualCoordConverter: ILocalCoordConverter;
  VBitmapCoordConverter: ILocalCoordConverter;
  VGeoConvert: ICoordConverter;
  VZoomDelta: Integer;
begin
  FViewRectDrawLayer.Bitmap.Clear(0);
  VVisualCoordConverter := ViewCoordConverter;
  VBitmapCoordConverter := LayerCoordConverter;
  if (VVisualCoordConverter <> nil) and (VBitmapCoordConverter <> nil) then begin
    VGeoConvert := VVisualCoordConverter.GetGeoConverter;
    VZoomDelta := FConfig.ZoomDelta;
    if VZoomDelta > 0 then begin
      VLoadedRect := VVisualCoordConverter.GetRectInMapPixelFloat;
      VZoomSource := VBitmapCoordConverter.GetZoom;
      VZoom := VVisualCoordConverter.GetZoom;
      VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);
      VRelRect := VGeoConvert.PixelRectFloat2RelativeRect(VLoadedRect, VZoom);
      VMiniMapRect := VGeoConvert.RelativeRect2PixelRectFloat(VRelRect, VZoomSource);
      VBitmapRect := VBitmapCoordConverter.MapRectFloat2LocalRectFloat(VMiniMapRect);
      VBitmapRect.Left := VBitmapRect.Left + FViewRectMoveDelta.X;
      VBitmapRect.Top := VBitmapRect.Top + FViewRectMoveDelta.Y;
      VBitmapRect.Right := VBitmapRect.Right + FViewRectMoveDelta.X;
      VBitmapRect.Bottom := VBitmapRect.Bottom + FViewRectMoveDelta.Y;

      VBitmapSize := Point(FViewRectDrawLayer.Bitmap.Width, FViewRectDrawLayer.Bitmap.Height);
      if (VBitmapRect.Left >= 0) or (VBitmapRect.Top >= 0) or (VBitmapRect.Right <= VBitmapSize.X) or (VBitmapRect.Bottom <= VBitmapSize.Y) then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Antialiased := true;
          VPolygon.Add(FixedPoint(VBitmapRect.Left, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Bottom));
          VPolygon.Add(FixedPoint(VBitmapRect.Left, VBitmapRect.Bottom));
          VPolygon.DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clWhite32, (VZoomDelta) * 35));
          with VPolygon.Outline do begin
            try
              with Grow(Fixed(3.2 / 2), 0.5) do begin
                try
                  FillMode := pfWinding;
                  DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clNavy32, (VZoomDelta) * 43));
                finally
                  Free;
                end;
              end;
            finally
              Free;
            end;
          end;
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

procedure TMiniMapLayer.DrawBitmap(
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
  VProvider: IBitmapLayerProvider;
  VTileConverter: ILocalCoordConverter;
begin
  VBitmapConverter := LayerCoordConverter;
  FBitmapProviderCS.BeginRead;
  try
    VProvider := FBitmapProvider;
  finally
    FBitmapProviderCS.EndRead;
  end;

  if (VBitmapConverter = nil) or (VProvider = nil) then begin
    Exit;
  end;

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
        VProvider.GetBitmapRect(
          AOperationID,
          ACancelNotifier,
          VTileConverter
        );
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        break;
      end;
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
end;

function TMiniMapLayer.GetActualZoom(
  const AVisualCoordConverter: ILocalCoordConverter
): Byte;
var
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VZoomDelta: Integer;
begin
  VZoom := AVisualCoordConverter.GetZoom;
  VGeoConvert := AVisualCoordConverter.GetGeoConverter;
  VZoomDelta := FConfig.ZoomDelta;
  if VZoomDelta = 0 then begin
    Result := VZoom;
  end else if VZoomDelta > 0 then begin
    if VZoom > VZoomDelta then begin
      Result := VZoom - VZoomDelta;
    end else begin
      Result := 0;
    end;
  end else begin
    Result := VZoom - VZoomDelta;
    VGeoConvert.CheckZoom(Result);
  end;
end;

function TMiniMapLayer.GetMainMap: IMapType;
begin
  FMainMapCS.BeginRead;
  try
    Result := FMainMap;
  finally
    FMainMapCS.EndRead;
  end;
end;

function TMiniMapLayer.GetMapLayerLocationRect(const ANewVisualCoordConverter: ILocalCoordConverter): TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
begin
  if ANewVisualCoordConverter <> nil then begin
    VSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
    VViewSize := ANewVisualCoordConverter.GetLocalRectSize;
    Result.Right := VViewSize.X;
    Result.Bottom := VViewSize.Y - FConfig.BottomMargin;
    Result.Left := Result.Right - VSize.X;
    Result.Top := Result.Bottom - VSize.Y;
  end else begin
    Result.Left := 0;
    Result.Bottom := 0;
    Result.Right := 0;
    Result.Top := 0;
  end;
end;

procedure TMiniMapLayer.DoHide;
begin
  inherited;
  FViewRectDrawLayer.Visible := false;
  FViewRectDrawLayer.MouseEvents := false;

  FPlusButton.Visible := False;
  FPlusButton.MouseEvents := false;

  FMinusButton.Visible := False;
  FMinusButton.MouseEvents := false;
end;

procedure TMiniMapLayer.LayerMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
var
  VLayerSize: TPoint;
  VBitmapCenter: TDoublePoint;
  VVisibleCenter: TDoublePoint;
  Vlocation: TFloatRect;
begin
  FParentMap.PopupMenu := nil;
  case button of
    mbRight: begin
      FParentMap.PopupMenu := FPopup;
    end;
    mbLeft: begin
      VLayerSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
      VBitmapCenter := DoublePoint(VLayerSize.X / 2, VLayerSize.Y / 2);
      Vlocation := Layer.Location;
      VVisibleCenter.X := VBitmapCenter.X + Vlocation.Left;
      VVisibleCenter.Y := VBitmapCenter.Y + Vlocation.Top;
      FPosMoved := True;
      FViewRectMoveDelta := DoublePoint(X - VVisibleCenter.X, Y - VVisibleCenter.Y);
      DrawMainViewRect;
    end;
  end;
end;

procedure TMiniMapLayer.LayerMouseMove(
  Sender: TObject;
  Shift: TShiftState;
  X, Y: Integer
);
var
  VBitmapSize: TPoint;
  VBitmapCenter: TDoublePoint;
  VVisibleCenter: TDoublePoint;
  VLocation: TFloatRect;
begin
  if FPosMoved then begin
    VBitmapSize := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
    VBitmapCenter := DoublePoint(VBitmapSize.X / 2, VBitmapSize.Y / 2);

    VLocation := Layer.Location;

    VVisibleCenter.X := VLocation.Left + VBitmapCenter.X;
    VVisibleCenter.Y := VLocation.Top + VBitmapCenter.Y;

    if X < VLocation.Left then begin
      FViewRectMoveDelta.X := VLocation.Left - VVisibleCenter.X;
    end else if X > VLocation.Right then begin
      FViewRectMoveDelta.X := VLocation.Right - VVisibleCenter.X;
    end else begin
      FViewRectMoveDelta.X := X - VVisibleCenter.X;
    end;
    if Y < VLocation.Top then begin
      FViewRectMoveDelta.Y := VLocation.Top - VVisibleCenter.Y;
    end else if Y > VLocation.Bottom then begin
      FViewRectMoveDelta.Y := VLocation.Bottom - VVisibleCenter.Y;
    end else begin
      FViewRectMoveDelta.Y := Y - VVisibleCenter.Y;
    end;

    DrawMainViewRect;
  end;
end;

procedure TMiniMapLayer.LayerMouseUP(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
var
  VBitmapCoordConverter: ILocalCoordConverter;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VBitmapPos: TDoublePoint;
  Vlocation: TFloatRect;
  VMapPoint: TDoublePoint;
  VLonLat: TDoublePoint;
begin
  if FPosMoved then begin
    if Layer.HitTest(X, Y) then begin
      VBitmapCoordConverter := LayerCoordConverter;
      Vlocation := Layer.Location;
      VBitmapPos.X := X - Vlocation.Left;
      VBitmapPos.Y := Y - Vlocation.Top;
      VConverter := VBitmapCoordConverter.GetGeoConverter;
      VZoom := VBitmapCoordConverter.GetZoom;

      VMapPoint := VBitmapCoordConverter.LocalPixelFloat2MapPixelFloat(VBitmapPos);
      VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoom, false);
      VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoom);
      FViewRectMoveDelta := DoublePoint(0, 0);

      FViewPortState.ChangeLonLat(VLonLat);
    end else begin
      FViewRectMoveDelta := DoublePoint(0, 0);
      DrawMainViewRect;
    end;
  end;
  FPosMoved := False;
end;

procedure TMiniMapLayer.MinusButtonMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbLeft then begin
    FMinusButtonPressed := True;
  end;
end;

procedure TMiniMapLayer.MinusButtonMouseUP(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbLeft then begin
    if FMinusButtonPressed then begin
      FMinusButtonPressed := False;
      if FMinusButton.HitTest(X, Y) then begin
        FConfig.LockWrite;
        try
          FConfig.ZoomDelta := FConfig.ZoomDelta + 1;
        finally
          FConfig.UnlockWrite;
        end;
      end;
    end;
  end;
end;

procedure TMiniMapLayer.OnClickLayerItem(Sender: TObject);
var
  VSender: TTBCustomItem;
  VAtiveMap: IActiveMapSingle;
  VMap: IMapType;
begin
  if Sender is TTBCustomItem then begin
    VSender := TTBCustomItem(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMap := VAtiveMap.GetMapType;
      if VMap <> nil then begin
        FConfig.MapsConfig.LockWrite;
        try
          if not FConfig.MapsConfig.GetActiveLayersSet.IsGUIDSelected(VMap.GUID) then begin
            FConfig.MapsConfig.SelectLayerByGUID(VMap.GUID);
          end else begin
            FConfig.MapsConfig.UnSelectLayerByGUID(VMap.GUID);
          end;
        finally
          FConfig.MapsConfig.UnlockWrite;
        end;
      end;
    end;
  end;
end;

procedure TMiniMapLayer.OnClickMapItem(Sender: TObject);
var
  VSender: TComponent;
  VAtiveMap: IActiveMapSingle;
  VMap: IMapType;
begin
  if Sender is TComponent then begin
    VSender := TComponent(Sender);
    VAtiveMap := IActiveMapSingle(VSender.Tag);
    if VAtiveMap <> nil then begin
      VMap := VAtiveMap.GetMapType;
      if VMap <> nil then begin
        FConfig.MapsConfig.SelectMainByGUID(VMap.GUID);
      end else begin
        FConfig.MapsConfig.SelectMainByGUID(CGUID_Zero);
      end;
    end;
  end;
end;

procedure TMiniMapLayer.OnConfigChange;
var
  VBitmapStatic: IBitmap32Static;
begin
  ViewUpdateLock;
  try
    FConfig.LockRead;
    try
      FUsePrevZoomAtMap := FConfig.UsePrevZoomAtMap;
      FUsePrevZoomAtLayer := FConfig.UsePrevZoomAtLayer;
      VBitmapStatic := FConfig.PlusButton;
      if VBitmapStatic <> nil then begin
        FPlusButton.Bitmap.Assign(VBitmapStatic.Bitmap);
      end else begin
        FPlusButton.Bitmap.Delete;
      end;
      FPlusButton.Bitmap.DrawMode := dmTransparent;

      VBitmapStatic := FConfig.MinusButton;
      if VBitmapStatic <> nil then begin
        FMinusButton.Bitmap.Assign(VBitmapStatic.Bitmap);
      end else begin
        FMinusButton.Bitmap.Delete;
      end;
      FMinusButton.Bitmap.DrawMode := dmTransparent;

      FMinusButton.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      FPlusButton.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      Layer.Bitmap.Lock;
      try
        Layer.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      finally
        Layer.Bitmap.Unlock;
      end;
      SetVisible(FConfig.Visible);
      PosChange(ViewCoordConverter);
      SetNeedRedraw;
      SetNeedUpdateLayerSize;
    finally
      FConfig.UnlockRead;
    end;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayer.OnDrawBitmap(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation
);
var
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FBgDrawCounter.StartOperation;
  try
    DrawBitmap(AOperationID, ACancelNotifier);
  finally
    FBgDrawCounter.FinishOperation(VCounterContext);
  end;
end;

procedure TMiniMapLayer.OnLayerSetChange;
var
  VNewLayersSet: IMapTypeSet;
begin
  ViewUpdateLock;
  try
    VNewLayersSet := FConfig.MapsConfig.GetActiveLayersSet.GetSelectedMapsSet;
    LayersSet := VNewLayersSet;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayer.OnMainMapChange;
var
  VNewMainMap: IMapType;
begin
  ViewUpdateLock;
  try
    VNewMainMap := FConfig.MapsConfig.GetActiveMiniMap;
    MainMap := VNewMainMap;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
end;

procedure TMiniMapLayer.OnTimer;
begin
  if FUpdateFlag.CheckFlagAndReset then begin
    Layer.Changed;
  end;
end;

procedure TMiniMapLayer.PlusButtonMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbLeft then begin
    FPlusButtonPressed := True;
  end;
end;

procedure TMiniMapLayer.PlusButtonMouseUP(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer
);
begin
  if Button = mbLeft then begin
    if FPlusButtonPressed then begin
      if FPlusButton.HitTest(X, Y) then begin
        FConfig.LockWrite;
        try
          FConfig.ZoomDelta := FConfig.ZoomDelta - 1;
        finally
          FConfig.UnlockWrite;
        end;
      end;
      FPlusButtonPressed := False;
    end;
  end;
end;

procedure TMiniMapLayer.SendTerminateToThreads;
begin
  inherited;
  FDrawTask.Terminate;
end;

procedure TMiniMapLayer.SetBitmapChanged;
begin
  FUpdateFlag.SetFlag;
end;

procedure TMiniMapLayer.SetLayerCoordConverter(
  const AValue: ILocalCoordConverter
);
var
  VNewSize: TPoint;
  VLocalConverter: ILocalCoordConverter;
begin
  VLocalConverter := LayerCoordConverter;
  VNewSize := GetLayerSizeForView(AValue);
  Layer.Bitmap.Lock;
  try
    if Visible then begin
      FClearStrategy := FClearStrategyFactory.GetStrategy(VLocalConverter, AValue, Layer.Bitmap, FClearStrategy);
    end else begin
      FClearStrategy := nil;
    end;
    if (Layer.Bitmap.Width <> VNewSize.X) or (Layer.Bitmap.Height <> VNewSize.Y) then begin
      SetNeedUpdateLayerSize;
    end;
  finally
    Layer.Bitmap.Unlock;
  end;
  if (VLocalConverter = nil) or (not VLocalConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  SetNeedUpdateLocation;
  inherited;
end;

procedure TMiniMapLayer.SetLayersSet(const Value: IMapTypeSet);
begin
  FLayersSetCS.BeginWrite;
  try
    FLayersSet := Value;
  finally
    FLayersSetCS.EndWrite;
  end;
end;

procedure TMiniMapLayer.SetMainMap(const Value: IMapType);
begin
  FMainMapCS.BeginWrite;
  try
    FMainMap := Value;
  finally
    FMainMapCS.EndWrite;
  end;
end;

procedure TMiniMapLayer.SetNeedRedraw;
begin
  FDrawTask.StopExecute;
  inherited;
end;

procedure TMiniMapLayer.SetNeedUpdateLayerSize;
begin
  inherited;
  FDrawTask.StopExecute;
end;

procedure TMiniMapLayer.StartThreads;
begin
  inherited;
  OnConfigChange;
  OnMainMapChange;
  OnLayerSetChange;
  FDrawTask.Start;
  if Visible then begin
    FDrawTask.StartExecute;
  end;
end;

procedure TMiniMapLayer.DoShow;
begin
  inherited;
  FViewRectDrawLayer.Visible := True;
  FViewRectDrawLayer.MouseEvents := True;

  FPlusButton.Visible := True;
  FPlusButton.MouseEvents := True;

  FMinusButton.Visible := True;
  FMinusButton.MouseEvents := True;
end;

procedure TMiniMapLayer.DoUpdateLayerLocation(const ANewLocation: TFloatRect);
var
  VRect: TFloatRect;
begin
  inherited;
  FViewRectDrawLayer.Location := ANewLocation;

  VRect.Left := ANewLocation.Left + 6;
  VRect.Top := ANewLocation.Top + 6;
  VRect.Right := VRect.Left + FPlusButton.Bitmap.Width;
  VRect.Bottom := VRect.Top + FPlusButton.Bitmap.Height;
  FPlusButton.Location := VRect;

  VRect.Left := ANewLocation.Left + 19;
  VRect.Top := ANewLocation.Top + 6;
  VRect.Right := VRect.Left + FMinusButton.Bitmap.Width;
  VRect.Bottom := VRect.Top + FMinusButton.Bitmap.Height;
  FMinusButton.Location := VRect;
end;

procedure TMiniMapLayer.DoUpdateLayerSize(const ANewSize: TPoint);
var
  VBitmapSizeInPixel: TPoint;
begin
  inherited;
  VBitmapSizeInPixel := Point(Layer.Bitmap.Width, Layer.Bitmap.Height);
  FViewRectDrawLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
end;

end.
