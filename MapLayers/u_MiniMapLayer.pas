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
  i_JclNotify,
  t_GeoTypes,
  i_OperationNotifier,
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
  i_MapTypeGUIConfigList,
  i_MiniMapLayerConfig,
  i_ConfigDataProvider,
  i_BitmapPostProcessingConfig,
  u_MapType,
  u_WindowLayerWithPos;

type
  TMiniMapLayer = class(TWindowLayerFixedSizeWithBitmap)
  private
    FConfig: IMiniMapLayerConfig;
    FViewConfig: IGlobalViewMainConfig;
    FParentMap: TImage32;
    FCoordConverterFactory: ILocalCoordConverterFactorySimpe;
    FGUIConfigList: IMapTypeGUIConfigList;
    FPostProcessingConfig: IBitmapPostProcessingConfig;

    FPopup: TTBXPopupMenu;
    FIconsList: IMapTypeIconsList;
    FPlusButton: TBitmapLayer;
    FPlusButtonPressed: Boolean;
    FMinusButton: TBitmapLayer;
    FMinusButtonPressed: Boolean;
    FLeftBorder: TBitmapLayer;
    FLeftBorderMoved: Boolean;
    FLeftBorderMovedClickDelta: Double;
    FTopBorder: TBitmapLayer;
    FViewRectDrawLayer: TBitmapLayer;
    FPosMoved: Boolean;
    FViewRectMoveDelta: TDoublePoint;

    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;

    FMainMap: IMapType;
    FMainMapCS: IReadWriteSync;
    FLayersSet: IMapTypeSet;
    FLayersSetCS: IReadWriteSync;

    FDrawTask: IBackgroundTask;
    FUpdateCounter: Integer;
    FBgDrawCounter: IInternalPerformanceCounter;
    FClearStrategy: ILayerBitmapClearStrategy;
    FClearStrategyFactory: ILayerBitmapClearStrategyFactory;

    procedure DrawMainViewRect;

    procedure PlusButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure PlusButtonMouseUP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure MinusButtonMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MinusButtonMouseUP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    procedure LeftBorderMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LeftBorderMouseUP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LeftBorderMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    procedure LayerMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LayerMouseUP(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure LayerMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);


    function GetActualZoom(AVisualCoordConverter: ILocalCoordConverter): Byte;

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
      ACancelNotifier: IOperationNotifier
    );
    procedure OnTimer;

    procedure SetBitmapChanged;
    procedure DrawBitmap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier
    );
    function DrawMap(
      ATargetBmp: TCustomBitmap32;
      AMapType: TMapType;
      AGeoConvert: ICoordConverter;
      AZoom: Byte;
      ATile: TPoint;
      ADrawMode: TDrawMode;
      AUsePre: Boolean;
      ARecolorConfig: IBitmapPostProcessingConfigStatic
    ): Boolean;
    procedure ClearLayerBitmap;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure DoRedraw; override;
    procedure DoUpdateLayerSize(ANewSize: TPoint); override;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); override;
    function GetLayerSizeForView(ANewVisualCoordConverter: ILocalCoordConverter): TPoint; override;
    function GetLayerCoordConverterByViewConverter(ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter; override;
    procedure SetLayerCoordConverter(AValue: ILocalCoordConverter); override;
    procedure SetNeedRedraw; override;
    procedure SetNeedUpdateLayerSize; override;
  public
    procedure StartThreads; override;
    procedure SendTerminateToThreads; override;
  public
    constructor Create(
      AThreadPriorityByClass: IConfigDataProvider;
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      AConfig: IMiniMapLayerConfig;
      AViewConfig: IGlobalViewMainConfig;
      APostProcessingConfig:IBitmapPostProcessingConfig;
      AGUIConfigList: IMapTypeGUIConfigList;
      AIconsList: IMapTypeIconsList;
      ATimerNoifier: IJclNotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_Synchronizer,
  Types,
  GR32_Polygons,
  c_ZeroGUID,
  u_GeoFun,
  u_ResStrings,
  i_TileIterator,
  i_Bitmap32Static,
  u_NotifyEventListener,
  u_BackgroundTaskLayerDrawBase,
  u_TileIteratorSpiralByRect,
  u_ThreadPriorityByClass,
  u_MapTypeMenuItemsGeneratorBasic;

{ TMapMainLayer }

constructor TMiniMapLayer.Create(
  AThreadPriorityByClass: IConfigDataProvider;
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ACoordConverterFactory: ILocalCoordConverterFactorySimpe;
  AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
  AConfig: IMiniMapLayerConfig;
  AViewConfig: IGlobalViewMainConfig;
  APostProcessingConfig:IBitmapPostProcessingConfig;
  AGUIConfigList: IMapTypeGUIConfigList;
  AIconsList: IMapTypeIconsList;
  ATimerNoifier: IJclNotifier
);
begin
  inherited Create(APerfList, AParentMap, AViewPortState);
  FBgDrawCounter := PerfList.CreateAndAddNewCounter('BgDraw');
  FConfig := AConfig;
  FGUIConfigList := AGUIConfigList;
  FClearStrategyFactory := AClearStrategyFactory;
  FViewConfig := AViewConfig;
  FCoordConverterFactory := ACoordConverterFactory;
  FPostProcessingConfig := APostProcessingConfig;
  FParentMap := AParentMap;
  FIconsList := AIconsList;

  FMainMapCS := MakeSyncMulti(Self);
  FLayersSetCS := MakeSyncMulti(Self);

  FViewRectMoveDelta := DoublePoint(0, 0);

  FPopup := TTBXPopupMenu.Create(AParentMap);
  FPopup.Name := 'PopupMiniMap';
  FPopup.Images := FIconsList.GetImageList;

  FLayer.Bitmap.BeginUpdate;
  CreateLayers(AParentMap);

  FDrawTask :=
    TBackgroundTaskLayerDrawBase.Create(
      AAppClosingNotifier,
      OnDrawBitmap,
      GetThreadPriorityByClass(AThreadPriorityByClass, Self)
    );
  FUpdateCounter := 0;

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

destructor TMiniMapLayer.Destroy;
begin
  FMainMapCS := nil;
  FLayersSetCS := nil;
  
  FCoordConverterFactory := nil;
  FDrawTask := nil;
  inherited;
end;

procedure TMiniMapLayer.ClearLayerBitmap;
begin
  if Visible then begin
    FLayer.Bitmap.Lock;
    try
      if FClearStrategy <> nil then begin
        FClearStrategy.Clear(FLayer.Bitmap);
        FClearStrategy := nil;
      end;
    finally
      FLayer.Bitmap.UnLock;
    end;
  end;
end;

function TMiniMapLayer.GetLayerCoordConverterByViewConverter(
  ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
var
  VVisualMapCenter: TDoublePoint;
  VZoom: Byte;
  VSourceZoom: Byte;
  VConverter: ICoordConverter;
  VVisualMapCenterInRelative: TDoublePoint;
  VVisualMapCenterInLayerMap: TDoublePoint;
  VLocalTopLeftAtMap: TPoint;
  VLayerSize: TPoint;
begin
  VVisualMapCenter := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VSourceZoom := ANewVisualCoordConverter.GetZoom;
  VConverter := ANewVisualCoordConverter.GetGeoConverter;
  VConverter.CheckPixelPosFloatStrict(VVisualMapCenter, VSourceZoom, True);
  VVisualMapCenterInRelative := VConverter.PixelPosFloat2Relative(VVisualMapCenter, VSourceZoom);
  VZoom := GetActualZoom(ANewVisualCoordConverter);
  VVisualMapCenterInLayerMap := VConverter.Relative2PixelPosFloat(VVisualMapCenterInRelative, VZoom);
  VLayerSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  VLocalTopLeftAtMap :=
    PointFromDoublePoint(
      DoublePoint(
        VVisualMapCenterInLayerMap.X - VLayerSize.X / 2,
        VVisualMapCenterInLayerMap.Y - VLayerSize.Y / 2
      ),
      prToTopLeft
    );

  Result := FCoordConverterFactory.CreateConverterNoScale(
    Rect(0, 0, VLayerSize.X, VLayerSize.Y),
    VZoom,
    VConverter,
    VLocalTopLeftAtMap
  );
end;

function TMiniMapLayer.GetLayerSizeForView(
  ANewVisualCoordConverter: ILocalCoordConverter): TPoint;
var
  VWidth: Integer;
begin
  VWidth := FConfig.Width;
  Result := Point(VWidth, VWidth);
end;

procedure TMiniMapLayer.CreateLayers(AParentMap: TImage32);
begin
  FLeftBorder := TBitmapLayer.Create(AParentMap.Layers);
  FLeftBorder.Visible := False;
  FLeftBorder.MouseEvents := false;
  FLeftBorder.Cursor := crSizeNWSE;
  FLeftBorder.Bitmap.DrawMode := dmBlend;
  FLeftBorder.OnMouseDown := LeftBorderMouseDown;
  FLeftBorder.OnMouseUp := LeftBorderMouseUP;
  FLeftBorder.OnMouseMove := LeftBorderMouseMove;
  FLeftBorderMoved := False;

  FTopBorder := TBitmapLayer.Create(AParentMap.Layers);
  FTopBorder.Visible := False;
  FTopBorder.MouseEvents := false;
  FTopBorder.Bitmap.DrawMode := dmBlend;

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
      if (VBitmapRect.Left >= 0) or (VBitmapRect.Top >= 0)
        or (VBitmapRect.Right <= VBitmapSize.X)
        or (VBitmapRect.Bottom <= VBitmapSize.Y)
      then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Antialiased := true;
          VPolygon.Add(FixedPoint(VBitmapRect.Left, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Bottom));
          VPolygon.Add(FixedPoint(VBitmapRect.Left, VBitmapRect.Bottom));
          VPolygon.DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clWhite32, (VZoomDelta) * 35));
          with VPolygon.Outline do try
            with Grow(Fixed(3.2 / 2), 0.5) do try
              FillMode := pfWinding;
              DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clNavy32, (VZoomDelta)*43));
            finally
              Free;
            end;
          finally
            Free;
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
  ACancelNotifier: IOperationNotifier
);
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VLayersSet: IMapTypeSet;
  VRecolorConfig: IBitmapPostProcessingConfigStatic;
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
  { Прямоугольник тайла подлежащий отображению на текущий растр }
  VTilePixelsToDraw: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
  VTileIsEmpty: Boolean;
  VMainMap: IMapType;
  // draw mode - very first item is opaque, others - as dmBlend
  VDrawMode: TDrawMode;
begin
  VRecolorConfig := FPostProcessingConfig.GetStatic;

  VBitmapConverter := LayerCoordConverter;
  VGeoConvert := VBitmapConverter.GetGeoConverter;
  VZoom := VBitmapConverter.GetZoom;

  VBitmapOnMapPixelRect := VBitmapConverter.GetRectInMapPixel;
  VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

  VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
  VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);

  VTileToDrawBmp := TCustomBitmap32.Create;
  try
    if not ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      while VTileIterator.Next(VTile) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          break;
        end;
        VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

        VCurrTileOnBitmapRect := VBitmapConverter.MapRect2LocalRect(VCurrTilePixelRect);

        VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
        VTileIsEmpty := True;
        VDrawMode := dmOpaque;

        FMainMapCS.BeginRead;
        try
          VMainMap := FMainMap;
        finally
          FMainMapCS.EndRead;
        end;

        if VMainMap <> nil then begin
          if DrawMap(VTileToDrawBmp, VMainMap.MapType, VGeoConvert, VZoom, VTile, VDrawMode, FUsePrevZoomAtMap, VRecolorConfig) then begin
            VTileIsEmpty := False;
            VDrawMode := dmBlend;
          end;
        end;
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          break;
        end;

        FLayersSetCS.BeginRead;
        try
          VLayersSet := FLayersSet;
        finally
          FLayersSetCS.EndRead;
        end;

        if VLayersSet <> nil then begin
          VEnum := VLayersSet.GetIterator;
          while VEnum.Next(1, VGUID, i) = S_OK do begin
            VItem := VLayersSet.GetMapTypeByGUID(VGUID);
            VMapType := VItem.GetMapType;
            if VMapType.IsBitmapTiles then begin
              if DrawMap(VTileToDrawBmp, VMapType, VGeoConvert, VZoom, VTile, VDrawMode, FUsePrevZoomAtLayer, VRecolorConfig) then begin
                VTileIsEmpty := False;
                VDrawMode := dmBlend;
              end;
            end;
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              break;
            end;
          end;
        end;

        if not VTileIsEmpty then begin
          VRecolorConfig.ProcessBitmap(VTileToDrawBmp);
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            break;
          end;
        end else begin
          VTileToDrawBmp.Clear(0);
        end;

        FLayer.Bitmap.Lock;
        try
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            break;
          end;
          FLayer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, VTileToDrawBmp);
          SetBitmapChanged;
        finally
          FLayer.Bitmap.UnLock;
        end;
      end;
    end;
  finally
    VTileToDrawBmp.Free;
  end;
end;

function TMiniMapLayer.DrawMap(ATargetBmp: TCustomBitmap32; AMapType: TMapType;
  AGeoConvert: ICoordConverter; AZoom: Byte; ATile: TPoint;
  ADrawMode: TDrawMode; AUsePre: Boolean;
  ARecolorConfig: IBitmapPostProcessingConfigStatic): Boolean;
var
  VBmp: TCustomBitmap32;
begin
  Result := False;
  VBmp := TCustomBitmap32.Create;
  try
    if AMapType.LoadTileUni(VBmp, ATile, AZoom, AGeoConvert, AUsePre, True, True, AMapType.CacheBitmap) then begin
      VBmp.DrawMode := ADrawMode;
      VBmp.CombineMode := cmMerge;
      VBmp.DrawTo(ATargetBmp);
      Result := True;
    end;
  finally
    VBmp.Free;
  end;
end;

function TMiniMapLayer.GetActualZoom(AVisualCoordConverter: ILocalCoordConverter): Byte;
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

function TMiniMapLayer.GetMapLayerLocationRect: TFloatRect;
var
  VSize: TPoint;
  VViewSize: TPoint;
begin
  VSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  VViewSize := ViewCoordConverter.GetLocalRectSize;
  Result.Right := VViewSize.X;
  Result.Bottom := VViewSize.Y - FConfig.BottomMargin;
  Result.Left := Result.Right - VSize.X;
  Result.Top := Result.Bottom - VSize.Y;
end;

procedure TMiniMapLayer.DoHide;
begin
  inherited;
  FViewRectDrawLayer.Visible := false;
  FViewRectDrawLayer.MouseEvents := false;

  FLeftBorder.Visible := False;
  FLeftBorder.MouseEvents := false;

  FTopBorder.Visible := False;

  FPlusButton.Visible := False;
  FPlusButton.MouseEvents := false;

  FMinusButton.Visible := False;
  FMinusButton.MouseEvents := false;
end;

procedure TMiniMapLayer.LayerMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  VLayerSize: TPoint;
  VBitmapCenter: TDoublePoint;
  VVisibleCenter: TDoublePoint;
  Vlocation: TFloatRect;
begin
  FParentMap.PopupMenu := nil;
  case button of
    mbRight: FParentMap.PopupMenu := FPopup;
    mbLeft: begin
      VLayerSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
      VBitmapCenter := DoublePoint(VLayerSize.X / 2, VLayerSize.Y / 2);
      Vlocation := FLayer.Location;
      VVisibleCenter.X := VBitmapCenter.X + Vlocation.Left;
      VVisibleCenter.Y := VBitmapCenter.Y + Vlocation.Top;
      FPosMoved := True;
      FViewRectMoveDelta := DoublePoint(X - VVisibleCenter.X, Y - VVisibleCenter.Y);
      DrawMainViewRect;
    end;
  end;
end;

procedure TMiniMapLayer.LayerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  VBitmapSize: TPoint;
  VBitmapCenter: TDoublePoint;
  VVisibleCenter: TDoublePoint;
  VLocation: TFloatRect;
begin
  if FPosMoved then begin
    VBitmapSize := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
    VBitmapCenter := DoublePoint(VBitmapSize.X / 2, VBitmapSize.Y / 2);

    VLocation := FLayer.Location;

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

procedure TMiniMapLayer.LayerMouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
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
    if FLayer.HitTest(X, Y) then begin
      VBitmapCoordConverter := LayerCoordConverter;
      Vlocation := FLayer.Location;
      VBitmapPos.X := X - Vlocation.Left;
      VBitmapPos.Y := Y - Vlocation.Top;
      VConverter := VBitmapCoordConverter.GetGeoConverter;
      VZoom := VBitmapCoordConverter.GetZoom;

      VMapPoint := VBitmapCoordConverter.LocalPixelFloat2MapPixelFloat(VBitmapPos);
      VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoom, false);
      VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoom);
      FViewRectMoveDelta := DoublePoint(0, 0);

      ViewPortState.ChangeLonLat(VLonLat);
    end else begin
      FViewRectMoveDelta := DoublePoint(0, 0);
      DrawMainViewRect;
    end;
  end;
  FPosMoved := False;
end;

procedure TMiniMapLayer.LeftBorderMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FLeftBorderMoved := true;
    FLeftBorderMovedClickDelta := FLayer.Location.Left - X;
  end;
end;

procedure TMiniMapLayer.LeftBorderMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  VNewWidth: Integer;
  VVisibleSize: TPoint;
begin
  if FLeftBorderMoved then begin
    VVisibleSize := ViewCoordConverter.GetLocalRectSize;
    VNewWidth := Trunc(FLayer.Location.Right - X - FLeftBorderMovedClickDelta);
    if VNewWidth < 40 then begin
      VNewWidth := 40;
    end;
    if VNewWidth > VVisibleSize.X then begin
      VNewWidth := VVisibleSize.X;
    end;
    if VNewWidth > VVisibleSize.Y then begin
      VNewWidth := VVisibleSize.Y;
    end;
    FConfig.Width := VNewWidth;
  end;
end;

procedure TMiniMapLayer.LeftBorderMouseUP(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FLeftBorderMoved then begin
    SetNeedRedraw;
    FLeftBorderMoved := False;
    ViewUpdate;
  end;
end;

procedure TMiniMapLayer.MinusButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FMinusButtonPressed := True;
  end;
end;

procedure TMiniMapLayer.MinusButtonMouseUP(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    FViewConfig.LockRead;
    try
      FUsePrevZoomAtMap := FViewConfig.UsePrevZoomAtMap;
      FUsePrevZoomAtLayer := FViewConfig.UsePrevZoomAtLayer;
    finally
      FViewConfig.UnlockRead;
    end;
    FConfig.LockRead;
    try
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
      FTopBorder.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      FLeftBorder.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      FLayer.Bitmap.Lock;
      try
        FLayer.Bitmap.MasterAlpha := FConfig.MasterAlpha;
      finally
        FLayer.Bitmap.Unlock;
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
  ViewUpdate;
end;

procedure TMiniMapLayer.OnDrawBitmap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier
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
    FLayersSetCS.BeginWrite;
    try
      FLayersSet := VNewLayersSet;
    finally
      FLayersSetCS.EndWrite;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMiniMapLayer.OnMainMapChange;
var
  VNewMainMap: IMapType;
begin
  ViewUpdateLock;
  try
    VNewMainMap := FConfig.MapsConfig.GetActiveMiniMap;
    FMainMapCS.BeginWrite;
    try
      FMainMap := VNewMainMap;
    finally
      FMainMapCS.EndWrite;
    end;
    SetNeedRedraw;
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TMiniMapLayer.OnTimer;
begin
  if InterlockedExchange(FUpdateCounter, 0) > 0 then begin
    FLayer.Changed;
  end;
end;

procedure TMiniMapLayer.PlusButtonMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then begin
    FPlusButtonPressed := True;
  end;
end;

procedure TMiniMapLayer.PlusButtonMouseUP(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
  InterlockedIncrement(FUpdateCounter);
end;

procedure TMiniMapLayer.SetLayerCoordConverter(AValue: ILocalCoordConverter);
var
  VNewSize: TPoint;
begin
  VNewSize := GetLayerSizeForView(AValue);
  FLayer.Bitmap.Lock;
  try
    if Visible then begin
      FClearStrategy := FClearStrategyFactory.GetStrategy(LayerCoordConverter, AValue, FLayer.Bitmap, FClearStrategy);
    end else begin
      FClearStrategy := nil;
    end;
    if (FLayer.Bitmap.Width <> VNewSize.X) or (FLayer.Bitmap.Height <> VNewSize.Y) then begin
      SetNeedUpdateLayerSize;
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;
  if (LayerCoordConverter = nil) or (not LayerCoordConverter.GetIsSameConverter(AValue)) then begin
    SetNeedRedraw;
  end;
  SetNeedUpdateLocation;
  inherited;
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

  FLeftBorder.Visible := True;
  FLeftBorder.MouseEvents := True;

  FTopBorder.Visible := True;

  FPlusButton.Visible := True;
  FPlusButton.MouseEvents := True;

  FMinusButton.Visible := True;
  FMinusButton.MouseEvents := True;
end;

procedure TMiniMapLayer.DoUpdateLayerLocation(ANewLocation: TFloatRect);
var
  VRect: TFloatRect;
begin
  inherited;
  FViewRectDrawLayer.Location := ANewLocation;

  VRect.Left := ANewLocation.Left - 5;
  VRect.Top := ANewLocation.Top - 5;
  VRect.Right := ANewLocation.Left;
  VRect.Bottom := ANewLocation.Bottom;
  FLeftBorder.Location := VRect;

  VRect.Left := ANewLocation.Left;
  VRect.Top := ANewLocation.Top - 5;
  VRect.Right := ANewLocation.Right;
  VRect.Bottom := ANewLocation.Top;
  FTopBorder.Location := VRect;

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

procedure TMiniMapLayer.DoUpdateLayerSize(ANewSize: TPoint);
var
  VBitmapSizeInPixel: TPoint;
  VBorderWidth: Integer;
begin
  inherited;
  VBorderWidth := 5;
  VBitmapSizeInPixel := Point(FLayer.Bitmap.Width, FLayer.Bitmap.Height);
  FViewRectDrawLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
  if (FLeftBorder.Bitmap.Height <> VBitmapSizeInPixel.Y + VBorderWidth) then begin
    FLeftBorder.Bitmap.Lock;
    try
      FLeftBorder.Bitmap.SetSize(VBorderWidth, VBitmapSizeInPixel.Y + VBorderWidth);
      FLeftBorder.Bitmap.Clear(clLightGray32);
      FLeftBorder.Bitmap.VertLineS(0, 0, VBitmapSizeInPixel.Y + VBorderWidth - 1, clBlack32);
      FLeftBorder.Bitmap.VertLineS(VBorderWidth - 1, VBorderWidth - 1, VBitmapSizeInPixel.Y + VBorderWidth - 1, clBlack32);
      FLeftBorder.Bitmap.HorzLineS(0, 0, VBorderWidth - 1, clBlack32);
      FLeftBorder.bitmap.Pixel[2, VBorderWidth + (VBitmapSizeInPixel.Y div 2) - 6] := 0;
      FLeftBorder.bitmap.Pixel[2, VBorderWidth + (VBitmapSizeInPixel.Y div 2) - 2] := 0;
      FLeftBorder.bitmap.Pixel[2, VBorderWidth + (VBitmapSizeInPixel.Y div 2) + 2] := 0;
      FLeftBorder.bitmap.Pixel[2, VBorderWidth + (VBitmapSizeInPixel.Y div 2) + 6] := 0;
    finally
      FLeftBorder.Bitmap.Unlock;
    end;
  end;
  if (FTopBorder.Bitmap.Width <> VBitmapSizeInPixel.X) then begin
    FTopBorder.Bitmap.Lock;
    try
      FTopBorder.Bitmap.SetSize(VBitmapSizeInPixel.X, VBorderWidth);
      FTopBorder.Bitmap.Clear(clLightGray32);
      FTopBorder.Bitmap.HorzLineS(0, 0, VBitmapSizeInPixel.X, clBlack32);
      FTopBorder.Bitmap.HorzLineS(0, VBorderWidth - 1, VBitmapSizeInPixel.X, clBlack32);
    finally
      FTopBorder.Bitmap.Unlock;
    end;
  end;
end;

end.


