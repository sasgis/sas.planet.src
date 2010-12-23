unit u_MiniMapLayer;

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  TBX,
  TB2Item,
  GR32,
  GR32_Image,
  GR32_Layers,
  i_JclNotify,
  t_GeoTypes,
  i_MapTypes,
  i_MapTypeIconsList,
  i_IMapTypeMenuItmesList,
  i_IActiveMapsConfig,
  i_IMapChangeMessage,
  i_IHybrChangeMessage,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_ILocalCoordConverter,
  i_ILocalCoordConverterFactorySimpe,
  u_MapViewPortState,
  UMapType,
  u_WindowLayerWithPos;

type
  TMiniMapLayer = class(TWindowLayerFixedSizeWithBitmap)
  private
    FParentMap: TImage32;
    FBitmapCoordConverter: ILocalCoordConverter;
    FBitmapCoordConverterFactory: ILocalCoordConverterFactorySimpe;

    FWidth: Integer;

    FMapsActive: IActiveMapWithHybrConfig;
    FPopup: TTBXPopupMenu;
    FIconsList: IMapTypeIconsList;
    FMasterAlpha: Integer;
    FZoomDelta: integer;
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

    FDefoultMap: TCustomBitmap32;
    FMiniMapSameAsMain: TTBXItem;
    FMapsList: IMapTypeList;
    FLayersList: IMapTypeList;
    FMapsItemsList: IMapTypeMenuItmesList;
    FLayersItemsList: IMapTypeMenuItmesList;
    FMainMapChangeListener: IJclListener;
    FMapChangeListener: IJclListener;
    FHybrChangeListener: IJclListener;
    FBottomMargin: Integer;


    procedure DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
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

    procedure LoadBitmaps;
    procedure BuildPopUpMenu;
    procedure BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
    procedure CreateLayers(AParentMap: TImage32);
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
    procedure SameAsMainClick(Sender: TObject);
    procedure OnNotifyMapChange(msg: IMapChangeMessage); virtual;
    procedure OnNotifyHybrChange(msg: IHybrChangeMessage); virtual;
    procedure OnNotifyMainMapChange(msg: IMapChangeMessage); virtual;
    procedure SetMasterAlpha(value:integer);
    procedure BuildMapsLists;
    function BuildBitmapCoordConverter(ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
  protected
    function GetMapLayerLocationRect: TFloatRect; override;
    procedure DoShow; override;
    procedure DoHide; override;
    procedure DoRedraw; override;
    procedure DoPosChange(ANewVisualCoordConverter: ILocalCoordConverter); override;
    procedure DoUpdateLayerSize(ANewSize: TPoint); override;
    procedure DoUpdateLayerLocation(ANewLocation: TFloatRect); override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
    property BottomMargin: Integer read FBottomMargin write FBottomMargin;
    property MasterAlpha: integer read FMasterAlpha write SetMasterAlpha;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  Types,
  GR32_Polygons,
  u_JclNotify,
  c_ZeroGUID,
  i_ICoordConverter,
  Uimgfun,
  Ugeofun,
  UResStrings,
  i_ActiveMapsConfigSaveLoad,
  u_GlobalState,
  u_LocalCoordConverterFactorySimpe,
  u_MapTypeList,
  u_MapTypeMenuItemsGeneratorBasic,
  u_ActiveMapWithHybrConfig,
  u_MapsConfigByConfigDataProvider,
  u_MiniMapMenuItemsFactory;

type
  TMiniMapListener = class(TJclBaseListener)
  private
    FOwnerItem: TMiniMapLayer;
  public
    constructor Create(AOwnerItem: TMiniMapLayer);
  end;

{ TMiniMapListener }

constructor TMiniMapListener.Create(
  AOwnerItem: TMiniMapLayer);
begin
  FOwnerItem := AOwnerItem;
end;

type
  TMiniMapMapChangeListener = class(TMiniMapListener)
  public
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

{ TMiniMapMapChangeListener }

procedure TMiniMapMapChangeListener.Notification(
  msg: IJclNotificationMessage);
begin
  FOwnerItem.OnNotifyMapChange(msg as IMapChangeMessage);
end;

type
  TMiniMapHybrChangeListener = class(TMiniMapListener)
  public
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

{ TMiniMapHybrChangeListener }

procedure TMiniMapHybrChangeListener.Notification(
  msg: IJclNotificationMessage);
begin
  FOwnerItem.OnNotifyHybrChange(msg as IHybrChangeMessage);
end;

type
  TMiniMapMainMapChangeListener = class(TMiniMapListener)
  public
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

{ TMiniMapMainMapChangeListener }

procedure TMiniMapMainMapChangeListener.Notification(
  msg: IJclNotificationMessage);
begin
  FOwnerItem.OnNotifyMainMapChange(msg as IMapChangeMessage);
end;

{ TMapMainLayer }

constructor TMiniMapLayer.Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
begin
  inherited;
  FBitmapCoordConverterFactory := TLocalCoordConverterFactorySimpe.Create;
  FParentMap := AParentMap;
  FIconsList := GState.MapTypeIcons18List;

  FViewRectMoveDelta := DoublePoint(0, 0);

  BuildMapsLists;

  FMapsActive := TActiveMapWithHybrConfig.Create(True, FMapsList, FLayersList);

  FZoomDelta := 4;

  FPopup := TTBXPopupMenu.Create(AParentMap);
  FPopup.Name := 'PopupMiniMap';
  FPopup.Images := FIconsList.GetImageList;

  CreateLayers(AParentMap);
  MasterAlpha := 150;

  LoadBitmaps;
  BuildPopUpMenu;
  FWidth := 100;
  UpdateLayerSize(Point(FWidth, FWidth));
end;

destructor TMiniMapLayer.Destroy;
begin
  FreeAndNil(FDefoultMap);
  FMapsActive.MapChangeNotifier.Remove(FMapChangeListener);
  FMapsActive.HybrChangeNotifier.Remove(FHybrChangeListener);
  GState.ViewState.MapChangeNotifier.Remove(FMainMapChangeListener);
  FMapChangeListener := nil;
  FHybrChangeListener := nil;
  FMainMapChangeListener := nil;
  FMapsItemsList := nil;
  FLayersItemsList := nil;
  FMapsList := nil;
  FLayersList := nil;
  FMapsActive := nil;
  FBitmapCoordConverterFactory := nil;
  inherited;
end;

function TMiniMapLayer.BuildBitmapCoordConverter(
  ANewVisualCoordConverter: ILocalCoordConverter): ILocalCoordConverter;
var
  VVisualMapCenter: TDoublePoint;
  VZoom: Byte;
  VSourceZoom: Byte;
  VConverter: ICoordConverter;
  VVisualMapCenterInRelative: TDoublePoint;
  VVisualMapCenterInLayerMap: TDoublePoint;
  VLocalTopLeftAtMap: TDoublePoint;
  VLayerSize: TPoint;
begin
  VVisualMapCenter := ANewVisualCoordConverter.GetCenterMapPixelFloat;
  VSourceZoom := ANewVisualCoordConverter.GetZoom;
  VConverter := ANewVisualCoordConverter.GetGeoConverter;
  VVisualMapCenterInRelative := VConverter.PixelPosFloat2Relative(VVisualMapCenter, VSourceZoom);
  VZoom := GetActualZoom(ANewVisualCoordConverter);
  VVisualMapCenterInLayerMap := VConverter.Relative2PixelPosFloat(VVisualMapCenterInRelative, VZoom);
  VLayerSize := LayerSize;
  VLocalTopLeftAtMap.X := VVisualMapCenterInLayerMap.X - (VLayerSize.X / 2);
  VLocalTopLeftAtMap.Y := VVisualMapCenterInLayerMap.Y - (VLayerSize.Y / 2);


  Result := FBitmapCoordConverterFactory.CreateConverter(
    Rect(0, 0, VLayerSize.X, VLayerSize.Y),
    VZoom,
    VConverter,
    DoublePoint(1, 1),
    VLocalTopLeftAtMap
  );
end;

procedure TMiniMapLayer.BuildMapsLists;
var
  VSourceList: IMapTypeList;
  VList: TMapTypeList;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  i: Cardinal;
  VMapType: IMapType;
  VMap: TMapType;
begin
  VList := TMapTypeList.Create(True);
  FMapsList := VList;
  VSourceList := GState.MapType.MapsList;
  VEnum := VSourceList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VMapType := VSourceList.GetMapTypeByGUID(VGUID);
    VMap := VMapType.MapType;
    if VMap.IsBitmapTiles and VMap.IsCanShowOnSmMap then begin
      VList.Add(VMapType);
    end;
  end;

  VList := TMapTypeList.Create(False);
  FLayersList := VList;
  VSourceList := GState.MapType.LayersList;
  VEnum := VSourceList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VMapType := VSourceList.GetMapTypeByGUID(VGUID);
    VMap := VMapType.MapType;
    if VMap.IsBitmapTiles and VMap.IsCanShowOnSmMap then begin
      VList.Add(VMapType);
    end;
  end;
end;

procedure TMiniMapLayer.CreateLayers(AParentMap: TImage32);
begin
  FLeftBorder := TBitmapLayer.Create(AParentMap.Layers);
  FLeftBorder.Visible := False;
  FLeftBorder.MouseEvents := false;
  FLeftBorder.Cursor := crSizeNWSE;
  FLeftBorder.Bitmap.DrawMode := dmBlend;
  FLeftBorder.Bitmap.CombineMode := cmMerge;
  FLeftBorder.OnMouseDown := LeftBorderMouseDown;
  FLeftBorder.OnMouseUp := LeftBorderMouseUP;
  FLeftBorder.OnMouseMove := LeftBorderMouseMove;
  FLeftBorderMoved := False;

  FTopBorder := TBitmapLayer.Create(AParentMap.Layers);
  FTopBorder.Visible := False;
  FTopBorder.MouseEvents := false;
  FTopBorder.Bitmap.DrawMode := dmBlend;
  FTopBorder.Bitmap.CombineMode := cmMerge;

  FViewRectDrawLayer := TBitmapLayer.Create(AParentMap.Layers);
  FViewRectDrawLayer.Visible := False;
  FViewRectDrawLayer.MouseEvents := false;
  FViewRectDrawLayer.Bitmap.DrawMode := dmBlend;
  FViewRectDrawLayer.Bitmap.CombineMode := cmMerge;
  FViewRectDrawLayer.OnMouseDown := LayerMouseDown;
  FViewRectDrawLayer.OnMouseUp := LayerMouseUP;
  FViewRectDrawLayer.OnMouseMove := LayerMouseMove;

  FPlusButton := TBitmapLayer.Create(AParentMap.Layers);
  FPlusButton.Visible := False;
  FPlusButton.MouseEvents := false;
  FPlusButton.Bitmap.DrawMode := dmBlend;
  FPlusButton.Bitmap.CombineMode := cmMerge;
  FPlusButton.OnMouseDown := PlusButtonMouseDown;
  FPlusButton.OnMouseUp := PlusButtonMouseUP;
  FPlusButton.Cursor := crHandPoint;
  FPlusButtonPressed := False;

  FMinusButton := TBitmapLayer.Create(AParentMap.Layers);
  FMinusButton.Visible := False;
  FMinusButton.MouseEvents := false;
  FMinusButton.Bitmap.DrawMode := dmBlend;
  FMinusButton.Bitmap.CombineMode := cmMerge;
  FMinusButton.OnMouseDown := MinusButtonMouseDown;
  FMinusButton.OnMouseUp := MinusButtonMouseUP;
  FMinusButton.Cursor := crHandPoint;
  FMinusButtonPressed := False;
end;

procedure TMiniMapLayer.LoadBitmaps;
begin
  FDefoultMap := TCustomBitmap32.Create;
  GState.LoadBitmapFromRes('MAINMAP', FDefoultMap);
  GState.LoadBitmapFromRes('ICONI', FPlusButton.Bitmap);
  FPlusButton.Bitmap.DrawMode := dmTransparent;
  GState.LoadBitmapFromRes('ICONII', FMinusButton.Bitmap);
  FMinusButton.Bitmap.DrawMode := dmTransparent;
end;

procedure TMiniMapLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
  VMapConfigLoader: IActiveMapsConfigLoader;
  VBitmapSize: TPoint;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('MINIMAP');
  if VConfigProvider <> nil then begin
    VBitmapSize := LayerSize;
    VBitmapSize.X := VConfigProvider.ReadInteger('Width', VBitmapSize.X);
    VBitmapSize.Y := VConfigProvider.ReadInteger('Height', VBitmapSize.Y);
    FZoomDelta := VConfigProvider.ReadInteger('ZoomDelta', FZoomDelta);
    MasterAlpha := VConfigProvider.ReadInteger('Alpha', 150);
    VMapConfigLoader := TMapsConfigLoaderByConfigDataProvider.Create(VConfigProvider.GetSubItem('Maps'));
    try
      VMapConfigLoader.Load(FMapsActive);
    finally
      VMapConfigLoader := nil;
    end;
    DoUpdateLayerSize(VBitmapSize);
    Visible := VConfigProvider.ReadBool('Visible', True);
  end else begin
    Visible := True;
  end;
end;

procedure TMiniMapLayer.BuildPopUpMenu;
var
  VMenuItem: TTBXItem;
  VSubMenuItem: TTBXSubmenuItem;
  VLayersSubMenu: TTBXSubmenuItem;
begin
  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.Name := 'MiniMapSameAsMain';
  VMenuItem.OnAdjustFont := AdjustFont;
  VMenuItem.OnClick := SameAsMainClick;
  VMenuItem.Caption := SAS_STR_MiniMapAsMainMap;
  VMenuItem.Hint := '';
  VMenuItem.Checked := true;
  FPopup.Items.Add(VMenuItem);
  FMiniMapSameAsMain := VMenuItem;

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
  VGenerator := TMapMenuGeneratorBasic.Create;
  try
    VGenerator.List := FMapsList;
    VGenerator.RootMenu := AMapssSubMenu;
    VGenerator.ItemsFactory := TMiniMapMenuItemsFactory.Create(FMapsActive, AMapssSubMenu, AdjustFont, FIconsList);
    FMapsItemsList := VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
  VGenerator := TMapMenuGeneratorBasic.Create;
  try
    VGenerator.List := FLayersList;
    VGenerator.RootMenu := ALayersSubMenu;
    VGenerator.ItemsFactory := TMiniMapMenuItemsFactory.Create(FMapsActive, ALayersSubMenu, AdjustFont, FIconsList);
    FLayersItemsList := VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
  FMapChangeListener := TMiniMapMapChangeListener.Create(Self);
  FMapsActive.MapChangeNotifier.Add(FMapChangeListener);
  FHybrChangeListener := TMiniMapHybrChangeListener.Create(Self);
  FMapsActive.HybrChangeNotifier.Add(FHybrChangeListener);
  FMainMapChangeListener := TMiniMapMainMapChangeListener.Create(Self);
  GState.ViewState.MapChangeNotifier.Add(FMainMapChangeListener);
end;

procedure TMiniMapLayer.AdjustFont(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
begin
  if TTBXItem(Item).Checked then begin
    TTBXItem(Item).FontSettings.Bold := tsTrue;
  end else begin
    TTBXItem(Item).FontSettings.Bold := tsDefault;
  end;
end;

procedure TMiniMapLayer.SameAsMainClick(Sender: TObject);
begin
  Assert(Sender is TTBXItem, 'Глюки однако. Этот обработчик не предназначен для этого контрола');
  FMapsActive.SelectMapByGUID(CGUID_Zero);
end;


procedure TMiniMapLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
  VMapConfigSaver: IActiveMapsConfigSaver;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('MINIMAP');
  VConfigProvider.WriteInteger('Width', LayerSize.X);
  VConfigProvider.WriteInteger('Height', LayerSize.Y);
  VConfigProvider.WriteInteger('ZoomDelta', FZoomDelta);
  VConfigProvider.WriteInteger('Alpha', MasterAlpha);
  VConfigProvider.WriteBool('Visible', Visible);

  VMapConfigSaver := TMapsConfigSaverByConfigDataProvider.Create(VConfigProvider.GetOrCreateSubItem('Maps'));
  try
    VMapConfigSaver.Save(FMapsActive);
  finally
    VMapConfigSaver := nil;
  end;
end;

procedure TMiniMapLayer.DoRedraw;
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
begin
  inherited;
  FBitmapCoordConverter := BuildBitmapCoordConverter(FVisualCoordConverter);
  FLayer.Bitmap.Clear(Color32(GState.BGround));
  VGUID := FMapsActive.SelectedMapGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    VMapType := GState.ViewState.GetCurrentMap;
  end else begin
    VItem := FMapsActive.MapsList.GetMapTypeByGUID(VGUID);
    VMapType := VItem.MapType;
  end;

  DrawMap(VMapType, dmOpaque);
  VEnum := FLayersList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    if FMapsActive.IsHybrGUIDSelected(VGUID) then begin
      VItem := FLayersList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      DrawMap(VMapType, dmBlend);
    end;
  end;
  DrawMainViewRect;
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
begin
  FViewRectDrawLayer.Bitmap.Clear(clBlack);
  VVisualCoordConverter := FVisualCoordConverter;
  VBitmapCoordConverter := FBitmapCoordConverter;
  VGeoConvert := VVisualCoordConverter.GetGeoConverter;
  if VGeoConvert <> nil then begin
    if FZoomDelta > 0 then begin
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

      VBitmapSize := LayerSize;
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
          with VPolygon.Outline do try
            with Grow(Fixed(3.2 / 2), 0.5) do try
              FillMode := pfWinding;
              DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clNavy32, (FZoomDelta)*43));
            finally
              Free;
            end;
          finally
            Free;
          end;
          VPolygon.DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clWhite32, (FZoomDelta) * 35));
        finally
          VPolygon.Free;
        end;
      end;
    end;
  end;
end;

procedure TMiniMapLayer.DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
var
  VZoom: Byte;
  VSourceMapType: TMapType;
  VBmp: TCustomBitmap32;

  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VBitmapOnMapPixelRect: TDoubleRect;

  {
    Географические координаты растра
  }
  VSourceLonLatRect: TDoubleRect;

  {
    Прямоугольник пикселов текущего зума, покрывающий растр, в кооординатах
    карты для которой строится слой
  }
  VPixelSourceRect: TRect;

  {
    Прямоугольник тайлов текущего зума, покрывающий растр, в кооординатах
    карты, для которой строится слой
  }
  VTileSourceRect: TRect;

  {
    Текущий тайл в кооординатах карты, для которой строится слой
  }
  VTile: TPoint;

  {
    Прямоугольник пикслов текущего тайла в кооординатах текущей карты
  }
  VCurrTilePixelRectSource: TRect;

  {
    Прямоугольник пикслов текущего тайла в кооординатах карты,
    для которой строится слой
  }
  VCurrTilePixelRect: TRect;

  {
    Прямоугольник пикслов текущего тайла в кооординатах текущего растра
  }
  VCurrTilePixelRectAtBitmap: TRect;

  {
    Прямоугольник тайла подлежащий отображению на текущий растр
  }
  VTilePixelsToDraw: TRect;


  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  i, j: integer;
  VUsePre: Boolean;
  VVisualConverter: ILocalCoordConverter;
  VBitmapConverter: ILocalCoordConverter;
begin
  if AMapType.asLayer then begin
    VUsePre := GState.UsePrevZoomLayer;
  end else begin
    VUsePre := GState.UsePrevZoom;
  end;
  VBmp := TCustomBitmap32.Create;
  try
    VVisualConverter := FBitmapCoordConverter;
    VBitmapConverter := FBitmapCoordConverter;
    VGeoConvert := VVisualConverter.GetGeoConverter;
    VZoom := VBitmapConverter.GetZoom;
    VSourceMapType := AMapType;
    VSourceGeoConvert := VSourceMapType.GeoConvert;

    VBitmapOnMapPixelRect := VVisualConverter.GetRectInMapPixelFloat;
    VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);

    VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
    VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
    VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);

    for i := VTileSourceRect.Left to VTileSourceRect.Right - 1 do begin
      VTile.X := i;
      for j := VTileSourceRect.Top to VTileSourceRect.Bottom - 1 do begin
        VTile.Y := j;
        VCurrTilePixelRectSource := VSourceGeoConvert.TilePos2PixelRect(VTile, VZoom);
        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRectSource.Right - VCurrTilePixelRectSource.Left;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRectSource.Bottom - VCurrTilePixelRectSource.Top;

        if VCurrTilePixelRectSource.Left < VPixelSourceRect.Left then begin
          VTilePixelsToDraw.Left := VPixelSourceRect.Left - VCurrTilePixelRectSource.Left;
          VCurrTilePixelRectSource.Left := VPixelSourceRect.Left;
        end;

        if VCurrTilePixelRectSource.Top < VPixelSourceRect.Top then begin
          VTilePixelsToDraw.Top := VPixelSourceRect.Top - VCurrTilePixelRectSource.Top;
          VCurrTilePixelRectSource.Top := VPixelSourceRect.Top;
        end;

        if VCurrTilePixelRectSource.Right > VPixelSourceRect.Right then begin
          VTilePixelsToDraw.Right := VTilePixelsToDraw.Right - (VCurrTilePixelRectSource.Right - VPixelSourceRect.Right);
          VCurrTilePixelRectSource.Right := VPixelSourceRect.Right;
        end;

        if VCurrTilePixelRectSource.Bottom > VPixelSourceRect.Bottom then begin
          VTilePixelsToDraw.Bottom := VTilePixelsToDraw.Bottom - (VCurrTilePixelRectSource.Bottom - VPixelSourceRect.Bottom);
          VCurrTilePixelRectSource.Bottom := VPixelSourceRect.Bottom;
        end;

        VCurrTilePixelRect.TopLeft := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.TopLeft, VZoom, VGeoConvert);
        VCurrTilePixelRect.BottomRight := VSourceGeoConvert.PixelPos2OtherMap(VCurrTilePixelRectSource.BottomRight, VZoom, VGeoConvert);

        VCurrTilePixelRectAtBitmap := VBitmapConverter.MapRect2LocalRect(VCurrTilePixelRect);
        if VSourceMapType.LoadTileOrPreZ(VBmp, VTile, VZoom, true, False, VUsePre) then begin
          Gamma(VBmp);
        end;
        FLayer.Bitmap.Lock;
        try
          VBmp.DrawMode := ADrawMode;
          FLayer.Bitmap.Draw(VCurrTilePixelRectAtBitmap, VTilePixelsToDraw, Vbmp);
        finally
          FLayer.Bitmap.UnLock;
        end;
      end;
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
  VZoomDelta := FZoomDelta;
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
  VSize := LayerSize;
  VViewSize := FVisualCoordConverter.GetLocalRectSize;
  Result.Right := VViewSize.X;
  Result.Bottom := VViewSize.Y - FBottomMargin;
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

procedure TMiniMapLayer.DoPosChange(
  ANewVisualCoordConverter: ILocalCoordConverter);
begin
  inherited;
  Redraw;
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
      VLayerSize := LayerSize;
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
    VBitmapSize := LayerSize;
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
      VBitmapCoordConverter := FBitmapCoordConverter;
      Vlocation := FLayer.Location;
      VBitmapPos.X := X - Vlocation.Left;
      VBitmapPos.Y := Y - Vlocation.Top;
      VConverter := VBitmapCoordConverter.GetGeoConverter;
      VZoom := VBitmapCoordConverter.GetZoom;

      VMapPoint := VBitmapCoordConverter.LocalPixelFloat2MapPixelFloat(VBitmapPos);
      VConverter.CheckPixelPosFloatStrict(VMapPoint, VZoom, false);
      VLonLat := VConverter.PixelPosFloat2LonLat(VMapPoint, VZoom);
      FViewRectMoveDelta := DoublePoint(0, 0);

      FViewPortState.LockWrite;
      FViewPortState.ChangeLonLatAndUnlock(VLonLat);
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
    VVisibleSize := FVisualCoordConverter.GetLocalRectSize;
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
    UpdateLayerSize(Point(VNewWidth, VNewWidth));
    Redraw;
  end;
end;

procedure TMiniMapLayer.LeftBorderMouseUP(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FLeftBorderMoved then begin
    Redraw;
    FLeftBorderMoved := False;
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
      if FMinusButton.HitTest(X, Y) then begin
        if (FZoomDelta < 10) then begin
          Inc(FZoomDelta);
          Redraw;
        end;
      end;
      FMinusButtonPressed := False;
    end;
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
        if (FZoomDelta > -2) then begin
          Dec(FZoomDelta);
          Redraw;
        end;
      end;
      FPlusButtonPressed := False;
    end;
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
  Polygon: TPolygon32;
begin
  inherited;
  VBitmapSizeInPixel := LayerSize;
  FLayer.Bitmap.Lock;
  try
    if (FLayer.Bitmap.Width <> VBitmapSizeInPixel.X) or (FLayer.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
      FLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
    end;
  finally
    FLayer.Bitmap.Unlock;
  end;

  FViewRectDrawLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
  if (FLeftBorder.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
    FLeftBorder.Bitmap.Lock;
    try
      FLeftBorder.Bitmap.SetSize(5, VBitmapSizeInPixel.Y + 5);
      FLeftBorder.Bitmap.Clear(clLightGray32);
      Polygon := TPolygon32.Create;
      try
        Polygon.Antialiased := False;
        Polygon.Closed := false;
        Polygon.Add(FixedPoint(4, 0));
        Polygon.Add(FixedPoint(0, 0));
        Polygon.Add(FixedPoint(0, VBitmapSizeInPixel.Y + 5));
        Polygon.Add(FixedPoint(4, VBitmapSizeInPixel.Y + 5));
        Polygon.Add(FixedPoint(4, 3));
        Polygon.DrawEdge(FLeftBorder.Bitmap, clBlack32);
      finally
        Polygon.Free;
      end;
      FLeftBorder.bitmap.Pixel[2, 5 + (VBitmapSizeInPixel.Y div 2) - 6] := clBlack;
      FLeftBorder.bitmap.Pixel[2, 5 + (VBitmapSizeInPixel.Y div 2) - 2] := clBlack;
      FLeftBorder.bitmap.Pixel[2, 5 + (VBitmapSizeInPixel.Y div 2) + 2] := clBlack;
      FLeftBorder.bitmap.Pixel[2, 5 + (VBitmapSizeInPixel.Y div 2) + 6] := clBlack;
    finally
      FLeftBorder.Bitmap.Unlock;
    end;
  end;
  if (FTopBorder.Bitmap.Width <> VBitmapSizeInPixel.X) then begin
    FTopBorder.Bitmap.Lock;
    try
      FTopBorder.Bitmap.SetSize(VBitmapSizeInPixel.X, 5);
      FTopBorder.Bitmap.Clear(clLightGray32);
      Polygon := TPolygon32.Create;
      try
        Polygon.Antialiased := False;
        Polygon.Closed := false;
        Polygon.Add(FixedPoint(0, 0));
        Polygon.Add(FixedPoint(VBitmapSizeInPixel.X, 0));
        Polygon.Add(FixedPoint(VBitmapSizeInPixel.X, 4));
        Polygon.Add(FixedPoint(-1, 4));
        Polygon.DrawEdge(FTopBorder.Bitmap, clBlack32);
      finally
        Polygon.Free;
      end;
    finally
      FTopBorder.Bitmap.Unlock;
    end;
  end;
end;

procedure TMiniMapLayer.OnNotifyHybrChange(msg: IHybrChangeMessage);
begin
  Redraw;
end;

procedure TMiniMapLayer.OnNotifyMapChange(msg: IMapChangeMessage);
begin
  if msg.GetNewMap = nil then begin
    FMiniMapSameAsMain.Checked := True;
    GState.ViewState.MapChangeNotifier.Add(FMainMapChangeListener);
  end else begin
    FMiniMapSameAsMain.Checked := False;
    GState.ViewState.MapChangeNotifier.Remove(FMainMapChangeListener);
  end;
  Redraw;
end;

procedure TMiniMapLayer.OnNotifyMainMapChange(msg: IMapChangeMessage);
begin
  if IsEqualGUID(FMapsActive.SelectedMapGUID, CGUID_Zero) then begin
    Redraw;
  end;
end;

procedure TMiniMapLayer.SetMasterAlpha(value:integer);
begin
  FMasterAlpha:=value;
  FMinusButton.Bitmap.MasterAlpha:=FMasterAlpha;
  FPlusButton.Bitmap.MasterAlpha:=FMasterAlpha;
  FTopBorder.Bitmap.MasterAlpha:=FMasterAlpha;
  FLeftBorder.Bitmap.MasterAlpha:=FMasterAlpha;
  FLayer.Bitmap.MasterAlpha:=FMasterAlpha;
end;

end.


