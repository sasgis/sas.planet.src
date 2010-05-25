unit u_MiniMapLayer;

interface

uses
  Windows,
  Classes,
  Graphics,
  Controls,
  ImgList,
  TBX,
  TB2Item,
  GR32,
  GR32_Image,
  GR32_Layers,
  JclNotify,
  t_GeoTypes,
  i_MapTypes,
  i_IMapTypeMenuItmesList,
  i_IActiveMapsConfig,
  i_IMapChangeMessage,
  i_IHybrChangeMessage,
  UMapType,
  u_MapLayerBasic;

type
  TChangePosEvent = procedure (APoint: TPoint; AZoom: Byte) of object;

  TMiniMapLayer = class(TMapLayerBasic)
  protected
    FOnChangePos: TChangePosEvent;
    FMapsActive: IActiveMapsConfig;
    FPopup: TTBXPopupMenu;
    FAlpha: Integer;
    FZoomDelta: integer;
    FPlusButton: TBitmapLayer;
    FPlusButtonPressed: Boolean;
    FMinusButton: TBitmapLayer;
    FMinusButtonPressed: Boolean;
    FLeftBorder: TBitmapLayer;
    FLeftBorderMoved: Boolean;
    FTopBorder: TBitmapLayer;
    FViewRectDrawLayer: TBitmapLayer;
    FPosMoved: Boolean;
    FViewRectMoveDelta: TPoint;

    FDefoultMap: TBitmap32;
    FBitmapSize: TPoint;
    FMiniMapSameAsMain: TTBXItem;
    FMapsList: IMapTypeList;
    FLayersList: IMapTypeList;
    FMapsItemsList: IMapTypeMenuItmesList;
    FLayersItemsList: IMapTypeMenuItmesList;
    FMainMapChangeListener: IJclListener;
    FMapChangeListener: IJclListener;
    FHybrChangeListener: IJclListener;


    procedure DoChangePos(APoint: TPoint);
    procedure DrawMap(AMapType: TMapType; ADrawMode: TDrawMode);
    procedure DrawMainViewRect;
    procedure DoRedraw; override;

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

    function GetBitmapSizeInPixel: TPoint; override;
    function GetFreezePointInVisualPixel: TPoint; override;
    function GetFreezePointInBitmapPixel: TPoint; override;
    function GetActualZoom: Byte;

    function BitmapPixel2MapPixel(Pnt: TPoint): TPoint; overload; override;
    function BitmapPixel2MapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; override;
    function MapPixel2BitmapPixel(Pnt: TPoint): TPoint; overload; override;
    function MapPixel2BitmapPixel(Pnt: TExtendedPoint): TExtendedPoint; overload; override;

    procedure LoadBitmapFromRes(Instance: HInst; const Name: String; Abmp: TBitmap32);
    procedure LoadBitmaps;
    procedure BuildPopUpMenu;
    procedure BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
    procedure DoResize; override;
    procedure DoResizeBitmap; override;
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
    procedure SameAsMainClick(Sender: TObject);
    procedure OnNotifyMapChange(msg: IMapChangeMessage); virtual;
    procedure OnNotifyHybrChange(msg: IHybrChangeMessage); virtual;
    procedure OnNotifyMainMapChange(msg: IMapChangeMessage); virtual;
  public
    constructor Create(AParentMap: TImage32; ACenter: TPoint; AImages: TCustomImageList);
    destructor Destroy; override;
    procedure Show; override;
    procedure Hide; override;
    property OnChangePos: TChangePosEvent read FOnChangePos write FOnChangePos;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  Math,
  GR32_Polygons,
  i_ICoordConverter,
  i_BitmapTileSaveLoad,
  i_IMapTypeMenuItem,
  Uimgfun,
  u_GeoToStr,
  u_GlobalState,
  u_WindowLayerBasic,
  u_MapTypeMenuItemsGeneratorBasic,
  u_MapTypeListGeneratorFromFullListForMiniMap,
  u_MapTypeBasic,
  u_ActiveMapsConfigBasic,
  i_IBitmapTypeExtManager, Types;

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

constructor TMiniMapLayer.Create(AParentMap: TImage32; ACenter: TPoint; AImages: TCustomImageList);
var
  VFactory: IMapTypeListFactory;
begin
  inherited Create(AParentMap, ACenter);
  FViewRectMoveDelta := Point(0, 0);

  VFactory := TMapTypeListGeneratorFromFullListForMiniMap.Create(True, TMapTypeBasicFactory.Create);
  FMapsList := VFactory.CreateList;
  VFactory := nil;

  VFactory := TMapTypeListGeneratorFromFullListForMiniMap.Create(False, TMapTypeBasicFactory.Create);
  FLayersList := VFactory.CreateList;
  VFactory := nil;

  FMapsActive := TActiveMapsConfigBasic.Create(nil, FMapsList, FLayersList);

  FZoomDelta := 3;
  FBitmapSize.X := 256;
  FBitmapSize.Y := 256;

  FPopup := TTBXPopupMenu.Create(AParentMap);
  FPopup.Name := 'PopupMiniMap';
  FPopup.Images := AImages;

  FLeftBorder := TBitmapLayer.Create(AParentMap.Layers);
  FLeftBorder.Cursor := crSizeNWSE;
  FLeftBorder.Bitmap.DrawMode := dmBlend;
  FLeftBorder.Bitmap.CombineMode := cmMerge;
  FLeftBorder.OnMouseDown := LeftBorderMouseDown;
  FLeftBorder.OnMouseUp := LeftBorderMouseUP;
  FLeftBorder.OnMouseMove := LeftBorderMouseMove;
  FLeftBorderMoved := False;

  FTopBorder := TBitmapLayer.Create(AParentMap.Layers);
  FTopBorder.Bitmap.DrawMode := dmBlend;
  FTopBorder.Bitmap.CombineMode := cmMerge;


  FViewRectDrawLayer := TBitmapLayer.Create(AParentMap.Layers);
  FViewRectDrawLayer.Bitmap.DrawMode := dmBlend;
  FViewRectDrawLayer.Bitmap.CombineMode := cmMerge;
  FViewRectDrawLayer.OnMouseDown := LayerMouseDown;
  FViewRectDrawLayer.OnMouseUp := LayerMouseUP;
  FViewRectDrawLayer.OnMouseMove := LayerMouseMove;

  FPlusButton := TBitmapLayer.Create(AParentMap.Layers);
  FPlusButton.Bitmap.DrawMode := dmBlend;
  FPlusButton.Bitmap.CombineMode := cmMerge;
  FPlusButton.OnMouseDown := PlusButtonMouseDown;
  FPlusButton.OnMouseUp := PlusButtonMouseUP;
  FPlusButton.Cursor := crHandPoint;
  FPlusButtonPressed := False;

  FMinusButton := TBitmapLayer.Create(AParentMap.Layers);
  FMinusButton.Bitmap.DrawMode := dmBlend;
  FMinusButton.Bitmap.CombineMode := cmMerge;
  FMinusButton.OnMouseDown := MinusButtonMouseDown;
  FMinusButton.OnMouseUp := MinusButtonMouseUP;
  FMinusButton.Cursor := crHandPoint;
  FMinusButtonPressed := False;

  LoadBitmaps;
  BuildPopUpMenu;
end;

destructor TMiniMapLayer.Destroy;
begin
  FreeAndNil(FDefoultMap);
  FMapsActive.MapChangeNotifier.Remove(FMapChangeListener);
  FMapsActive.HybrChangeNotifier.Remove(FHybrChangeListener);
  GState.MainMapChangeNotifier.Remove(FMainMapChangeListener);
  FMapChangeListener := nil;
  FHybrChangeListener := nil;
  FMainMapChangeListener := nil;
  FMapsItemsList := nil;
  FLayersItemsList := nil;
  FMapsList := nil;
  FLayersList := nil;
  FMapsActive := nil;
  inherited;
end;

procedure TMiniMapLayer.LoadBitmapFromRes(Instance: HInst;
  const Name: String; Abmp: TBitmap32);
var
  ResStream: TResourceStream;
  VImageLoader: IBitmapTileLoader;
begin
  VImageLoader := GState.BitmapTypeManager.GetBitmapLoaderForExt('.png');
  {Creates an especial stream to load from the resource}
  ResStream := TResourceStream.Create(Instance, Name, RT_RCDATA);

  {Loads the png image from the resource}
  try
    VImageLoader.LoadFromStream(ResStream, Abmp);
  finally
    ResStream.Free;
  end;
end;

procedure TMiniMapLayer.LoadBitmaps;
var
  VBitmap: TBitmap32;
begin
  FDefoultMap := TBitmap32.Create;
  LoadBitmapFromRes(HInstance, 'MAINMAP', FDefoultMap);
  LoadBitmapFromRes(HInstance, 'ICONI', FPlusButton.Bitmap);
  FPlusButton.Bitmap.DrawMode := dmTransparent;
  LoadBitmapFromRes(HInstance, 'ICONII', FMinusButton.Bitmap);
  FMinusButton.Bitmap.DrawMode := dmTransparent;
end;

procedure TMiniMapLayer.BuildPopUpMenu;
var
  VMenuItem: TTBXItem;
  VSubMenuItem: TTBXSubmenuItem;
  VLayersSubMenu: TTBXSubmenuItem;
  VMapType: TMapType;
begin
  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.Name := 'MiniMapSameAsMain';
  VMenuItem.OnAdjustFont := AdjustFont;
  VMenuItem.OnClick := SameAsMainClick;
  VMenuItem.Caption := 'Как на главной карте';
  VMenuItem.Hint := '';
  FPopup.Items.Add(VMenuItem);
  FMiniMapSameAsMain := VMenuItem;

  VSubMenuItem := TTBXSubmenuItem.Create(FPopup);
  VSubMenuItem.Name := 'MiniMapLayers';
  VSubMenuItem.Caption := 'Слои';
  VSubMenuItem.Hint := '';
  VSubMenuItem.SubMenuImages := FPopup.Images;
  FPopup.Items.Add(VSubMenuItem);
  VLayersSubMenu := VSubMenuItem;

  BuildMapsListUI(FPopup.Items, VLayersSubMenu);

  VMapType := FMapsActive.SelectedMap;
  if VMapType = nil then begin
    FMiniMapSameAsMain.Checked := true;
    GState.MainMapChangeNotifier.Add(FMainMapChangeListener);
  end else begin
//    FMapsList.GetMapTypeItemByGUID(VMapType.GUID).GetMenuItem.Checked := true;
  end;
end;

procedure TMiniMapLayer.BuildMapsListUI(AMapssSubMenu, ALayersSubMenu: TTBCustomItem);
var
  VGenerator: TMapMenuGeneratorBasic;
begin
  VGenerator := TMapMenuGeneratorBasic.Create;
  try
    VGenerator.List := FMapsList;
    VGenerator.RootMenu := AMapssSubMenu;
    VGenerator.ItemOnAdjustFont := AdjustFont;
    VGenerator.Images := FPopup.Images;
    VGenerator.MapsActive := FMapsActive;
    FMapsItemsList := VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
  VGenerator := TMapMenuGeneratorBasic.Create;
  try
    VGenerator.List := FLayersList;
    VGenerator.RootMenu := ALayersSubMenu;
    VGenerator.ItemOnAdjustFont := AdjustFont;
    VGenerator.Images := FPopup.Images;
    VGenerator.MapsActive := FMapsActive;
    FLayersItemsList := VGenerator.BuildControls;
  finally
    FreeAndNil(VGenerator);
  end;
  FMapChangeListener := TMiniMapMapChangeListener.Create(Self);
  FMapsActive.MapChangeNotifier.Add(FMapChangeListener);
  FHybrChangeListener := TMiniMapHybrChangeListener.Create(Self);
  FMapsActive.HybrChangeNotifier.Add(FHybrChangeListener);
  FMainMapChangeListener := TMiniMapMainMapChangeListener.Create(Self);
end;

procedure TMiniMapLayer.AdjustFont(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
begin
 if TTBXItem(Item).Checked then TTBXItem(Item).FontSettings.Bold:=tsTrue
                           else TTBXItem(Item).FontSettings.Bold:=tsDefault;
end;

procedure TMiniMapLayer.SameAsMainClick(Sender: TObject);
begin
  Assert(Sender is TTBXItem, 'Глюки однако. Этот обработчик не предназначен для этого контрола');
  FMapsActive.SelectMap(nil);
end;


function TMiniMapLayer.BitmapPixel2MapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
var
  VScreenCenterInBitmap: TPoint;
  VMapCenter: TExtendedPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  VMapCenter := FGeoConvert.Relative2ExtendedPixel(FGeoConvert.PixelPos2Relative(ScreenCenterPos, FZoom), GetActualZoom);
  Result.X := VMapCenter.X - VScreenCenterInBitmap.X + Pnt.X;
  Result.Y := VMapCenter.Y - VScreenCenterInBitmap.Y + Pnt.y;
end;

function TMiniMapLayer.BitmapPixel2MapPixel(Pnt: TPoint): TPoint;
var
  VScreenCenterInBitmap: TPoint;
  VMapCenter: TPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  VMapCenter := FGeoConvert.Relative2Pixel(FGeoConvert.PixelPos2Relative(ScreenCenterPos, FZoom), GetActualZoom);
  Result.X := VMapCenter.X - VScreenCenterInBitmap.X + Pnt.X;
  Result.Y := VMapCenter.Y - VScreenCenterInBitmap.Y + Pnt.y;
end;

function TMiniMapLayer.MapPixel2BitmapPixel(
  Pnt: TExtendedPoint): TExtendedPoint;
var
  VScreenCenterInBitmap: TPoint;
  VMapCenter: TExtendedPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  VMapCenter := FGeoConvert.Relative2ExtendedPixel(FGeoConvert.PixelPos2Relative(ScreenCenterPos, FZoom), GetActualZoom);
  Result.X := Pnt.X - VMapCenter.X + VScreenCenterInBitmap.X;
  Result.Y := Pnt.Y - VMapCenter.Y + VScreenCenterInBitmap.Y;
end;

function TMiniMapLayer.MapPixel2BitmapPixel(Pnt: TPoint): TPoint;
var
  VScreenCenterInBitmap: TPoint;
  VMapCenter: TPoint;
begin
  VScreenCenterInBitmap := GetScreenCenterInBitmapPixels;
  VMapCenter := FGeoConvert.Relative2Pixel(FGeoConvert.PixelPos2Relative(ScreenCenterPos, FZoom), GetActualZoom);
  Result.X := Pnt.X - VMapCenter.X + VScreenCenterInBitmap.X;
  Result.Y := Pnt.Y - VMapCenter.Y + VScreenCenterInBitmap.Y;
end;

procedure TMiniMapLayer.DoChangePos(APoint: TPoint);
begin
  if Assigned(FOnChangePos) then begin
    FOnChangePos(APoint, FZoom);
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
  FLayer.Bitmap.Clear(Color32(GState.BGround));
  VMapType := FMapsActive.SelectedMap;
  if VMapType = nil then begin
    VMapType := GState.sat_map_both;
  end;

  DrawMap(VMapType, dmOpaque);
  VEnum := FLayersList.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK  do begin
    VItem := FLayersList.GetMapTypeByGUID(VGUID);
    VMapType := VItem.GetMapType;
    if FMapsActive.IsHybrSelected(VMapType) then begin
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
  VBitmapOnMapPixelRect: TRect;
  VViewSize: TPoint;
  VZoomSource: Byte;
  VZoom: Byte;
  VMiniMapRect: TRect;
  VBitmapRect: TRect;
  VRelRect: TExtendedRect;
  VPolygon: TPolygon32;
  VBitmapSize: TPoint;
begin
  FViewRectDrawLayer.Bitmap.Clear(clBlack);
  if FGeoConvert <> nil then begin
    if FZoomDelta > 0 then begin
      VViewSize := Point(FParentMap.Width, FParentMap.Height);
      VBitmapOnMapPixelRect.Left := FScreenCenterPos.X - VViewSize.X div 2;
      VBitmapOnMapPixelRect.Top := FScreenCenterPos.Y - VViewSize.Y div 2;
      VBitmapOnMapPixelRect.Right := FScreenCenterPos.X + VViewSize.X div 2;
      VBitmapOnMapPixelRect.Bottom := FScreenCenterPos.Y + VViewSize.Y div 2;
      VZoomSource := FZoom;
      VZoom := GetActualZoom;
      FGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoomSource, False);
      VRelRect := FGeoConvert.PixelRect2RelativeRect(VBitmapOnMapPixelRect, VZoomSource);
      VMiniMapRect := FGeoConvert.RelativeRect2PixelRect(VRelRect, VZoom);
      VBitmapRect.TopLeft := MapPixel2BitmapPixel(VMiniMapRect.TopLeft);
      VBitmapRect.BottomRight := MapPixel2BitmapPixel(VMiniMapRect.BottomRight);
      Inc(VBitmapRect.Left, FViewRectMoveDelta.X);
      Inc(VBitmapRect.Top, FViewRectMoveDelta.Y);
      Inc(VBitmapRect.Right, FViewRectMoveDelta.X);
      Inc(VBitmapRect.Bottom, FViewRectMoveDelta.Y);

      VBitmapSize := GetBitmapSizeInPixel;
      if (VBitmapRect.Left >= 0) or (VBitmapRect.Top >= 0)
        or (VBitmapRect.Right <= VBitmapSize.X)
        or (VBitmapRect.Bottom <= VBitmapSize.Y)
      then begin
        VPolygon := TPolygon32.Create;
        try
          VPolygon.Antialiased:=true;
          VPolygon.Add(FixedPoint(VBitmapRect.TopLeft));
          VPolygon.Add(FixedPoint(VBitmapRect.Right, VBitmapRect.Top));
          VPolygon.Add(FixedPoint(VBitmapRect.BottomRight));
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
          VPolygon.DrawFill(FViewRectDrawLayer.Bitmap, SetAlpha(clWhite32,(FZoomDelta)*35));
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
  VBmp: TBitmap32;

  {
    Прямоугольник пикселей растра в координатах текущей основной карты
  }
  VBitmapOnMapPixelRect: TRect;

  {
    Географические координаты растра
  }
  VSourceLonLatRect: TExtendedRect;

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
begin
  VBmp := TBitmap32.Create;
  try
    VZoom := GetActualZoom;
    VSourceMapType := AMapType;
    VSourceGeoConvert := VSourceMapType.GeoConvert;
    VGeoConvert := FGeoConvert;
    VBitmapOnMapPixelRect.TopLeft := BitmapPixel2MapPixel(Point(0, 0));
    VBitmapOnMapPixelRect.BottomRight := BitmapPixel2MapPixel(GetBitmapSizeInPixel);
    VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom, False);

    VSourceLonLatRect := VGeoConvert.PixelRect2LonLatRect(VBitmapOnMapPixelRect, VZoom);
    VPixelSourceRect := VSourceGeoConvert.LonLatRect2PixelRect(VSourceLonLatRect, VZoom);
    VTileSourceRect := VSourceGeoConvert.PixelRect2TileRect(VPixelSourceRect, VZoom);

    for i := VTileSourceRect.Left to VTileSourceRect.Right do begin
      VTile.X := i;
      for j:= VTileSourceRect.Top to VTileSourceRect.Bottom do begin
        VTile.Y := j;
        VCurrTilePixelRectSource := VSourceGeoConvert.TilePos2PixelRect(VTile, VZoom);
        VTilePixelsToDraw.TopLeft := Point(0, 0);
        VTilePixelsToDraw.Right := VCurrTilePixelRectSource.Right - VCurrTilePixelRectSource.Left + 1;
        VTilePixelsToDraw.Bottom := VCurrTilePixelRectSource.Bottom - VCurrTilePixelRectSource.Top + 1;

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

        VCurrTilePixelRect.TopLeft := VSourceGeoConvert.Pos2OtherMap(VCurrTilePixelRectSource.TopLeft, VZoom + 8, VGeoConvert);
        VCurrTilePixelRect.BottomRight := VSourceGeoConvert.Pos2OtherMap(VCurrTilePixelRectSource.BottomRight, VZoom + 8, VGeoConvert);

        VCurrTilePixelRectAtBitmap.TopLeft := MapPixel2BitmapPixel(VCurrTilePixelRect.TopLeft);
        VCurrTilePixelRectAtBitmap.BottomRight := MapPixel2BitmapPixel(VCurrTilePixelRect.BottomRight);
        Inc(VCurrTilePixelRectAtBitmap.Bottom);
        Inc(VCurrTilePixelRectAtBitmap.Right);
        if VSourceMapType.LoadTileOrPreZ(VBmp, VTile, VZoom, true, False) then begin
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

function TMiniMapLayer.GetActualZoom: Byte;
begin
  if FZoomDelta = 0 then begin
    Result := FZoom;
  end else if FZoomDelta > 0 then begin
    if FZoom > FZoomDelta then begin
      Result := FZoom - FZoomDelta;
    end else begin
      Result := 0;
    end;
  end else begin
    Result := FZoom - FZoomDelta;
    FGeoConvert.CheckZoom(Result);
  end;
end;

function TMiniMapLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := FBitmapSize;
end;

function TMiniMapLayer.GetFreezePointInBitmapPixel: TPoint;
var
  VBitmapSize: TPoint;
begin
  VBitmapSize := GetBitmapSizeInPixel;
  Result := Point(VBitmapSize.X, 0);
end;

function TMiniMapLayer.GetFreezePointInVisualPixel: TPoint;
var
  VVisibleSize: TPoint;
begin
  VVisibleSize := GetVisibleSizeInPixel;
  Result := Point(VVisibleSize.X, 20);
end;

procedure TMiniMapLayer.Hide;
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
  VBitmapCenter: TPoint;
  VVisibleCenter: TPoint;
begin
  FParentMap.PopupMenu:=nil;
  case button of
    mbRight: FParentMap.PopupMenu := FPopup;
    mbLeft: begin
      VBitmapCenter := GetBitmapSizeInPixel;
      VBitmapCenter := Point(VBitmapCenter.X div 2, VBitmapCenter.Y div 2);
      VVisibleCenter := BitmapPixel2VisiblePixel(VBitmapCenter);
      FPosMoved := True;
      FViewRectMoveDelta := Point(X - VVisibleCenter.X, Y - VVisibleCenter.Y);
      DrawMainViewRect;
    end;
  end;
end;

procedure TMiniMapLayer.LayerMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  VBitmapSize: TPoint;
  VBitmapCenter: TPoint;
  VVisibleCenter: TPoint;
  VVisibleRect: TRect;
begin
  if FPosMoved then begin
    VBitmapSize := GetBitmapSizeInPixel;
    VBitmapCenter := Point(VBitmapSize.X div 2, VBitmapSize.Y div 2);
    VVisibleCenter := BitmapPixel2VisiblePixel(VBitmapCenter);

    VVisibleRect.TopLeft := BitmapPixel2VisiblePixel(Point(0, 0));
    VVisibleRect.BottomRight := BitmapPixel2VisiblePixel(VBitmapSize);

    if X < VVisibleRect.Left then begin
      FViewRectMoveDelta.X := VVisibleRect.Left - VVisibleCenter.X;
    end else if X > VVisibleRect.Right then begin
      FViewRectMoveDelta.X := VVisibleRect.Right - VVisibleCenter.X;
    end else begin
      FViewRectMoveDelta.X := X - VVisibleCenter.X;
    end;
    if Y < VVisibleRect.Top then begin
      FViewRectMoveDelta.Y := VVisibleRect.Top - VVisibleCenter.Y;
    end else if Y > VVisibleRect.Bottom then begin
      FViewRectMoveDelta.Y := VVisibleRect.Bottom - VVisibleCenter.Y;
    end else begin
      FViewRectMoveDelta.Y := Y - VVisibleCenter.Y;
    end;

    DrawMainViewRect;
  end;
end;

procedure TMiniMapLayer.LayerMouseUP(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  VNewPos: TPoint;
  VActualZoom: Byte;
begin
  if FPosMoved then begin
    if FLayer.HitTest(X, Y) then begin
      VNewPos := VisiblePixel2MapPixel(Point(X, Y));
      VActualZoom := GetActualZoom;
      GeoConvert.CheckPixelPosStrict(VNewPos, VActualZoom, False);
      VNewPos := FGeoConvert.Relative2Pixel(FGeoConvert.PixelPos2Relative(VNewPos, VActualZoom), FZoom);
      FViewRectMoveDelta := Point(0, 0);
      DoChangePos(VNewPos);
    end else begin
      FViewRectMoveDelta := Point(0, 0);
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
  end;
end;

procedure TMiniMapLayer.LeftBorderMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  VNewWidth: Integer;
  VBitmapPos: TPoint;
  VVisibleSize: TPoint;
begin
  if FLeftBorderMoved then begin
    VVisibleSize := GetVisibleSizeInPixel;

    VBitmapPos := VisiblePixel2BitmapPixel(Point(X, Y));
    VNewWidth := FBitmapSize.X - VBitmapPos.X;
    if VNewWidth < 40 then begin
      VNewWidth := 40;
    end;
    if VNewWidth > VVisibleSize.X then begin
      VNewWidth := VVisibleSize.X;
    end;
    if VNewWidth > VVisibleSize.Y then begin
      VNewWidth := VVisibleSize.Y;
    end;
    FBitmapSize.X := VNewWidth;
    FBitmapSize.Y := VNewWidth;
    DoResizeBitmap;
    DoRedraw;
    Resize;
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

procedure TMiniMapLayer.Show;
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

procedure TMiniMapLayer.DoResize;
var
  VBitmapSize: TPoint;
  VRect: TRect;
begin
  inherited;
  FViewRectDrawLayer.Location := FloatRect(GetMapLayerLocationRect);

  VBitmapSize := GetBitmapSizeInPixel;
  VRect.TopLeft := BitmapPixel2VisiblePixel(Point(-5, -5));
  VRect.BottomRight := BitmapPixel2VisiblePixel(Point(0, VBitmapSize.Y));
  FLeftBorder.Location := FloatRect(VRect);
  VRect.TopLeft := BitmapPixel2VisiblePixel(Point(0, -5));
  VRect.BottomRight := BitmapPixel2VisiblePixel(Point(VBitmapSize.X, 0));
  FTopBorder.Location := FloatRect(VRect);

  VRect.TopLeft := BitmapPixel2VisiblePixel(Point(6, 6));
  VRect.BottomRight := BitmapPixel2VisiblePixel(Point(6 + FPlusButton.Bitmap.Width, 6 + FPlusButton.Bitmap.Height));
  FPlusButton.Location := FloatRect(VRect);

  VRect.TopLeft := BitmapPixel2VisiblePixel(Point(19, 6));
  VRect.BottomRight := BitmapPixel2VisiblePixel(Point(19 + FMinusButton.Bitmap.Width, 6 + FMinusButton.Bitmap.Height));
  FMinusButton.Location := FloatRect(VRect);

  DrawMainViewRect;
end;

procedure TMiniMapLayer.DoResizeBitmap;
var
  VBitmapSizeInPixel: TPoint;
  Polygon: TPolygon32;
begin
  inherited;
  VBitmapSizeInPixel := GetBitmapSizeInPixel;
  FViewRectDrawLayer.Bitmap.SetSize(VBitmapSizeInPixel.X, VBitmapSizeInPixel.Y);
  if (FLeftBorder.Bitmap.Height <> VBitmapSizeInPixel.Y) then begin
    FLeftBorder.Bitmap.Lock;
    try
      FLeftBorder.Bitmap.SetSize(5, VBitmapSizeInPixel.Y + 5);
      FLeftBorder.Bitmap.Clear(clLightGray32);
      Polygon := TPolygon32.Create;
      try
        Polygon.Antialiased:=False;
        Polygon.Closed := false;
        Polygon.Add(FixedPoint(4, 0));
        Polygon.Add(FixedPoint(0, 0));
        Polygon.Add(FixedPoint(0, VBitmapSizeInPixel.Y + 4));
        Polygon.Add(FixedPoint(4, VBitmapSizeInPixel.Y + 4));
        Polygon.Add(FixedPoint(4, 3));
        Polygon.DrawEdge(FLeftBorder.Bitmap, clBlack32);
      finally
        Polygon.Free;
      end;
      FLeftBorder.bitmap.Pixel[2,5 + (VBitmapSizeInPixel.Y div 2)-6]:=clBlack;
      FLeftBorder.bitmap.Pixel[2,5 + (VBitmapSizeInPixel.Y div 2)-2]:=clBlack;
      FLeftBorder.bitmap.Pixel[2,5 + (VBitmapSizeInPixel.Y div 2)+2]:=clBlack;
      FLeftBorder.bitmap.Pixel[2,5 + (VBitmapSizeInPixel.Y div 2)+6]:=clBlack;
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
        Polygon.Antialiased:=False;
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
    GState.MainMapChangeNotifier.Add(FMainMapChangeListener);
  end else begin
    FMiniMapSameAsMain.Checked := False;
    GState.MainMapChangeNotifier.Remove(FMainMapChangeListener);
  end;
  Redraw;
end;

procedure TMiniMapLayer.OnNotifyMainMapChange(msg: IMapChangeMessage);
begin
  if FMapsActive.SelectedMap = nil then begin
    Redraw;
  end;
end;

end.
