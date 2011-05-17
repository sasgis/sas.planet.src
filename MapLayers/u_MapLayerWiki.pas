unit u_MapLayerWiki;

interface

uses
  Windows,
  Types,
  Classes,
  GR32,
  GR32_Polygons,
  GR32_Image,
  i_JclNotify,
  t_CommonTypes,
  u_GeoFun,
  u_MapType,
  u_MapLayerBasic,
  t_GeoTypes,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_KmlLayerConfig,
  i_LocalCoordConverter,
  i_ViewPortState,
  i_VectorDataItemSimple,
  u_MapLayerWithThreadDraw;

type
  TWikiLayer = class(TMapLayerWithThreadDraw)
  private
    FConfig: IKmlLayerConfig;
    FLayersSet: IActiveMapsSet;

    FMapsList: IMapTypeList;
    FElments: IInterfaceList;

    FFixedPointArray: TArrayOfFixedPoint;
    FPolygon: TPolygon32;

    procedure addWL(AData: IVectorDataItemSimple; ALocalConverter: ILocalCoordConverter);
    procedure DrawWikiElement(
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      AData: IVectorDataItemSimple;
      ALocalConverter: ILocalCoordConverter
    );
    procedure AddFromLayer(Alayer: TMapType; ALocalConverter: ILocalCoordConverter);
    procedure OnConfigChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      ATimerNoifier: IJclNotifier;
      AConfig: IKmlLayerConfig;
      ALayersSet: IActiveMapsSet
    );
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple; out AItemS: Double); overload;
    procedure MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple); overload;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  Graphics,
  i_CoordConverter,
  i_TileIterator,
  u_NotifyEventListener,
  u_TileIteratorByRect;

{ TWikiLayer }

constructor TWikiLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  ATimerNoifier: IJclNotifier;
  AConfig: IKmlLayerConfig;
  ALayersSet: IActiveMapsSet
);
begin
  inherited Create(AParentMap, AViewPortState, ATimerNoifier, tpLower);
  FConfig := AConfig;
  FLayersSet := ALayersSet;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnLayerSetChange),
    FLayersSet.GetChangeNotifier
  );

  FElments := TInterfaceList.Create;
  SetLength(FFixedPointArray, 256);

  FPolygon := TPolygon32.Create;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;
end;

destructor TWikiLayer.Destroy;
begin
  FElments := nil;
  FFixedPointArray := nil;
  FreeAndNil(FPolygon);
  inherited;
end;

procedure TWikiLayer.AddFromLayer(Alayer: TMapType; ALocalConverter: ILocalCoordConverter);
var
  ii: integer;
  kml: IVectorDataItemList;
  VTileIterator: ITileIterator;
  VZoom: Byte;
  VSourceGeoConvert: ICoordConverter;
  VGeoConvert: ICoordConverter;
  VBitmapOnMapPixelRect: TDoubleRect;
  VSourceLonLatRect: TDoubleRect;
  VTileSourceRect: TRect;
  VTile: TPoint;
begin
  VZoom := ALocalConverter.GetZoom;
  VSourceGeoConvert := Alayer.GeoConvert;
  VGeoConvert := ALocalConverter.GetGeoConverter;

  VBitmapOnMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VBitmapOnMapPixelRect, VZoom);

  VSourceLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VBitmapOnMapPixelRect, VZoom);
  VTileSourceRect := VSourceGeoConvert.LonLatRect2TileRect(VSourceLonLatRect, VZoom);
  VTileIterator := TTileIteratorByRect.Create(VTileSourceRect);

  while VTileIterator.Next(VTile) do begin
      if Alayer.LoadTile(kml, VTile, Vzoom, true, True) then begin
        for ii := 0 to KML.Count - 1 do begin
          addWL(KML.GetItem(ii), ALocalConverter);
        end;
      end;
  end;
end;

procedure TWikiLayer.addWL(AData: IVectorDataItemSimple; ALocalConverter: ILocalCoordConverter);
var
  VConverter: ICoordConverter;
  VSize: TPoint;
  VLLRect: TDoubleRect;
  VBounds: TDoubleRect;
begin
  if AData <> nil then begin
    VSize := ALocalConverter.GetLocalRectSize;
    VConverter := ALocalConverter.GetGeoConverter;
    VLLRect := AData.LLRect;
    VConverter.CheckLonLatRect(VLLRect);
    VBounds := ALocalConverter.LonLatRect2LocalRectFloat(VLLRect);
    if AData.IsPoint or (((VBounds.Right - VBounds.Left) > 1) and ((VBounds.Bottom - VBounds.Top) > 1)) then begin
      if ((VBounds.Top < VSize.Y) and (VBounds.Bottom > 0) and (VBounds.Left < VSize.X) and (VBounds.Right > 0)) then begin
        FElments.Add(AData);
      end;
    end;
  end;
end;

procedure TWikiLayer.OnConfigChange(Sender: TObject);
begin
  SetNeedRedraw;
  ViewUpdate;
end;

procedure TWikiLayer.OnLayerSetChange(Sender: TObject);
var
  VGUID: TGUID;
  i: Cardinal;
begin
  FMapsList := FLayersSet.GetSelectedMapsList;
  ViewUpdateLock;
  try
    SetNeedRedraw;
    SetVisible(FMapsList.GetIterator.Next(1, VGUID, i) = S_OK);
  finally
    ViewUpdateUnlock;
  end;
  ViewUpdate;
end;

procedure TWikiLayer.StartThreads;
begin
  inherited;
  OnLayerSetChange(nil);
end;

procedure TWikiLayer.DrawBitmap(AIsStop: TIsCancelChecker);
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VHybrList: IMapTypeList;
  ii: Integer;
  VLocalConverter: ILocalCoordConverter;
  VColorMain: TColor32;
  VColorBG: TColor32;
  VPointColor: TColor32;
begin
  inherited;
  FElments.Clear;
  FConfig.LockRead;
  try
    VColorMain := FConfig.MainColor;
    VColorBG := FConfig.ShadowColor;
    VPointColor := FConfig.PointColor;
  finally
    FConfig.UnlockRead;
  end;
  VHybrList := FMapsList;
  if VHybrList <> nil then begin
    VLocalConverter := LayerCoordConverter;
    VEnum := VHybrList.GetIterator;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsKmlTiles then begin
        AddFromLayer(VMapType, VLocalConverter);
        if AIsStop then begin
          Break;
        end;
      end;
    end;
    if not AIsStop then begin
      for ii := 0 to FElments.Count - 1 do begin
        DrawWikiElement(VColorMain, VColorBG, VPointColor,IVectorDataItemSimple(Pointer(FElments[ii])), VLocalConverter);
        if AIsStop then begin
          Break;
        end;
      end;
    end;
    if not AIsStop then begin
      SetBitmapChanged;
    end;
  end;
end;

procedure TWikiLayer.DrawWikiElement(
  AColorMain: TColor32;
  AColorBG: TColor32;
  APointColor: TColor32;
  AData: IVectorDataItemSimple;
  ALocalConverter: ILocalCoordConverter
);
var
  VLen: integer;
  i: integer;
  VRect: TRect;
  VPointLL: TDoublePoint;
  VConverter: ICoordConverter;
  VPointOnBitmap: TDoublePoint;
begin
  VConverter := ALocalConverter.GetGeoConverter;
  if AData.IsPoint then begin
    VPointLL := AData.Points[0];
    VConverter.CheckLonLatPos(VPointLL);
    VRect.TopLeft := ALocalConverter.LonLat2LocalPixel(VPointLL);
    VRect.BottomRight := VRect.TopLeft;
    Dec(VRect.Left, 3);
    Dec(VRect.Top, 3);
    Inc(VRect.Right, 3);
    Inc(VRect.Bottom, 3);
    Layer.Bitmap.Lock;
    try
      Layer.Bitmap.FillRectS(VRect, AColorBG);
    finally
      Layer.Bitmap.Unlock;
    end;
    Inc(VRect.Left);
    Inc(VRect.Top);
    Dec(VRect.Right);
    Dec(VRect.Bottom);
    Layer.Bitmap.Lock;
    try
      Layer.Bitmap.FillRectS(VRect, APointColor);
    finally
      Layer.Bitmap.Unlock;
    end;
  end else begin
    VLen := Length(AData.Points);
    if Length(FFixedPointArray) < VLen then begin
      SetLength(FFixedPointArray, VLen);
    end;
    for i := 0 to VLen - 1 do begin
      VPointLL := AData.Points[i];
      VConverter.CheckLonLatPos(VPointLL);
      VPointOnBitmap := ALocalConverter.LonLat2LocalPixelFloat(VPointLL);
      FFixedPointArray[i] := FixedPoint(VPointOnBitmap.X, VPointOnBitmap.Y);
    end;
    FPolygon.Clear;
    FPolygon.AddPoints(FFixedPointArray[0], VLen);
    Layer.Bitmap.Lock;
    try
      FPolygon.DrawEdge(Layer.Bitmap, AColorBG);
    finally
      Layer.Bitmap.Unlock;
    end;
    FPolygon.Offset(Fixed(0.9), Fixed(0.9));
    Layer.Bitmap.Lock;
    try
      FPolygon.DrawEdge(Layer.Bitmap, AColorMain);
    finally
      Layer.Bitmap.Unlock;
    end;
  end;
end;

procedure TWikiLayer.MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple;
  out AItemS: Double);
var
  VLineOnBitmap: TArrayOfDoublePoint;
  VLonLatRect: TDoubleRect;
  VRect: TRect;
  VConverter: ICoordConverter;
  VMarkLonLatRect: TDoubleRect;
  VPixelPos: TDoublePoint;
  VZoom: Byte;
  VMapRect: TDoubleRect;
  VLocalConverter: ILocalCoordConverter;
  VVisualConverter: ILocalCoordConverter;
  VSquare:Double;
  i: integer;
  VList: IInterfaceList;
  VItem: IVectorDataItemSimple;
begin
  AItem := nil;
  AItemS := 0;
  VList := TInterfaceList.Create;
  FElments.Lock;
  try
    for i := 0 to FElments.Count - 1 do begin
      VList.Add(FElments[i]);
    end;
  finally
    FElments.Unlock;
  end;
  if VList.Count > 0 then begin
    VRect.Left := xy.X - 3;
    VRect.Top := xy.Y - 3;
    VRect.Right := xy.X + 3;
    VRect.Bottom := xy.Y + 3;
    VLocalConverter := LayerCoordConverter;
    VConverter := VLocalConverter.GetGeoConverter;
    VZoom := VLocalConverter.GetZoom;
    VVisualConverter := ViewCoordConverter;
    VMapRect := VVisualConverter.LocalRect2MapRectFloat(VRect);
    VConverter.CheckPixelRectFloat(VMapRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VMapRect, VZoom);
    VPixelPos := VVisualConverter.LocalPixel2MapPixelFloat(xy);
    for i := 0 to VList.Count - 1 do begin
      VItem := IVectorDataItemSimple(Pointer(VList[i]));
      VMarkLonLatRect := VItem.LLRect;
      if((VLonLatRect.Right>VMarkLonLatRect.Left)and(VLonLatRect.Left<VMarkLonLatRect.Right)and
      (VLonLatRect.Bottom<VMarkLonLatRect.Top)and(VLonLatRect.Top>VMarkLonLatRect.Bottom))then begin
        if VItem.IsPoint then begin
          AItem := VItem;
          AItemS := 0;
          exit;
        end else begin
          VLineOnBitmap := VConverter.LonLatArray2PixelArrayFloat(VItem.Points, VZoom);
          if VItem.IsLine then begin
            if PointOnPath(VPixelPos, VLineOnBitmap, 2) then begin
              AItem := VItem;
              AItemS := 0;
              exit;
            end;
          end else begin
            if (PtInRgn(VLineOnBitmap,VPixelPos)) then begin
              VSquare := PolygonSquare(VLineOnBitmap);
              if (AItem = nil) or (VSquare<AItemS) then begin
                AItem := VItem;
                AItemS := VSquare;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple);
var
  VItemS: Double;
begin
  MouseOnReg(xy, AItem, VItemS);
end;

end.
