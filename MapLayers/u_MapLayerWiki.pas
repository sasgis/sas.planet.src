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
  i_ImageResamplerConfig,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ViewPortState,
  i_VectorDataItemSimple,
  u_MapLayerWithThreadDraw;

type
  TWikiLayer = class(TMapLayerTiledWithThreadDraw)
  private
    FConfig: IKmlLayerConfig;
    FLayersSet: IActiveMapsSet;

    FMapsList: IMapTypeList;
    FElments: IInterfaceList;

    FFixedPointArray: TArrayOfFixedPoint;
    FPolygon: TPolygon32;
    procedure ElementsClear;
    procedure DrawWikiElement(
      ATargetBmp: TCustomBitmap32;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32;
      AData: IVectorDataItemSimple;
      ALocalConverter: ILocalCoordConverter
    );
    procedure AddWikiElement(
      AElments: IInterfaceList;
      AData: IVectorDataItemSimple;
      ALocalConverter: ILocalCoordConverter
    );
    procedure AddElementsFromMap(
      AElments: IInterfaceList;
      AIsStop: TIsCancelChecker;
      Alayer: TMapType;
      ALocalConverter: ILocalCoordConverter
    );
    procedure PrepareWikiElements(
      AElments: IInterfaceList;
      AIsStop: TIsCancelChecker;
      ALocalConverter: ILocalCoordConverter
    );
    procedure OnConfigChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
    procedure GetBitmapRect(
      AElments: IInterfaceList;
      AIsStop: TIsCancelChecker;
      ATargetBmp: TCustomBitmap32;
      ALocalConverter: ILocalCoordConverter;
      AColorMain: TColor32;
      AColorBG: TColor32;
      APointColor: TColor32
    );
    procedure ProcessDraw(
      AElments: IInterfaceList;
      AIsStop: TIsCancelChecker;
      ALocalConverter: ILocalCoordConverter
    );
  protected
    procedure DrawBitmap(AIsStop: TIsCancelChecker); override;
    procedure DoHide; override;
  public
    constructor Create(
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AResamplerConfig: IImageResamplerConfig;
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
  u_TileIteratorByRect,
  u_TileIteratorSpiralByRect;

{ TWikiLayer }

constructor TWikiLayer.Create(
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  AResamplerConfig: IImageResamplerConfig;
  ATimerNoifier: IJclNotifier;
  AConfig: IKmlLayerConfig;
  ALayersSet: IActiveMapsSet
);
begin
  inherited Create(AParentMap, AViewPortState, AConverterFactory, AResamplerConfig, ATimerNoifier, tpLower);
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

procedure TWikiLayer.DoHide;
begin
  inherited;
  ElementsClear;
end;

procedure TWikiLayer.AddWikiElement(AElments: IInterfaceList; AData: IVectorDataItemSimple; ALocalConverter: ILocalCoordConverter);
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
        AElments.Add(AData);
      end;
    end;
  end;
end;

procedure TWikiLayer.AddElementsFromMap(
  AElments: IInterfaceList;
  AIsStop: TIsCancelChecker;
  Alayer: TMapType;
  ALocalConverter: ILocalCoordConverter
);
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
    if Alayer.LoadTile(kml, VTile, Vzoom, True, Alayer.Cache) then begin
      AElments.Lock;
      try
        if AIsStop then begin
          Break;
        end else begin
          for ii := 0 to KML.Count - 1 do begin
            AddWikiElement(AElments, KML.GetItem(ii), ALocalConverter);
          end;
        end;
      finally
        AElments.Unlock;
      end;
    end;
    kml := nil;
  end;
end;

procedure TWikiLayer.PrepareWikiElements(
  AElments: IInterfaceList;
  AIsStop: TIsCancelChecker;
  ALocalConverter: ILocalCoordConverter
);
var
  VHybrList: IMapTypeList;
  VEnum: IEnumGUID;
  VGUID: TGUID;
  Vcnt: Cardinal;
  VItem: IMapType;
  VMapType: TMapType;
begin
  VHybrList := FMapsList;
  if VHybrList <> nil then begin
    VEnum := VHybrList.GetIterator;
    while VEnum.Next(1, VGUID, Vcnt) = S_OK do begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsKmlTiles then begin
        AddElementsFromMap(AElments, AIsStop, VMapType, ALocalConverter);
        if AIsStop then begin
          Break;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.ProcessDraw(AElments: IInterfaceList;
  AIsStop: TIsCancelChecker; ALocalConverter: ILocalCoordConverter);
var
  VColorMain: TColor32;
  VColorBG: TColor32;
  VPointColor: TColor32;
  VTileToDrawBmp: TCustomBitmap32;
  VTileIterator: ITileIterator;
  VGeoConvert: ICoordConverter;

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
begin
  FConfig.LockRead;
  try
    VColorMain := FConfig.MainColor;
    VColorBG := FConfig.ShadowColor;
    VPointColor := FConfig.PointColor;
  finally
    FConfig.UnlockRead;
  end;
  if AElments.Count > 0 then begin
    if not AIsStop then begin
      VTileToDrawBmp := TCustomBitmap32.Create;
      try
        VGeoConvert := ALocalConverter.GetGeoConverter;
        VZoom := ALocalConverter.GetZoom;

        VBitmapOnMapPixelRect := ALocalConverter.GetRectInMapPixel;
        VGeoConvert.CheckPixelRect(VBitmapOnMapPixelRect, VZoom);

        VTileSourceRect := VGeoConvert.PixelRect2TileRect(VBitmapOnMapPixelRect, VZoom);
        VTileIterator := TTileIteratorSpiralByRect.Create(VTileSourceRect);
        while VTileIterator.Next(VTile) do begin
          if AIsStop then begin
            break;
          end;
          VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

          VTilePixelsToDraw.TopLeft := Point(0, 0);
          VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
          VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

          VCurrTileOnBitmapRect.TopLeft := ALocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
          VCurrTileOnBitmapRect.BottomRight := ALocalConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);

          VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
          VTileToDrawBmp.Clear(0);
          GetBitmapRect(
            AElments,
            AIsStop,
            VTileToDrawBmp,
            ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert),
            VColorMain,
            VColorBG,
            VPointColor
          );

          Layer.Bitmap.Lock;
          try
            if AIsStop then begin
              break;
            end;
            Layer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, VTileToDrawBmp);
            SetBitmapChanged;
          finally
            Layer.Bitmap.UnLock;
          end;
        end;
      finally
        VTileToDrawBmp.Free;
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
  VLocalConverter: ILocalCoordConverter;
  i: Integer;
  VList: IInterfaceList;
begin
  inherited;
  VLocalConverter := LayerCoordConverter;
  ElementsClear;
  PrepareWikiElements(FElments, AIsStop, VLocalConverter);
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
    ProcessDraw(FElments, AIsStop, VLocalConverter);
  end;
end;

procedure TWikiLayer.GetBitmapRect(
  AElments: IInterfaceList;
  AIsStop: TIsCancelChecker;
  ATargetBmp: TCustomBitmap32;
  ALocalConverter: ILocalCoordConverter; AColorMain, AColorBG,
  APointColor: TColor32);
var
  i: Integer;
  VItem: IVectorDataItemSimple;
  VZoom: Byte;
  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
  VLLRect: TDoubleRect;
  VMarkLonLatRect: TDoubleRect;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;
  VGeoConvert.CheckPixelRectFloat(VMapPixelRect, VZoom);
  VLLRect := VGeoConvert.PixelRectFloat2LonLatRect(VMapPixelRect, VZoom);

  for i := 0 to AElments.Count - 1 do begin
    VItem := IVectorDataItemSimple(AElments[i]);
    VMarkLonLatRect := VItem.LLRect;
    if(
      (VLLRect.Right >= VMarkLonLatRect.Left)and
      (VLLRect.Left <= VMarkLonLatRect.Right)and
      (VLLRect.Bottom <= VMarkLonLatRect.Top)and
      (VLLRect.Top >= VMarkLonLatRect.Bottom))
    then begin
      DrawWikiElement(ATargetBmp, AColorMain, AColorBG, APointColor, VItem, ALocalConverter);
      if AIsStop then begin
        Break;
      end;
    end;
  end;
end;

procedure TWikiLayer.DrawWikiElement(
  ATargetBmp: TCustomBitmap32;
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
    ATargetBmp.FillRectS(VRect, AColorBG);
    Inc(VRect.Left);
    Inc(VRect.Top);
    Dec(VRect.Right);
    Dec(VRect.Bottom);
    ATargetBmp.FillRectS(VRect, APointColor);
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
//    FPolygon.Points[0] := Copy(FFixedPointArray, 0, VLen);
    FPolygon.Clear;
    FPolygon.AddPoints(FFixedPointArray[0], VLen);
    FPolygon.DrawEdge(ATargetBmp, AColorBG);
    FPolygon.Offset(Fixed(0.9), Fixed(0.9));
    FPolygon.DrawEdge(ATargetBmp, AColorMain);
  end;
end;

procedure TWikiLayer.ElementsClear;
begin
  FElments.Clear;
end;

procedure TWikiLayer.MouseOnReg(xy: TPoint; out AItem: IVectorDataItemSimple;
  out AItemS: Double);
var
  VLonLatLine: TArrayOfDoublePoint;
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
          VLonLatLine := VItem.Points;
          VConverter.CheckLonLatArray(VLonLatLine);
          VLineOnBitmap := VConverter.LonLatArray2PixelArrayFloat(VLonLatLine, VZoom);
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
