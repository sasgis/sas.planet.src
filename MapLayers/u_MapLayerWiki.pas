unit u_MapLayerWiki;

interface

uses
  Windows,
  Types,
  Classes,
  GR32,
  GR32_Image,
  u_GeoFun,
  u_MapType,
  u_MapLayerBasic,
  t_GeoTypes,
  i_MapTypes,
  i_ActiveMapsConfig,
  i_KmlLayerConfig,
  i_LocalCoordConverter,
  i_ViewPortState,
  u_KmlInfoSimple;

type
  TWikiLayerElement = class
  public
    name_blok: string;
    num_blok: string;
    description: string;
    FBounds: TDoubleRect;
    FPolygonOnBitmap: TArrayOfDoublePoint;
    constructor Create;
    destructor Destroy; override;
  end;

  TWikiLayer = class(TMapLayerBasic)
  private
    FConfig: IKmlLayerConfig;
    FLayersSet: IActiveMapsSet;

    FMapsList: IMapTypeList;
    FFixedPointArray: TArrayOfFixedPoint;
    FWikiLayerElments: array of TWikiLayerElement;
    procedure addWL(var AData: TKMLData; ALocalConverter: ILocalCoordConverter);
    procedure DrawWikiElement(var AData: TWikiLayerElement; ALocalConverter: ILocalCoordConverter);
    procedure Clear;
    procedure AddFromLayer(Alayer: TMapType; ALocalConverter: ILocalCoordConverter);
    procedure OnConfigChange(Sender: TObject);
    procedure OnLayerSetChange(Sender: TObject);
  protected
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: IKmlLayerConfig; ALayersSet: IActiveMapsSet);
    destructor Destroy; override;
    procedure StartThreads; override;
    procedure MouseOnReg(var APWL: TResObj; xy: TPoint);
    property Visible: Boolean read GetVisible;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  StrUtils,
  Graphics,
  GR32_Polygons,
  i_CoordConverter,
  i_TileIterator,
  u_NotifyEventListener,
  u_TileIteratorByRect;

{ TWikiLayerElement }

constructor TWikiLayerElement.Create;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  FPolygonOnBitmap := nil;
end;

destructor TWikiLayerElement.Destroy;
begin
  name_blok := '';
  num_blok := '';
  description := '';
  FPolygonOnBitmap := nil;
  inherited;
end;

{ TWikiLayer }

constructor TWikiLayer.Create(AParentMap: TImage32; AViewPortState: IViewPortState; AConfig: IKmlLayerConfig; ALayersSet: IActiveMapsSet);
begin
  inherited Create(AParentMap, AViewPortState);
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

  Layer.Bitmap.DrawMode := dmTransparent;
  Layer.Bitmap.Font.Charset := RUSSIAN_CHARSET;

  FWikiLayerElments := nil;
  SetLength(FFixedPointArray, 256);
end;

destructor TWikiLayer.Destroy;
begin
  Clear;
  FWikiLayerElments := nil;
  FFixedPointArray := nil;
  inherited;
end;

procedure TWikiLayer.AddFromLayer(Alayer: TMapType; ALocalConverter: ILocalCoordConverter);
var
  ii: integer;
  kml: TKmlInfoSimple;
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
    KML := TKmlInfoSimple.Create;
    try
      if Alayer.LoadTile(kml, VTile, Vzoom, true, True) then begin
        for ii := 0 to length(KML.Data) - 1 do begin
          addWL(KML.Data[ii], ALocalConverter);
        end;
      end;
    finally
      KML.Free;
    end;
  end;
end;

procedure TWikiLayer.addWL(var AData: TKMLData; ALocalConverter: ILocalCoordConverter);
var
  i, lenLay: integer;
  VConverter: ICoordConverter;
  VSize: TPoint;
  VElement: TWikiLayerElement;
  VBounds: TDoubleRect;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  VConverter := ALocalConverter.GetGeoConverter;
  Delete(AData.description, posEx('#ge', AData.description, 1), 1);
  VConverter.CheckLonLatRect(AData.Bounds);
  VBounds := ALocalConverter.LonLatRect2LocalRectFloat(AData.Bounds);
  if AData.Bounds.Left = AData.Bounds.Right then begin
    VBounds.Left := VBounds.Left - 3;
    VBounds.Right := VBounds.Right + 3;
  end;
  if AData.Bounds.Top = AData.Bounds.Bottom then begin
    VBounds.Top := VBounds.Top - 3;
    VBounds.Bottom := VBounds.Bottom + 3;
  end;
  if (((VBounds.Right - VBounds.Left) <= 1) or ((VBounds.Bottom - VBounds.Top) <= 1) or
    ((VBounds.Top > VSize.Y) or (VBounds.Bottom < 0) or (VBounds.Left > VSize.X) or (VBounds.Right < 0))) then begin
  end else begin
    VElement := TWikiLayerElement.Create;
    With VElement do begin
      FBounds := VBounds;
      name_blok := AData.name;
      num_blok := AData.PlacemarkID;
      description := AData.description;
      setLength(FPolygonOnBitmap, length(AData.coordinates));
      for i := 0 to length(AData.coordinates) - 1 do begin
        VConverter.CheckLonLatPos(AData.coordinates[i]);
        FPolygonOnBitmap[i] := ALocalConverter.LonLat2LocalPixelFloat(AData.coordinates[i]);
      end;
    end;
    setLength(FWikiLayerElments, length(FWikiLayerElments) + 1);
    lenLay := length(FWikiLayerElments);
    FWikiLayerElments[lenLay - 1] := VElement;
  end;
end;

procedure TWikiLayer.Clear;
var
  i: integer;
begin
  for i := 0 to length(FWikiLayerElments) - 1 do begin
    FreeAndNil(FWikiLayerElments[i]);
  end;
  FWikiLayerElments := nil;
end;

procedure TWikiLayer.MouseOnReg(var APWL: TResObj; xy: TPoint);
var
  i: integer;
  VLen: integer;
  VXY: TDoublePoint;
  VSquare: Double;
begin
  VXY := LayerCoordConverter.MapPixelFloat2LocalPixelFloat(ViewCoordConverter.LocalPixelFloat2MapPixelFloat(DoublePoint(xy)));
  for i := 0 to length(FWikiLayerElments) - 1 do begin
    if (VXY.x > FWikiLayerElments[i].FBounds.Left - 5) and (VXY.x < FWikiLayerElments[i].FBounds.Right + 5) and
      (VXY.y > FWikiLayerElments[i].FBounds.Top - 5) and (VXY.y < FWikiLayerElments[i].FBounds.Bottom + 5) then begin
      VLen := length(FWikiLayerElments[i].FPolygonOnBitmap);
      if VLen > 0 then begin
        if VLen = 1 then begin
          APWL.name := FWikiLayerElments[i].name_blok;
          APWL.descr := FWikiLayerElments[i].description;
          APWL.numid := FWikiLayerElments[i].num_blok;
          APWL.find := true;
          Break;
        end else begin
          if (FWikiLayerElments[i].FPolygonOnBitmap[0].X <> FWikiLayerElments[i].FPolygonOnBitmap[VLen - 1].x) or
            (FWikiLayerElments[i].FPolygonOnBitmap[0].y <> FWikiLayerElments[i].FPolygonOnBitmap[VLen - 1].y) then begin
            if PointOnPath(VXY, FWikiLayerElments[i].FPolygonOnBitmap, 3) then begin
              APWL.name := FWikiLayerElments[i].name_blok;
              APWL.descr := FWikiLayerElments[i].description;
              APWL.numid := FWikiLayerElments[i].num_blok;
              APWL.find := true;
              exit;
            end;
          end else if PtInRgn(FWikiLayerElments[i].FPolygonOnBitmap, VXY) then begin
            VSquare := PolygonSquare(FWikiLayerElments[i].FPolygonOnBitmap);
            if (VSquare > APWL.S) and (APWL.S <> 0) then begin
              continue;
            end;
            APWL.S := VSquare;
            APWL.name := FWikiLayerElments[i].name_blok;
            APWL.descr := FWikiLayerElments[i].description;
            APWL.numid := FWikiLayerElments[i].num_blok;
            APWL.find := true;
          end;
        end;
      end;
    end;
  end;
end;

procedure TWikiLayer.OnConfigChange(Sender: TObject);
begin
  Redraw;
end;

procedure TWikiLayer.OnLayerSetChange(Sender: TObject);
var
  VGUID: TGUID;
  i: Cardinal;
begin
  FMapsList := FLayersSet.GetSelectedMapsList;
  Redraw;
  SetVisible(FMapsList.GetIterator.Next(1, VGUID, i) = S_OK);
end;

procedure TWikiLayer.StartThreads;
begin
  inherited;
  OnLayerSetChange(nil);
end;

procedure TWikiLayer.DrawWikiElement(var AData: TWikiLayerElement; ALocalConverter: ILocalCoordConverter);
var
  VPolygon: TPolygon32;
  VLen: integer;
  i: integer;
  VColorMain: TColor32;
  VColorBG: TColor32;
  VPointColor: TColor32;
  VRect: TRect;
begin
  FConfig.LockRead;
  try
    VColorMain := FConfig.MainColor;
    VColorBG := FConfig.ShadowColor;
    VPointColor := FConfig.PointColor;
  finally
    FConfig.UnlockRead;
  end;
  VPolygon := TPolygon32.Create;
  VPolygon.Antialiased := true;
  VPolygon.AntialiasMode := am4times;
  try
    VLen := Length(AData.FPolygonOnBitmap);
    if VLen > 1 then begin
      if Length(FFixedPointArray) < VLen then begin
        SetLength(FFixedPointArray, VLen);
      end;
      for i := 0 to VLen - 1 do begin
        FFixedPointArray[i] := FixedPoint(AData.FPolygonOnBitmap[i].X, AData.FPolygonOnBitmap[i].Y);
      end;
      VPolygon.AddPoints(FFixedPointArray[0], VLen);
      VPolygon.DrawEdge(Layer.Bitmap, VColorBG);
      VPolygon.Offset(Fixed(0.9), Fixed(0.9));
      VPolygon.DrawEdge(Layer.Bitmap, VColorMain);
    end else begin
      VRect.Left := Trunc(AData.FPolygonOnBitmap[0].X - 3);
      VRect.Top := Trunc(AData.FPolygonOnBitmap[0].Y - 3);
      VRect.Right := VRect.Left + 7;
      VRect.Bottom := VRect.Top + 7;
      Layer.Bitmap.FillRectS(VRect, VColorBG);
      Inc(VRect.Left);
      Inc(VRect.Top);
      Dec(VRect.Right);
      Dec(VRect.Bottom);
      Layer.Bitmap.FillRectS(VRect, VPointColor);
    end;
  finally
    FreeAndNil(VPolygon);
  end;
end;

procedure TWikiLayer.DoRedraw;
var
  i: Cardinal;
  VMapType: TMapType;
  VGUID: TGUID;
  VItem: IMapType;
  VEnum: IEnumGUID;
  VHybrList: IMapTypeList;
  ii: Integer;
  VLocalConverter: ILocalCoordConverter;
begin
  inherited;
  Clear;
  Layer.Bitmap.Clear(clBlack);
  if FMapsList <> nil then begin
    VLocalConverter := LayerCoordConverter;
    VHybrList := FMapsList;
    VEnum := VHybrList.GetIterator;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := VHybrList.GetMapTypeByGUID(VGUID);
      VMapType := VItem.GetMapType;
      if VMapType.IsKmlTiles then begin
        AddFromLayer(VMapType, VLocalConverter);
      end;
    end;
    Layer.Bitmap.BeginUpdate;
    try
      for ii := 0 to Length(FWikiLayerElments) - 1 do begin
        DrawWikiElement(FWikiLayerElments[ii], VLocalConverter);
      end;
    finally
      Layer.Bitmap.EndUpdate;
    end;
  end;
end;

end.
