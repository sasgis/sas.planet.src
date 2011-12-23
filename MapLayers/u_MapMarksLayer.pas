unit u_MapMarksLayer;

interface

uses
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
  u_GeoFun,
  i_ViewPortState,
  i_CoordConverter,
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
    FMarkDB: TMarksSystem;
    FMarksSubset: IMarksSubset;
    FGetMarksCounter: IInternalPerformanceCounter;
    FMouseOnRegCounter: IInternalPerformanceCounter;
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
      APerfList: IInternalPerformanceCounterList;
      AAppClosingNotifier: IJclNotifier;
      AParentMap: TImage32;
      AViewPortState: IViewPortState;
      AResamplerConfig: IImageResamplerConfig;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      AClearStrategyFactory: ILayerBitmapClearStrategyFactory;
      ATimerNoifier: IJclNotifier;
      AConfig: IMarksLayerConfig;
      AMarkDB: TMarksSystem
    );
    procedure MouseOnReg(xy: TPoint; out AMark: IMark; out AMarkS: Double); overload;
    procedure MouseOnReg(xy: TPoint; out AMark: IMark); overload;

    function GetIntersection(CurrLonLat: TDoublePoint; var IntersectionLonLat: TDoublePoint; VMarkPoly: IMarkPoly; AConverter: ICoordConverter; AZoom: byte):boolean; overload;
    function GetIntersection(CurrLonLat: TDoublePoint; var IntersectionLonLat: TDoublePoint; VMarkLine: IMarkLine; AConverter: ICoordConverter; AZoom: byte):boolean; overload;
  end;

implementation

uses
  ActiveX,
  Types,
  Classes,
  SysUtils,
  Math,
  i_TileIterator,
  i_BitmapLayerProvider,
  u_MapMarksBitmapLayerProviderByMarksSubset,
  u_NotifyEventListener,
  u_TileIteratorSpiralByRect;

{ TMapMarksLayer }

constructor TMapMarksLayer.Create(
  APerfList: IInternalPerformanceCounterList;
  AAppClosingNotifier: IJclNotifier;
  AParentMap: TImage32;
  AViewPortState: IViewPortState;
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
    tpLower
  );
  FGetMarksCounter := PerfList.CreateAndAddNewCounter('GetMarks');
  FMouseOnRegCounter := PerfList.CreateAndAddNewCounter('MouseOnReg');

  FConfig := AConfig;
  FMarkDB := AMarkDB;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.MarksShowConfig.GetChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.MarksDrawConfig.GetChangeNotifier
  );
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
  { Прямоугольник тайла подлежащий отображению на текущий растр }
  VTilePixelsToDraw: TRect;
  { Прямоугольник пикселов в которые будет скопирован текущий тайл }
  VCurrTileOnBitmapRect: TRect;
  VProv: IBitmapLayerProvider;
  VMarksSubset: IMarksSubset;
  VCounterContext: TInternalPerformanceCounterContext;
begin
  VCounterContext := FGetMarksCounter.StartOperation;
  VBitmapConverter := LayerCoordConverter;
  if VBitmapConverter <> nil then begin
    try
      VMarksSubset := GetMarksSubset(VBitmapConverter);
      FMarksSubset := VMarksSubset;
    finally
      FGetMarksCounter.FinishOperation(VCounterContext);
    end;
    if (VMarksSubset <> nil) and (not VMarksSubset.IsEmpty) then begin
      VProv :=
        TMapMarksBitmapLayerProviderByMarksSubset.Create(
          FDrawConfigStatic,
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
            VCurrTilePixelRect := VGeoConvert.TilePos2PixelRect(VTile, VZoom);

            VTilePixelsToDraw.TopLeft := Point(0, 0);
            VTilePixelsToDraw.Right := VCurrTilePixelRect.Right - VCurrTilePixelRect.Left;
            VTilePixelsToDraw.Bottom := VCurrTilePixelRect.Bottom - VCurrTilePixelRect.Top;

            VCurrTileOnBitmapRect.TopLeft := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.TopLeft);
            VCurrTileOnBitmapRect.BottomRight := VBitmapConverter.MapPixel2LocalPixel(VCurrTilePixelRect.BottomRight);

            VTileToDrawBmp.SetSize(VTilePixelsToDraw.Right, VTilePixelsToDraw.Bottom);
            VTileToDrawBmp.Clear(0);
            VProv.GetBitmapRect(
              AOperationID,
              ACancelNotifier,
              VTileToDrawBmp,
              ConverterFactory.CreateForTile(VTile, VZoom, VGeoConvert)
            );
            Layer.Bitmap.Lock;
            try
              if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
                break;
              end;
              Layer.Bitmap.Draw(VCurrTileOnBitmapRect, VTilePixelsToDraw, VTileToDrawBmp);
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
  VLonLatLine: TArrayOfDoublePoint;
  VLineOnBitmap: TArrayOfDoublePoint;
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
begin
  VCounterContext := FMouseOnRegCounter.StartOperation;
  try
    AMark := nil;
    AMarkS := 0;
    VMarksSubset := FMarksSubset;
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
                VLonLatLine := VMarkLine.Points;
                VConverter.CheckLonLatArray(VLonLatLine);
                VLineOnBitmap := VConverter.LonLatArray2PixelArrayFloat(VLonLatLine, VZoom);
                if PointOnPath(VPixelPos, VLineOnBitmap, (VMarkLine.LineWidth / 2) + 3) then begin
                  AMark := VMark;
                  AMarkS := 0;
                  exit;
                end;
              end else if Supports(VMark, IMarkPoly, VMarkPoly) then begin
                VLonLatLine := VMarkPoly.Points;
                VConverter.CheckLonLatArray(VLonLatLine);
                VLineOnBitmap := VConverter.LonLatArray2PixelArrayFloat(VLonLatLine, VZoom);
                if (PtInRgn(@VLineOnBitmap[0], Length(VLineOnBitmap), VPixelPos)) or
                   (PointOnPath(VPixelPos, VLineOnBitmap, (VMarkPoly.LineWidth / 2) + 3)) then begin
                  VSquare := PolygonSquare(VLineOnBitmap);
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

procedure TMapMarksLayer.MouseOnReg(xy: TPoint; out AMark: IMark);
var
  VMarkS: Double;
begin
  MouseOnReg(xy, AMark, VMarkS);
end;

function TMapMarksLayer.GetIntersection(CurrLonLat: TDoublePoint; var IntersectionLonLat: TDoublePoint; VMarkPoly: IMarkPoly; AConverter: ICoordConverter; AZoom: byte):boolean;
var i:integer;
    LineXY21, LineXY22, ResultXY: TDoublePoint;
    k,b,d,r: double;
    CircXY1,CircXY2:TDoublePoint;
begin
  Result:=False;
  r:=(VMarkPoly.LineWidth / 2) + 3;

  for i:=0 to length(VMarkPoly.Points) - 1 do begin
    LineXY21:=AConverter.LonLat2PixelPosFloat(VMarkPoly.Points[i],AZoom);
    LineXY22:=AConverter.LonLat2PixelPosFloat(CurrLonLat,AZoom);
    if (LineXY22.x>=LineXY21.X-r)and(LineXY22.x<=LineXY21.X+r)and
       (LineXY22.y>=LineXY21.Y-r)and(LineXY22.y<=LineXY21.Y+r) then begin
         IntersectionLonLat:=VMarkPoly.Points[i];
         Result:=true;
         exit;
       end;
  end;

  CurrLonLat:=AConverter.LonLat2PixelPosFloat(CurrLonLat,AZoom);
  for i:=0 to length(VMarkPoly.Points) - 2 do begin
    LineXY21:=VMarkPoly.Points[i];
    LineXY21:=AConverter.LonLat2PixelPosFloat(LineXY21,AZoom);
    LineXY22:=VMarkPoly.Points[i+1];
    LineXY22:=AConverter.LonLat2PixelPosFloat(LineXY22,AZoom);

    k:=(LineXY21.y-LineXY22.y)/(LineXY21.x-LineXY22.x);
    b:=LineXY21.y-k*LineXY21.x;
    d:=sqr(2*k*b-2*CurrLonLat.x-2*CurrLonLat.y*k)-(4+4*sqr(k))*
       (sqr(b)-sqr(r)+sqr(CurrLonLat.x)+sqr(CurrLonLat.y)-2*CurrLonLat.y*b);
    if(d>=0) then begin
      CircXY1.x:=((-(2*k*b-2*CurrLonLat.x-2*CurrLonLat.y*k)-sqrt(d))/(2+2*sqr(k)));
      CircXY2.x:=((-(2*k*b-2*CurrLonLat.x-2*CurrLonLat.y*k)+sqrt(d))/(2+2*sqr(k)));
      if (CircXY1.x=CircXY2.x) then begin
        ResultXY:=DoublePoint(CircXY1.x,CircXY1.x);
      end else begin
        CircXY1.y:=k*CircXY1.x+b;
        CircXY2.y:=k*CircXY2.x+b;
        ResultXY:=DoublePoint((CircXY1.x+CircXY2.x)/2,(CircXY1.y+CircXY2.y)/2);
      end;
      if (ResultXY.x>min(LineXY21.x,LineXY22.x))and(ResultXY.x<max(LineXY21.x,LineXY22.x))and
         (ResultXY.y>min(LineXY21.y,LineXY22.y))and(ResultXY.y<max(LineXY21.y,LineXY22.y))then begin
        IntersectionLonLat:=AConverter.PixelPosFloat2LonLat(ResultXY,AZoom);
        exit;
      end;
    end;
  end;
end;

function TMapMarksLayer.GetIntersection(CurrLonLat: TDoublePoint; var IntersectionLonLat: TDoublePoint; VMarkLine: IMarkLine; AConverter: ICoordConverter; AZoom: byte):boolean;
var i:integer;
    LineXY21, LineXY22, ResultXY: TDoublePoint;
    k,b,d,r: double;
    CircXY1,CircXY2:TDoublePoint;
begin
  Result:=False;
  r:=(VMarkLine.LineWidth / 2) + 3;

  for i:=0 to length(VMarkLine.Points) - 1 do begin
    LineXY21:=AConverter.LonLat2PixelPosFloat(VMarkLine.Points[i],AZoom);
    LineXY22:=AConverter.LonLat2PixelPosFloat(CurrLonLat,AZoom);
    if (LineXY22.x>=LineXY21.X-r)and(LineXY22.x<=LineXY21.X+r)and
       (LineXY22.y>=LineXY21.Y-r)and(LineXY22.y<=LineXY21.Y+r) then begin
         IntersectionLonLat:=VMarkLine.Points[i];
         Result:=true;
         exit;
       end;
  end;

  CurrLonLat:=AConverter.LonLat2PixelPosFloat(CurrLonLat,AZoom);
  for i:=0 to length(VMarkLine.Points) - 2 do begin
    LineXY21:=VMarkLine.Points[i];
    LineXY21:=AConverter.LonLat2PixelPosFloat(LineXY21,AZoom);
    LineXY22:=VMarkLine.Points[i+1];
    LineXY22:=AConverter.LonLat2PixelPosFloat(LineXY22,AZoom);

    k:=(LineXY21.y-LineXY22.y)/(LineXY21.x-LineXY22.x);
    b:=LineXY21.y-k*LineXY21.x;
    d:=sqr(2*k*b-2*CurrLonLat.x-2*CurrLonLat.y*k)-(4+4*sqr(k))*
       (sqr(b)-sqr(r)+sqr(CurrLonLat.x)+sqr(CurrLonLat.y)-2*CurrLonLat.y*b);
    if(d>=0) then begin
      CircXY1.x:=((-(2*k*b-2*CurrLonLat.x-2*CurrLonLat.y*k)-sqrt(d))/(2+2*sqr(k)));
      CircXY2.x:=((-(2*k*b-2*CurrLonLat.x-2*CurrLonLat.y*k)+sqrt(d))/(2+2*sqr(k)));
      if (CircXY1.x=CircXY2.x) then begin
        ResultXY:=DoublePoint(CircXY1.x,CircXY1.x);
      end else begin
        CircXY1.y:=k*CircXY1.x+b;
        CircXY2.y:=k*CircXY2.x+b;
        ResultXY:=DoublePoint((CircXY1.x+CircXY2.x)/2,(CircXY1.y+CircXY2.y)/2);
      end;
      if (ResultXY.x>min(LineXY21.x,LineXY22.x))and(ResultXY.x<max(LineXY21.x,LineXY22.x))and
         (ResultXY.y>min(LineXY21.y,LineXY22.y))and(ResultXY.y<max(LineXY21.y,LineXY22.y))then begin
        IntersectionLonLat:=AConverter.PixelPosFloat2LonLat(ResultXY,AZoom);
        exit;
      end;
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
