unit u_ImageLineProvider;

interface

uses
  Types,
  GR32,
  i_OperationNotifier,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_LocalCoordConverterFactorySimpe,
  i_ImageLineProvider,
  i_BitmapPostProcessingConfig,
  i_VectorItemProjected,
  i_BitmapLayerProvider,
  u_MapType;

type
  TImageLineProviderAbstract = class(TInterfacedObject, IImageLineProvider)
  private
    FCancelNotifier: IOperationNotifier;
    FOperationID: Integer;
    FRecolorConfig: IBitmapPostProcessingConfigStatic;
    FConverterFactory: ILocalCoordConverterFactorySimpe;
    FLocalConverter: ILocalCoordConverter;
    FBytesPerPixel: Integer;
    FMapTypeMain: TMapType;
    FMapTypeHybr: TMapType;
    FPolyProjected: IProjectedPolygon;
    FMarksImageProvider: IBitmapLayerProvider;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
    FBackGroundColor: TColor32;

    FZoom: byte;
    FMainGeoConverter: ICoordConverter;
    FLine: IProjectedPolygonLine;
    FTempBitmap: TCustomBitmap32;
    FPreparedData: array of Pointer;
    FPreparedConverter: ILocalCoordConverter;

    function PrepareConverterForLocalLine(ALine: Integer): ILocalCoordConverter;
    function GetLocalLine(ALine: Integer): Pointer;
    procedure AddTile(
      ATargetBitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    );
    function PrepareTileBitmap(
      ATargetBitmap: TCustomBitmap32;
      AConverter: ILocalCoordConverter
    ): Boolean;
    procedure PrepareBufferMem(ARect: TRect);
    procedure ClearBuffer;
    procedure PrepareBufferData(AConverter: ILocalCoordConverter);
  protected
    procedure PreparePixleLine(ASource: PColor32; ATarget: Pointer; ACount: Integer); virtual; abstract;
  private
    function GetLocalConverter: ILocalCoordConverter;
    function GetBytesPerPixel: Integer;
    function GetLine(ALine: Integer): Pointer;
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      ALocalConverter: ILocalCoordConverter;
      APolyProjected: IProjectedPolygon;
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AMarksImageProvider: IBitmapLayerProvider;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean;
      ABackGroundColor: TColor32;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AConverterFactory: ILocalCoordConverterFactorySimpe;
      ABytesPerPixel: Integer
    );
    destructor Destroy; override;
  end;

  TImageLineProviderNoAlfa = class(TImageLineProviderAbstract)
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      ALocalConverter: ILocalCoordConverter;
      APolyProjected: IProjectedPolygon;
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AMarksImageProvider: IBitmapLayerProvider;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean;
      ABackGroundColor: TColor32;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AConverterFactory: ILocalCoordConverterFactorySimpe
    );
  end;

  TImageLineProviderWithAlfa = class(TImageLineProviderAbstract)
  public
    constructor Create(
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer;
      ALocalConverter: ILocalCoordConverter;
      APolyProjected: IProjectedPolygon;
      AMapTypeMain: TMapType;
      AMapTypeHybr: TMapType;
      AMarksImageProvider: IBitmapLayerProvider;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean;
      ABackGroundColor: TColor32;
      ARecolorConfig: IBitmapPostProcessingConfigStatic;
      AConverterFactory: ILocalCoordConverterFactorySimpe
    );
  end;

  TImageLineProviderRGB = class(TImageLineProviderNoAlfa)
  protected
    procedure PreparePixleLine(ASource: PColor32; ATarget: Pointer; ACount: Integer); override;
  end;

  TImageLineProviderBGR = class(TImageLineProviderNoAlfa)
  protected
    procedure PreparePixleLine(ASource: PColor32; ATarget: Pointer; ACount: Integer); override;
  end;

  TImageLineProviderRGBA = class(TImageLineProviderWithAlfa)
  protected
    procedure PreparePixleLine(ASource: PColor32; ATarget: Pointer; ACount: Integer); override;
  end;

  TImageLineProviderBGRA = class(TImageLineProviderWithAlfa)
  protected
    procedure PreparePixleLine(ASource: PColor32; ATarget: Pointer; ACount: Integer); override;
  end;

implementation

uses
  SysUtils,
  u_GeoFun;

{ TImageLineProviderAbstract }

constructor TImageLineProviderAbstract.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  ALocalConverter: ILocalCoordConverter;
  APolyProjected: IProjectedPolygon;
  AMapTypeMain, AMapTypeHybr: TMapType;
  AMarksImageProvider: IBitmapLayerProvider;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean;
  ABackGroundColor: TColor32;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AConverterFactory: ILocalCoordConverterFactorySimpe;
  ABytesPerPixel: Integer
);
begin
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FLocalConverter := ALocalConverter;
  FPolyProjected := APolyProjected;
  FMapTypeMain := AMapTypeMain;
  FMapTypeHybr := AMapTypeHybr;
  FMarksImageProvider := AMarksImageProvider;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
  FBackGroundColor := ABackGroundColor;
  FRecolorConfig := ARecolorConfig;
  FConverterFactory := AConverterFactory;
  FBytesPerPixel := ABytesPerPixel;

  FZoom := FLocalConverter.Zoom;
  FMainGeoConverter := FLocalConverter.GeoConverter;
  FLine := FPolyProjected.Item[0];
  FTempBitmap := TCustomBitmap32.Create;
end;

destructor TImageLineProviderAbstract.Destroy;
begin
  ClearBuffer;
  FreeAndNil(FTempBitmap);
  inherited;
end;

procedure TImageLineProviderAbstract.AddTile(ATargetBitmap: TCustomBitmap32;
  AConverter: ILocalCoordConverter);
var
  i: Integer;
  VBitmapRect: TRect;
  VPreparedMapRect: TRect;
  VIntersectionAtPrepared: TRect;
  VIntersectionAtBitmap: TRect;
begin
  VBitmapRect := AConverter.GetRectInMapPixel;
  VPreparedMapRect := FPreparedConverter.GetRectInMapPixel;
  Assert(VBitmapRect.Top = VPreparedMapRect.Top);
  Assert(VBitmapRect.Bottom = VPreparedMapRect.Bottom);
  Assert(VBitmapRect.Right > VPreparedMapRect.Left);
  Assert(VBitmapRect.Left < VPreparedMapRect.Right);

  if VBitmapRect.Left < VPreparedMapRect.Left then begin
    VBitmapRect.Left := VPreparedMapRect.Left;
  end;
  if VBitmapRect.Right > VPreparedMapRect.Right then begin
    VBitmapRect.Right := VPreparedMapRect.Right;
  end;
  VIntersectionAtPrepared := FPreparedConverter.MapRect2LocalRect(VBitmapRect);
  VIntersectionAtBitmap := AConverter.MapRect2LocalRect(VBitmapRect);
  for i := 0 to (VBitmapRect.Bottom - VBitmapRect.Top - 1) do begin
    PreparePixleLine(
      ATargetBitmap.PixelPtr[i, VIntersectionAtBitmap.Left],
      Pointer(Integer(FPreparedData[i]) + VIntersectionAtPrepared.Left),
      VBitmapRect.Right - VBitmapRect.Right
    );
  end;
end;

procedure TImageLineProviderAbstract.ClearBuffer;
var
  i: Integer;
begin
  for i := 0 to Length(FPreparedData) - 1 do begin
    if FPreparedData[i] <> nil then begin
      FreeMem(FPreparedData[i]);
      FPreparedData[i] := nil;
    end;
  end;
  FPreparedData := nil;
end;

function TImageLineProviderAbstract.GetBytesPerPixel: Integer;
begin
  Result := FBytesPerPixel;
end;

function TImageLineProviderAbstract.GetLine(ALine: Integer): Pointer;
var
  VRect: TRect;
begin
  if (FPreparedConverter <> nil) then begin
    VRect := FPreparedConverter.GetLocalRect;
    if (ALine < VRect.Top) or (ALine >= VRect.Bottom) then begin
      FPreparedConverter := nil;
    end;
  end;

  if FPreparedConverter =  nil then begin
    FPreparedConverter := PrepareConverterForLocalLine(ALine);
    PrepareBufferData(FPreparedConverter);
  end;
  Result := GetLocalLine(ALine);
end;

function TImageLineProviderAbstract.GetLocalConverter: ILocalCoordConverter;
begin
  Result := FLocalConverter;
end;

function TImageLineProviderAbstract.GetLocalLine(ALine: Integer): Pointer;
var
  VPreparedLocalRect: TRect;
begin
  VPreparedLocalRect := FPreparedConverter.GetLocalRect;
  Assert(ALine >= VPreparedLocalRect.Top);
  Assert(ALine < VPreparedLocalRect.Bottom);
  Result := FPreparedData[ALine - VPreparedLocalRect.Top];
end;

procedure TImageLineProviderAbstract.PrepareBufferData(
  AConverter: ILocalCoordConverter);
var
  VTile: TPoint;
  i, j: Integer;
  VTileConverter: ILocalCoordConverter;
  VRectOfTile: TRect;
begin
  PrepareBufferMem(AConverter.GetLocalRect);
  VRectOfTile := FMainGeoConverter.PixelRect2TileRect(AConverter.GetRectInMapPixel, FZoom);
  for i := VRectOfTile.Top to VRectOfTile.Bottom - 1 do begin
    VTile.Y := i;
    for j := VRectOfTile.Left to VRectOfTile.Right - 1 do begin
      VTile.X := j;
      VTileConverter := FConverterFactory.CreateForTile(VTile, FZoom, FMainGeoConverter);
      PrepareTileBitmap(FTempBitmap, VTileConverter);
      AddTile(FTempBitmap, VTileConverter);
    end;
  end;
end;

procedure TImageLineProviderAbstract.PrepareBufferMem(ARect: TRect);
var
  VLinesExists: Integer;
  VLinesNeed: Integer;
  VWidth: Integer;
  i: Integer;
begin
  VWidth := ARect.Right - ARect.Left;
  VLinesNeed :=  ARect.Bottom - ARect.Top;
  VLinesExists := Length(FPreparedData);
  if VLinesExists < VLinesNeed then begin
    SetLength(FPreparedData, VLinesNeed);
    for i := VLinesExists to VLinesNeed - 1 do begin
      GetMem(FPreparedData[i], (VWidth + 1) * FBytesPerPixel);
    end;
  end;
end;

function TImageLineProviderAbstract.PrepareConverterForLocalLine(
  ALine: Integer): ILocalCoordConverter;
var
  VPixel: TPoint;
  VTile: TPoint;
  VPreparedMapRect: TRect;
  VCurrentPieceRect: TRect;
  VCurrentPieceMapRect: TRect;
  VPreparedLocalRect: TRect;
  VPixelRect: TRect;
begin
  VCurrentPieceRect := FLocalConverter.GetLocalRect;
  VPixel :=
    FLocalConverter.LocalPixel2MapPixel(
      Point(
        VCurrentPieceRect.Left,
        ALine
      )
    );
  VTile := FMainGeoConverter.PixelPos2TilePos(VPixel, FZoom);
  VPixelRect := FMainGeoConverter.TilePos2PixelRect(VTile, FZoom);
  VCurrentPieceMapRect := FLocalConverter.GetRectInMapPixel;
  VPreparedMapRect :=
    Rect(
      VCurrentPieceMapRect.Left,
      VPixelRect.Top,
      VCurrentPieceMapRect.Right,
      VPixelRect.Bottom
    );

  VPreparedLocalRect := FLocalConverter.MapRect2LocalRect(VPreparedMapRect);
  Result :=
    FConverterFactory.CreateConverter(
      VPreparedLocalRect,
      FZoom,
      FMainGeoConverter,
      DoublePoint(1, 1),
      DoublePoint(VCurrentPieceMapRect.TopLeft)
    );
end;

function TImageLineProviderAbstract.PrepareTileBitmap(
  ATargetBitmap: TCustomBitmap32; AConverter: ILocalCoordConverter): Boolean;
var
  VLoadResult: Boolean;
  VTileSize: TPoint;
begin
  VTileSize := AConverter.GetLocalRectSize;
  FTempBitmap.SetSize(VTileSize.X, VTileSize.Y);
  Result := False;
  if FLine.IsRectIntersectPolygon(AConverter.GetRectInMapPixelFloat) then begin
    VLoadResult := False;
    if FMapTypeMain <> nil then begin
      VLoadResult := FMapTypeMain.LoadBtimapUni(ATargetBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, AConverter.GetGeoConverter, FUsePrevZoomAtMap, True, True);
    end;
    if not VLoadResult then begin
      ATargetBitmap.Clear(FBackGroundColor);
    end else begin
      Result := True;
    end;
    if FMapTypeHybr <> nil then begin
      VLoadResult := FMapTypeHybr.LoadBtimapUni(FTempBitmap, AConverter.GetRectInMapPixel, AConverter.GetZoom, AConverter.GetGeoConverter, FUsePrevZoomAtLayer, True, True);
      if VLoadResult then begin
        FTempBitmap.DrawMode := dmBlend;
        ATargetBitmap.Draw(0, 0, FTempBitmap);
        Result := True;
      end;
    end;
    if Result then begin
      if FRecolorConfig <> nil then begin
        FRecolorConfig.ProcessBitmap(ATargetBitmap);
      end;
    end;
    if FMarksImageProvider <> nil then begin
      FMarksImageProvider.GetBitmapRect(FOperationID, FCancelNotifier, ATargetBitmap, AConverter);
      Result := True;
    end;
  end else begin
    ATargetBitmap.Clear(FBackGroundColor);
  end;
end;

{ TImageLineProviderNoAlfa }

constructor TImageLineProviderNoAlfa.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  ALocalConverter: ILocalCoordConverter;
  APolyProjected: IProjectedPolygon;
  AMapTypeMain, AMapTypeHybr: TMapType;
  AMarksImageProvider: IBitmapLayerProvider;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean;
  ABackGroundColor: TColor32;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    ALocalConverter,
    APolyProjected,
    AMapTypeMain, AMapTypeHybr,
    AMarksImageProvider,
    AUsePrevZoomAtMap, AUsePrevZoomAtLayer,
    ABackGroundColor,
    ARecolorConfig,
    AConverterFactory,
    3
  );
end;

{ TImageLineProviderWithAlfa }

constructor TImageLineProviderWithAlfa.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer;
  ALocalConverter: ILocalCoordConverter;
  APolyProjected: IProjectedPolygon;
  AMapTypeMain, AMapTypeHybr: TMapType;
  AMarksImageProvider: IBitmapLayerProvider;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean;
  ABackGroundColor: TColor32;
  ARecolorConfig: IBitmapPostProcessingConfigStatic;
  AConverterFactory: ILocalCoordConverterFactorySimpe
);
begin
  inherited Create(
    ACancelNotifier,
    AOperationID,
    ALocalConverter,
    APolyProjected,
    AMapTypeMain, AMapTypeHybr,
    AMarksImageProvider,
    AUsePrevZoomAtMap, AUsePrevZoomAtLayer,
    ABackGroundColor,
    ARecolorConfig,
    AConverterFactory,
    3
  );
end;

type
  TBGR = packed record
    B: Byte;
    G: Byte;
    R: Byte;
  end;

  TRGB = packed record
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  TRGBA = packed record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;



{ TImageLineProviderRGB }

procedure TImageLineProviderRGB.PreparePixleLine(ASource: PColor32;
  ATarget: Pointer; ACount: Integer);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TRGB;
begin
  VSource := PColor32Entry(ASource);
  VTarget := ATarget;
  for i := 0 to  ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageLineProviderBGR }

procedure TImageLineProviderBGR.PreparePixleLine(ASource: PColor32;
  ATarget: Pointer; ACount: Integer);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TBGR;
begin
  VSource := PColor32Entry(ASource);
  VTarget := ATarget;
  for i := 0 to  ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageLineProviderARGB }

procedure TImageLineProviderRGBA.PreparePixleLine(ASource: PColor32;
  ATarget: Pointer; ACount: Integer);
var
  i: Integer;
  VSource: PColor32Entry;
  VTarget: ^TRGBA;
begin
  VSource := PColor32Entry(ASource);
  VTarget := ATarget;
  for i := 0 to  ACount - 1 do begin
    VTarget.B := VSource.B;
    VTarget.G := VSource.G;
    VTarget.R := VSource.R;
    VTarget.A := VSource.A;
    Inc(VSource);
    Inc(VTarget);
  end;
end;

{ TImageLineProviderBGRA }

procedure TImageLineProviderBGRA.PreparePixleLine(ASource: PColor32;
  ATarget: Pointer; ACount: Integer);
begin
  Move(ASource^, ATarget^, ACount); 
end;

end.
