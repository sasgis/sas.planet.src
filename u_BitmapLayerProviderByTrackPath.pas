unit u_BitmapLayerProviderByTrackPath;

interface

uses
  GR32,
  GR32_Polygons,
  t_GeoTypes,
  i_NotifierOperation,
  i_ProjectionInfo,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_GPSRecorder,
  i_MapLayerGPSTrackConfig,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByTrackPath = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FLineWidth: Double;
    FTrackColorer: ITrackColorerStatic;
    FBitmapFactory: IBitmap32StaticFactory;

    FRectIsEmpty: Boolean;
    FLonLatRect: TDoubleRect;
    FPoints: array of TGPSTrackPoint;
    FPointsCount: Integer;
    FPolygon: TPolygon32;
    procedure PrepareProjectedPointsByEnum(
      AMaxPointsCount: Integer;
      const AProjection: IProjectionInfo;
      const AEnum: IEnumGPSTrackPoint
    );
    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure DrawSection(
      ATargetBmp: TCustomBitmap32;
      const ATrackColorer: ITrackColorerStatic;
      const ALineWidth: Double;
      const APointPrev, APointCurr: TDoublePoint;
      const ASpeed: Double
    );
    function DrawPath(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATrackColorer: ITrackColorerStatic;
      const ALineWidth: Double;
      APointsCount: Integer
    ): Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AMaxPointsCount: Integer;
      const ALineWidth: Double;
      const ATrackColorer: ITrackColorerStatic;
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProjection: IProjectionInfo;
      const AEnum: IEnumGPSTrackPoint
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_CoordConverter,
  u_Bitmap32ByStaticBitmap,
  u_GeoFun;

{ TBitmapLayerProviderByTrackPath }

constructor TBitmapLayerProviderByTrackPath.Create(
  AMaxPointsCount: Integer;
  const ALineWidth: Double;
  const ATrackColorer: ITrackColorerStatic;
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProjection: IProjectionInfo;
  const AEnum: IEnumGPSTrackPoint
);
begin
  inherited Create;
  FLineWidth := ALineWidth;
  FTrackColorer := ATrackColorer;
  FBitmapFactory := ABitmapFactory;
  Assert(FLineWidth >= 0);
  Assert(FTrackColorer <> nil);
  PrepareProjectedPointsByEnum(AMaxPointsCount, AProjection, AEnum);

  FPolygon := TPolygon32.Create;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;
  FPolygon.Closed := false;
end;

destructor TBitmapLayerProviderByTrackPath.Destroy;
begin
  FreeAndNil(FPolygon);
  inherited;
end;

function TBitmapLayerProviderByTrackPath.DrawPath(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  ATargetBmp: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter;
  const ATrackColorer: ITrackColorerStatic;
  const ALineWidth: Double;
  APointsCount: Integer
): Boolean;
  function GetCode(
  const AMapRect: TDoubleRect;
  const APoint: TDoublePoint
  ): Byte;
    //  Смысл разрядов кода:

    // 1 рр = 1 - точка над верхним краем окна;

    // 2 рр = 1 - точка под нижним краем окна;

    // 3 рр = 1 - точка справа от правого края окна;

    // 4 рр = 1 - точка слева от левого края окна.
  begin
    Result := 0;
    if AMapRect.Top > APoint.Y then begin
      Result := 1;
    end else if AMapRect.Bottom < APoint.Y then begin
      Result := 2;
    end;

    if AMapRect.Left > APoint.X then begin
      Result := Result or 8;
    end else if AMapRect.Right < APoint.X then begin
      Result := Result or 4;
    end;
  end;

var
  VPointPrev: TDoublePoint;
  VPointPrevIsEmpty: Boolean;
  VPointPrevCode: Byte;
  VPointPrevLocal: TDoublePoint;
  i: Integer;
  VPointCurr: TDoublePoint;
  VPointCurrIsEmpty: Boolean;
  VPointCurrCode: Byte;
  VPointCurrLocal: TDoublePoint;

  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
begin
  Result := False;
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;

  VPointCurrCode := 0;
  VPointPrevCode := 0;
  VPointPrev := FPoints[APointsCount - 1].Point;
  VPointPrevIsEmpty := PointIsEmpty(VPointPrev);
  if not VPointPrevIsEmpty then begin
    VPointPrevCode := GetCode(VMapPixelRect, VPointPrev);
    VPointPrevLocal := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPointPrev);
  end;
  for i := APointsCount - 2 downto 0 do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Break;
    end;
    VPointCurr := FPoints[i].Point;
    VPointCurrIsEmpty := PointIsEmpty(VPointCurr);
    if not VPointCurrIsEmpty then begin
      VPointCurrCode := GetCode(VMapPixelRect, VPointCurr);
      VPointCurrLocal := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPointCurr);
      if not VPointPrevIsEmpty then begin
        if (VPointPrevCode and VPointCurrCode) = 0 then begin
          if not Result then begin
            InitBitmap(ATargetBmp, ALocalConverter);
            Result := True;
          end;
          DrawSection(ATargetBmp, ATrackColorer, ALineWidth, VPointPrevLocal, VPointCurrLocal, FPoints[i].Speed);
        end;
      end;
    end;
    VPointPrev := VPointCurr;
    VPointPrevLocal := VPointCurrLocal;
    VPointPrevIsEmpty := VPointCurrIsEmpty;
    VPointPrevCode := VPointCurrCode;
  end;
end;

procedure TBitmapLayerProviderByTrackPath.DrawSection(
  ATargetBmp: TCustomBitmap32;
  const ATrackColorer: ITrackColorerStatic;
  const ALineWidth: Double;
  const APointPrev, APointCurr: TDoublePoint;
  const ASpeed: Double
);
var
  VFixedPointsPair: array [0..10] of TFixedPoint;
  VSegmentColor: TColor32;
begin
  if (APointPrev.x < 32767) and (APointPrev.x > -32767) and (APointPrev.y < 32767) and (APointPrev.y > -32767) then begin
    VFixedPointsPair[0] := FixedPoint(APointPrev.X, APointPrev.Y);
    VFixedPointsPair[1] := FixedPoint(APointCurr.X, APointCurr.Y);
    FPolygon.Clear;
    FPolygon.AddPoints(VFixedPointsPair[0], 2);
    with FPolygon.Outline do begin
      try
        with Grow(Fixed(ALineWidth / 2), 0.5) do begin
          try
            VSegmentColor := ATrackColorer.GetColorForSpeed(ASpeed);
            DrawFill(ATargetBmp, VSegmentColor);
          finally
            free;
          end;
        end;
      finally
        free;
      end;
    end;
    FPolygon.Clear;
  end;
end;

function TBitmapLayerProviderByTrackPath.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTargetRect: TRect;
  VLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;
  if not FRectIsEmpty then begin
    VZoom := ALocalConverter.GetZoom;
    VConverter := ALocalConverter.GetGeoConverter;
    VTargetRect := ALocalConverter.GetRectInMapPixel;
    VConverter.CheckPixelRect(VTargetRect, VZoom);
    VLonLatRect := VConverter.PixelRect2LonLatRect(VTargetRect, VZoom);
    if IsIntersecLonLatRect(FLonLatRect, VLonLatRect) then begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
      try
        if
          DrawPath(
            AOperationID,
            ACancelNotifier,
            VBitmap,
            ALocalConverter,
            FTrackColorer,
            FLineWidth,
            FPointsCount
          )
        then begin
          Result := VBitmap.BitmapStatic;
        end;
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

procedure TBitmapLayerProviderByTrackPath.InitBitmap(
  ATargetBmp: TCustomBitmap32; const ALocalConverter: ILocalCoordConverter);
var
  VSize: TPoint;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  ATargetBmp.SetSize(VSize.X, VSize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

procedure TBitmapLayerProviderByTrackPath.PrepareProjectedPointsByEnum(
  AMaxPointsCount: Integer;
  const AProjection: IProjectionInfo;
  const AEnum: IEnumGPSTrackPoint
);
var
  i: Integer;
  VIndex: Integer;
  VPoint: TGPSTrackPoint;
  VGeoConverter: ICoordConverter;
  VZoom: Byte;
  VCurrPointIsEmpty: Boolean;
  VPrevPointIsEmpty: Boolean;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
begin
  FRectIsEmpty := True;
  FPointsCount := 0;
  if Length(FPoints) < AMaxPointsCount then begin
    SetLength(FPoints, AMaxPointsCount);
  end;
  VGeoConverter := AProjection.GeoConverter;
  VZoom := AProjection.Zoom;
  i := 0;
  VIndex := 0;
  VPrevPointIsEmpty := True;
  while (i < AMaxPointsCount) and AEnum.Next(VPoint) do begin
    VCurrPointIsEmpty := PointIsEmpty(VPoint.Point);
    if not VCurrPointIsEmpty then begin
      VGeoConverter.CheckLonLatPos(VPoint.Point);
      if FRectIsEmpty then begin
        FLonLatRect.TopLeft := VPoint.Point;
        FLonLatRect.BottomRight := VPoint.Point;
        FRectIsEmpty := False;
      end else begin
        if FLonLatRect.Left > VPoint.Point.X then begin
          FLonLatRect.Left := VPoint.Point.X;
        end;
        if FLonLatRect.Top < VPoint.Point.Y then begin
          FLonLatRect.Top := VPoint.Point.Y;
        end;
        if FLonLatRect.Right < VPoint.Point.X then begin
          FLonLatRect.Right := VPoint.Point.X;
        end;
        if FLonLatRect.Bottom > VPoint.Point.Y then begin
          FLonLatRect.Bottom := VPoint.Point.Y;
        end;
      end;
      VPoint.Point := VGeoConverter.LonLat2PixelPosFloat(VPoint.Point, VZoom);
    end;

    VCurrPoint := VPoint.Point;
    if VCurrPointIsEmpty then begin
      if not VPrevPointIsEmpty then begin
        FPoints[VIndex] := VPoint;
        Inc(VIndex);
        VPrevPointIsEmpty := VCurrPointIsEmpty;
        VPrevPoint := VCurrPoint;
      end;
    end else begin
      if VPrevPointIsEmpty then begin
        FPoints[VIndex] := VPoint;
        Inc(VIndex);
        VPrevPointIsEmpty := VCurrPointIsEmpty;
        VPrevPoint := VCurrPoint;
      end else begin
        if (abs(VPrevPoint.X - VCurrPoint.X) > 2) or
          (abs(VPrevPoint.Y - VCurrPoint.Y) > 2) then begin
          FPoints[VIndex] := VPoint;
          Inc(VIndex);
          VPrevPointIsEmpty := VCurrPointIsEmpty;
          VPrevPoint := VCurrPoint;
        end;
      end;
    end;
    Inc(i);
  end;
  FPointsCount := VIndex;
end;

end.
