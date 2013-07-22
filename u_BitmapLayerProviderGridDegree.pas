unit u_BitmapLayerProviderGridDegree;

interface

uses
  Types,
  SysUtils,
  GR32,
  i_SimpleFlag,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_ValueToStringConverter,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderGridDegree = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FColor: TColor32;
    FShowText: Boolean;
    FShowLines: Boolean;
    FScale: Double;
    FBitmapFactory: IBitmap32StaticFactory;
    FValueConverter: IValueToStringConverter;

    FCS: IReadWriteSync;
    FBitmap: TBitmap32;
    FBitmapChangeFlag: ISimpleFlag;
    procedure OnBitmapChange(Sender: TObject);
    procedure InitBitmap(const ALocalConverter: ILocalCoordConverter);
    procedure DrawLines(
      const ALocalConverter: ILocalCoordConverter
    );
    procedure DrawCaptions(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    );
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      AColor: TColor32;
      AScale: Double;
      AShowText: Boolean;
      AShowLines: Boolean;
      const AValueConverter: IValueToStringConverter
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  t_GeoTypes,
  i_CoordConverter,
  u_SimpleFlagWithInterlock,
  u_GeoFun,
  u_Synchronizer;

{ TBitmapLayerProviderGridGenshtab }

constructor TBitmapLayerProviderGridDegree.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  AColor: TColor32;
  AScale: Double;
  AShowText, AShowLines: Boolean;
  const AValueConverter: IValueToStringConverter
);
begin
  inherited Create;
  FColor := AColor;
  FScale := AScale;
  FShowText := AShowText;
  FShowLines := AShowLines;
  FBitmapFactory := ABitmapFactory;
  FValueConverter := AValueConverter;

  FCS := MakeSyncRW_Var(Self, False);
  FBitmapChangeFlag := TSimpleFlagWithInterlock.Create;
  FBitmap := TBitmap32.Create;
  FBitmap.SetSize(256, 256);
  FBitmap.OnChange := Self.OnBitmapChange;
end;

destructor TBitmapLayerProviderGridDegree.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TBitmapLayerProviderGridDegree.DrawCaptions(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLocalRect: TRect;
  z: TDoublePoint;
  VZoom: Byte;
  VLoadedRect: TDoubleRect;
  VLoadedLonLatRect: TDoubleRect;
  VGridLonLatRect: TDoubleRect;
  VLonLatRectOfCell: TDoubleRect;
  VLocalRectOfCell: TDoubleRect;
  VGeoConvert: ICoordConverter;
  VGridRect: TRect;
  i, j: Integer;
  VTextSize: TSize;
  VListName: String;
  VLocalCellCenter: TDoublePoint;
  VOutPoint: TPoint;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  z := GetDegBordersStepByScale(FScale, VZoom);
  VLocalRect := ALocalConverter.GetLocalRect;
  VLoadedRect := ALocalConverter.GetRectInMapPixelFloat;

  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);

  VLoadedLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VLoadedRect, VZoom);
  if VLoadedLonLatRect.Top > 90 then begin
    VLoadedLonLatRect.Top := 90;
  end;
  if VLoadedLonLatRect.Bottom < -90 then begin
    VLoadedLonLatRect.Bottom := -90;
  end;
  VGridRect.Left := Floor(VLoadedLonLatRect.Left / z.X);
  VGridRect.Top := Ceil(VLoadedLonLatRect.Top / z.Y);
  VGridRect.Right := Ceil(VLoadedLonLatRect.Right / z.X);
  VGridRect.Bottom := Floor(VLoadedLonLatRect.Bottom / z.Y);

  VGridLonLatRect.Left := VGridRect.Left * z.X;
  VGridLonLatRect.Top := VGridRect.Top * z.Y;
  VGridLonLatRect.Right := VGridRect.Right * z.X;
  VGridLonLatRect.Bottom := VGridRect.Bottom * z.Y;

  VLonLatRectOfCell.TopLeft := VGridLonLatRect.TopLeft;
  VLonLatRectOfCell.BottomRight := DoublePoint(VGridLonLatRect.Left + z.X, VGridLonLatRect.Top - z.Y);
  VGeoConvert.CheckLonLatRect(VLonLatRectOfCell);
  VLocalRectOfCell := ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCell);
  if abs(VLocalRectOfCell.Right - VLocalRectOfCell.Left) < 30 then begin
    exit;
  end;
  VLonLatRectOfCell.Right := VGridLonLatRect.Left;
  for i := VGridRect.Left + 1 to VGridRect.Right do begin
    VLonLatRectOfCell.Left := VLonLatRectOfCell.Right;
    VLonLatRectOfCell.Right := i * z.X;
    VLonLatRectOfCell.Top := VGridLonLatRect.Bottom;
    for j := VGridRect.Bottom + 1 to VGridRect.Top do begin
      VLonLatRectOfCell.Bottom := VLonLatRectOfCell.Top;
      VLonLatRectOfCell.Top := j * z.Y;
      VGeoConvert.CheckLonLatRect(VLonLatRectOfCell);

      VLocalRectOfCell := ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCell);
      VLocalCellCenter := RectCenter(VLocalRectOfCell);

      if abs(VLonLatRectOfCell.Top)<=85 then
        VListName := FValueConverter.LatConvert(VLonLatRectOfCell.Top,true)
      else VListName := '';

      VTextSize := FBitmap.TextExtent(VListName);
      VOutPoint := Types.Point(Trunc(VLocalCellCenter.X - VTextSize.cx / 2), Trunc(VLocalRectOfCell.Top));
      FBitmap.RenderText(VOutPoint.X, VOutPoint.Y, VListName, 0, FColor);
// **************************************************
      VListName := FValueConverter.LonConvert(VLonLatRectOfCell.Left,true);
      VTextSize := FBitmap.TextExtent(VListName);
      VOutPoint := Types.Point(Trunc(VLocalRectOfCell.Left)+ 3, Trunc(VLocalCellCenter.Y - VTextSize.cy / 2));
      FBitmap.RenderText(VOutPoint.X, VOutPoint.Y, VListName, 0, FColor);
    end;
  end;
end;

procedure TBitmapLayerProviderGridDegree.DrawLines(
  const ALocalConverter: ILocalCoordConverter
);
var
  VLocalRect: TRect;
  z: TDoublePoint;
  VZoom: Byte;
  VLoadedRect: TDoubleRect;
  VLoadedLonLatRect: TDoubleRect;
  VGridLonLatRect: TDoubleRect;
  VLonLatRectOfCellsLine: TDoubleRect;
  VLocalRectOfCellsLine: TRect;
  VGeoConvert: ICoordConverter;
  VGridRect: TRect;
  i: Integer;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  z := GetDegBordersStepByScale(FScale, VZoom);
  VLocalRect := ALocalConverter.GetLocalRect;
  VLoadedRect := ALocalConverter.GetRectInMapPixelFloat;

  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);

  VLoadedLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VLoadedRect, VZoom);
  if VLoadedLonLatRect.Top > 90 then begin
    VLoadedLonLatRect.Top := 90;
  end;
  if VLoadedLonLatRect.Bottom < -90 then begin
    VLoadedLonLatRect.Bottom := -90;
  end;
  VGridRect.Left := Floor(VLoadedLonLatRect.Left / z.X);
  VGridRect.Top := Ceil(VLoadedLonLatRect.Top / z.Y);
  VGridRect.Right := Ceil(VLoadedLonLatRect.Right / z.X);
  VGridRect.Bottom := Floor(VLoadedLonLatRect.Bottom / z.Y);

  VGridLonLatRect.Left := VGridRect.Left * z.X;
  VGridLonLatRect.Top := VGridRect.Top * z.Y;
  VGridLonLatRect.Right := VGridRect.Right * z.X;
  VGridLonLatRect.Bottom := VGridRect.Bottom * z.Y;

  VLonLatRectOfCellsLine.TopLeft := VGridLonLatRect.TopLeft;
  VLonLatRectOfCellsLine.BottomRight := DoublePoint(VGridLonLatRect.Left + z.X, VGridLonLatRect.Top - z.Y);
  VGeoConvert.CheckLonLatRect(VLonLatRectOfCellsLine);
  VLocalRectOfCellsLine :=
    RectFromDoubleRect(
      ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCellsLine),
      rrToTopLeft
    );
  if abs(VLocalRectOfCellsLine.Right - VLocalRectOfCellsLine.Left) < 4 then begin
    exit;
  end;

  for i := VGridRect.Left to VGridRect.Right do begin
    VLonLatRectOfCellsLine.Left := i * z.X;
    VLonLatRectOfCellsLine.Top := VGridLonLatRect.Top;
    VLonLatRectOfCellsLine.Right := VLonLatRectOfCellsLine.Left;
    VLonLatRectOfCellsLine.Bottom := VGridLonLatRect.Bottom;
    VGeoConvert.CheckLonLatRect(VLonLatRectOfCellsLine);
    VLocalRectOfCellsLine :=
      RectFromDoubleRect(
        ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCellsLine),
        rrToTopLeft
      );
    VLocalRectOfCellsLine.Top := VLocalRect.Top;
    VLocalRectOfCellsLine.Bottom := VLocalRect.Bottom;

    if (VLocalRectOfCellsLine.Left >= VLocalRect.Left) and
      (VLocalRectOfCellsLine.Left < VLocalRect.Right) then begin
      FBitmap.VertLineTS(
        VLocalRectOfCellsLine.Left,
        VLocalRectOfCellsLine.Top,
        VLocalRectOfCellsLine.Bottom,
        FColor
      );
    end;
  end;

  for i := VGridRect.Bottom to VGridRect.Top do begin
    VLonLatRectOfCellsLine.Left := VGridLonLatRect.Left;
    VLonLatRectOfCellsLine.Top := i * z.Y;
    VLonLatRectOfCellsLine.Right := VGridLonLatRect.Right;
    VLonLatRectOfCellsLine.Bottom := VLonLatRectOfCellsLine.Top;

    VGeoConvert.CheckLonLatRect(VLonLatRectOfCellsLine);
    VLocalRectOfCellsLine :=
      RectFromDoubleRect(
        ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCellsLine),
        rrToTopLeft
      );
    VLocalRectOfCellsLine.Left := VLocalRect.Left;
    VLocalRectOfCellsLine.Right := VLocalRect.Right;

    if (VLocalRectOfCellsLine.Top >= VLocalRect.Top) and
      (VLocalRectOfCellsLine.Bottom < VLocalRect.Bottom) then begin
      FBitmap.HorzLineTS(
        VLocalRectOfCellsLine.Left,
        VLocalRectOfCellsLine.Top,
        VLocalRectOfCellsLine.Right,
        FColor
      );
    end;
  end;
end;

function TBitmapLayerProviderGridDegree.GetBitmapRect(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter): IBitmap32Static;
begin
  Result := nil;
  FCS.BeginWrite;
  try
    InitBitmap(ALocalConverter);
    FBitmapChangeFlag.CheckFlagAndReset;
    if FShowLines then begin
      DrawLines(ALocalConverter);
    end;

    if FShowText then begin
      DrawCaptions(AOperationID, ACancelNotifier, ALocalConverter);
    end;
    if FBitmapChangeFlag.CheckFlagAndReset then begin
      Result := FBitmapFactory.Build(Types.Point(FBitmap.Width, FBitmap.Height), FBitmap.Bits);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TBitmapLayerProviderGridDegree.InitBitmap(
  const ALocalConverter: ILocalCoordConverter);
var
  VSize: TPoint;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  FBitmap.SetSize(VSize.X, VSize.Y);
  FBitmap.Clear(0);
end;

procedure TBitmapLayerProviderGridDegree.OnBitmapChange(Sender: TObject);
begin
  FBitmapChangeFlag.SetFlag;
end;

end.
