unit u_SelectionLayer;

interface

uses
  GR32,
  GR32_Image,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_ClipPolygonByRect,
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasic)
  protected
    FBitmapClip: IPolyClip;
    FLineColor: TColor32;
    FLineWidth: Integer;
  protected
    procedure DoResizeBitmap; override;
    procedure DoRedraw; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
  end;

implementation

uses
  Types,
  SysUtils,
  Graphics,
  GR32_Polygons,
  u_GlobalState,
  Ugeofun,
  u_ConfigProviderHelpers,
  u_WindowLayerBasic;

{ TSelectionLayer }

constructor TSelectionLayer.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FLineColor := SetAlpha(Color32(clBlack), 210);
  FLineWidth := 2;
end;

destructor TSelectionLayer.Destroy;
begin
  FBitmapClip := nil;
  inherited;
end;

procedure TSelectionLayer.DoRedraw;
var
  VZoomCurr: Byte;
  i: integer;
  VLonLat: TExtendedPoint;
  VPolygon: TExtendedPointArray;
  VPolygon32: TPolygon32;
  VPointsOnBitmap: TExtendedPointArray;
  VPointsCount: Integer;
  VPointsOnBitmapPrepared: TExtendedPointArray;
  VPointsProcessedCount: Integer;
  VPathFixedPoints: TArrayOfFixedPoint;
begin
  inherited;
  VPolygon := Copy(GState.LastSelectionPolygon);
  VPointsCount := Length(VPolygon);
  if VPointsCount > 0 then begin
    FLayer.Bitmap.Clear(clBlack);
    VZoomCurr := FZoom;
    SetLength(VPointsOnBitmap, VPointsCount + 1);
    for i := 0 to VPointsCount - 1 do begin
      VLonLat := VPolygon[i];
      FGeoConvert.CheckLonLatPos(VLonLat);
      VPointsOnBitmap[i] := MapPixel2BitmapPixel(FGeoConvert.LonLat2PixelPosFloat(VLonLat, VZoomCurr));
    end;
    if not compare2EP(VPointsOnBitmap[0], VPointsOnBitmap[VPointsCount - 1]) then begin
      VPointsOnBitmap[VPointsCount] := VPointsOnBitmap[0];
      Inc(VPointsCount);
    end;
    VPointsProcessedCount := FBitmapClip.Clip(VPointsOnBitmap, VPointsCount, VPointsOnBitmapPrepared);
    if VPointsProcessedCount > 0 then begin
      SetLength(VPathFixedPoints, VPointsProcessedCount);
      for i := 0 to VPointsProcessedCount - 1 do begin
        VPathFixedPoints[i] := FixedPoint(VPointsOnBitmapPrepared[i].X, VPointsOnBitmapPrepared[i].Y);
      end;
      VPolygon32 := TPolygon32.Create;
      try
        VPolygon32.Antialiased := true;
        VPolygon32.AntialiasMode := am4times;
        VPolygon32.Closed := True;
        VPolygon32.AddPoints(VPathFixedPoints[0], VPointsProcessedCount);
        with VPolygon32.Outline do try
           with Grow(Fixed(FLineWidth / 2), 0.5) do try
             FillMode := pfWinding;
             DrawFill(FLayer.Bitmap, FLineColor);
           finally
             free;
           end;
        finally
          free;
        end;
      finally
        VPolygon32.Free;
      end;
    end;
  end else begin
    Visible := False;
  end;
end;

procedure TSelectionLayer.DoResizeBitmap;
var
  VSize: TPoint;
begin
  inherited;
  VSize := GetBitmapSizeInPixel;
  FBitmapClip := TPolyClipByRect.Create(MakeRect(0, 0, VSize.X, VSize.Y));
end;

procedure TSelectionLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    VConfigProvider := VConfigProvider.GetSubItem('LastSelection');
    if VConfigProvider <> nil then begin
      FLineColor := LoadColor32(VConfigProvider, 'LineColor', FLineColor);
      FLineWidth := VConfigProvider.ReadInteger('LineWidth', FLineWidth);
      Visible := VConfigProvider.ReadBool('Visible',false);
    end;
  end;
end;

procedure TSelectionLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider := VConfigProvider.GetOrCreateSubItem('LastSelection');
  VConfigProvider.WriteBool('Visible', Visible);
  WriteColor32(VConfigProvider, 'LineColor', FLineColor);
  VConfigProvider.WriteInteger('LineWidth', FLineWidth);
end;

end.
