unit u_SelectionLayer;

interface

uses
  GR32,
  GR32_Polygons,
  GR32_Image,
  t_GeoTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  u_MapViewPortState,
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasicNoBitmap)
  protected
    procedure DoRedraw; override;
    procedure PaintLayer(Sender: TObject; Buffer: TBitmap32);
    function PreparePolygon(APolygon: TPointArray): TPointArray;
    function GetBitmapSizeInPixel: TPoint; override;
  public
    constructor Create(AParentMap: TImage32; AViewPortState: TMapViewPortState);
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); override;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); override;
  end;

implementation

uses
  Types,
  SysUtils,
  Graphics,
  u_GlobalState,
  u_WindowLayerBasic;

{ TSelectionLayer }

constructor TSelectionLayer.Create(AParentMap: TImage32;
  AViewPortState: TMapViewPortState);
begin
  inherited;
  FLayerPositioned.OnPaint := PaintLayer;
end;

procedure TSelectionLayer.DoRedraw;
var
  VZoomCurr: Byte;
  VPolygon: TPointArray;
  VPolygonOnBitmap: TPointArray;
  i: integer;
  VPolygon32: TPolygon32;
begin
  inherited;
  FLayerPositioned.Update;
end;

function TSelectionLayer.GetBitmapSizeInPixel: TPoint;
begin
  Result := FParentMap.ClientRect.BottomRight;
end;

procedure TSelectionLayer.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetSubItem('VIEW');
  if VConfigProvider <> nil then begin
    Visible := VConfigProvider.ReadBool('ShowLastSelection',false);
  end;
end;

procedure TSelectionLayer.PaintLayer(Sender: TObject; Buffer: TBitmap32);
var
  VZoomCurr: Byte;
  VPolygon: TPointArray;
  VPolygonOnBitmap: TPointArray;
  i: integer;
  VPolygon32: TPolygon32;
begin
  VPolygon := nil;
  VPolygonOnBitmap := nil;
  if FGeoConvert <> nil then begin
    if Length(GState.LastSelectionPolygon) > 0 then begin
      VZoomCurr := FZoom;
      VPolygon := FGeoConvert.LonLatArray2PixelArray(GState.LastSelectionPolygon, VZoomCurr);
      try
        VPolygonOnBitmap := PreparePolygon(VPolygon);
        VPolygon32 := TPolygon32.Create;
        try
          for i := 0 to Length(VPolygonOnBitmap) - 1 do begin
            VPolygon32.Add(FixedPoint(VPolygonOnBitmap[i]));
          end;
          VPolygon32.Antialiased:=True;
          VPolygon32.Closed:=true;
          with VPolygon32.Outline do try
            with Grow(Fixed(1), 0.5) do try
              FillMode := pfWinding;
              DrawFill(Buffer, SetAlpha(GState.LastSelectionColor, GState.LastSelectionAlfa));
            finally
              free;
            end;
          finally
            free;
          end;
        finally
          FreeAndNil(VPolygon32);
        end;
      finally
        VPolygon := nil;
      end;
    end else begin
      Visible := False;
    end;
  end;
end;

function TSelectionLayer.PreparePolygon(
  APolygon: TPointArray): TPointArray;
var
  i: integer;
  VSourcePoint: TExtendedPoint;
  VTargetPoint: TExtendedPoint;
  VTargetPointAbs: TExtendedPoint;
const
  CRectSize = 1 shl 14;
begin
  SetLength(Result, Length(APolygon));
  for i := 0 to Length(APolygon) - 1 do begin
    VSourcePoint.X := APolygon[i].X;
    VSourcePoint.Y := APolygon[i].Y;
    VTargetPoint := MapPixel2VisiblePixel(VSourcePoint);
    VTargetPointAbs.X := Abs(VTargetPoint.X);
    VTargetPointAbs.Y := Abs(VTargetPoint.Y);
    if (VTargetPointAbs.X >= CRectSize) or (VTargetPointAbs.Y >= CRectSize) then begin
      if (VTargetPointAbs.X >= CRectSize) and (VTargetPointAbs.Y >= CRectSize) then begin
        if VTargetPoint.Y > 0 then begin
          VTargetPoint.Y := CRectSize;
        end else begin
          VTargetPoint.Y := -CRectSize;
        end;
        if VTargetPoint.X > 0 then begin
          VTargetPoint.X := CRectSize;
        end else begin
          VTargetPoint.X := -CRectSize;
        end;
      end else begin
        if VTargetPointAbs.X < VTargetPointAbs.Y then begin
          if VTargetPoint.Y > 0 then begin
            VTargetPoint.Y := CRectSize;
          end else begin
            VTargetPoint.Y := -CRectSize;
          end;
        end else begin
          if VTargetPoint.X > 0 then begin
            VTargetPoint.X := CRectSize;
          end else begin
            VTargetPoint.X := -CRectSize;
          end;
        end;
      end;
    end;
    Result[i].X := Round(VTargetPoint.X);
    Result[i].Y := Round(VTargetPoint.Y);
  end;

end;

procedure TSelectionLayer.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('VIEW');
  VConfigProvider.WriteBool('ShowLastSelection', Visible);
end;

end.
