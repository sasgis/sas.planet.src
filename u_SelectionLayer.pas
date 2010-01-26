unit u_SelectionLayer;

interface

uses
  GR32,
  GR32_Polygons,
  t_GeoTypes,
  u_MapLayerBasic;

type
  TSelectionLayer = class(TMapLayerBasic)
  protected
    procedure DoRedraw; override;
    function PreparePolygon(APolygon: TPointArray): TPointArray;
  public
  end;

implementation

uses
  Types,
  SysUtils,
  Graphics,
  u_GlobalState,
  uMapType,
  u_WindowLayerBasic;

{ TSelectionLayer }

procedure TSelectionLayer.DoRedraw;
var
  VZoomCurr: Byte;
  VPolygon: TPointArray;
  VPolygonOnBitmap: TPointArray;
  i: integer;
  VPolygon32: TPolygon32;
begin
  inherited;
  VPolygon := nil;
  VPolygonOnBitmap := nil;
  if Length(GState.LastSelectionPolygon) > 0 then begin
    FLayer.Bitmap.Clear(clBlack);
    VZoomCurr := GState.zoom_size - 1;
    VPolygon := FGeoConvert.PoligonProject(VZoomCurr + 8, GState.LastSelectionPolygon);
    try
      VPolygonOnBitmap := PreparePolygon(VPolygon);
      VPolygon32:=TPolygon32.Create;
      try
        for i := 0 to Length(VPolygonOnBitmap) - 1 do begin
          VPolygon32.Add(FixedPoint(VPolygonOnBitmap[i]));
        end;
        VPolygon32.Antialiased:=True;
        VPolygon32.Closed:=true;
        with VPolygon32.Outline do try
          with Grow(Fixed(1), 0.5) do try
            FillMode := pfWinding;
            DrawFill(FLayer.Bitmap, clBlack32);
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
    VTargetPoint := MapPixel2BitmapPixel(VSourcePoint);
    VTargetPointAbs.X := Abs(VTargetPoint.X);
    VTargetPointAbs.Y := Abs(VTargetPoint.Y);
    if (VTargetPointAbs.X >= CRectSize) or (VTargetPointAbs.Y >= CRectSize) then begin
      if (VTargetPointAbs.X >= CRectSize) and (VTargetPointAbs.Y >= CRectSize) then begin
        if VTargetPoint.Y > 0 then begin
          VTargetPoint.Y := CRectSize;
        end else begin
          VTargetPoint.Y := - CRectSize;
        end;
        if VTargetPoint.X > 0 then begin
          VTargetPoint.X := CRectSize;
        end else begin
          VTargetPoint.X := - CRectSize;
        end;
      end else begin
        if VTargetPointAbs.X < VTargetPointAbs.Y then begin
          if VTargetPoint.Y > 0 then begin
            VTargetPoint.Y := CRectSize;
          end else begin
            VTargetPoint.Y := - CRectSize;
          end;
        end else begin
          if VTargetPoint.X > 0 then begin
            VTargetPoint.X := CRectSize;
          end else begin
            VTargetPoint.X := - CRectSize;
          end;
        end;
      end;
    end;
    Result[i].X := Round(VTargetPoint.X);
    Result[i].Y := Round(VTargetPoint.Y);
  end;

end;

end.
