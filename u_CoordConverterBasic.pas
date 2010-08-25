unit u_CoordConverterBasic;

interface

uses
  Types,
  i_ICoordConverter,
  t_GeoTypes,
  u_CoordConverterAbstract;

type
  TCoordConverterBasic = class(TCoordConverterAbstract, ICoordConverterSimple)
  protected
    FValidLonLatRect: TExtendedRect;
    function GetValidLonLatRect: TExtendedRect; virtual;

    procedure CheckZoomInternal(var AZoom: Byte); virtual;
    procedure CheckTilePosInternal(var XY: TPoint; var Azoom: byte); override;
    procedure CheckTilePosStrictInternal(var XY: TPoint; var Azoom: byte); override;
    procedure CheckTileRectInternal(var XY: TRect; var Azoom: byte); override;

    procedure CheckPixelPosInternal(var XY: TPoint; var Azoom: byte); override;
    procedure CheckPixelPosStrictInternal(var XY: TPoint; var Azoom: byte); override;
    procedure CheckPixelRectInternal(var XY: TRect; var Azoom: byte); override;

    procedure CheckRelativePosInternal(var XY: TExtendedPoint); override;
    procedure CheckRelativeRectInternal(var XY: TExtendedRect); override;

    procedure CheckLonLatPosInternal(var XY: TExtendedPoint); override;
    procedure CheckLonLatRectInternal(var XY: TExtendedRect); override;

    function TilesAtZoomInternal(AZoom: byte): Longint; override;
    function TilesAtZoomFloatInternal(AZoom: byte): Extended; override;
    function PixelsAtZoomInternal(AZoom: byte): Longint; override;
    function PixelsAtZoomFloatInternal(AZoom: byte): Extended; override;


    function PixelPos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; override;
    function PixelPos2TilePosInternal(const XY: TPoint; Azoom: byte): TPoint; override;
    function PixelPos2RelativeInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; override;

    function PixelPosFloat2PixelPosInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; override;

    function PixelRect2TileRectInternal(const XY: TRect; AZoom: byte): TRect; override;
    function PixelRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; override;
    function PixelRect2LonLatRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; override;

    function TilePos2PixelPosInternal(const XY: TPoint; Azoom: byte): TPoint; override;
    function TilePos2PixelRectInternal(const XY: TPoint; Azoom: byte): TRect; override;
    function TilePos2LonLatRectInternal(const XY: TPoint; Azoom: byte): TExtendedRect; override;
    function TilePos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; override;
    function TilePos2RelativeInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; override;
    function TilePos2RelativeRectInternal(const XY: TPoint; Azoom: byte): TExtendedRect; override;

    function TileRect2PixelRectInternal(const XY: TRect; AZoom: byte): TRect; override;
    function TileRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; override;
    function TileRect2LonLatRectInternal(const XY: TRect; Azoom: byte): TExtendedRect; override;

    function Relative2PixelInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; override;
    function Relative2PixelPosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; override;
    function Relative2TileInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; override;
    function Relative2TilePosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; override;

    function RelativeRect2LonLatRectInternal(const XY: TExtendedRect): TExtendedRect; override;
    function RelativeRect2TileRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; override;
    function RelativeRect2TileRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; override;
    function RelativeRect2PixelRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; override;
    function RelativeRect2PixelRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; override;


    function LonLat2PixelPosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; override;
    function LonLat2PixelPosfInternal(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; override;
    function LonLat2TilePosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; override;
    function LonLat2TilePosfInternal(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; override;

    function LonLatRect2RelativeRectInternal(const XY: TExtendedRect): TExtendedRect; override;
    function LonLatRect2PixelRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; override;
    function LonLatRect2TileRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; override;
  public
    function CheckZoom(var AZoom: Byte): boolean; override;
    function CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; override;
    function CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; override;
    function CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; override;

    function CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; override;
    function CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; override;
    function CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; override;

    function CheckRelativePos(var XY: TExtendedPoint): boolean; override;
    function CheckRelativeRect(var XY: TExtendedRect): boolean; override;

    function CheckLonLatPos(var XY: TExtendedPoint): boolean; override;
    function CheckLonLatRect(var XY: TExtendedRect): boolean; override;

    function Pos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2Pos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function GetTileSplitCode: Integer; override;
    function GetTileSize(const XY: TPoint; Azoom: byte): TPoint; override;
    procedure AfterConstruction; override;
  end;

const
  CTileRelativeEpsilon = (1 / (1 shl 30 + (1 shl 30 - 1))) / 2;
  CTileSplitQuadrate256x256 = 1;

implementation

uses
  SysUtils,
  Math;

function TCoordConverterBasic.GetValidLonLatRect: TExtendedRect;
begin
  Result := TilePos2LonLatRectInternal(Point(0, 0), 0);
end;

procedure TCoordConverterBasic.AfterConstruction;
begin
  inherited;
  FValidLonLatRect := GetValidLonLatRect;
end;

function TCoordConverterBasic.LonLat2Pos(const AXY: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  if Azoom > 23 then begin
    VZoom := VZoom - 8;
    CheckZoomInternal(VZoom);
    Result := LonLat2PixelPosInternal(VXY, Vzoom);
  end else begin
    CheckZoomInternal(VZoom);
    Result := LonLat2TilePosInternal(VXY, Vzoom);
  end;
end;

function TCoordConverterBasic.Pos2LonLat(const AXY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  if Azoom > 23 then begin
    VZoom := VZoom - 8;
    CheckPixelPosInternal(VXY, VZoom);
    Result := PixelPos2LonLatInternal(VXY, Vzoom);
  end else begin
    CheckTilePosInternal(VXY, VZoom);
    Result := TilePos2LonLatInternal(VXY, Vzoom);
  end;
end;

function TCoordConverterBasic.GetTileSplitCode: Integer;
begin
  Result := CTileSplitQuadrate256x256;
end;

function TCoordConverterBasic.GetTileSize(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result := Point(256, 256);
end;

//------------------------------------------------------------------------------
procedure TCoordConverterBasic.CheckZoomInternal(var AZoom: Byte);
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
end;

procedure TCoordConverterBasic.CheckTilePosInternal(var XY: TPoint; var Azoom: byte);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  if XY.X < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if XY.X > VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + IntToStr(VTilesAtZoom));
      XY.X := VTilesAtZoom;
    end;
  end;

  if XY.Y < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if XY.Y > VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + IntToStr(VTilesAtZoom));
      XY.Y := VTilesAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckTileRectInternal(var XY: TRect; var Azoom: byte);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  if XY.Left < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if XY.Left >= VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше или равна ' + IntToStr(VTilesAtZoom));
      XY.Left := VTilesAtZoom - 1;
    end;
  end;
  if XY.Top < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if XY.Top >= VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше или равна ' + IntToStr(VTilesAtZoom));
      XY.Top := VTilesAtZoom - 1;
    end;
  end;
  if XY.Right < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if XY.Right >= VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше или равна ' + IntToStr(VTilesAtZoom));
      XY.Right := VTilesAtZoom - 1;
    end;
  end;
  if XY.Bottom < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if XY.Bottom >= VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше или равна ' + IntToStr(VTilesAtZoom));
      XY.Bottom := VTilesAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckTilePosStrictInternal(var XY: TPoint; var Azoom: byte);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomInternal(Azoom);
  if XY.X < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if XY.X >= VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше или равной ' + IntToStr(VTilesAtZoom));
      XY.X := VTilesAtZoom - 1;
    end;
  end;
  if XY.Y < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if XY.Y >= VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше или равной ' + IntToStr(VTilesAtZoom));
      XY.Y := VTilesAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckPixelPosInternal(var XY: TPoint; var Azoom: byte);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);

  if XY.X < 0 then begin
    if (Azoom < 23) or (XY.X <> VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела не может быть меньше нуля');
      XY.X := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.X > VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.X := VPixelsAtZoom;
    end;
  end;

  if XY.Y < 0 then begin
    if (Azoom < 23) or (XY.Y <> VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела не может быть меньше нуля');
      XY.Y := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.Y := VPixelsAtZoom;
    end;
  end;

end;

procedure TCoordConverterBasic.CheckPixelRectInternal(var XY: TRect; var Azoom: byte);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);

  if XY.Left < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if (Azoom < 23) and (XY.Left >= VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше или равна ' + IntToStr(VPixelsAtZoom));
      XY.Left := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Top < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if (Azoom < 23) and (XY.Top > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше или равна' + IntToStr(VPixelsAtZoom));
      XY.Top := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Right < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if (Azoom < 23) and (XY.Right >= VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше или равна ' + IntToStr(VPixelsAtZoom));
      XY.Right := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Bottom < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if (Azoom < 23) and (XY.Bottom > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше или равна' + IntToStr(VPixelsAtZoom));
      XY.Bottom := VPixelsAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckPixelPosStrictInternal(var XY: TPoint; var Azoom: byte);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);
  if XY.X < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if (Azoom < 23) and (XY.X >= VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше или равна ' + IntToStr(VPixelsAtZoom));
      XY.X := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Y < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if (Azoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше или равна' + IntToStr(VPixelsAtZoom));
      XY.Y := VPixelsAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckRelativePosInternal(var XY: TExtendedPoint);
begin
  if XY.X < 0 then begin
    Assert(False, 'Относительная координата X не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if XY.X > 1 then begin
      Assert(False, 'Относительная координата X не может быть больше единицы');
      XY.X := 1;
    end;
  end;

  if XY.Y < 0 then begin
    Assert(False, 'Относительная координата Y не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if XY.Y > 1 then begin
      Assert(False, 'Относительная координата Y не может быть больше единицы');
      XY.Y := 1;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckRelativeRectInternal(var XY: TExtendedRect);
begin
  if XY.Left < 0 then begin
    Assert(False, 'Относительная координата X не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if XY.Left > 1 then begin
      Assert(False, 'Относительная координата X не может быть больше единицы');
      XY.Left := 1;
    end;
  end;

  if XY.Top < 0 then begin
    Assert(False, 'Относительная координата Y не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if XY.Top > 1 then begin
      Assert(False, 'Относительная координата Y не может быть больше единицы');
      XY.Top := 1;
    end;
  end;

  if XY.Right < 0 then begin
    Assert(False, 'Относительная координата X не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if XY.Right > 1 then begin
      Assert(False, 'Относительная координата X не может быть больше единицы');
      XY.Right := 1;
    end;
  end;

  if XY.Bottom < 0 then begin
    Assert(False, 'Относительная координата Y не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > 1 then begin
      Assert(False, 'Относительная координата Y не может быть больше единицы');
      XY.Bottom := 1;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckLonLatPosInternal(var XY: TExtendedPoint);
begin
  if XY.X < FValidLonLatRect.Left then begin
    Assert(False, 'Долгота не может быть меньше чем ' + FloatToStr(FValidLonLatRect.Left));
    XY.X := FValidLonLatRect.Left;
  end else begin
    if XY.X > FValidLonLatRect.Right then begin
      Assert(False, 'Долгота не может быть больше чем ' + FloatToStr(FValidLonLatRect.Right));
      XY.X := FValidLonLatRect.Right;
    end;
  end;
  if XY.Y < FValidLonLatRect.Bottom then begin
    Assert(False, 'Широта не может быть меньше чем ' + FloatToStr(FValidLonLatRect.Bottom));
    XY.Y := FValidLonLatRect.Bottom;
  end else begin
    if XY.Y > FValidLonLatRect.Top then begin
      Assert(False, 'Широта не может быть больше чем ' + FloatToStr(FValidLonLatRect.Top));
      XY.Y := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TCoordConverterBasic.CheckLonLatRectInternal(var XY: TExtendedRect);
begin
  if XY.Left < FValidLonLatRect.Left then begin
    Assert(False, 'Долгота не может быть меньше чем ' + FloatToStr(FValidLonLatRect.Left));
    XY.Left := FValidLonLatRect.Left;
  end else begin
    if XY.Left > FValidLonLatRect.Right then begin
      Assert(False, 'Долгота не может быть больше чем ' + FloatToStr(FValidLonLatRect.Right));
      XY.Left := FValidLonLatRect.Right;
    end;
  end;
  if XY.Bottom < FValidLonLatRect.Bottom then begin
    Assert(False, 'Широта не может быть меньше чем ' + FloatToStr(FValidLonLatRect.Bottom));
    XY.Bottom := FValidLonLatRect.Bottom;
  end else begin
    if XY.Bottom > FValidLonLatRect.Top then begin
      Assert(False, 'Широта не может быть больше чем ' + FloatToStr(FValidLonLatRect.Top));
      XY.Bottom := FValidLonLatRect.Top;
    end;
  end;

  if XY.Right < FValidLonLatRect.Left then begin
    Assert(False, 'Долгота не может быть меньше чем ' + FloatToStr(FValidLonLatRect.Left));
    XY.Right := FValidLonLatRect.Left;
  end else begin
    if XY.Right > FValidLonLatRect.Right then begin
      Assert(False, 'Долгота не может быть больше чем ' + FloatToStr(FValidLonLatRect.Right));
      XY.Right := FValidLonLatRect.Right;
    end;
  end;
  if XY.Top < FValidLonLatRect.Bottom then begin
    Assert(False, 'Широта не может быть меньше чем ' + FloatToStr(FValidLonLatRect.Bottom));
    XY.Top := FValidLonLatRect.Bottom;
  end else begin
    if XY.Top > FValidLonLatRect.Top then begin
      Assert(False, 'Широта не может быть больше чем ' + FloatToStr(FValidLonLatRect.Top));
      XY.Top := FValidLonLatRect.Top;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TCoordConverterBasic.TilePos2PixelRectInternal(const XY: TPoint;
  Azoom: byte): TRect;
begin
  Result.Left := XY.X shl 8;
  Result.Top := XY.Y shl 8;
  Result.Right := Result.Left + ((1 shl 8) - 1);
  Result.Bottom := Result.Top + ((1 shl 8) - 1);
end;

function TCoordConverterBasic.TilePos2LonLatRectInternal(const XY: TPoint;
  Azoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(TilePos2RelativeRectInternal(XY, Azoom));
end;

function TCoordConverterBasic.PixelsAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl (AZoom + 8);
end;

function TCoordConverterBasic.PixelsAtZoomFloatInternal(
  AZoom: byte): Extended;
begin
  Result := 1 shl AZoom;
  Result := Result * 256;
end;

function TCoordConverterBasic.TilesAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterBasic.TilesAtZoomFloatInternal(
  AZoom: byte): Extended;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterBasic.PixelPos2RelativeInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoomExt: Extended;
  VPixelsAtZoom: Integer;
begin
  VPixelsAtZoom := PixelsAtZoomInternal(Azoom);
  VPixelsAtZoomExt := VPixelsAtZoom;
  VPixelsAtZoomExt := abs(VPixelsAtZoomExt);
  if XY.X = VPixelsAtZoom then begin
    Result.X := 1;
  end else begin
    Result.X := XY.X / VPixelsAtZoomExt;
  end;

  if XY.Y = VPixelsAtZoom then begin
    Result.Y := 1;
  end else begin
    Result.Y := XY.Y / VPixelsAtZoomExt;
  end;
end;

function TCoordConverterBasic.Relative2PixelInternal(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);
  Result.X := Trunc(RoundTo(XY.X * VPixelsAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VPixelsAtZoom, -2));
end;

function TCoordConverterBasic.Relative2PixelPosFloatInternal(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);
  Result.X := XY.X * VPixelsAtZoom;
  Result.Y := XY.Y * VPixelsAtZoom;
end;

function TCoordConverterBasic.LonLat2PixelPosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2PixelInternal(LonLat2RelativeInternal(LL), AZoom);
end;

function TCoordConverterBasic.LonLat2PixelPosfInternal(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);

  Result := LonLat2RelativeInternal(LL);
  Result.X := Result.X * VPixelsAtZoom;
  Result.Y := Result.Y * VPixelsAtZoom;
end;

function TCoordConverterBasic.LonLat2TilePosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2TileInternal(LonLat2RelativeInternal(LL), AZoom);
end;

function TCoordConverterBasic.PixelPos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(PixelPos2RelativeInternal(XY, Azoom));
end;

function TCoordConverterBasic.TilePos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(TilePos2RelativeInternal(XY, Azoom));
end;

function TCoordConverterBasic.TilePos2RelativeInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.X := XY.X / VTilesAtZoom;
  Result.Y := XY.Y / VTilesAtZoom;
end;

function TCoordConverterBasic.TilePos2RelativeRectInternal(const XY: TPoint;
  Azoom: byte): TExtendedRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.Left := XY.X / VTilesAtZoom;
  Result.Top := XY.Y / VTilesAtZoom;
  Result.Right := (XY.X + 1) / VTilesAtZoom;
  Result.Bottom := (XY.Y + 1) / VTilesAtZoom;
end;

function TCoordConverterBasic.LonLatRect2RelativeRectInternal(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := LonLat2RelativeInternal(XY.TopLeft);
  Result.BottomRight := LonLat2RelativeInternal(XY.BottomRight);
end;

function TCoordConverterBasic.Relative2TileInternal(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.X := Trunc(RoundTo(XY.X * VTilesAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VTilesAtZoom, -2));
end;

function TCoordConverterBasic.Relative2TilePosFloatInternal(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.X := XY.X * VTilesAtZoom;
  Result.Y := XY.Y * VTilesAtZoom;
end;

function TCoordConverterBasic.RelativeRect2LonLatRectInternal(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := Relative2LonLatInternal(XY.TopLeft);
  Result.BottomRight := Relative2LonLatInternal(XY.BottomRight);
end;

function TCoordConverterBasic.RelativeRect2PixelRectInternal(const XY: TExtendedRect;
  Azoom: byte): TRect;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);

  Result.Left := Trunc((XY.Left + CTileRelativeEpsilon) * VPixelsAtZoom);
  Result.Top := Trunc((XY.Top + CTileRelativeEpsilon) * VPixelsAtZoom);

  Result.Right := Trunc((XY.Right - CTileRelativeEpsilon) * VPixelsAtZoom);
  Result.Bottom := Trunc((XY.Bottom - CTileRelativeEpsilon) * VPixelsAtZoom);
end;

function TCoordConverterBasic.RelativeRect2PixelRectFloatInternal(
  const XY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);

  Result.Left := XY.Left * VPixelsAtZoom;
  Result.Top := XY.Top * VPixelsAtZoom;

  Result.Right := XY.Right * VPixelsAtZoom;
  Result.Bottom := XY.Bottom * VPixelsAtZoom;
end;

function TCoordConverterBasic.RelativeRect2TileRectInternal(const XY: TExtendedRect;
  Azoom: byte): TRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);

  Result.Left := Trunc((XY.Left + CTileRelativeEpsilon) * VTilesAtZoom);
  Result.Top := Trunc((XY.Top + CTileRelativeEpsilon) * VTilesAtZoom);

  Result.Right := Trunc((XY.Right - CTileRelativeEpsilon) * VTilesAtZoom);
  Result.Bottom := Trunc((XY.Bottom - CTileRelativeEpsilon) * VTilesAtZoom);
end;

function TCoordConverterBasic.RelativeRect2TileRectFloatInternal(
  const XY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);

  Result.Left := XY.Left * VTilesAtZoom;
  Result.Top := XY.Top * VTilesAtZoom;

  Result.Right := XY.Right * VTilesAtZoom;
  Result.Bottom := XY.Bottom * VTilesAtZoom;
end;

function TCoordConverterBasic.PixelPos2TilePosInternal(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shr 8;
  Result.Y := XY.Y shr 8;
end;

function TCoordConverterBasic.PixelPosFloat2PixelPosInternal(
  const XY: TExtendedPoint; Azoom: byte): TPoint;
begin
  Result.X := Trunc(RoundTo(XY.X, -2));
  Result.Y := Trunc(RoundTo(XY.Y, -2));
end;

function TCoordConverterBasic.PixelRect2TileRectInternal(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shr 8;
  Result.Top := XY.Top shr 8;
  Result.Right := XY.Right shr 8;
  Result.Bottom := XY.Bottom shr 8;
end;

function TCoordConverterBasic.TileRect2PixelRectInternal(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shl 8;
  Result.Top := XY.Top shl 8;
  Result.Right := (XY.Right + 1) shl 8 - 1;
  Result.Bottom := (XY.Bottom + 1) shl 8 - 1;
end;

function TCoordConverterBasic.LonLatRect2PixelRectInternal(
  const XY: TExtendedRect; Azoom: byte): TRect;
begin
  Result := RelativeRect2PixelRectInternal(LonLatRect2RelativeRectInternal(XY), Azoom);
end;


function TCoordConverterBasic.LonLatRect2TileRectInternal(
  const XY: TExtendedRect; Azoom: byte): TRect;
begin
  Result := RelativeRect2TileRectInternal(LonLatRect2RelativeRectInternal(XY), Azoom);
end;

function TCoordConverterBasic.TileRect2LonLatRectInternal(
  const XY: TRect; Azoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(TileRect2RelativeRectInternal(XY, Azoom));
end;

function TCoordConverterBasic.TileRect2RelativeRectInternal(
  const XY: TRect; AZoom: byte): TExtendedRect;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.Left := XY.Left / VTilesAtZoom;
  Result.Top := XY.Top / VTilesAtZoom;
  Result.Right := (XY.Right + 1) / VTilesAtZoom;
  Result.Bottom := (XY.Bottom + 1) / VTilesAtZoom;
end;

function TCoordConverterBasic.PixelRect2RelativeRectInternal(const XY: TRect;
  AZoom: byte): TExtendedRect;
var
  VBottomRight: TPoint;
begin
  Result.TopLeft := PixelPos2RelativeInternal(XY.TopLeft, AZoom);
  VBottomRight.X := XY.Right + 1;
  VBottomRight.Y := XY.Bottom + 1;
  Result.BottomRight := PixelPos2RelativeInternal(VBottomRight, AZoom);
end;

function TCoordConverterBasic.PixelRect2LonLatRectInternal(
  const XY: TRect; AZoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(PixelRect2RelativeRectInternal(XY, AZoom));
end;

function TCoordConverterBasic.TilePos2PixelPosInternal(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shl 8;
  Result.Y := XY.Y shl 8;
end;

function TCoordConverterBasic.LonLat2TilePosfInternal(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result := LonLat2RelativeInternal(Ll);
  Result.X := Result.X * VTilesAtZoom;
  Result.Y := Result.Y * VTilesAtZoom;
end;

//------------------------------------------------------------------------------
function TCoordConverterBasic.CheckZoom(var AZoom: Byte): boolean;
begin
  Result := True;
  if AZoom > 23 then begin
    AZoom := 23;
    Result := False;
  end;
end;

function TCoordConverterBasic.CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
var
  VTilesAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    AZoom := 23;
    Result := False;
  end;
  VTilesAtZoom := TilesAtZoom(Azoom);

  if XY.X < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.X := XY.X mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if XY.X > VTilesAtZoom then begin
      Result := False;
      if ACicleMap then begin
        XY.X := XY.X mod VTilesAtZoom;
      end else begin
        XY.X := VTilesAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    XY.Y := 0;
  end else begin
    if XY.Y > VTilesAtZoom then begin
      Result := False;
      XY.Y := VTilesAtZoom;
    end;
  end;
end;

function TCoordConverterBasic.CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean;
var
  VTilesAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(Azoom);

  if XY.Left < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.Left := XY.Left mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.Left := 0;
    end;
  end else begin
    if XY.Left >= VTilesAtZoom then begin
      Result := False;
      if ACicleMap then begin
        XY.Left := XY.Left mod VTilesAtZoom;
      end else begin
        XY.Left := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    XY.Top := 0;
  end else begin
    if XY.Top >= VTilesAtZoom then begin
      Result := False;
      XY.Top := VTilesAtZoom - 1;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.Right := XY.Right mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.Right := 0;
    end;
  end else begin
    if XY.Right >= VTilesAtZoom then begin
      Result := False;
      if ACicleMap then begin
        XY.Right := XY.Right mod VTilesAtZoom;
      end else begin
        XY.Right := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    XY.Bottom := 0;
  end else begin
    if XY.Bottom >= VTilesAtZoom then begin
      Result := False;
      XY.Bottom := VTilesAtZoom - 1;
    end;
  end;
end;

function TCoordConverterBasic.CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
var
  VTilesAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(Azoom);

  if XY.X < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.X := XY.X mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if XY.X >= VTilesAtZoom then begin
      Result := False;
      if ACicleMap then begin
        XY.X := XY.X mod VTilesAtZoom;
      end else begin
        XY.X := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    XY.Y := 0;
  end else begin
    if XY.Y >= VTilesAtZoom then begin
      Result := False;
      XY.Y := VTilesAtZoom - 1;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
var
  VPixelsAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(Azoom);

  if XY.X < 0 then begin
    Result := False;
    if (Azoom < 23) then begin
      if ACicleMap then begin
        XY.X := XY.X mod VPixelsAtZoom + VPixelsAtZoom;
      end else begin
        XY.X := 0;
      end;
    end else begin
      if (XY.X <> VPixelsAtZoom) then begin
        if ACicleMap then begin
          XY.X := VPixelsAtZoom + XY.X;
        end else begin
          XY.X := 0;
        end;
      end;
    end;
  end else begin
    if (Azoom < 23) and (XY.X > VPixelsAtZoom) then begin
      Result := False;
      if ACicleMap then begin
        XY.X := XY.X mod VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    if (Azoom < 23) or (XY.Y <> VPixelsAtZoom) then begin
      XY.Y := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      Result := False;
      XY.Y := VPixelsAtZoom;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean;
var
  VPixelsAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(Azoom);

  if XY.Left < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.Left := XY.Left mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.Left := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Left >= VPixelsAtZoom) then begin
      Result := False;
      if ACicleMap then begin
        XY.Left := XY.Left mod VPixelsAtZoom;
      end else begin
        XY.Left := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    XY.Top := 0;
  end else begin
    if (Azoom < 23) and (XY.Top >= VPixelsAtZoom) then begin
      Result := False;
      XY.Top := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.Right := XY.Right mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.Right := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.Right >= VPixelsAtZoom) then begin
      Result := False;
      if ACicleMap then begin
        XY.Right := XY.Right mod VPixelsAtZoom;
      end else begin
        XY.Right := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    XY.Bottom := 0;
  end else begin
    if (Azoom < 23) and (XY.Bottom >= VPixelsAtZoom) then begin
      Result := False;
      XY.Bottom := VPixelsAtZoom - 1;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
var
  VPixelsAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(Azoom);
  if XY.X < 0 then begin
    Result := False;
    if ACicleMap then begin
      XY.X := XY.X mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if (Azoom < 23) and (XY.X >= VPixelsAtZoom) then begin
      Result := False;
      if ACicleMap then begin
        XY.X := XY.X mod VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    XY.Y := 0;
  end else begin
    if (Azoom < 23) and (XY.Y >= VPixelsAtZoom) then begin
      Result := False;
      XY.Y := VPixelsAtZoom - 1;
    end;
  end;
end;

function TCoordConverterBasic.CheckRelativePos(var XY: TExtendedPoint): boolean;
begin
  Result := True;
  if XY.X < 0 then begin
    Result := False;
    XY.X := 0;
  end else begin
    if XY.X > 1 then begin
      Result := False;
      XY.X := 1;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    XY.Y := 0;
  end else begin
    if XY.Y > 1 then begin
      Result := False;
      XY.Y := 1;
    end;
  end;
end;

function TCoordConverterBasic.CheckRelativeRect(var XY: TExtendedRect): boolean;
begin
  Result := True;
  if XY.Left < 0 then begin
    Result := False;
    XY.Left := 0;
  end else begin
    if XY.Left > 1 then begin
      Result := False;
      XY.Left := 1;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    XY.Top := 0;
  end else begin
    if XY.Top > 1 then begin
      Result := False;
      XY.Top := 1;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    XY.Right := 0;
  end else begin
    if XY.Right > 1 then begin
      Result := False;
      XY.Right := 1;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > 1 then begin
      Result := False;
      XY.Bottom := 1;
    end;
  end;
end;

function TCoordConverterBasic.CheckLonLatPos(var XY: TExtendedPoint): boolean;
begin
  Result := True;
  if XY.X < FValidLonLatRect.Left then begin
    Result := False;
    XY.X := FValidLonLatRect.Left;
  end else begin
    if XY.X > FValidLonLatRect.Right then begin
      Result := False;
      XY.X := FValidLonLatRect.Right;
    end;
  end;
  if XY.Y < FValidLonLatRect.Bottom then begin
    Result := False;
    XY.Y := FValidLonLatRect.Bottom;
  end else begin
    if XY.Y > FValidLonLatRect.Top then begin
      Result := False;
      XY.Y := FValidLonLatRect.Top;
    end;
  end;
end;

function TCoordConverterBasic.CheckLonLatRect(var XY: TExtendedRect): boolean;
begin
  Result := True;
  if XY.Left < FValidLonLatRect.Left then begin
    Result := False;
    XY.Left := FValidLonLatRect.Left;
  end else begin
    if XY.Left > FValidLonLatRect.Right then begin
      Result := False;
      XY.Left := FValidLonLatRect.Right;
    end;
  end;
  if XY.Bottom < FValidLonLatRect.Bottom then begin
    Result := False;
    XY.Bottom := FValidLonLatRect.Bottom;
  end else begin
    if XY.Bottom > FValidLonLatRect.Top then begin
      Result := False;
      XY.Bottom := FValidLonLatRect.Top;
    end;
  end;

  if XY.Right < FValidLonLatRect.Left then begin
    Result := False;
    XY.Right := FValidLonLatRect.Left;
  end else begin
    if XY.Right > FValidLonLatRect.Right then begin
      Result := False;
      XY.Right := FValidLonLatRect.Right;
    end;
  end;
  if XY.Top < FValidLonLatRect.Bottom then begin
    Result := False;
    XY.Top := FValidLonLatRect.Bottom;
  end else begin
    if XY.Top > FValidLonLatRect.Top then begin
      Result := False;
      XY.Top := FValidLonLatRect.Top;
    end;
  end;
end;


end.
