unit u_CoordConverterAbstract;

interface

uses
  Types,
  i_ICoordConverter,
  t_GeoTypes;

type
  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter, ICoordConverterSimple)
  protected
    FRadiusa: Extended;
    FProjEPSG: integer;
    FDatumEPSG: integer;
    FCellSizeUnits: TCellSizeUnits;
    FValidLonLatRect: TExtendedRect;
    function GetValidLonLatRect: TExtendedRect; virtual;

    procedure CheckZoomInternal(var AZoom: Byte); virtual;
    procedure CheckTilePosInternal(var XY: TPoint; var Azoom: byte); virtual;
    procedure CheckTilePosStrictInternal(var XY: TPoint; var Azoom: byte); virtual;
    procedure CheckTileRectInternal(var XY: TRect; var Azoom: byte); virtual;

    procedure CheckPixelPosInternal(var XY: TPoint; var Azoom: byte); virtual;
    procedure CheckPixelPosStrictInternal(var XY: TPoint; var Azoom: byte); virtual;
    procedure CheckPixelRectInternal(var XY: TRect; var Azoom: byte); virtual;

    procedure CheckRelativePosInternal(var XY: TExtendedPoint); virtual;
    procedure CheckRelativeRectInternal(var XY: TExtendedRect); virtual;

    procedure CheckLonLatPosInternal(var XY: TExtendedPoint); virtual;
    procedure CheckLonLatRectInternal(var XY: TExtendedRect); virtual;

    function Pos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2PosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2MetrInternal(const Ll: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function LonLat2MetrS(Ll: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;

    function TilesAtZoomInternal(AZoom: byte): Longint; virtual; stdcall;
    function TilesAtZoomFloatInternal(AZoom: byte): Extended; virtual; stdcall;
    function PixelsAtZoomInternal(AZoom: byte): Longint; virtual; stdcall;
    function PixelsAtZoomFloatInternal(AZoom: byte): Extended; virtual; stdcall;


    function PixelPos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function PixelPos2TilePosInternal(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function PixelPos2RelativeInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;

    function PixelPosFloat2PixelPosInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; stdcall;

    function PixelRect2TileRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function PixelRect2LonLatRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;

    function TilePos2PixelPosInternal(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function TilePos2PixelRectInternal(const XY: TPoint; Azoom: byte): TRect; virtual; stdcall;
    function TilePos2LonLatRectInternal(const XY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall;
    function TilePos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePos2RelativeInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePos2RelativeRectInternal(const XY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall;

    function TileRect2PixelRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function TileRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function TileRect2LonLatRectInternal(const XY: TRect; Azoom: byte): TExtendedRect; virtual; stdcall;

    function Relative2PixelInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function Relative2PixelPosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function Relative2TileInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function Relative2TilePosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function Relative2LonLatInternal(const XY: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;

    function RelativeRect2LonLatRectInternal(const XY: TExtendedRect): TExtendedRect; virtual; stdcall;
    function RelativeRect2TileRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;
    function RelativeRect2TileRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;
    function RelativeRect2PixelRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;
    function RelativeRect2PixelRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;


    function LonLat2PixelPosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2PixelPosfInternal(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2TilePosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2TilePosfInternal(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2RelativeInternal(const XY: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;

    function LonLatRect2RelativeRectInternal(const XY: TExtendedRect): TExtendedRect; virtual; stdcall;
    function LonLatRect2PixelRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;//TODO: Автотест
    function LonLatRect2TileRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;//TODO: Автотест
  public
    function Pos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2Pos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2Metr(const AXY: TExtendedPoint): TExtendedPoint; virtual; stdcall;

    function TilesAtZoom(AZoom: byte): Longint; stdcall;
    function TilesAtZoomFloat(AZoom: byte): Extended; stdcall;
    function PixelsAtZoom(AZoom: byte): Longint; stdcall;
    function PixelsAtZoomFloat(AZoom: byte): Extended; stdcall;


    function PixelPos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function PixelPos2TilePos(const AXY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function PixelPos2Relative(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;

    function PixelRect2TileRect(const AXY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRect2RelativeRect(const AXY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function PixelRect2LonLatRect(const AXY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;

    function TilePos2PixelPos(const AXY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function TilePos2PixelRect(const AXY: TPoint; Azoom: byte): TRect; virtual; stdcall;
    function TilePos2LonLatRect(const AXY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall;
    function TilePos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePos2Relative(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePos2RelativeRect(const AXY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall;

    function TileRect2PixelRect(const AXY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function TileRect2RelativeRect(const AXY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function TileRect2LonLatRect(const AXY: TRect; Azoom: byte): TExtendedRect; virtual; stdcall;


    function LonLat2PixelPos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2PixelPosFloat(const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2TilePos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2TilePosFloat(const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2Relative(const AXY: TExtendedPoint): TExtendedPoint; virtual; stdcall;

    function LonLatRect2RelativeRect(const AXY: TExtendedRect): TExtendedRect; virtual; stdcall;
    function LonLatRect2PixelRect(const AXY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;//TODO: Автотест
    function LonLatRect2TileRect(const AXY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;//TODO: Автотест

    function Relative2Pixel(const AXY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function Relative2PixelPosFloat(const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function Relative2Tile(const AXY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function Relative2TilePosFloat(const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function Relative2LonLat(const AXY: TExtendedPoint): TExtendedPoint; virtual; stdcall;

    function RelativeRect2LonLatRect(const AXY: TExtendedRect): TExtendedRect; virtual; stdcall;
    function RelativeRect2TileRect(const AXY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;
    function RelativeRect2TileRectFloat(const AXY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;
    function RelativeRect2PixelRect(const AXY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;
    function RelativeRect2PixelRectFloat(const AXY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;

    function GetTileSize(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function Pos2OtherMap(XY: TPoint; Azoom: byte; AOtherMapCoordConv: ICoordConverter): TPoint; virtual;
    function CalcPoligonArea(polygon: TExtendedPointArray): Extended; virtual;
    function PoligonProject(AZoom: byte; APolyg: TExtendedPointArray): TPointArray; virtual;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; virtual; abstract;

    function CheckZoom(var AZoom: Byte): boolean; virtual; stdcall;
    function CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall;
    function CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall;
    function CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall;

    function CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall;
    function CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall;
    function CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall;

    function CheckRelativePos(var XY: TExtendedPoint): boolean; virtual; stdcall;
    function CheckRelativeRect(var XY: TExtendedRect): boolean; virtual; stdcall;

    function CheckLonLatPos(var XY: TExtendedPoint): boolean; virtual; stdcall;
    function CheckLonLatRect(var XY: TExtendedRect): boolean; virtual; stdcall;

    function GetProjectionEPSG: Integer; virtual; stdcall;
    function GetDatumEPSG: integer; virtual; stdcall;
    function GetSpheroidRadius: Double; virtual; stdcall;
    function GetCellSizeUnits: TCellSizeUnits; virtual; stdcall;
    function GetTileSplitCode: Integer; virtual; stdcall;

    procedure AfterConstruction; override;
  end;

const
  CTileRelativeEpsilon = (1 / (1 shl 30 + (1 shl 30 - 1))) / 2;
  CTileSplitQuadrate256x256 = 1;

implementation

uses
  SysUtils,
  Math;

{ TCoordConverterAbstract }

function TCoordConverterAbstract.GetValidLonLatRect: TExtendedRect;
begin
  Result := TilePos2LonLatRectInternal(Point(0, 0), 0);
end;

procedure TCoordConverterAbstract.AfterConstruction;
begin
  inherited;
  FValidLonLatRect := GetValidLonLatRect;
end;

function TCoordConverterAbstract.CalcPoligonArea(
  polygon: TExtendedPointArray): extended;
var
  L, i: integer;
  LLPrev, LLCurr: TExtendedPoint;
begin
  result := 0;
  l := length(polygon);
  LLPrev := LonLat2MetrS(polygon[0]);
  for i := 1 to L - 1 do begin
    LLCurr := LonLat2MetrS(polygon[i]);
    result := result + (LLPrev.x + LLCurr.x) * (LLPrev.y - LLCurr.y);
    LLPrev := LLCurr;
  end;
  result := 0.5 * abs(result) / 1000000;
end;

function TCoordConverterAbstract.PoligonProject(AZoom: byte;
  APolyg: TExtendedPointArray): TPointArray;
var
  i: integer;
  VTilesAtZoom: Integer;
  VPoint: TExtendedPoint;
begin
  VTilesAtZoom := TilesAtZoomInternal(AZoom);
  SetLength(Result, length(APolyg));
  for i := 0 to length(APolyg) - 1 do begin
    VPoint := Apolyg[i];
    CheckLonLatPosInternal(VPoint);
    Result[i] := LonLat2PosInternal(VPoint, AZoom);
    if Result[i].y < 0 then begin
      Result[i].y := 0;
    end;
    if Result[i].y > VTilesAtZoom then begin
      Result[i].y := VTilesAtZoom - 1;
    end;
  end;
end;

function TCoordConverterAbstract.Pos2OtherMap(XY: TPoint; Azoom: byte;
  AOtherMapCoordConv: ICoordConverter): TPoint;
begin
  if (Self = nil) or (AOtherMapCoordConv = nil) then begin
    Result := XY;
  end else begin
    if (AOtherMapCoordConv.GetTileSplitCode = Self.GetTileSplitCode) and
      (AOtherMapCoordConv.GetProjectionEPSG <> 0) and
      (Self.GetProjectionEPSG <> 0) and
      (AOtherMapCoordConv.GetProjectionEPSG = Self.GetProjectionEPSG) then begin
      Result := XY;
    end else begin
      if Azoom > 23 then begin
        Result := AOtherMapCoordConv.LonLat2PixelPos(PixelPos2LonLat(XY, Azoom - 8), Azoom - 8);
      end else begin
        Result := AOtherMapCoordConv.LonLat2TilePos(TilePos2LonLat(XY, Azoom), Azoom);
      end;
    end;
  end;
end;


//------------------------------------------------------------------------------
procedure TCoordConverterAbstract.CheckZoomInternal(var AZoom: Byte);
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
end;

procedure TCoordConverterAbstract.CheckTilePosInternal(var XY: TPoint; var Azoom: byte);
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

procedure TCoordConverterAbstract.CheckTileRectInternal(var XY: TRect; var Azoom: byte);
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

procedure TCoordConverterAbstract.CheckTilePosStrictInternal(var XY: TPoint; var Azoom: byte);
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

procedure TCoordConverterAbstract.CheckPixelPosInternal(var XY: TPoint; var Azoom: byte);
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

procedure TCoordConverterAbstract.CheckPixelRectInternal(var XY: TRect; var Azoom: byte);
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

procedure TCoordConverterAbstract.CheckPixelPosStrictInternal(var XY: TPoint; var Azoom: byte);
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

procedure TCoordConverterAbstract.CheckRelativePosInternal(var XY: TExtendedPoint);
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

procedure TCoordConverterAbstract.CheckRelativeRectInternal(var XY: TExtendedRect);
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

procedure TCoordConverterAbstract.CheckLonLatPosInternal(var XY: TExtendedPoint);
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

procedure TCoordConverterAbstract.CheckLonLatRectInternal(var XY: TExtendedRect);
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
function TCoordConverterAbstract.CheckZoom(var AZoom: Byte): boolean;
begin
  Result := True;
  if AZoom > 23 then begin
    AZoom := 23;
    Result := False;
  end;
end;

function TCoordConverterAbstract.CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
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

function TCoordConverterAbstract.CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean;
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

function TCoordConverterAbstract.CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
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

function TCoordConverterAbstract.CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
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

function TCoordConverterAbstract.CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean;
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

function TCoordConverterAbstract.CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean;
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

function TCoordConverterAbstract.CheckRelativePos(var XY: TExtendedPoint): boolean;
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

function TCoordConverterAbstract.CheckRelativeRect(var XY: TExtendedRect): boolean;
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

function TCoordConverterAbstract.CheckLonLatPos(var XY: TExtendedPoint): boolean;
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

function TCoordConverterAbstract.CheckLonLatRect(var XY: TExtendedRect): boolean;
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

//------------------------------------------------------------------------------
function TCoordConverterAbstract.TilePos2PixelRectInternal(const XY: TPoint;
  Azoom: byte): TRect;
begin
  Result.Left := XY.X shl 8;
  Result.Top := XY.Y shl 8;
  Result.Right := Result.Left + ((1 shl 8) - 1);
  Result.Bottom := Result.Top + ((1 shl 8) - 1);
end;

function TCoordConverterAbstract.TilePos2LonLatRectInternal(const XY: TPoint;
  Azoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(TilePos2RelativeRectInternal(XY, Azoom));
end;

function TCoordConverterAbstract.PixelsAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl (AZoom + 8);
end;

function TCoordConverterAbstract.PixelsAtZoomFloatInternal(
  AZoom: byte): Extended;
begin
  Result := 1 shl AZoom;
  Result := Result * 256;
end;

function TCoordConverterAbstract.TilesAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterAbstract.TilesAtZoomFloatInternal(
  AZoom: byte): Extended;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterAbstract.PixelPos2RelativeInternal(const XY: TPoint;
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

function TCoordConverterAbstract.Relative2PixelInternal(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);
  Result.X := Trunc(RoundTo(XY.X * VPixelsAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VPixelsAtZoom, -2));
end;

function TCoordConverterAbstract.Relative2PixelPosFloatInternal(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);
  Result.X := XY.X * VPixelsAtZoom;
  Result.Y := XY.Y * VPixelsAtZoom;
end;

function TCoordConverterAbstract.LonLat2PixelPosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2PixelInternal(LonLat2RelativeInternal(LL), AZoom);
end;

function TCoordConverterAbstract.LonLat2PixelPosfInternal(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VPixelsAtZoom: Extended;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(Azoom);

  Result := LonLat2RelativeInternal(LL);
  Result.X := Result.X * VPixelsAtZoom;
  Result.Y := Result.Y * VPixelsAtZoom;
end;

function TCoordConverterAbstract.LonLat2TilePosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  Result := Relative2TileInternal(LonLat2RelativeInternal(LL), AZoom);
end;

function TCoordConverterAbstract.PixelPos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(PixelPos2RelativeInternal(XY, Azoom));
end;

function TCoordConverterAbstract.TilePos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  Result := Relative2LonLatInternal(TilePos2RelativeInternal(XY, Azoom));
end;

function TCoordConverterAbstract.TilePos2RelativeInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.X := XY.X / VTilesAtZoom;
  Result.Y := XY.Y / VTilesAtZoom;
end;

function TCoordConverterAbstract.TilePos2RelativeRectInternal(const XY: TPoint;
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

function TCoordConverterAbstract.LonLatRect2RelativeRectInternal(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := LonLat2RelativeInternal(XY.TopLeft);
  Result.BottomRight := LonLat2RelativeInternal(XY.BottomRight);
end;

function TCoordConverterAbstract.Relative2TileInternal(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.X := Trunc(RoundTo(XY.X * VTilesAtZoom, -2));
  Result.Y := Trunc(RoundTo(XY.Y * VTilesAtZoom, -2));
end;

function TCoordConverterAbstract.Relative2TilePosFloatInternal(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result.X := XY.X * VTilesAtZoom;
  Result.Y := XY.Y * VTilesAtZoom;
end;

function TCoordConverterAbstract.RelativeRect2LonLatRectInternal(
  const XY: TExtendedRect): TExtendedRect;
begin
  Result.TopLeft := Relative2LonLatInternal(XY.TopLeft);
  Result.BottomRight := Relative2LonLatInternal(XY.BottomRight);
end;

function TCoordConverterAbstract.RelativeRect2PixelRectInternal(const XY: TExtendedRect;
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

function TCoordConverterAbstract.RelativeRect2PixelRectFloatInternal(
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

function TCoordConverterAbstract.RelativeRect2TileRectInternal(const XY: TExtendedRect;
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

function TCoordConverterAbstract.RelativeRect2TileRectFloatInternal(
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

function TCoordConverterAbstract.PixelPos2TilePosInternal(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shr 8;
  Result.Y := XY.Y shr 8;
end;

function TCoordConverterAbstract.PixelPosFloat2PixelPosInternal(
  const XY: TExtendedPoint; Azoom: byte): TPoint;
begin
  Result.X := Trunc(RoundTo(XY.X, -2));
  Result.Y := Trunc(RoundTo(XY.Y, -2));
end;

function TCoordConverterAbstract.PixelRect2TileRectInternal(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shr 8;
  Result.Top := XY.Top shr 8;
  Result.Right := XY.Right shr 8;
  Result.Bottom := XY.Bottom shr 8;
end;

function TCoordConverterAbstract.TileRect2PixelRectInternal(const XY: TRect;
  AZoom: byte): TRect;
begin
  Result.Left := XY.Left shl 8;
  Result.Top := XY.Top shl 8;
  Result.Right := (XY.Right + 1) shl 8 - 1;
  Result.Bottom := (XY.Bottom + 1) shl 8 - 1;
end;

function TCoordConverterAbstract.LonLatRect2PixelRectInternal(
  const XY: TExtendedRect; Azoom: byte): TRect;
begin
  Result := RelativeRect2PixelRectInternal(LonLatRect2RelativeRectInternal(XY), Azoom);
end;


function TCoordConverterAbstract.LonLatRect2TileRectInternal(
  const XY: TExtendedRect; Azoom: byte): TRect;
begin
  Result := RelativeRect2TileRectInternal(LonLatRect2RelativeRectInternal(XY), Azoom);
end;

function TCoordConverterAbstract.TileRect2LonLatRectInternal(
  const XY: TRect; Azoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(TileRect2RelativeRectInternal(XY, Azoom));
end;

function TCoordConverterAbstract.TileRect2RelativeRectInternal(
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

function TCoordConverterAbstract.PixelRect2RelativeRectInternal(const XY: TRect;
  AZoom: byte): TExtendedRect;
var
  VBottomRight: TPoint;
begin
  Result.TopLeft := PixelPos2RelativeInternal(XY.TopLeft, AZoom);
  VBottomRight.X := XY.Right + 1;
  VBottomRight.Y := XY.Bottom + 1;
  Result.BottomRight := PixelPos2RelativeInternal(VBottomRight, AZoom);
end;

function TCoordConverterAbstract.PixelRect2LonLatRectInternal(
  const XY: TRect; AZoom: byte): TExtendedRect;
begin
  Result := RelativeRect2LonLatRectInternal(PixelRect2RelativeRectInternal(XY, AZoom));
end;

function TCoordConverterAbstract.TilePos2PixelPosInternal(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result.X := XY.X shl 8;
  Result.Y := XY.Y shl 8;
end;

function TCoordConverterAbstract.LonLat2TilePosfInternal(const Ll: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VTilesAtZoom: Extended;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(Azoom);
  Result := LonLat2RelativeInternal(Ll);
  Result.X := Result.X * VTilesAtZoom;
  Result.Y := Result.Y * VTilesAtZoom;
end;

function TCoordConverterAbstract.LonLat2PosInternal(const Ll: TExtendedPoint;
  Azoom: byte): Tpoint;
begin
  if Azoom > 23 then begin
    Result := LonLat2PixelPosInternal(Ll, Azoom - 8);
  end else begin
    Result := LonLat2TilePosInternal(Ll, Azoom);
  end;
end;

function TCoordConverterAbstract.Pos2LonLatInternal(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
begin
  if Azoom > 23 then begin
    Result := PixelPos2LonLatInternal(XY, Azoom - 8);
  end else begin
    Result := TilePos2LonLatInternal(XY, Azoom);
  end;
end;

//------------------------------------------------------------------------------
function TCoordConverterAbstract.TilePos2PixelRect(const AXY: TPoint;
  Azoom: byte): TRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2LonLatRect(const AXY: TPoint;
  Azoom: byte): TExtendedRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelsAtZoom(AZoom: byte): Longint;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := PixelsAtZoomInternal(Vzoom);
end;

function TCoordConverterAbstract.PixelsAtZoomFloat(AZoom: byte): Extended;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := PixelsAtZoomFloatInternal(Vzoom);
end;

function TCoordConverterAbstract.TilesAtZoom(AZoom: byte): Longint;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := TilesAtZoomInternal(Vzoom);
end;

function TCoordConverterAbstract.TilesAtZoomFloat(AZoom: byte): Extended;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := TilesAtZoomFloatInternal(Vzoom);
end;

function TCoordConverterAbstract.PixelPos2Relative(const AXY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckPixelPosInternal(VXY, VZoom);
  Result := PixelPos2RelativeInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.Relative2LonLat(
  const AXY: TExtendedPoint): TExtendedPoint;
var
  VXY: TExtendedPoint;
begin
  VXY := AXY;
  CheckRelativePosInternal(VXY);
  Result := Relative2LonLatInternal(VXY);
end;

function TCoordConverterAbstract.Relative2Pixel(const AXY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2PixelInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.Relative2PixelPosFloat(
  const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2PixelPosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2Metr(
  const AXY: TExtendedPoint): TExtendedPoint;
var
  VXY: TExtendedPoint;
begin
  VXY := AXY;
  CheckLonLatPosInternal(VXY);
  Result := LonLat2MetrInternal(VXY);
end;

function TCoordConverterAbstract.LonLat2PixelPos(const AXY: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  Result := LonLat2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2PixelPosFloat(const AXY: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLat2PixelPosfInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPos2LonLat(const AXY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckPixelPosInternal(VXY, VZoom);
  Result := PixelPos2LonLatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2LonLat(const AXY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosInternal(VXY, VZoom);
  Result := TilePos2LonLatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2Relative(const AXY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosInternal(VXY, VZoom);
  Result := TilePos2RelativeInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2RelativeRect(const AXY: TPoint;
  Azoom: byte): TExtendedRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLatRect2RelativeRect(
  const AXY: TExtendedRect): TExtendedRect;
var
  VXY: TExtendedRect;
begin
  VXY := AXY;
  CheckLonLatRectInternal(VXY);
  Result := LonLatRect2RelativeRectInternal(VXY);
end;

function TCoordConverterAbstract.Relative2Tile(const AXY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2TileInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.Relative2TilePosFloat(
  const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2TilePosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2LonLatRect(
  const AXY: TExtendedRect): TExtendedRect;
var
  VXY: TExtendedRect;
begin
  VXY := AXY;
  CheckRelativeRectInternal(VXY);
  Result := RelativeRect2LonLatRectInternal(VXY);
end;

function TCoordConverterAbstract.RelativeRect2PixelRect(const AXY: TExtendedRect;
  Azoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2PixelRectFloat(
  const AXY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2PixelRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRect(const AXY: TExtendedRect;
  Azoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRectFloat(
  const AXY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2TileRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPos2TilePos(const AXY: TPoint;
  Azoom: byte): TPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckPixelPosInternal(VXY, VZoom);
  Result := PixelPos2TilePosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRect2LonLatRect(const AXY: TRect;
  AZoom: byte): TExtendedRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckPixelRectInternal(VXY, VZoom);
  Result := PixelRect2LonLatRectInternal(VXY, Vzoom);
end;


function TCoordConverterAbstract.PixelRect2TileRect(const AXY: TRect;
  AZoom: byte): TRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckPixelRectInternal(VXY, VZoom);
  Result := PixelRect2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRect2PixelRect(const AXY: TRect;
  AZoom: byte): TRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTileRectInternal(VXY, VZoom);
  Result := TileRect2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRect2RelativeRect(const AXY: TRect;
  AZoom: byte): TExtendedRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckPixelRectInternal(VXY, VZoom);
  Result := PixelRect2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2PixelPos(const AXY: TPoint;
  Azoom: byte): TPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosInternal(VXY, VZoom);
  Result := TilePos2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2TilePos(const AXY: TExtendedPoint;
  Azoom: byte): Tpoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLat2TilePosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2TilePosFloat(const AXY: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLat2TilePosfInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2Pos(const AXY: TExtendedPoint;
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

function TCoordConverterAbstract.LonLat2Relative(
  const AXY: TExtendedPoint): TExtendedPoint;
var
  VXY: TExtendedPoint;
begin
  VXY := AXY;
  CheckLonLatPosInternal(VXY);
  Result := LonLat2RelativeInternal(VXY);
end;

function TCoordConverterAbstract.Pos2LonLat(const AXY: TPoint;
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

function TCoordConverterAbstract.TileRect2LonLatRect(const AXY: TRect;
  Azoom: byte): TExtendedRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTileRectInternal(VXY, VZoom);
  Result := TileRect2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRect2RelativeRect(const AXY: TRect;
  AZoom: byte): TExtendedRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTileRectInternal(VXY, VZoom);
  Result := TileRect2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLatRect2TileRect(const AXY: TExtendedRect;
  Azoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLatRect2PixelRect(const AXY: TExtendedRect;
  Azoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2PixelRectInternal(VXY, Vzoom);
end;


function TCoordConverterAbstract.GetTileSize(const XY: TPoint;
  Azoom: byte): TPoint;
begin
  Result := Point(256, 256);
end;

function TCoordConverterAbstract.GetDatumEPSG: integer;
begin
  Result := FDatumEPSG;
end;

function TCoordConverterAbstract.GetProjectionEPSG: Integer;
begin
  Result := FProjEPSG;
end;

function TCoordConverterAbstract.GetSpheroidRadius: Double;
begin
  Result := FRadiusa;
end;

function TCoordConverterAbstract.GetCellSizeUnits: TCellSizeUnits;
begin
  Result := FCellSizeUnits;
end;

function TCoordConverterAbstract.GetTileSplitCode: Integer;
begin
  Result := CTileSplitQuadrate256x256;
end;

end.
