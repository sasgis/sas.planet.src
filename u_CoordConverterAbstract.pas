unit u_CoordConverterAbstract;

interface

uses
  Types,
  i_ICoordConverter,
  t_GeoTypes;

type
  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  protected
    FRadiusa: Extended;
    FProjEPSG: integer;
    FDatumEPSG: integer;
    FCellSizeUnits: TCellSizeUnits;

    procedure CheckZoomInternal(var AZoom: Byte); virtual; stdcall; abstract;

    procedure CheckPixelPosInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelPosStrictInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelPosFloatInternal(var XY: TExtendedPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelRectInternal(var XY: TRect; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelRectFloatInternal(var XY: TExtendedRect; var Azoom: byte); virtual; stdcall; abstract;

    procedure CheckTilePosInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTilePosStrictInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTilePosFloatInternal(var XY: TExtendedPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTileRectInternal(var XY: TRect; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTileRectFloatInternal(var XY: TExtendedRect; var Azoom: byte); virtual; stdcall; abstract;

    procedure CheckRelativePosInternal(var XY: TExtendedPoint); virtual; stdcall; abstract;
    procedure CheckRelativeRectInternal(var XY: TExtendedRect); virtual; stdcall; abstract;

    procedure CheckLonLatPosInternal(var XY: TExtendedPoint); virtual; stdcall; abstract;
    procedure CheckLonLatRectInternal(var XY: TExtendedRect); virtual; stdcall; abstract;

    function LonLat2MetrInternal(const Ll: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;
    function LonLat2MetrS(Ll: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;

    function TilesAtZoomInternal(AZoom: byte): Longint; virtual; stdcall; abstract;
    function TilesAtZoomFloatInternal(AZoom: byte): Extended; virtual; stdcall; abstract;
    function PixelsAtZoomInternal(AZoom: byte): Longint; virtual; stdcall; abstract;
    function PixelsAtZoomFloatInternal(AZoom: byte): Extended; virtual; stdcall; abstract;


    function PixelPos2TilePosInternal(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function PixelPos2TilePosFloatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function PixelPos2RelativeInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function PixelPos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;

    function PixelPosFloat2PixelPosInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function PixelPosFloat2TilePosInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function PixelPosFloat2TilePosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function PixelPosFloat2RelativeInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function PixelPosFloat2LonLatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;

    function PixelRect2TileRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRect2TileRectFloatInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function PixelRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function PixelRect2LonLatRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;

    function PixelRectFloat2PixelRectInternal(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRectFloat2TileRectInternal(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRectFloat2TileRectFloatInternal(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function PixelRectFloat2RelativeRectInternal(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function PixelRectFloat2LonLatRectInternal(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;

    function TilePos2PixelPosInternal(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function TilePos2PixelRectInternal(const XY: TPoint; Azoom: byte): TRect; virtual; stdcall; abstract;
    function TilePos2LonLatRectInternal(const XY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function TilePos2LonLatInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function TilePos2RelativeInternal(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function TilePos2RelativeRectInternal(const XY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;

    function TilePosFloat2TilePosInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function TilePosFloat2PixelPosInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function TilePosFloat2PixelPosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function TilePosFloat2RelativeInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function TilePosFloat2LonLatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;

    function TileRect2PixelRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function TileRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function TileRect2LonLatRectInternal(const XY: TRect; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;

    function TileRectFloat2TileRectInternal(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function TileRectFloat2PixelRectInternal(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function TileRectFloat2PixelRectFloatInternal(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function TileRectFloat2RelativeRectInternal(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function TileRectFloat2LonLatRectInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;

    function Relative2PixelInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function Relative2PixelPosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function Relative2TileInternal(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function Relative2TilePosFloatInternal(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function Relative2LonLatInternal(const XY: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;

    function RelativeRect2LonLatRectInternal(const XY: TExtendedRect): TExtendedRect; virtual; stdcall; abstract;
    function RelativeRect2TileRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function RelativeRect2TileRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function RelativeRect2PixelRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function RelativeRect2PixelRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;


    function LonLat2PixelPosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall; abstract;
    function LonLat2PixelPosFloatInternal(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function LonLat2TilePosInternal(const Ll: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall; abstract;
    function LonLat2TilePosFloatInternal(const Ll: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall; abstract;
    function LonLat2RelativeInternal(const XY: TExtendedPoint): TExtendedPoint; virtual; stdcall; abstract;

    function LonLatRect2RelativeRectInternal(const XY: TExtendedRect): TExtendedRect; virtual; stdcall; abstract;
    function LonLatRect2PixelRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function LonLatRect2PixelRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;
    function LonLatRect2TileRectInternal(const XY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function LonLatRect2TileRectFloatInternal(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall; abstract;
  public
    function TilesAtZoom(AZoom: byte): Longint; stdcall;
    function TilesAtZoomFloat(AZoom: byte): Extended; stdcall;
    function PixelsAtZoom(AZoom: byte): Longint; stdcall;
    function PixelsAtZoomFloat(AZoom: byte): Extended; stdcall;


    function PixelPos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function PixelPos2TilePos(const AXY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function PixelPos2Relative(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function PixelPos2TilePosFloat(const XY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;

    function PixelPosFloat2PixelPos(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function PixelPosFloat2TilePos(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function PixelPosFloat2TilePosFloat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function PixelPosFloat2Relative(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function PixelPosFloat2LonLat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;

    function PixelRect2TileRect(const AXY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRect2RelativeRect(const AXY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function PixelRect2LonLatRect(const AXY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function PixelRect2TileRectFloat(const XY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;

    function PixelRectFloat2PixelRect(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRectFloat2TileRect(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall;
    function PixelRectFloat2TileRectFloat(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function PixelRectFloat2RelativeRect(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function PixelRectFloat2LonLatRect(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall;

    function TilePos2PixelPos(const AXY: TPoint; Azoom: byte): TPoint; virtual; stdcall;
    function TilePos2PixelRect(const AXY: TPoint; Azoom: byte): TRect; virtual; stdcall;
    function TilePos2LonLatRect(const AXY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall;
    function TilePos2LonLat(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePos2Relative(const AXY: TPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePos2RelativeRect(const AXY: TPoint; Azoom: byte): TExtendedRect; virtual; stdcall;

    function TilePosFloat2TilePos(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function TilePosFloat2PixelPos(const XY: TExtendedPoint; Azoom: byte): TPoint; virtual; stdcall;
    function TilePosFloat2PixelPosFloat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePosFloat2Relative(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function TilePosFloat2LonLat(const XY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;

    function TileRect2PixelRect(const AXY: TRect; AZoom: byte): TRect; virtual; stdcall;
    function TileRect2RelativeRect(const AXY: TRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function TileRect2LonLatRect(const AXY: TRect; Azoom: byte): TExtendedRect; virtual; stdcall;

    function TileRectFloat2TileRect(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall;
    function TileRectFloat2PixelRect(const XY: TExtendedRect; AZoom: byte): TRect; virtual; stdcall;
    function TileRectFloat2PixelRectFloat(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function TileRectFloat2RelativeRect(const XY: TExtendedRect; AZoom: byte): TExtendedRect; virtual; stdcall;
    function TileRectFloat2LonLatRect(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;

    function LonLat2PixelPos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2PixelPosFloat(const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2TilePos(const AXY: TExtendedPoint; Azoom: byte): Tpoint; virtual; stdcall;
    function LonLat2TilePosFloat(const AXY: TExtendedPoint; Azoom: byte): TExtendedPoint; virtual; stdcall;
    function LonLat2Relative(const AXY: TExtendedPoint): TExtendedPoint; virtual; stdcall;

    function LonLatRect2RelativeRect(const AXY: TExtendedRect): TExtendedRect; virtual; stdcall;
    function LonLatRect2PixelRect(const AXY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;//TODO: Автотест
    function LonLatRect2PixelRectFloat(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;
    function LonLatRect2TileRect(const AXY: TExtendedRect; Azoom: byte): TRect; virtual; stdcall;//TODO: Автотест
    function LonLatRect2TileRectFloat(const XY: TExtendedRect; Azoom: byte): TExtendedRect; virtual; stdcall;

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

    function LonLatArray2PixelArray(APolyg: TExtendedPointArray; AZoom: byte): TPointArray; virtual; stdcall;
    function LonLatArray2PixelArrayFloat(APolyg: TExtendedPointArray; AZoom: byte): TExtendedPointArray; virtual; stdcall;

    function GetTileSize(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function PixelPos2OtherMap(XY: TPoint; Azoom: byte; AOtherMapCoordConv: ICoordConverter): TPoint; virtual; stdcall;
    function CalcPoligonArea(polygon: TExtendedPointArray): Extended; virtual;
    function CalcDist(AStart: TExtendedPoint; AFinish: TExtendedPoint): Extended; virtual; abstract;

    function CheckZoom(var AZoom: Byte): boolean; virtual; stdcall; abstract;
    function CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckTileRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;

    function CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckPixelRect(var XY: TRect; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;

    function CheckRelativePos(var XY: TExtendedPoint): boolean; virtual; stdcall; abstract;
    function CheckRelativeRect(var XY: TExtendedRect): boolean; virtual; stdcall; abstract;

    function CheckLonLatPos(var XY: TExtendedPoint): boolean; virtual; stdcall; abstract;
    function CheckLonLatRect(var XY: TExtendedRect): boolean; virtual; stdcall; abstract;

    function GetProjectionEPSG: Integer; virtual; stdcall;
    function GetDatumEPSG: integer; virtual; stdcall;
    function GetSpheroidRadius: Double; virtual; stdcall;
    function GetCellSizeUnits: TCellSizeUnits; virtual; stdcall;
    function GetTileSplitCode: Integer; virtual; stdcall; abstract;
    function IsSameConverter(AOtherMapCoordConv: ICoordConverter): Boolean; virtual; stdcall;

    function LonLat2Metr(const AXY: TExtendedPoint): TExtendedPoint; virtual; stdcall;
  end;

implementation

uses
  SysUtils;

{ TCoordConverterAbstract }

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

function TCoordConverterAbstract.LonLatArray2PixelArray(
  APolyg: TExtendedPointArray; AZoom: byte): TPointArray;
var
  i: integer;
  VPoint: TExtendedPoint;
begin
  SetLength(Result, length(APolyg));
  for i := 0 to length(APolyg) - 1 do begin
    VPoint := Apolyg[i];
    CheckLonLatPosInternal(VPoint);
    Result[i] := LonLat2PixelPosInternal(VPoint, AZoom);
  end;
end;

function TCoordConverterAbstract.LonLatArray2PixelArrayFloat(
  APolyg: TExtendedPointArray; AZoom: byte): TExtendedPointArray;
var
  i: integer;
  VPoint: TExtendedPoint;
begin
  SetLength(Result, length(APolyg));
  for i := 0 to length(APolyg) - 1 do begin
    VPoint := Apolyg[i];
    CheckLonLatPosInternal(VPoint);
    Result[i] := LonLat2PixelPosFloatInternal(VPoint, AZoom);
  end;
end;

function TCoordConverterAbstract.PixelPos2OtherMap(XY: TPoint; Azoom: byte;
  AOtherMapCoordConv: ICoordConverter): TPoint;
var
  VLonLat: TExtendedPoint;
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
      CheckPixelPosInternal(XY, Azoom);
      VLonLat := PixelPos2LonLatInternal(XY, Azoom);
      AOtherMapCoordConv.CheckLonLatPos(VLonLat);
      Result := AOtherMapCoordConv.LonLat2PixelPos(VLonLat, Azoom);
    end;
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
  Result := LonLat2PixelPosFloatInternal(VXY, Vzoom);
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

function TCoordConverterAbstract.TilePosFloat2LonLat(const XY: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2LonLatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2PixelPos(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2PixelPosFloat(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2PixelPosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2Relative(const XY: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2RelativeInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2TilePos(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2TilePosInternal(VXY, Vzoom);
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

function TCoordConverterAbstract.PixelPos2TilePosFloat(const XY: TPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosInternal(VXY, VZoom);
  Result := PixelPos2TilePosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2LonLat(const XY: TExtendedPoint;
  Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2LonLatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2PixelPos(
  const XY: TExtendedPoint; Azoom: byte): TPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2Relative(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2RelativeInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2TilePos(const XY: TExtendedPoint;
  Azoom: byte): TPoint;
var
  VXY: TExtendedPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2TilePosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2TilePosFloat(
  const XY: TExtendedPoint; Azoom: byte): TExtendedPoint;
begin

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

function TCoordConverterAbstract.PixelRect2TileRectFloat(const XY: TRect;
  AZoom: byte): TExtendedRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectInternal(VXY, VZoom);
  Result := PixelRect2TileRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2LonLatRect(
  const XY: TExtendedRect; AZoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2PixelRect(
  const XY: TExtendedRect; AZoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2RelativeRect(
  const XY: TExtendedRect; AZoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2TileRect(
  const XY: TExtendedRect; AZoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2TileRectFloat(
  const XY: TExtendedRect; AZoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2TileRectFloatInternal(VXY, Vzoom);
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
  Result := LonLat2TilePosFloatInternal(VXY, Vzoom);
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

function TCoordConverterAbstract.TileRectFloat2LonLatRect(
  const XY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2PixelRect(
  const XY: TExtendedRect; AZoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2PixelRectFloat(
  const XY: TExtendedRect; AZoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2PixelRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2RelativeRect(
  const XY: TExtendedRect; AZoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2TileRect(const XY: TExtendedRect;
  AZoom: byte): TRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2TileRectInternal(VXY, Vzoom);
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

function TCoordConverterAbstract.LonLatRect2TileRectFloat(
  const XY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2TileRectFloatInternal(VXY, Vzoom);
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


function TCoordConverterAbstract.LonLatRect2PixelRectFloat(
  const XY: TExtendedRect; Azoom: byte): TExtendedRect;
var
  VXY: TExtendedRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2PixelRectFloatInternal(VXY, Vzoom);
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

function TCoordConverterAbstract.IsSameConverter(
  AOtherMapCoordConv: ICoordConverter): Boolean;
begin
  Result :=
    (Self.GetTileSplitCode <> 0) and
    (AOtherMapCoordConv.GetTileSplitCode <> 0) and
    (AOtherMapCoordConv.GetTileSplitCode = Self.GetTileSplitCode) and
    (AOtherMapCoordConv.GetProjectionEPSG <> 0) and
    (Self.GetProjectionEPSG <> 0) and
    (AOtherMapCoordConv.GetProjectionEPSG = Self.GetProjectionEPSG);
end;

end.
