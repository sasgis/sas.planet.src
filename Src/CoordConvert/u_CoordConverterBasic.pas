{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_CoordConverterBasic;

interface

uses
  Types,
  i_CoordConverter,
  t_GeoTypes,
  u_CoordConverterAbstract;

type
  TCoordConverterBasic = class(TCoordConverterAbstract, ICoordConverterSimple)
  private
    FValidLonLatRect: TDoubleRect;
  protected
    function GetValidLonLatRect: TDoubleRect;

    procedure ValidateZoomInternal(var AZoom: Byte); override;

    procedure ValidatePixelPosInternal(
      var XY: TPoint;
      var AZoom: byte
    ); override;
    procedure ValidatePixelPosStrictInternal(
      var XY: TPoint;
      var AZoom: byte
    ); override;
    procedure ValidatePixelPosFloatInternal(
      var XY: TDoublePoint;
      var AZoom: byte
    ); override;
    procedure ValidatePixelRectInternal(
      var XY: TRect;
      var AZoom: byte
    ); override;
    procedure ValidatePixelRectFloatInternal(
      var XY: TDoubleRect;
      var AZoom: byte
    ); override;

    procedure ValidateTilePosInternal(
      var XY: TPoint;
      var AZoom: byte
    ); override;
    procedure ValidateTilePosStrictInternal(
      var XY: TPoint;
      var AZoom: byte
    ); override;
    procedure ValidateTilePosFloatInternal(
      var XY: TDoublePoint;
      var AZoom: byte
    ); override;
    procedure ValidateTileRectInternal(
      var XY: TRect;
      var AZoom: byte
    ); override;
    procedure ValidateTileRectFloatInternal(
      var XY: TDoubleRect;
      var AZoom: byte
    ); override;

    procedure ValidateRelativePosInternal(var XY: TDoublePoint); override;
    procedure ValidateRelativeRectInternal(var XY: TDoubleRect); override;

    procedure ValidateLonLatPosInternal(var XY: TDoublePoint); override;
    procedure ValidateLonLatRectInternal(var XY: TDoubleRect); override;

    function TileRectAtZoomInternal(const AZoom: byte): TRect; override;
    function PixelRectAtZoomInternal(const AZoom: byte): TRect; override;

    function TilesAtZoomInternal(AZoom: byte): Longint; override;
    function TilesAtZoomFloatInternal(AZoom: byte): Double; override;
    function PixelsAtZoomInternal(AZoom: byte): Longint; override;
    function PixelsAtZoomFloatInternal(AZoom: byte): Double; override;


    function PixelPos2TilePosInternal(
      const XY: TPoint;
      AZoom: byte;
      ARounding: TPointRounding
    ): TPoint; override;
    function PixelPos2TilePosFloatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; override;
    function PixelPos2RelativeInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; override;
    function PixelPos2LonLatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; override;

    function PixelPosFloat2TilePosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;
    function PixelPosFloat2RelativeInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;
    function PixelPosFloat2LonLatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;

    function PixelRect2TileRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TRect; override;
    function PixelRect2TileRectFloatInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; override;
    function PixelRect2RelativeRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; override;
    function PixelRect2LonLatRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; override;

    function PixelRectFloat2TileRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function PixelRectFloat2RelativeRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function PixelRectFloat2LonLatRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;

    function TilePos2PixelPosInternal(
      const XY: TPoint;
      AZoom: byte
    ): TPoint; override;
    function TilePos2PixelRectInternal(
      const XY: TPoint;
      AZoom: byte
    ): TRect; override;
    function TilePos2PixelRectFloatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoubleRect; override;
    function TilePos2LonLatRectInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoubleRect; override;
    function TilePos2LonLatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; override;
    function TilePos2RelativeInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; override;
    function TilePos2RelativeRectInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoubleRect; override;

    function TilePosFloat2PixelPosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;
    function TilePosFloat2RelativeInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;
    function TilePosFloat2LonLatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;

    function TileRect2PixelRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TRect; override;
    function TileRect2RelativeRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; override;
    function TileRect2LonLatRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; override;

    function TileRectFloat2PixelRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function TileRectFloat2RelativeRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function TileRectFloat2LonLatRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;

    function Relative2PixelPosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;
    function Relative2TilePosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;

    function RelativeRect2PixelRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function RelativeRect2TileRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function RelativeRect2LonLatRectInternal(const XY: TDoubleRect): TDoubleRect; override;


    function LonLat2PixelPosFloatInternal(
      const ALonLat: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;
    function LonLat2TilePosFloatInternal(
      const ALonLat: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; override;

    function LonLatRect2RelativeRectInternal(const XY: TDoubleRect): TDoubleRect; override;
    function LonLatRect2PixelRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
    function LonLatRect2TileRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; override;
  protected
    procedure ValidateZoom(var AZoom: Byte); override;
    procedure ValidateTilePos(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); override;
    procedure ValidateTilePosStrict(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); override;
    procedure ValidateTileRect(
      var XY: TRect;
      var AZoom: byte
    ); override;

    procedure ValidatePixelPos(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); override;
    procedure ValidatePixelPosFloat(
      var XY: TDoublePoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); override;
    procedure ValidatePixelPosStrict(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); override;
    procedure ValidatePixelPosFloatStrict(
      var XY: TDoublePoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ); override;
    procedure ValidatePixelRect(
      var XY: TRect;
      var AZoom: byte
    ); override;
    procedure ValidatePixelRectFloat(
      var XY: TDoubleRect;
      var AZoom: byte
    ); override;

    procedure ValidateRelativePos(var XY: TDoublePoint); override;
    procedure ValidateRelativeRect(var XY: TDoubleRect); override;

    procedure ValidateLonLatPos(var XY: TDoublePoint); override;
    procedure ValidateLonLatRect(var XY: TDoubleRect); override;

    function CheckZoom(const AZoom: Byte): boolean; override;
    function CheckTilePos(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; override;
    function CheckTilePosStrict(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; override;
    function CheckTileRect(
      const XY: TRect;
      const AZoom: byte
    ): boolean; override;

    function CheckPixelPos(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; override;
    function CheckPixelPosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): boolean; override;
    function CheckPixelPosStrict(
      const XY: TPoint;
      const AZoom: byte
    ): boolean; override;
    function CheckPixelPosFloatStrict(
      const XY: TDoublePoint;
      const AZoom: byte
    ): boolean; override;
    function CheckPixelRect(
      const XY: TRect;
      const AZoom: byte
    ): boolean; override;
    function CheckPixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): boolean; override;

    function CheckRelativePos(const XY: TDoublePoint): boolean; override;
    function CheckRelativeRect(const XY: TDoubleRect): boolean; override;

    function CheckLonLatPos(const XY: TDoublePoint): boolean; override;
    function CheckLonLatRect(const XY: TDoubleRect): boolean; override;

    function GetTileSplitCode: Integer; override;
    function GetTileSize(
      const XY: TPoint;
      const AZoom: byte
    ): TPoint; override;
    function LonLat2MetrInternal(const ALl: TDoublePoint): TDoublePoint; override;
    function Metr2LonLatInternal(const AMm: TDoublePoint): TDoublePoint; override;
  private
    function Pos2LonLat(
      const AXY: TPoint;
      AZoom: byte
    ): TDoublePoint; stdcall;
    function LonLat2Pos(
      const AXY: TDoublePoint;
      AZoom: byte
    ): Tpoint; stdcall;
  public
    procedure AfterConstruction; override;
  end;

const
  CTileRelativeEpsilon = (1 / (1 shl 30 + (1 shl 30 - 1))) / 2;

implementation

uses
  SysUtils,
  c_CoordConverter,
  u_GeoFunc;

function TCoordConverterBasic.GetValidLonLatRect: TDoubleRect;
begin
  Result := TilePos2LonLatRectInternal(Point(0, 0), 0);
end;

procedure TCoordConverterBasic.AfterConstruction;
begin
  inherited;
  FValidLonLatRect := GetValidLonLatRect;
end;

function TCoordConverterBasic.LonLat2Pos(
  const AXY: TDoublePoint;
  AZoom: byte
): Tpoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateLonLatPosInternal(VXY);
  if AZoom > 23 then begin
    VZoom := VZoom - 8;
    ValidateZoomInternal(VZoom);
    Result := PointFromDoublePoint(Relative2PixelPosFloatInternal(LonLat2RelativeInternal(VXY), VZoom), prToTopLeft);
  end else begin
    ValidateZoomInternal(VZoom);
    Result := PointFromDoublePoint(Relative2TilePosFloatInternal(LonLat2RelativeInternal(VXY), VZoom), prToTopLeft);
  end;
end;

function TCoordConverterBasic.Pos2LonLat(
  const AXY: TPoint;
  AZoom: byte
): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  if AZoom > 23 then begin
    VZoom := VZoom - 8;
    ValidatePixelPosInternal(VXY, VZoom);
    Result := PixelPos2LonLatInternal(VXY, VZoom);
  end else begin
    ValidateTilePosInternal(VXY, VZoom);
    Result := TilePos2LonLatInternal(VXY, VZoom);
  end;
end;

function TCoordConverterBasic.GetTileSplitCode: Integer;
begin
  Result := CTileSplitQuadrate256x256;
end;

function TCoordConverterBasic.GetTileSize(
  const XY: TPoint;
  const AZoom: byte
): TPoint;
begin
  Result := Point(256, 256);
end;

//------------------------------------------------------------------------------
procedure TCoordConverterBasic.ValidateZoomInternal(var AZoom: Byte);
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
end;

procedure TCoordConverterBasic.ValidateTilePosInternal(
  var XY: TPoint;
  var AZoom: byte
);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomInternal(AZoom);
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

procedure TCoordConverterBasic.ValidateTileRectInternal(
  var XY: TRect;
  var AZoom: byte
);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomInternal(AZoom);
  if XY.Left < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if XY.Left > VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + IntToStr(VTilesAtZoom));
      XY.Left := VTilesAtZoom;
    end;
  end;
  if XY.Top < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if XY.Top > VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + IntToStr(VTilesAtZoom));
      XY.Top := VTilesAtZoom;
    end;
  end;
  if XY.Right < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if XY.Right > VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + IntToStr(VTilesAtZoom));
      XY.Right := VTilesAtZoom;
    end;
  end;
  if XY.Bottom < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + IntToStr(VTilesAtZoom));
      XY.Bottom := VTilesAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateTilePosStrictInternal(
  var XY: TPoint;
  var AZoom: byte
);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomInternal(AZoom);
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

procedure TCoordConverterBasic.ValidatePixelPosInternal(
  var XY: TPoint;
  var AZoom: byte
);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomInternal(AZoom);

  if XY.X < 0 then begin
    if (AZoom < 23) or (XY.X <> VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела не может быть меньше нуля');
      XY.X := 0;
    end;
  end else begin
    if (AZoom < 23) and (XY.X > VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.X := VPixelsAtZoom;
    end;
  end;

  if XY.Y < 0 then begin
    if (AZoom < 23) or (XY.Y <> VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела не может быть меньше нуля');
      XY.Y := 0;
    end;
  end else begin
    if (AZoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.Y := VPixelsAtZoom;
    end;
  end;

end;

procedure TCoordConverterBasic.ValidatePixelRectInternal(
  var XY: TRect;
  var AZoom: byte
);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomInternal(AZoom);

  if XY.Left < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if (AZoom < 23) and (XY.Left > VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.Left := VPixelsAtZoom;
    end;
  end;

  if XY.Top < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if (AZoom < 23) and (XY.Top > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.Top := VPixelsAtZoom;
    end;
  end;

  if XY.Right < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if (AZoom < 23) and (XY.Right > VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.Right := VPixelsAtZoom;
    end;
  end;

  if XY.Bottom < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if (AZoom < 23) and (XY.Bottom > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + IntToStr(VPixelsAtZoom));
      XY.Bottom := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelPosStrictInternal(
  var XY: TPoint;
  var AZoom: byte
);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomInternal(AZoom);
  if XY.X < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if (AZoom < 23) and (XY.X >= VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше или равна ' + IntToStr(VPixelsAtZoom));
      XY.X := VPixelsAtZoom - 1;
    end;
  end;

  if XY.Y < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if (AZoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше или равна' + IntToStr(VPixelsAtZoom));
      XY.Y := VPixelsAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateRelativePosInternal(var XY: TDoublePoint);
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

procedure TCoordConverterBasic.ValidateRelativeRectInternal(var XY: TDoubleRect);
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

procedure TCoordConverterBasic.ValidateLonLatPosInternal(var XY: TDoublePoint);
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

procedure TCoordConverterBasic.ValidateLonLatRectInternal(var XY: TDoubleRect);
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

procedure TCoordConverterBasic.ValidateTilePosFloatInternal(
  var XY: TDoublePoint;
  var AZoom: byte
);
var
  VTilesAtZoom: Double;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + FloatToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  if XY.X < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if XY.X > VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + FloatToStr(VTilesAtZoom));
      XY.X := VTilesAtZoom;
    end;
  end;
  if XY.Y < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if XY.Y > VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + FloatToStr(VTilesAtZoom));
      XY.Y := VTilesAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelRectFloatInternal(
  var XY: TDoubleRect;
  var AZoom: byte
);
var
  VPixelsAtZoom: Double;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + FloatToStr(AZoom));
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);

  if XY.Left < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if (XY.Left > VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + FloatToStr(VPixelsAtZoom));
      XY.Left := VPixelsAtZoom;
    end;
  end;

  if XY.Top < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if (XY.Top > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + FloatToStr(VPixelsAtZoom));
      XY.Top := VPixelsAtZoom;
    end;
  end;

  if XY.Right < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if (XY.Right > VPixelsAtZoom) then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + FloatToStr(VPixelsAtZoom));
      XY.Right := VPixelsAtZoom;
    end;
  end;

  if XY.Bottom < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if (XY.Bottom > VPixelsAtZoom) then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + FloatToStr(VPixelsAtZoom));
      XY.Bottom := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelPosFloatInternal(
  var XY: TDoublePoint;
  var AZoom: byte
);
var
  VPixelsAtZoom: Double;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + IntToStr(AZoom));
    AZoom := 23;
  end;

  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  if XY.X < 0 then begin
    Assert(False, 'Координата X пиксела не может быть меньше нуля');
    XY.X := 0;
  end else begin
    if XY.X > VPixelsAtZoom then begin
      Assert(False, 'Координата X пиксела на этом зуме не может быть больше ' + FloatToStr(VPixelsAtZoom));
      XY.X := VPixelsAtZoom;
    end;
  end;

  if XY.Y < 0 then begin
    Assert(False, 'Координата Y пиксела не может быть меньше нуля');
    XY.Y := 0;
  end else begin
    if XY.Y > VPixelsAtZoom then begin
      Assert(False, 'Координата Y пиксела на этом зуме не может быть больше ' + FloatToStr(VPixelsAtZoom));
      XY.Y := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateTileRectFloatInternal(
  var XY: TDoubleRect;
  var AZoom: byte
);
var
  VTilesAtZoom: Double;
begin
  if AZoom > 23 then begin
    Assert(False, 'Слишком большой зум ' + FloatToStr(AZoom));
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  if XY.Left < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.Left := 0;
  end else begin
    if XY.Left > VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше или равна ' + FloatToStr(VTilesAtZoom));
      XY.Left := VTilesAtZoom;
    end;
  end;
  if XY.Top < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Top := 0;
  end else begin
    if XY.Top > VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + FloatToStr(VTilesAtZoom));
      XY.Top := VTilesAtZoom;
    end;
  end;
  if XY.Right < 0 then begin
    Assert(False, 'Координата X тайла не может быть меньше нуля');
    XY.Right := 0;
  end else begin
    if XY.Right > VTilesAtZoom then begin
      Assert(False, 'Координата X тайла на этом зуме не может быть больше ' + FloatToStr(VTilesAtZoom));
      XY.Right := VTilesAtZoom;
    end;
  end;
  if XY.Bottom < 0 then begin
    Assert(False, 'Координата Y тайла не может быть меньше нуля');
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > VTilesAtZoom then begin
      Assert(False, 'Координата Y тайла на этом зуме не может быть больше ' + FloatToStr(VTilesAtZoom));
      XY.Bottom := VTilesAtZoom;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TCoordConverterBasic.PixelsAtZoomInternal(AZoom: byte): Longint;
begin
  if AZoom < 23 then begin
    Result := 1 shl (AZoom + 8);
  end else begin
    Result := MaxInt;
  end;
end;

function TCoordConverterBasic.PixelsAtZoomFloatInternal(
  AZoom: byte): Double;
begin
  Result := 1 shl AZoom;
  Result := Result * 256;
end;

function TCoordConverterBasic.PixelRectAtZoomInternal(const AZoom: byte): TRect;
var
  VCnt: Longint;
begin
  VCnt := PixelsAtZoomInternal(AZoom);
  Result := Rect(0, 0, VCnt, VCnt);
end;

function TCoordConverterBasic.TilesAtZoomInternal(AZoom: byte): Longint;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterBasic.TilesAtZoomFloatInternal(
  AZoom: byte): Double;
begin
  Result := 1 shl AZoom;
end;

function TCoordConverterBasic.TileRectAtZoomInternal(const AZoom: byte): TRect;
var
  VCnt: Longint;
begin
  VCnt := TilesAtZoomInternal(AZoom);
  Result := Rect(0, 0, VCnt, VCnt);
end;

//------------------------------------------------------------------------------
// PixelPos
function TCoordConverterBasic.PixelPos2RelativeInternal(
  const XY: TPoint;
  AZoom: byte
): TDoublePoint;
var
  VPixelsAtZoom: Double;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  Result.X := XY.X / VPixelsAtZoom;
  Result.Y := XY.Y / VPixelsAtZoom;
end;

function TCoordConverterBasic.PixelPos2LonLatInternal(
  const XY: TPoint;
  AZoom: byte
): TDoublePoint;
begin
  Result := Relative2LonLatInternal(PixelPos2RelativeInternal(XY, AZoom));
end;

function TCoordConverterBasic.PixelPos2TilePosFloatInternal(
  const XY: TPoint;
  AZoom: byte
): TDoublePoint;
begin
  Result.X := XY.X / 256;
  Result.Y := XY.Y / 256;
end;

function TCoordConverterBasic.PixelPos2TilePosInternal(
  const XY: TPoint;
  AZoom: byte;
  ARounding: TPointRounding
): TPoint;
begin
  case ARounding of
    prClosest: begin
      Result.X := (XY.X + 127) shr 8;
      Result.Y := (XY.Y + 127) shr 8;
    end;
    prToTopLeft: begin
      Result.X := XY.X shr 8;
      Result.Y := XY.Y shr 8;
    end;
    prToBottomRight: begin
      Result.X := (XY.X + 255) shr 8;
      Result.Y := (XY.Y + 255) shr 8;
    end;
  end;
end;

//------------------------------------------------------------------------------
// PixelPosFloat
function TCoordConverterBasic.PixelPosFloat2LonLatInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
var
  VRelative: TDoublePoint;
begin
  VRelative := PixelPosFloat2RelativeInternal(XY, AZoom);
  Result := Relative2LonLatInternal(VRelative);
end;

function TCoordConverterBasic.PixelPosFloat2RelativeInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
var
  VPixelsAtZoom: Double;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  Result.X := XY.X / VPixelsAtZoom;
  Result.Y := XY.Y / VPixelsAtZoom;
end;

function TCoordConverterBasic.PixelPosFloat2TilePosFloatInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
begin
  Result.X := XY.X / 256;
  Result.Y := XY.Y / 256;
end;

//------------------------------------------------------------------------------
// PixelRect
function TCoordConverterBasic.PixelRect2TileRectFloatInternal(
  const XY: TRect;
  AZoom: byte
): TDoubleRect;
begin
  Result.Left := XY.Left / 256;
  Result.Top := XY.Top / 256;
  Result.Right := XY.Right / 256;
  Result.Bottom := XY.Bottom / 256;
end;

function TCoordConverterBasic.PixelRect2TileRectInternal(
  const XY: TRect;
  AZoom: byte
): TRect;
begin
  Result.Left := XY.Left shr 8;
  Result.Top := XY.Top shr 8;
  Result.Right := (XY.Right + 255) shr 8;
  Result.Bottom := (XY.Bottom + 255) shr 8;
end;

function TCoordConverterBasic.PixelRect2RelativeRectInternal(
  const XY: TRect;
  AZoom: byte
): TDoubleRect;
begin
  Result.TopLeft := PixelPos2RelativeInternal(XY.TopLeft, AZoom);
  Result.BottomRight := PixelPos2RelativeInternal(XY.BottomRight, AZoom);
end;

function TCoordConverterBasic.PixelRect2LonLatRectInternal(
  const XY: TRect;
  AZoom: byte
): TDoubleRect;
begin
  Result := RelativeRect2LonLatRectInternal(PixelRect2RelativeRectInternal(XY, AZoom));
end;

//------------------------------------------------------------------------------
// PixelRectFloat
function TCoordConverterBasic.PixelRectFloat2LonLatRectInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
var
  VRelativeRect: TDoubleRect;
begin
  VRelativeRect := PixelRectFloat2RelativeRectInternal(XY, AZoom);
  Result := RelativeRect2LonLatRect(VRelativeRect);
end;

function TCoordConverterBasic.PixelRectFloat2RelativeRectInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
var
  VPixelsAtZoom: Double;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  Result.Left := XY.Left / VPixelsAtZoom;
  Result.Top := XY.Top / VPixelsAtZoom;
  Result.Right := XY.Right / VPixelsAtZoom;
  Result.Bottom := XY.Bottom / VPixelsAtZoom;
end;

function TCoordConverterBasic.PixelRectFloat2TileRectFloatInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
begin
  Result.Left := XY.Left / 256;
  Result.Top := XY.Top / 256;
  Result.Right := XY.Right / 256;
  Result.Bottom := XY.Bottom / 256;
end;

//------------------------------------------------------------------------------
// TilePos
function TCoordConverterBasic.TilePos2LonLatInternal(
  const XY: TPoint;
  AZoom: byte
): TDoublePoint;
begin
  Result := Relative2LonLatInternal(TilePos2RelativeInternal(XY, AZoom));
end;

function TCoordConverterBasic.TilePos2PixelRectInternal(
  const XY: TPoint;
  AZoom: byte
): TRect;
begin
  Result.Left := XY.X shl 8;
  Result.Top := XY.Y shl 8;
  Result.Right := Result.Left + (1 shl 8);
  Result.Bottom := Result.Top + (1 shl 8);
  if AZoom >= 23 then begin
    if Result.Right < 0 then begin
      Result.Right := MaxInt;
    end;
    if Result.Bottom < 0 then begin
      Result.Bottom := MaxInt;
    end;
  end;
end;

function TCoordConverterBasic.TilePos2PixelRectFloatInternal(
  const XY: TPoint;
  AZoom: byte
): TDoubleRect;
begin
  Result.Left := XY.X shl 8;
  Result.Top := XY.Y shl 8;
  Result.Right := Result.Left + (1 shl 8);
  Result.Bottom := Result.Top + (1 shl 8);
end;

function TCoordConverterBasic.TilePos2LonLatRectInternal(
  const XY: TPoint;
  AZoom: byte
): TDoubleRect;
begin
  Result := RelativeRect2LonLatRectInternal(TilePos2RelativeRectInternal(XY, AZoom));
end;

function TCoordConverterBasic.TilePos2RelativeInternal(
  const XY: TPoint;
  AZoom: byte
): TDoublePoint;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  Result.X := XY.X / VTilesAtZoom;
  Result.Y := XY.Y / VTilesAtZoom;
end;

function TCoordConverterBasic.TilePos2RelativeRectInternal(
  const XY: TPoint;
  AZoom: byte
): TDoubleRect;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  Result.Left := XY.X / VTilesAtZoom;
  Result.Top := XY.Y / VTilesAtZoom;
  Result.Right := (XY.X + 1) / VTilesAtZoom;
  Result.Bottom := (XY.Y + 1) / VTilesAtZoom;
end;

function TCoordConverterBasic.TilePos2PixelPosInternal(
  const XY: TPoint;
  AZoom: byte
): TPoint;
begin
  Result.X := XY.X shl 8;
  Result.Y := XY.Y shl 8;
end;

//------------------------------------------------------------------------------
// TilePosFloat
function TCoordConverterBasic.TilePosFloat2LonLatInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
var
  VRelative: TDoublePoint;
begin
  VRelative := TilePosFloat2RelativeInternal(XY, AZoom);
  Result := Relative2LonLatInternal(VRelative);
end;

function TCoordConverterBasic.TilePosFloat2PixelPosFloatInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
begin
  Result.X := XY.X * 256;
  Result.Y := XY.Y * 256;
end;

function TCoordConverterBasic.TilePosFloat2RelativeInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  Result.X := XY.X / VTilesAtZoom;
  Result.Y := XY.Y / VTilesAtZoom;
end;

//------------------------------------------------------------------------------
// TileRect
function TCoordConverterBasic.TileRect2PixelRectInternal(
  const XY: TRect;
  AZoom: byte
): TRect;
begin
  Result.Left := XY.Left shl 8;
  Result.Top := XY.Top shl 8;
  Result.Right := XY.Right shl 8;
  Result.Bottom := XY.Bottom shl 8;
  if AZoom >= 23 then begin
    if Result.Right < 0 then begin
      Result.Right := MaxInt;
    end;
    if Result.Bottom < 0 then begin
      Result.Bottom := MaxInt;
    end;
  end;
end;

function TCoordConverterBasic.TileRect2LonLatRectInternal(
  const XY: TRect;
  AZoom: byte
): TDoubleRect;
begin
  Result := RelativeRect2LonLatRectInternal(TileRect2RelativeRectInternal(XY, AZoom));
end;

function TCoordConverterBasic.TileRect2RelativeRectInternal(
  const XY: TRect;
  AZoom: byte
): TDoubleRect;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  Result.Left := XY.Left / VTilesAtZoom;
  Result.Top := XY.Top / VTilesAtZoom;
  Result.Right := XY.Right / VTilesAtZoom;
  Result.Bottom := XY.Bottom / VTilesAtZoom;
end;

//------------------------------------------------------------------------------
// TileRectFloat
function TCoordConverterBasic.TileRectFloat2LonLatRectInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
var
  VRelativeRect: TDoubleRect;
begin
  VRelativeRect := TileRectFloat2RelativeRectInternal(XY, AZoom);
  Result := RelativeRect2LonLatRectInternal(VRelativeRect);
end;

function TCoordConverterBasic.TileRectFloat2PixelRectFloatInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
begin
  Result.Left := XY.Left * 256;
  Result.Top := XY.Top * 256;
  Result.Right := XY.Right * 256;
  Result.Bottom := XY.Bottom * 256;
end;

function TCoordConverterBasic.TileRectFloat2RelativeRectInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  Result.Left := XY.Left / VTilesAtZoom;
  Result.Top := XY.Top / VTilesAtZoom;
  Result.Right := XY.Right / VTilesAtZoom;
  Result.Bottom := XY.Bottom / VTilesAtZoom;
end;

//------------------------------------------------------------------------------
// RelativePos
function TCoordConverterBasic.Relative2PixelPosFloatInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
var
  VPixelsAtZoom: Double;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  Result.X := XY.X * VPixelsAtZoom;
  Result.Y := XY.Y * VPixelsAtZoom;
end;

function TCoordConverterBasic.Relative2TilePosFloatInternal(
  const XY: TDoublePoint;
  AZoom: byte
): TDoublePoint;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);
  Result.X := XY.X * VTilesAtZoom;
  Result.Y := XY.Y * VTilesAtZoom;
end;

//------------------------------------------------------------------------------
// RelativeRect
function TCoordConverterBasic.RelativeRect2LonLatRectInternal(
  const XY: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := Relative2LonLatInternal(XY.TopLeft);
  Result.BottomRight := Relative2LonLatInternal(XY.BottomRight);
end;

function TCoordConverterBasic.RelativeRect2PixelRectFloatInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
var
  VPixelsAtZoom: Double;
begin
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);

  Result.Left := XY.Left * VPixelsAtZoom;
  Result.Top := XY.Top * VPixelsAtZoom;

  Result.Right := XY.Right * VPixelsAtZoom;
  Result.Bottom := XY.Bottom * VPixelsAtZoom;
end;

function TCoordConverterBasic.RelativeRect2TileRectFloatInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
var
  VTilesAtZoom: Double;
begin
  VTilesAtZoom := TilesAtZoomFloatInternal(AZoom);

  Result.Left := XY.Left * VTilesAtZoom;
  Result.Top := XY.Top * VTilesAtZoom;

  Result.Right := XY.Right * VTilesAtZoom;
  Result.Bottom := XY.Bottom * VTilesAtZoom;
end;

//------------------------------------------------------------------------------
// LonLatPos
function TCoordConverterBasic.LonLat2MetrInternal(
  const ALl: TDoublePoint
): TDoublePoint;
var
  VRelative: TDoublePoint;
  VMul: Double;
begin
  VRelative := LonLat2RelativeInternal(ALl);
  VMul := 2 * Pi * GetDatum.GetSpheroidRadiusA;
  Result.X := (VRelative.X - 0.5) * VMul;
  Result.Y := (0.5 - VRelative.Y) * VMul;
end;

function TCoordConverterBasic.LonLat2PixelPosFloatInternal(
  const ALonLat: TDoublePoint;
  AZoom: byte
): TDoublePoint;
begin
  Result := Relative2PixelPosFloatInternal(LonLat2RelativeInternal(ALonLat), AZoom);
end;

function TCoordConverterBasic.LonLat2TilePosFloatInternal(
  const ALonLat: TDoublePoint;
  AZoom: byte
): TDoublePoint;
begin
  Result := Relative2TilePosFloatInternal(LonLat2RelativeInternal(ALonLat), AZoom);
end;

//------------------------------------------------------------------------------
// LonLatRect
function TCoordConverterBasic.LonLatRect2RelativeRectInternal(
  const XY: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := LonLat2RelativeInternal(XY.TopLeft);
  Result.BottomRight := LonLat2RelativeInternal(XY.BottomRight);
end;

function TCoordConverterBasic.LonLatRect2PixelRectFloatInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
begin
  Result :=
    RelativeRect2PixelRectFloatInternal(
      LonLatRect2RelativeRectInternal(XY),
      AZoom
    );
end;

function TCoordConverterBasic.LonLatRect2TileRectFloatInternal(
  const XY: TDoubleRect;
  AZoom: byte
): TDoubleRect;
begin
  Result :=
    RelativeRect2TileRectFloatInternal(
      LonLatRect2RelativeRectInternal(XY),
      AZoom
    );
end;

function TCoordConverterBasic.Metr2LonLatInternal(
  const AMm: TDoublePoint
): TDoublePoint;
var
  VRelative: TDoublePoint;
  VMul: Double;
begin
  VMul := 1.0 / (2 * Pi * GetDatum.GetSpheroidRadiusA);
  VRelative.X := AMm.X * VMul - 0.5;
  VRelative.Y := 0.5 - AMm.Y * VMul;
  Result := Relative2LonLatInternal(VRelative);
end;

//------------------------------------------------------------------------------
procedure TCoordConverterBasic.ValidateZoom(var AZoom: Byte);
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
end;

procedure TCoordConverterBasic.ValidateTilePos(
  var XY: TPoint;
  var AZoom: byte;
  ACicleMap: Boolean
);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(AZoom);

  if XY.X < 0 then begin
    if ACicleMap then begin
      XY.X := XY.X mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if XY.X > VTilesAtZoom then begin
      if ACicleMap then begin
        XY.X := XY.X mod VTilesAtZoom;
      end else begin
        XY.X := VTilesAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if XY.Y > VTilesAtZoom then begin
      XY.Y := VTilesAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateTileRect(
  var XY: TRect;
  var AZoom: byte
);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(AZoom);

  if XY.Left < 0 then begin
    XY.Left := 0;
  end else begin
    if XY.Left > VTilesAtZoom then begin
      XY.Left := VTilesAtZoom;
    end;
  end;

  if XY.Top < 0 then begin
    XY.Top := 0;
  end else begin
    if XY.Top > VTilesAtZoom then begin
      XY.Top := VTilesAtZoom;
    end;
  end;

  if XY.Right < 0 then begin
    XY.Right := 0;
  end else begin
    if XY.Right > VTilesAtZoom then begin
      XY.Right := VTilesAtZoom;
    end;
  end;

  if XY.Bottom < 0 then begin
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > VTilesAtZoom then begin
      XY.Bottom := VTilesAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateTilePosStrict(
  var XY: TPoint;
  var AZoom: byte;
  ACicleMap: Boolean
);
var
  VTilesAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VTilesAtZoom := TilesAtZoom(AZoom);

  if XY.X < 0 then begin
    if ACicleMap then begin
      XY.X := XY.X mod VTilesAtZoom + VTilesAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if XY.X >= VTilesAtZoom then begin
      if ACicleMap then begin
        XY.X := XY.X mod VTilesAtZoom;
      end else begin
        XY.X := VTilesAtZoom - 1;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if XY.Y >= VTilesAtZoom then begin
      XY.Y := VTilesAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelPos(
  var XY: TPoint;
  var AZoom: byte;
  ACicleMap: Boolean
);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(AZoom);

  if XY.X < 0 then begin
    if (AZoom < 23) then begin
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
    if (AZoom < 23) and (XY.X > VPixelsAtZoom) then begin
      if ACicleMap then begin
        XY.X := XY.X mod VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    if (AZoom < 23) or (XY.Y <> VPixelsAtZoom) then begin
      XY.Y := 0;
    end;
  end else begin
    if (AZoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      XY.Y := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelPosFloat(
  var XY: TDoublePoint;
  var AZoom: byte;
  ACicleMap: Boolean
);
var
  VPixelsAtZoom: Double;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;

  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);

  if XY.X < 0 then begin
    if ACicleMap then begin
      XY.X := XY.X - Int(XY.X / VPixelsAtZoom) * VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if (XY.X > VPixelsAtZoom) then begin
      if ACicleMap then begin
        XY.X := XY.X - Int(XY.X / VPixelsAtZoom) * VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if (XY.Y > VPixelsAtZoom) then begin
      XY.Y := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelRect(
  var XY: TRect;
  var AZoom: byte
);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(AZoom);

  if XY.Left < 0 then begin
    if AZoom < 23 then begin
      XY.Left := 0;
    end else begin
      if XY.Left < -(MaxInt div 2) then begin
        XY.Left := VPixelsAtZoom;
      end else begin
        XY.Left := 0;
      end;
    end;
  end else begin
    if (AZoom < 23) and (XY.Left > VPixelsAtZoom) then begin
      XY.Left := VPixelsAtZoom;
    end;
  end;

  if XY.Top < 0 then begin
    if AZoom < 23 then begin
      XY.Top := 0;
    end else begin
      if XY.Top < -(MaxInt div 2) then begin
        XY.Top := VPixelsAtZoom;
      end else begin
        XY.Top := 0;
      end;
    end;
  end else begin
    if (AZoom < 23) and (XY.Top > VPixelsAtZoom) then begin
      XY.Top := VPixelsAtZoom;
    end;
  end;

  if XY.Right < 0 then begin
    if AZoom < 23 then begin
      XY.Right := 0;
    end else begin
      if XY.Right < -(MaxInt div 2) then begin
        XY.Right := VPixelsAtZoom;
      end else begin
        XY.Right := 0;
      end;
    end;
  end else begin
    if (AZoom < 23) and (XY.Right > VPixelsAtZoom) then begin
      XY.Right := VPixelsAtZoom;
    end;
  end;

  if XY.Bottom < 0 then begin
    if AZoom < 23 then begin
      XY.Bottom := 0;
    end else begin
      if XY.Bottom < -(MaxInt div 2) then begin
        XY.Bottom := VPixelsAtZoom;
      end else begin
        XY.Bottom := 0;
      end;
    end;
  end else begin
    if (AZoom < 23) and (XY.Bottom > VPixelsAtZoom) then begin
      XY.Bottom := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelRectFloat(
  var XY: TDoubleRect;
  var AZoom: byte
);
var
  VPixelsAtZoom: Double;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);

  if XY.Left < 0 then begin
    XY.Left := 0;
  end else begin
    if XY.Left > VPixelsAtZoom then begin
      XY.Left := VPixelsAtZoom;
    end;
  end;

  if XY.Top < 0 then begin
    XY.Top := 0;
  end else begin
    if XY.Top > VPixelsAtZoom then begin
      XY.Top := VPixelsAtZoom;
    end;
  end;

  if XY.Right < 0 then begin
    XY.Right := 0;
  end else begin
    if XY.Right > VPixelsAtZoom then begin
      XY.Right := VPixelsAtZoom;
    end;
  end;

  if XY.Bottom < 0 then begin
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > VPixelsAtZoom then begin
      XY.Bottom := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelPosStrict(
  var XY: TPoint;
  var AZoom: byte;
  ACicleMap: Boolean
);
var
  VPixelsAtZoom: Integer;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoom(AZoom);
  if XY.X < 0 then begin
    if ACicleMap then begin
      XY.X := XY.X mod VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if (AZoom < 23) and (XY.X >= VPixelsAtZoom) then begin
      if ACicleMap then begin
        XY.X := XY.X mod VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom - 1;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if (AZoom < 23) and (XY.Y >= VPixelsAtZoom) then begin
      XY.Y := VPixelsAtZoom - 1;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidatePixelPosFloatStrict(
  var XY: TDoublePoint;
  var AZoom: byte;
  ACicleMap: Boolean
);
var
  VPixelsAtZoom: Double;
begin
  if AZoom > 23 then begin
    AZoom := 23;
  end;
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  if XY.X < 0 then begin
    if ACicleMap then begin
      XY.X := XY.X - Int(XY.X / VPixelsAtZoom) * VPixelsAtZoom + VPixelsAtZoom;
    end else begin
      XY.X := 0;
    end;
  end else begin
    if (XY.X >= VPixelsAtZoom) then begin
      if ACicleMap then begin
        XY.X := XY.X - Int(XY.X / VPixelsAtZoom) * VPixelsAtZoom;
      end else begin
        XY.X := VPixelsAtZoom;
      end;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if (XY.Y >= VPixelsAtZoom) then begin
      XY.Y := VPixelsAtZoom;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateRelativePos(var XY: TDoublePoint);
begin
  if XY.X < 0 then begin
    XY.X := 0;
  end else begin
    if XY.X > 1 then begin
      XY.X := 1;
    end;
  end;

  if XY.Y < 0 then begin
    XY.Y := 0;
  end else begin
    if XY.Y > 1 then begin
      XY.Y := 1;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateRelativeRect(var XY: TDoubleRect);
begin
  if XY.Left < 0 then begin
    XY.Left := 0;
  end else begin
    if XY.Left > 1 then begin
      XY.Left := 1;
    end;
  end;

  if XY.Top < 0 then begin
    XY.Top := 0;
  end else begin
    if XY.Top > 1 then begin
      XY.Top := 1;
    end;
  end;

  if XY.Right < 0 then begin
    XY.Right := 0;
  end else begin
    if XY.Right > 1 then begin
      XY.Right := 1;
    end;
  end;

  if XY.Bottom < 0 then begin
    XY.Bottom := 0;
  end else begin
    if XY.Bottom > 1 then begin
      XY.Bottom := 1;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateLonLatPos(var XY: TDoublePoint);
begin
  if XY.X < FValidLonLatRect.Left then begin
    XY.X := FValidLonLatRect.Left;
  end else begin
    if XY.X > FValidLonLatRect.Right then begin
      XY.X := FValidLonLatRect.Right;
    end;
  end;
  if XY.Y < FValidLonLatRect.Bottom then begin
    XY.Y := FValidLonLatRect.Bottom;
  end else begin
    if XY.Y > FValidLonLatRect.Top then begin
      XY.Y := FValidLonLatRect.Top;
    end;
  end;
end;

procedure TCoordConverterBasic.ValidateLonLatRect(var XY: TDoubleRect);
begin
  if XY.Left < FValidLonLatRect.Left then begin
    XY.Left := FValidLonLatRect.Left;
  end else begin
    if XY.Left > FValidLonLatRect.Right then begin
      XY.Left := FValidLonLatRect.Right;
    end;
  end;
  if XY.Bottom < FValidLonLatRect.Bottom then begin
    XY.Bottom := FValidLonLatRect.Bottom;
  end else begin
    if XY.Bottom > FValidLonLatRect.Top then begin
      XY.Bottom := FValidLonLatRect.Top;
    end;
  end;

  if XY.Right < FValidLonLatRect.Left then begin
    XY.Right := FValidLonLatRect.Left;
  end else begin
    if XY.Right > FValidLonLatRect.Right then begin
      XY.Right := FValidLonLatRect.Right;
    end;
  end;
  if XY.Top < FValidLonLatRect.Bottom then begin
    XY.Top := FValidLonLatRect.Bottom;
  end else begin
    if XY.Top > FValidLonLatRect.Top then begin
      XY.Top := FValidLonLatRect.Top;
    end;
  end;
end;

//------------------------------------------------------------------------------
function TCoordConverterBasic.CheckZoom(const AZoom: Byte): boolean;
begin
  Result := AZoom <= 23;
end;

function TCoordConverterBasic.CheckTilePos(
  const XY: TPoint;
  const AZoom: byte
): boolean;
var
  VTilesAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VTilesAtZoom := TilesAtZoom(AZoom);

  if XY.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.X > VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Y > VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckTileRect(
  const XY: TRect;
  const AZoom: byte
): boolean;
var
  VTilesAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VTilesAtZoom := TilesAtZoom(AZoom);

  if XY.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Left > VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Top > VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Right > VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Bottom > VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckTilePosStrict(
  const XY: TPoint;
  const AZoom: byte
): boolean;
var
  VTilesAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VTilesAtZoom := TilesAtZoom(AZoom);

  if XY.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.X >= VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Y >= VTilesAtZoom then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelPos(
  const XY: TPoint;
  const AZoom: byte
): boolean;
var
  VPixelsAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VPixelsAtZoom := PixelsAtZoom(AZoom);

  if XY.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.X > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.Y > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelPosFloat(
  const XY: TDoublePoint;
  const AZoom: byte
): boolean;
var
  VPixelsAtZoom: Double;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;

  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);

  if XY.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (XY.X > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (XY.Y > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelRect(
  const XY: TRect;
  const AZoom: byte
): boolean;
var
  VPixelsAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VPixelsAtZoom := PixelsAtZoom(AZoom);

  if XY.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.Left > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.Top > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.Right > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.Bottom > VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelRectFloat(
  const XY: TDoubleRect;
  const AZoom: byte
): boolean;
var
  VPixelsAtZoom: Double;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);

  if XY.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Left > VPixelsAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Top > VPixelsAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Right > VPixelsAtZoom then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Bottom > VPixelsAtZoom then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelPosStrict(
  const XY: TPoint;
  const AZoom: byte
): boolean;
var
  VPixelsAtZoom: Integer;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VPixelsAtZoom := PixelsAtZoom(AZoom);
  if XY.X < 0 then begin
    Result := False;
      Exit;
  end else begin
    if (AZoom < 23) and (XY.X >= VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (AZoom < 23) and (XY.Y >= VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckPixelPosFloatStrict(
  const XY: TDoublePoint;
  const AZoom: byte
): boolean;
var
  VPixelsAtZoom: Double;
begin
  Result := True;
  if AZoom > 23 then begin
    Result := False;
    Exit;
  end;
  VPixelsAtZoom := PixelsAtZoomFloatInternal(AZoom);
  if XY.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (XY.X >= VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if (XY.Y >= VPixelsAtZoom) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckRelativePos(const XY: TDoublePoint): boolean;
begin
  Result := True;
  if XY.X < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.X > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Y < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Y > 1 then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckRelativeRect(const XY: TDoubleRect): boolean;
begin
  Result := True;
  if XY.Left < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Left > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Top < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Top > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Right < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Right > 1 then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Bottom < 0 then begin
    Result := False;
    Exit;
  end else begin
    if XY.Bottom > 1 then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckLonLatPos(const XY: TDoublePoint): boolean;
begin
  Result := True;
  if XY.X < FValidLonLatRect.Left then begin
    Result := False;
    Exit;
  end else begin
    if XY.X > FValidLonLatRect.Right then begin
      Result := False;
      Exit;
    end;
  end;
  if XY.Y < FValidLonLatRect.Bottom then begin
    Result := False;
    Exit;
  end else begin
    if XY.Y > FValidLonLatRect.Top then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TCoordConverterBasic.CheckLonLatRect(const XY: TDoubleRect): boolean;
begin
  Result := True;
  if XY.Left < FValidLonLatRect.Left then begin
    Result := False;
    Exit;
  end else begin
    if XY.Left > FValidLonLatRect.Right then begin
      Result := False;
      Exit;
    end;
  end;
  if XY.Bottom < FValidLonLatRect.Bottom then begin
    Result := False;
    Exit;
  end else begin
    if XY.Bottom > FValidLonLatRect.Top then begin
      Result := False;
      Exit;
    end;
  end;

  if XY.Right < FValidLonLatRect.Left then begin
    Result := False;
    Exit;
  end else begin
    if XY.Right > FValidLonLatRect.Right then begin
      Result := False;
      Exit;
    end;
  end;
  if XY.Top < FValidLonLatRect.Bottom then begin
    Result := False;
    Exit;
  end else begin
    if XY.Top > FValidLonLatRect.Top then begin
      Result := False;
      Exit;
    end;
  end;
end;

end.
