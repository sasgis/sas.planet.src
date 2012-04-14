{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_CoordConverterAbstract;

interface

uses
  Types,
  t_GeoTypes,
  i_Datum,
  i_CoordConverter;

type
  TCoordConverterAbstract = class(TInterfacedObject, ICoordConverter)
  private
    FDatum: IDatum;
    FProjEPSG: integer;
    FCellSizeUnits: TCellSizeUnits;
  protected
    procedure CheckZoomInternal(var AZoom: Byte); virtual; stdcall; abstract;

    procedure CheckPixelPosInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelPosStrictInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelPosFloatInternal(var XY: TDoublePoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelRectInternal(var XY: TRect; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckPixelRectFloatInternal(var XY: TDoubleRect; var Azoom: byte); virtual; stdcall; abstract;

    procedure CheckTilePosInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTilePosStrictInternal(var XY: TPoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTilePosFloatInternal(var XY: TDoublePoint; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTileRectInternal(var XY: TRect; var Azoom: byte); virtual; stdcall; abstract;
    procedure CheckTileRectFloatInternal(var XY: TDoubleRect; var Azoom: byte); virtual; stdcall; abstract;

    procedure CheckRelativePosInternal(var XY: TDoublePoint); virtual; stdcall; abstract;
    procedure CheckRelativeRectInternal(var XY: TDoubleRect); virtual; stdcall; abstract;

    procedure CheckLonLatPosInternal(var XY: TDoublePoint); virtual; stdcall; abstract;
    procedure CheckLonLatRectInternal(var XY: TDoubleRect); virtual; stdcall; abstract;

    function LonLat2MetrInternal(const Ll: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;
    function Metr2LonLatInternal(const Mm: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;

    function TileRectAtZoomInternal(const AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRectAtZoomInternal(const AZoom: byte): TRect; virtual; stdcall; abstract;

    function TilesAtZoomInternal(AZoom: byte): Longint; virtual; stdcall; abstract;
    function TilesAtZoomFloatInternal(AZoom: byte): Double; virtual; stdcall; abstract;
    function PixelsAtZoomInternal(AZoom: byte): Longint; virtual; stdcall; abstract;
    function PixelsAtZoomFloatInternal(AZoom: byte): Double; virtual; stdcall; abstract;


    function PixelPos2TilePosInternal(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function PixelPos2TilePosFloatInternal(const XY: TPoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function PixelPos2RelativeInternal(const XY: TPoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function PixelPos2LonLatInternal(const XY: TPoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;

    function PixelPosFloat2TilePosInternal(const XY: TDoublePoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function PixelPosFloat2TilePosFloatInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function PixelPosFloat2RelativeInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function PixelPosFloat2LonLatInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;

    function PixelRect2TileRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRect2TileRectFloatInternal(const XY: TRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function PixelRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function PixelRect2LonLatRectInternal(const XY: TRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;

    function PixelRectFloat2TileRectInternal(const XY: TDoubleRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRectFloat2TileRectFloatInternal(const XY: TDoubleRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function PixelRectFloat2RelativeRectInternal(const XY: TDoubleRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function PixelRectFloat2LonLatRectInternal(const XY: TDoubleRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;

    function TilePos2PixelPosInternal(const XY: TPoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function TilePos2PixelRectInternal(const XY: TPoint; Azoom: byte): TRect; virtual; stdcall; abstract;
    function TilePos2PixelRectFloatInternal(const XY: TPoint; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function TilePos2LonLatRectInternal(const XY: TPoint; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function TilePos2LonLatInternal(const XY: TPoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function TilePos2RelativeInternal(const XY: TPoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function TilePos2RelativeRectInternal(const XY: TPoint; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;

    function TilePosFloat2PixelPosInternal(const XY: TDoublePoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function TilePosFloat2PixelPosFloatInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function TilePosFloat2RelativeInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function TilePosFloat2LonLatInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;

    function TileRect2PixelRectInternal(const XY: TRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function TileRect2RelativeRectInternal(const XY: TRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function TileRect2LonLatRectInternal(const XY: TRect; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;

    function TileRectFloat2PixelRectInternal(const XY: TDoubleRect; AZoom: byte): TRect; virtual; stdcall; abstract;
    function TileRectFloat2PixelRectFloatInternal(const XY: TDoubleRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function TileRectFloat2RelativeRectInternal(const XY: TDoubleRect; AZoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function TileRectFloat2LonLatRectInternal(const XY: TDoubleRect; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;

    function Relative2PixelPosInternal(const XY: TDoublePoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function Relative2PixelPosFloatInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function Relative2TilePosInternal(const XY: TDoublePoint; Azoom: byte): TPoint; virtual; stdcall; abstract;
    function Relative2TilePosFloatInternal(const XY: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;

    function RelativeRect2LonLatRectInternal(const XY: TDoubleRect): TDoubleRect; virtual; stdcall; abstract;
    function RelativeRect2TileRectInternal(const XY: TDoubleRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function RelativeRect2TileRectFloatInternal(const XY: TDoubleRect; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function RelativeRect2PixelRectInternal(const XY: TDoubleRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function RelativeRect2PixelRectFloatInternal(const XY: TDoubleRect; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;


    function LonLat2PixelPosInternal(const Ll: TDoublePoint; Azoom: byte): Tpoint; virtual; stdcall; abstract;
    function LonLat2PixelPosFloatInternal(const Ll: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function LonLat2TilePosInternal(const Ll: TDoublePoint; Azoom: byte): Tpoint; virtual; stdcall; abstract;
    function LonLat2TilePosFloatInternal(const Ll: TDoublePoint; Azoom: byte): TDoublePoint; virtual; stdcall; abstract;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;

    function LonLatRect2RelativeRectInternal(const XY: TDoubleRect): TDoubleRect; virtual; stdcall; abstract;
    function LonLatRect2PixelRectInternal(const XY: TDoubleRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function LonLatRect2PixelRectFloatInternal(const XY: TDoubleRect; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;
    function LonLatRect2TileRectInternal(const XY: TDoubleRect; Azoom: byte): TRect; virtual; stdcall; abstract;
    function LonLatRect2TileRectFloatInternal(const XY: TDoubleRect; Azoom: byte): TDoubleRect; virtual; stdcall; abstract;
  protected
    property Datum: IDatum read FDatum;
    function TilesAtZoom(const AZoom: byte): Longint; stdcall;
    function PixelsAtZoom(const AZoom: byte): Longint; stdcall;
  protected
    function GetDatum: IDatum; stdcall;

    function GetMinZoom: Byte; stdcall;
    function GetMaxZoom: Byte; stdcall;

    function TileRectAtZoom(const AZoom: byte): TRect; stdcall;
    function PixelRectAtZoom(const AZoom: byte): TRect; stdcall;

    function TilesAtZoomFloat(const AZoom: byte): Double; stdcall;
    function PixelsAtZoomFloat(const AZoom: byte): Double; stdcall;


    function PixelPos2LonLat(const AXY: TPoint; const Azoom: byte): TDoublePoint; stdcall;
    function PixelPos2TilePos(const AXY: TPoint; const Azoom: byte): TPoint;  stdcall;
    function PixelPos2Relative(const AXY: TPoint; const Azoom: byte): TDoublePoint; stdcall;
    function PixelPos2TilePosFloat(const XY: TPoint; const Azoom: byte): TDoublePoint; stdcall;

    function PixelPosFloat2TilePos(const XY: TDoublePoint; const Azoom: byte): TPoint; stdcall;
    function PixelPosFloat2TilePosFloat(const XY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function PixelPosFloat2Relative(const XY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function PixelPosFloat2LonLat(const XY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;

    function PixelRect2TileRect(const AXY: TRect; const AZoom: byte): TRect; stdcall;
    function PixelRect2RelativeRect(const AXY: TRect; const AZoom: byte): TDoubleRect; stdcall;
    function PixelRect2LonLatRect(const AXY: TRect; const AZoom: byte): TDoubleRect; stdcall;
    function PixelRect2TileRectFloat(const XY: TRect; const AZoom: byte): TDoubleRect; stdcall;

    function PixelRectFloat2TileRect(const XY: TDoubleRect; const AZoom: byte): TRect; stdcall;
    function PixelRectFloat2TileRectFloat(const XY: TDoubleRect; const AZoom: byte): TDoubleRect; stdcall;
    function PixelRectFloat2RelativeRect(const XY: TDoubleRect; const AZoom: byte): TDoubleRect; stdcall;
    function PixelRectFloat2LonLatRect(const XY: TDoubleRect; const AZoom: byte): TDoubleRect; stdcall;

    function TilePos2PixelPos(const AXY: TPoint; const Azoom: byte): TPoint; stdcall;
    function TilePos2PixelRect(const AXY: TPoint; const Azoom: byte): TRect; stdcall;
    function TilePos2PixelRectFloat(const AXY: TPoint; const Azoom: byte): TDoubleRect; stdcall;
    function TilePos2LonLatRect(const AXY: TPoint; const Azoom: byte): TDoubleRect; stdcall;
    function TilePos2LonLat(const AXY: TPoint; const Azoom: byte): TDoublePoint; stdcall;
    function TilePos2Relative(const AXY: TPoint; const Azoom: byte): TDoublePoint; stdcall;
    function TilePos2RelativeRect(const AXY: TPoint; const Azoom: byte): TDoubleRect; stdcall;

    function TilePosFloat2PixelPos(const XY: TDoublePoint; const Azoom: byte): TPoint; stdcall;
    function TilePosFloat2PixelPosFloat(const XY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function TilePosFloat2Relative(const XY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function TilePosFloat2LonLat(const XY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;

    function TileRect2PixelRect(const AXY: TRect; const AZoom: byte): TRect; stdcall;
    function TileRect2RelativeRect(const AXY: TRect; const AZoom: byte): TDoubleRect; stdcall;
    function TileRect2LonLatRect(const AXY: TRect; const Azoom: byte): TDoubleRect; stdcall;

    function TileRectFloat2PixelRect(const XY: TDoubleRect; const AZoom: byte): TRect; stdcall;
    function TileRectFloat2PixelRectFloat(const XY: TDoubleRect; const AZoom: byte): TDoubleRect; stdcall;
    function TileRectFloat2RelativeRect(const XY: TDoubleRect; const AZoom: byte): TDoubleRect; stdcall;
    function TileRectFloat2LonLatRect(const XY: TDoubleRect; const Azoom: byte): TDoubleRect; stdcall;

    function LonLat2PixelPos(const AXY: TDoublePoint; const Azoom: byte): Tpoint; stdcall;
    function LonLat2PixelPosFloat(const AXY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function LonLat2TilePos(const AXY: TDoublePoint; const Azoom: byte): Tpoint; stdcall;
    function LonLat2TilePosFloat(const AXY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function LonLat2Relative(const AXY: TDoublePoint): TDoublePoint; stdcall;

    function LonLatRect2RelativeRect(const AXY: TDoubleRect): TDoubleRect; stdcall;
    function LonLatRect2PixelRect(const AXY: TDoubleRect; const Azoom: byte): TRect; stdcall;
    function LonLatRect2PixelRectFloat(const XY: TDoubleRect; const Azoom: byte): TDoubleRect; stdcall;
    function LonLatRect2TileRect(const AXY: TDoubleRect; const Azoom: byte): TRect; stdcall;
    function LonLatRect2TileRectFloat(const XY: TDoubleRect; const Azoom: byte): TDoubleRect; stdcall;

    function Relative2PixelPos(const AXY: TDoublePoint; const Azoom: byte): TPoint; stdcall;
    function Relative2PixelPosFloat(const AXY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function Relative2TilePos(const AXY: TDoublePoint; const Azoom: byte): TPoint; stdcall;
    function Relative2TilePosFloat(const AXY: TDoublePoint; const Azoom: byte): TDoublePoint; stdcall;
    function Relative2LonLat(const AXY: TDoublePoint): TDoublePoint; stdcall;

    function RelativeRect2LonLatRect(const AXY: TDoubleRect): TDoubleRect; stdcall;
    function RelativeRect2TileRect(const AXY: TDoubleRect; const Azoom: byte): TRect; stdcall;
    function RelativeRect2TileRectFloat(const AXY: TDoubleRect; const Azoom: byte): TDoubleRect; stdcall;
    function RelativeRect2PixelRect(const AXY: TDoubleRect; const Azoom: byte): TRect; stdcall;
    function RelativeRect2PixelRectFloat(const AXY: TDoubleRect; const Azoom: byte): TDoubleRect; stdcall;
  protected
    function GetTileSize(const XY: TPoint; const Azoom: byte): TPoint; virtual; stdcall; abstract;

    function CheckZoom(var AZoom: Byte): boolean; virtual; stdcall; abstract;
    function CheckTilePos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckTilePosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckTileRect(var XY: TRect; var Azoom: byte): boolean; virtual; stdcall; abstract;

    function CheckPixelPos(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckPixelPosFloat(var XY: TDoublePoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckPixelPosStrict(var XY: TPoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckPixelPosFloatStrict(var XY: TDoublePoint; var Azoom: byte; ACicleMap: Boolean): boolean; virtual; stdcall; abstract;
    function CheckPixelRect(var XY: TRect; var Azoom: byte): boolean; virtual; stdcall; abstract;
    function CheckPixelRectFloat(var XY: TDoubleRect; var Azoom: byte): boolean; virtual; stdcall; abstract;

    function CheckRelativePos(var XY: TDoublePoint): boolean; virtual; stdcall; abstract;
    function CheckRelativeRect(var XY: TDoubleRect): boolean; virtual; stdcall; abstract;

    function CheckLonLatPos(var XY: TDoublePoint): boolean; virtual; stdcall; abstract;
    function CheckLonLatRect(var XY: TDoubleRect): boolean; virtual; stdcall; abstract;
    function GetTileSplitCode: Integer; virtual; stdcall; abstract;
    function LonLat2Metr(const AXY: TDoublePoint): TDoublePoint; virtual; stdcall;
    function Metr2LonLat(const AXY: TDoublePoint): TDoublePoint; virtual; stdcall;
  private
    function GetProjectionEPSG: Integer; stdcall;
    function GetCellSizeUnits: TCellSizeUnits; stdcall;
    function IsSameConverter(const AOtherMapCoordConv: ICoordConverter): Boolean; stdcall;

  public
    constructor Create(
      const ADatum: IDatum;
      AProjEPSG: integer;
      ACellSizeUnits: TCellSizeUnits
    );
  end;

implementation

{ TCoordConverterAbstract }

//------------------------------------------------------------------------------
function TCoordConverterAbstract.TilePos2PixelRect(const AXY: TPoint;
  const Azoom: byte): TRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2PixelRectFloat(const AXY: TPoint;
  const Azoom: byte): TDoubleRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2PixelRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePos2LonLatRect(const AXY: TPoint;
  const Azoom: byte): TDoubleRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelsAtZoom(const AZoom: byte): Longint;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := PixelsAtZoomInternal(Vzoom);
end;

function TCoordConverterAbstract.PixelsAtZoomFloat(const AZoom: byte): Double;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := PixelsAtZoomFloatInternal(Vzoom);
end;

function TCoordConverterAbstract.PixelRectAtZoom(const AZoom: byte): TRect;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := PixelRectAtZoomInternal(Vzoom);
end;

function TCoordConverterAbstract.TilesAtZoom(const AZoom: byte): Longint;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := TilesAtZoomInternal(Vzoom);
end;

function TCoordConverterAbstract.TilesAtZoomFloat(const AZoom: byte): Double;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := TilesAtZoomFloatInternal(Vzoom);
end;

function TCoordConverterAbstract.TileRectAtZoom(const AZoom: byte): TRect;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  CheckZoomInternal(VZoom);
  Result := TileRectAtZoomInternal(Vzoom);
end;

function TCoordConverterAbstract.PixelPos2Relative(const AXY: TPoint;
  const Azoom: byte): TDoublePoint;
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
  const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  CheckRelativePosInternal(VXY);
  Result := Relative2LonLatInternal(VXY);
end;

function TCoordConverterAbstract.Relative2PixelPos(const AXY: TDoublePoint;
  const Azoom: byte): TPoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.Relative2PixelPosFloat(
  const AXY: TDoublePoint; const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2PixelPosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2Metr(
  const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  CheckLonLatPosInternal(VXY);
  Result := LonLat2MetrInternal(VXY);
end;

function TCoordConverterAbstract.LonLat2PixelPos(const AXY: TDoublePoint;
  const Azoom: byte): Tpoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  Result := LonLat2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2PixelPosFloat(const AXY: TDoublePoint;
  const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLat2PixelPosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPos2LonLat(const AXY: TPoint;
  const Azoom: byte): TDoublePoint;
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
  const Azoom: byte): TDoublePoint;
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
  const Azoom: byte): TDoublePoint;
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
  const Azoom: byte): TDoubleRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2LonLat(const XY: TDoublePoint;
  const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2LonLatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2PixelPos(const XY: TDoublePoint;
  const Azoom: byte): TPoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2PixelPosFloat(
  const XY: TDoublePoint; const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2PixelPosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TilePosFloat2Relative(const XY: TDoublePoint;
  const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2RelativeInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLatRect2RelativeRect(
  const AXY: TDoubleRect): TDoubleRect;
var
  VXY: TDoubleRect;
begin
  VXY := AXY;
  CheckLonLatRectInternal(VXY);
  Result := LonLatRect2RelativeRectInternal(VXY);
end;

function TCoordConverterAbstract.Relative2TilePos(const AXY: TDoublePoint;
  const Azoom: byte): TPoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2TilePosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.Relative2TilePosFloat(
  const AXY: TDoublePoint; const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativePosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := Relative2TilePosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2LonLatRect(
  const AXY: TDoubleRect): TDoubleRect;
var
  VXY: TDoubleRect;
begin
  VXY := AXY;
  CheckRelativeRectInternal(VXY);
  Result := RelativeRect2LonLatRectInternal(VXY);
end;

function TCoordConverterAbstract.RelativeRect2PixelRect(const AXY: TDoubleRect;
  const Azoom: byte): TRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2PixelRectFloat(
  const AXY: TDoubleRect; const Azoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2PixelRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRect(const AXY: TDoubleRect;
  const Azoom: byte): TRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRectFloat(
  const AXY: TDoubleRect; const Azoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckRelativeRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := RelativeRect2TileRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPos2TilePos(const AXY: TPoint;
  const Azoom: byte): TPoint;
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
  const Azoom: byte): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosInternal(VXY, VZoom);
  Result := PixelPos2TilePosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2LonLat(const XY: TDoublePoint;
  const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2LonLatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2Relative(
  const XY: TDoublePoint; const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2RelativeInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2TilePos(const XY: TDoublePoint;
  const Azoom: byte): TPoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2TilePosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelPosFloat2TilePosFloat(
  const XY: TDoublePoint; const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2TilePosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRect2LonLatRect(const AXY: TRect;
  const AZoom: byte): TDoubleRect;
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
  const AZoom: byte): TRect;
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
  const AZoom: byte): TDoubleRect;
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
  const XY: TDoubleRect; const AZoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2RelativeRect(
  const XY: TDoubleRect; const AZoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2TileRect(
  const XY: TDoubleRect; const AZoom: byte): TRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.PixelRectFloat2TileRectFloat(
  const XY: TDoubleRect; const AZoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckPixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2TileRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRect2PixelRect(const AXY: TRect;
  const AZoom: byte): TRect;
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
  const AZoom: byte): TDoubleRect;
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
  const Azoom: byte): TPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckTilePosInternal(VXY, VZoom);
  Result := TilePos2PixelPosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2TilePos(const AXY: TDoublePoint;
  const Azoom: byte): Tpoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLat2TilePosInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2TilePosFloat(const AXY: TDoublePoint;
  const Azoom: byte): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatPosInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLat2TilePosFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLat2Relative(
  const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  CheckLonLatPosInternal(VXY);
  Result := LonLat2RelativeInternal(VXY);
end;

function TCoordConverterAbstract.TileRect2LonLatRect(const AXY: TRect;
  const Azoom: byte): TDoubleRect;
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
  const AZoom: byte): TDoubleRect;
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
  const XY: TDoubleRect; const Azoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2LonLatRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2PixelRect(
  const XY: TDoubleRect; const AZoom: byte): TRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2PixelRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2PixelRectFloat(
  const XY: TDoubleRect; const AZoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2PixelRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.TileRectFloat2RelativeRect(
  const XY: TDoubleRect; const AZoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2RelativeRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLatRect2TileRect(const AXY: TDoubleRect;
  const Azoom: byte): TRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2TileRectInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.LonLatRect2TileRectFloat(
  const XY: TDoubleRect; const Azoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2TileRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.Metr2LonLat(const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  //CheckMetrPosInternal(VXY);
  Result := Metr2LonLatInternal(VXY);
end;

function TCoordConverterAbstract.LonLatRect2PixelRect(const AXY: TDoubleRect;
  const Azoom: byte): TRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2PixelRectInternal(VXY, Vzoom);
end;


function TCoordConverterAbstract.LonLatRect2PixelRectFloat(
  const XY: TDoubleRect; const Azoom: byte): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  CheckLonLatRectInternal(VXY);
  CheckZoomInternal(VZoom);
  Result := LonLatRect2PixelRectFloatInternal(VXY, Vzoom);
end;

function TCoordConverterAbstract.GetDatum: IDatum;
begin
  Result := FDatum;
end;

function TCoordConverterAbstract.GetMaxZoom: Byte;
begin
  Result := 23;
end;

function TCoordConverterAbstract.GetMinZoom: Byte;
begin
  Result := 0;
end;

function TCoordConverterAbstract.GetProjectionEPSG: Integer;
begin
  Result := FProjEPSG;
end;

constructor TCoordConverterAbstract.Create(
  const ADatum: IDatum;
  AProjEPSG: integer;
  ACellSizeUnits: TCellSizeUnits
);
begin
  FDatum := ADatum;
  FProjEPSG := AProjEPSG;
  FCellSizeUnits := ACellSizeUnits;
end;

function TCoordConverterAbstract.GetCellSizeUnits: TCellSizeUnits;
begin
  Result := FCellSizeUnits;
end;

function TCoordConverterAbstract.IsSameConverter(
  const AOtherMapCoordConv: ICoordConverter
): Boolean;
var
  VSelf: ICoordConverter;
begin
  VSelf := Self;
  if VSelf = AOtherMapCoordConv then begin
    Result := True;
  end else begin
    Result :=
      (Self.GetTileSplitCode <> 0) and
      (AOtherMapCoordConv.GetTileSplitCode <> 0) and
      (AOtherMapCoordConv.GetTileSplitCode = Self.GetTileSplitCode) and
      (AOtherMapCoordConv.GetProjectionEPSG <> 0) and
      (Self.GetProjectionEPSG <> 0) and
      (AOtherMapCoordConv.GetProjectionEPSG = Self.GetProjectionEPSG);
  end;
end;

end.
