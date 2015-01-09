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

unit u_CoordConverterAbstract;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_Datum,
  i_CoordConverter,
  u_BaseInterfacedObject;

type
  TCoordConverterAbstract = class(TBaseInterfacedObject, ICoordConverter)
  private
    FHash: THashValue;
    FDatum: IDatum;
    FProjEPSG: integer;
  protected
    procedure ValidateZoomInternal(var AZoom: Byte); virtual; stdcall; abstract;

    procedure ValidatePixelPosInternal(
      var XY: TPoint;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidatePixelPosStrictInternal(
      var XY: TPoint;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidatePixelPosFloatInternal(
      var XY: TDoublePoint;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidatePixelRectInternal(
      var XY: TRect;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidatePixelRectFloatInternal(
      var XY: TDoubleRect;
      var AZoom: byte
    ); virtual; stdcall; abstract;

    procedure ValidateTilePosInternal(
      var XY: TPoint;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidateTilePosStrictInternal(
      var XY: TPoint;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidateTilePosFloatInternal(
      var XY: TDoublePoint;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidateTileRectInternal(
      var XY: TRect;
      var AZoom: byte
    ); virtual; stdcall; abstract;
    procedure ValidateTileRectFloatInternal(
      var XY: TDoubleRect;
      var AZoom: byte
    ); virtual; stdcall; abstract;

    procedure ValidateRelativePosInternal(var XY: TDoublePoint); virtual; stdcall; abstract;
    procedure ValidateRelativeRectInternal(var XY: TDoubleRect); virtual; stdcall; abstract;

    procedure ValidateLonLatPosInternal(var XY: TDoublePoint); virtual; stdcall; abstract;
    procedure ValidateLonLatRectInternal(var XY: TDoubleRect); virtual; stdcall; abstract;

    function LonLat2MetrInternal(const Ll: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;
    function Metr2LonLatInternal(const Mm: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;

    function TileRectAtZoomInternal(const AZoom: byte): TRect; virtual; stdcall; abstract;
    function PixelRectAtZoomInternal(const AZoom: byte): TRect; virtual; stdcall; abstract;

    function TilesAtZoomInternal(AZoom: byte): Longint; virtual; stdcall; abstract;
    function TilesAtZoomFloatInternal(AZoom: byte): Double; virtual; stdcall; abstract;
    function PixelsAtZoomInternal(AZoom: byte): Longint; virtual; stdcall; abstract;
    function PixelsAtZoomFloatInternal(AZoom: byte): Double; virtual; stdcall; abstract;


    function PixelPos2TilePosInternal(
      const XY: TPoint;
      AZoom: byte;
      ARounding: TPointRounding
    ): TPoint; virtual; stdcall; abstract;
    function PixelPos2TilePosFloatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function PixelPos2RelativeInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function PixelPos2LonLatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;

    function PixelPosFloat2TilePosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function PixelPosFloat2RelativeInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function PixelPosFloat2LonLatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;

    function PixelRect2TileRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TRect; virtual; stdcall; abstract;
    function PixelRect2TileRectFloatInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function PixelRect2RelativeRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function PixelRect2LonLatRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;

    function PixelRectFloat2TileRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function PixelRectFloat2RelativeRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function PixelRectFloat2LonLatRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;

    function TilePos2PixelPosInternal(
      const XY: TPoint;
      AZoom: byte
    ): TPoint; virtual; stdcall; abstract;
    function TilePos2PixelRectInternal(
      const XY: TPoint;
      AZoom: byte
    ): TRect; virtual; stdcall; abstract;
    function TilePos2PixelRectFloatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function TilePos2LonLatRectInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function TilePos2LonLatInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function TilePos2RelativeInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function TilePos2RelativeRectInternal(
      const XY: TPoint;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;

    function TilePosFloat2PixelPosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function TilePosFloat2RelativeInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function TilePosFloat2LonLatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;

    function TileRect2PixelRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TRect; virtual; stdcall; abstract;
    function TileRect2RelativeRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function TileRect2LonLatRectInternal(
      const XY: TRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;

    function TileRectFloat2PixelRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function TileRectFloat2RelativeRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function TileRectFloat2LonLatRectInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;

    function Relative2PixelPosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function Relative2TilePosFloatInternal(
      const XY: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function Relative2LonLatInternal(const XY: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;

    function RelativeRect2LonLatRectInternal(const XY: TDoubleRect): TDoubleRect; virtual; stdcall; abstract;
    function RelativeRect2TileRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function RelativeRect2PixelRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;


    function LonLat2PixelPosFloatInternal(
      const Ll: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function LonLat2TilePosFloatInternal(
      const Ll: TDoublePoint;
      AZoom: byte
    ): TDoublePoint; virtual; stdcall; abstract;
    function LonLat2RelativeInternal(const XY: TDoublePoint): TDoublePoint; virtual; stdcall; abstract;

    function LonLatRect2RelativeRectInternal(const XY: TDoubleRect): TDoubleRect; virtual; stdcall; abstract;
    function LonLatRect2PixelRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
    function LonLatRect2TileRectFloatInternal(
      const XY: TDoubleRect;
      AZoom: byte
    ): TDoubleRect; virtual; stdcall; abstract;
  protected
    property Datum: IDatum read FDatum;
    function TilesAtZoom(const AZoom: byte): Longint; stdcall;
    function PixelsAtZoom(const AZoom: byte): Longint; stdcall;
  protected
    function GetHash: THashValue;
    function GetDatum: IDatum; stdcall;

    function GetMinZoom: Byte; stdcall;
    function GetMaxZoom: Byte; stdcall;

    function TileRectAtZoom(const AZoom: byte): TRect; stdcall;
    function PixelRectAtZoom(const AZoom: byte): TRect; stdcall;

    function TilesAtZoomFloat(const AZoom: byte): Double; stdcall;
    function PixelsAtZoomFloat(const AZoom: byte): Double; stdcall;


    function PixelPos2LonLat(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function PixelPos2TilePos(
      const AXY: TPoint;
      const AZoom: byte;
      ARounding: TPointRounding
    ): TPoint; stdcall;
    function PixelPos2Relative(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function PixelPos2TilePosFloat(
      const XY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;

    function PixelPosFloat2TilePosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function PixelPosFloat2Relative(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function PixelPosFloat2LonLat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;

    function PixelRect2TileRect(
      const AXY: TRect;
      const AZoom: byte
    ): TRect; stdcall;
    function PixelRect2RelativeRect(
      const AXY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function PixelRect2LonLatRect(
      const AXY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function PixelRect2TileRectFloat(
      const XY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;

    function PixelRectFloat2TileRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function PixelRectFloat2RelativeRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function PixelRectFloat2LonLatRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;

    function TilePos2PixelPos(
      const AXY: TPoint;
      const AZoom: byte
    ): TPoint; stdcall;
    function TilePos2PixelRect(
      const AXY: TPoint;
      const AZoom: byte
    ): TRect; stdcall;
    function TilePos2PixelRectFloat(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function TilePos2LonLatRect(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function TilePos2LonLat(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function TilePos2Relative(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function TilePos2RelativeRect(
      const AXY: TPoint;
      const AZoom: byte
    ): TDoubleRect; stdcall;

    function TilePosFloat2PixelPosFloat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function TilePosFloat2Relative(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function TilePosFloat2LonLat(
      const XY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;

    function TileRect2PixelRect(
      const AXY: TRect;
      const AZoom: byte
    ): TRect; stdcall;
    function TileRect2RelativeRect(
      const AXY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function TileRect2LonLatRect(
      const AXY: TRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;

    function TileRectFloat2PixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function TileRectFloat2RelativeRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function TileRectFloat2LonLatRect(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;

    function LonLat2PixelPosFloat(
      const AXY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function LonLat2TilePosFloat(
      const AXY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function LonLat2Relative(const AXY: TDoublePoint): TDoublePoint; stdcall;

    function LonLatRect2RelativeRect(const AXY: TDoubleRect): TDoubleRect; stdcall;
    function LonLatRect2PixelRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function LonLatRect2TileRectFloat(
      const XY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;

    function Relative2PixelPosFloat(
      const AXY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function Relative2TilePosFloat(
      const AXY: TDoublePoint;
      const AZoom: byte
    ): TDoublePoint; stdcall;
    function Relative2LonLat(const AXY: TDoublePoint): TDoublePoint; stdcall;

    function RelativeRect2LonLatRect(const AXY: TDoubleRect): TDoubleRect; stdcall;
    function RelativeRect2TileRectFloat(
      const AXY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
    function RelativeRect2PixelRectFloat(
      const AXY: TDoubleRect;
      const AZoom: byte
    ): TDoubleRect; stdcall;
  protected
    function GetTileSize(
      const XY: TPoint;
      const AZoom: byte
    ): TPoint; virtual; stdcall; abstract;

    function ValidateZoom(var AZoom: Byte): boolean; virtual; stdcall; abstract;
    function ValidateTilePos(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ): boolean; virtual; stdcall; abstract;
    function ValidateTilePosStrict(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ): boolean; virtual; stdcall; abstract;
    function ValidateTileRect(
      var XY: TRect;
      var AZoom: byte
    ): boolean; virtual; stdcall; abstract;

    function ValidatePixelPos(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ): boolean; virtual; stdcall; abstract;
    function ValidatePixelPosFloat(
      var XY: TDoublePoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ): boolean; virtual; stdcall; abstract;
    function ValidatePixelPosStrict(
      var XY: TPoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ): boolean; virtual; stdcall; abstract;
    function ValidatePixelPosFloatStrict(
      var XY: TDoublePoint;
      var AZoom: byte;
      ACicleMap: Boolean
    ): boolean; virtual; stdcall; abstract;
    function ValidatePixelRect(
      var XY: TRect;
      var AZoom: byte
    ): boolean; virtual; stdcall; abstract;
    function ValidatePixelRectFloat(
      var XY: TDoubleRect;
      var AZoom: byte
    ): boolean; virtual; stdcall; abstract;

    function ValidateRelativePos(var XY: TDoublePoint): boolean; virtual; stdcall; abstract;
    function ValidateRelativeRect(var XY: TDoubleRect): boolean; virtual; stdcall; abstract;

    function ValidateLonLatPos(var XY: TDoublePoint): boolean; virtual; stdcall; abstract;
    function ValidateLonLatRect(var XY: TDoubleRect): boolean; virtual; stdcall; abstract;
    function GetTileSplitCode: Integer; virtual; stdcall; abstract;
    function LonLat2Metr(const AXY: TDoublePoint): TDoublePoint; stdcall;
    function Metr2LonLat(const AXY: TDoublePoint): TDoublePoint; stdcall;
  private
    function GetProjectionEPSG: Integer; stdcall;
    function IsSameConverter(const AOtherMapCoordConv: ICoordConverter): Boolean; stdcall;
  public
    constructor Create(
      const AHash: THashValue;
      const ADatum: IDatum;
      const AProjEPSG: integer
    );
  end;

implementation

{ TCoordConverterAbstract }

//------------------------------------------------------------------------------
function TCoordConverterAbstract.TilePos2PixelRect(
  const AXY: TPoint;
  const AZoom: byte
): TRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2PixelRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePos2PixelRectFloat(
  const AXY: TPoint;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2PixelRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePos2LonLatRect(
  const AXY: TPoint;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2LonLatRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelsAtZoom(const AZoom: byte): Longint;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  ValidateZoomInternal(VZoom);
  Result := PixelsAtZoomInternal(VZoom);
end;

function TCoordConverterAbstract.PixelsAtZoomFloat(const AZoom: byte): Double;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  ValidateZoomInternal(VZoom);
  Result := PixelsAtZoomFloatInternal(VZoom);
end;

function TCoordConverterAbstract.PixelRectAtZoom(const AZoom: byte): TRect;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  ValidateZoomInternal(VZoom);
  Result := PixelRectAtZoomInternal(VZoom);
end;

function TCoordConverterAbstract.TilesAtZoom(const AZoom: byte): Longint;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  ValidateZoomInternal(VZoom);
  Result := TilesAtZoomInternal(VZoom);
end;

function TCoordConverterAbstract.TilesAtZoomFloat(const AZoom: byte): Double;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  ValidateZoomInternal(VZoom);
  Result := TilesAtZoomFloatInternal(VZoom);
end;

function TCoordConverterAbstract.TileRectAtZoom(const AZoom: byte): TRect;
var
  VZoom: Byte;
begin
  VZoom := AZoom;
  ValidateZoomInternal(VZoom);
  Result := TileRectAtZoomInternal(VZoom);
end;

function TCoordConverterAbstract.PixelPos2Relative(
  const AXY: TPoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidatePixelPosInternal(VXY, VZoom);
  Result := PixelPos2RelativeInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.Relative2LonLat(
  const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  ValidateRelativePosInternal(VXY);
  Result := Relative2LonLatInternal(VXY);
end;

function TCoordConverterAbstract.Relative2PixelPosFloat(
  const AXY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateRelativePosInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := Relative2PixelPosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.LonLat2Metr(
  const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  ValidateLonLatPosInternal(VXY);
  Result := LonLat2MetrInternal(VXY);
end;

function TCoordConverterAbstract.LonLat2PixelPosFloat(
  const AXY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateLonLatPosInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := LonLat2PixelPosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelPos2LonLat(
  const AXY: TPoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidatePixelPosInternal(VXY, VZoom);
  Result := PixelPos2LonLatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePos2LonLat(
  const AXY: TPoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosInternal(VXY, VZoom);
  Result := TilePos2LonLatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePos2Relative(
  const AXY: TPoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosInternal(VXY, VZoom);
  Result := TilePos2RelativeInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePos2RelativeRect(
  const AXY: TPoint;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosStrictInternal(VXY, VZoom);
  Result := TilePos2RelativeRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePosFloat2LonLat(
  const XY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2LonLatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePosFloat2PixelPosFloat(
  const XY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2PixelPosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePosFloat2Relative(
  const XY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateTilePosFloatInternal(VXY, VZoom);
  Result := TilePosFloat2RelativeInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.LonLatRect2RelativeRect(
  const AXY: TDoubleRect): TDoubleRect;
var
  VXY: TDoubleRect;
begin
  VXY := AXY;
  ValidateLonLatRectInternal(VXY);
  Result := LonLatRect2RelativeRectInternal(VXY);
end;

function TCoordConverterAbstract.Relative2TilePosFloat(
  const AXY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateRelativePosInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := Relative2TilePosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.RelativeRect2LonLatRect(
  const AXY: TDoubleRect): TDoubleRect;
var
  VXY: TDoubleRect;
begin
  VXY := AXY;
  ValidateRelativeRectInternal(VXY);
  Result := RelativeRect2LonLatRectInternal(VXY);
end;

function TCoordConverterAbstract.RelativeRect2PixelRectFloat(
  const AXY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateRelativeRectInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := RelativeRect2PixelRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.RelativeRect2TileRectFloat(
  const AXY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateRelativeRectInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := RelativeRect2TileRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelPos2TilePos(
  const AXY: TPoint;
  const AZoom: byte;
  ARounding: TPointRounding
): TPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidatePixelPosInternal(VXY, VZoom);
  Result := PixelPos2TilePosInternal(VXY, VZoom, ARounding);
end;

function TCoordConverterAbstract.PixelPos2TilePosFloat(
  const XY: TPoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelPosInternal(VXY, VZoom);
  Result := PixelPos2TilePosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelPosFloat2LonLat(
  const XY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2LonLatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelPosFloat2Relative(
  const XY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2RelativeInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelPosFloat2TilePosFloat(
  const XY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelPosFloatInternal(VXY, VZoom);
  Result := PixelPosFloat2TilePosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelRect2LonLatRect(
  const AXY: TRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidatePixelRectInternal(VXY, VZoom);
  Result := PixelRect2LonLatRectInternal(VXY, VZoom);
end;


function TCoordConverterAbstract.PixelRect2TileRect(
  const AXY: TRect;
  const AZoom: byte
): TRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidatePixelRectInternal(VXY, VZoom);
  Result := PixelRect2TileRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelRect2TileRectFloat(
  const XY: TRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelRectInternal(VXY, VZoom);
  Result := PixelRect2TileRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelRectFloat2LonLatRect(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2LonLatRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelRectFloat2RelativeRect(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2RelativeRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelRectFloat2TileRectFloat(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidatePixelRectFloatInternal(VXY, VZoom);
  Result := PixelRectFloat2TileRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TileRect2PixelRect(
  const AXY: TRect;
  const AZoom: byte
): TRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTileRectInternal(VXY, VZoom);
  Result := TileRect2PixelRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.PixelRect2RelativeRect(
  const AXY: TRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidatePixelRectInternal(VXY, VZoom);
  Result := PixelRect2RelativeRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TilePos2PixelPos(
  const AXY: TPoint;
  const AZoom: byte
): TPoint;
var
  VXY: TPoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTilePosInternal(VXY, VZoom);
  Result := TilePos2PixelPosInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.LonLat2TilePosFloat(
  const AXY: TDoublePoint;
  const AZoom: byte
): TDoublePoint;
var
  VXY: TDoublePoint;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateLonLatPosInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := LonLat2TilePosFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.LonLat2Relative(
  const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  ValidateLonLatPosInternal(VXY);
  Result := LonLat2RelativeInternal(VXY);
end;

function TCoordConverterAbstract.TileRect2LonLatRect(
  const AXY: TRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTileRectInternal(VXY, VZoom);
  Result := TileRect2LonLatRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TileRect2RelativeRect(
  const AXY: TRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TRect;
  VZoom: Byte;
begin
  VXY := AXY;
  VZoom := AZoom;
  ValidateTileRectInternal(VXY, VZoom);
  Result := TileRect2RelativeRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TileRectFloat2LonLatRect(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2LonLatRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TileRectFloat2PixelRectFloat(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2PixelRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.TileRectFloat2RelativeRect(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateTileRectFloatInternal(VXY, VZoom);
  Result := TileRectFloat2RelativeRectInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.LonLatRect2TileRectFloat(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateLonLatRectInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := LonLatRect2TileRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.Metr2LonLat(const AXY: TDoublePoint): TDoublePoint;
var
  VXY: TDoublePoint;
begin
  VXY := AXY;
  //CheckMetrPosInternal(VXY);
  Result := Metr2LonLatInternal(VXY);
end;

function TCoordConverterAbstract.LonLatRect2PixelRectFloat(
  const XY: TDoubleRect;
  const AZoom: byte
): TDoubleRect;
var
  VXY: TDoubleRect;
  VZoom: Byte;
begin
  VXY := XY;
  VZoom := AZoom;
  ValidateLonLatRectInternal(VXY);
  ValidateZoomInternal(VZoom);
  Result := LonLatRect2PixelRectFloatInternal(VXY, VZoom);
end;

function TCoordConverterAbstract.GetDatum: IDatum;
begin
  Result := FDatum;
end;

function TCoordConverterAbstract.GetHash: THashValue;
begin
  Result := FHash;
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
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: integer
);
begin
  inherited Create;
  FHash := AHash;
  FDatum := ADatum;
  FProjEPSG := AProjEPSG;
end;

function TCoordConverterAbstract.IsSameConverter(
  const AOtherMapCoordConv: ICoordConverter
): Boolean;
begin
  if not Assigned(AOtherMapCoordConv) then begin
    Result := False;
    Exit;
  end;
  if ICoordConverter(Self) = AOtherMapCoordConv then begin
    Result := True;
  end else if AOtherMapCoordConv = nil then begin
    Result := False;
  end else if (FHash <> 0) and (AOtherMapCoordConv.Hash <> 0) and (FHash <> AOtherMapCoordConv.Hash) then begin
    Result := False;
  end else if not FDatum.IsSameDatum(AOtherMapCoordConv.Datum) then begin
    Result := False;
  end else if AOtherMapCoordConv.GetTileSplitCode <> Self.GetTileSplitCode then begin
    Result := False;
  end else if AOtherMapCoordConv.GetProjectionEPSG <> Self.GetProjectionEPSG then begin
    Result := False;
  end else begin
    Result := True;
  end;
end;

end.
