{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_TileRect;

interface

uses
  Types,
  i_Projection;

type
  ITileRect = interface
    ['{EF5843E4-504A-45E9-9605-980EF7C04E6B}']
    function GetLeft: Integer;
    property Left: Integer read GetLeft;
    function GetTop: Integer;
    property Top: Integer read GetTop;
    function GetRight: Integer;
    property Right: Integer read GetRight;
    function GetBottom: Integer;
    property Bottom: Integer read GetBottom;

    function GetTopLeft: TPoint;
    property TopLeft: TPoint read GetTopLeft;
    function GetBottomRight: TPoint;
    property BottomRight: TPoint read GetBottomRight;

    function GetRect: TRect;
    property Rect: TRect read GetRect;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetProjection: IProjection;
    property Projection: IProjection read GetProjection;

    function IsEqual(const ARect: TRect): Boolean; overload;
    function IsEqual(const ARect: ITileRect): Boolean; overload;
    function IsPointInRect(const APoint: TPoint): Boolean;
    function UnionWithRect(const ARect: TRect): TRect; overload;
    function UnionWithRect(const ARect: ITileRect): TRect; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: TRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: ITileRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: ITileRect): Boolean; overload;
  end;

implementation

end.
