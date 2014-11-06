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

unit i_LonLatRect;

interface

uses
  t_GeoTypes;

type
  ILonLatRect = interface
    ['{B93137DE-5BA8-4A3E-885E-0976CE70199E}']
    function GetLeft: Double;
    property Left: Double read GetLeft;
    function GetTop: Double;
    property Top: Double read GetTop;
    function GetRight: Double;
    property Right: Double read GetRight;
    function GetBottom: Double;
    property Bottom: Double read GetBottom;

    function GetTopLeft: TDoublePoint;
    property TopLeft: TDoublePoint read GetTopLeft;
    function GetBottomRight: TDoublePoint;
    property BottomRight: TDoublePoint read GetBottomRight;

    function GetRect: TDoubleRect;
    property Rect: TDoubleRect read GetRect;

    function CalcRectCenter: TDoublePoint;

    function IsEqual(const ARect: TDoubleRect): Boolean; overload;
    function IsEqual(const ARect: ILonLatRect): Boolean; overload;
    function IsPointInRect(const APoint: TDoublePoint): Boolean;
    function UnionWithRect(const ARect: TDoubleRect): TDoubleRect; overload;
    function UnionWithRect(const ARect: ILonLatRect): TDoubleRect; overload;
    function IntersecWithRect(
      out AResultRect: TDoubleRect;
      const ARect: TDoubleRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TDoubleRect;
      const ARect: ILonLatRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TDoubleRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: ILonLatRect): Boolean; overload;
    function IsContainRect(const ARect: ILonLatRect): Boolean;
  end;


implementation

end.
