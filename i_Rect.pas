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

unit i_Rect;

interface

uses
  Types;

type
  IRect = interface
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

    function IsEqual(const ARect: TRect): Boolean; overload;
    function IsEqual(const ARect: IRect): Boolean; overload;
    function IsPointInRect(const APoint: TPoint): Boolean;
    function UnionWithRect(const ARect: TRect): TRect; overload;
    function UnionWithRect(const ARect: IRect): TRect; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: TRect
    ): Boolean; overload;
    function IntersecWithRect(
      out AResultRect: TRect;
      const ARect: IRect
    ): Boolean; overload;
    function IsIntersecWithRect(const ARect: TRect): Boolean; overload;
    function IsIntersecWithRect(const ARect: IRect): Boolean; overload;
  end;

implementation

end.
