{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit i_Bitmap32Surface;

interface

uses
  Types,
  t_Bitmap32,
  i_Bitmap32Static;

type
  IBitmap32Surface = interface
    ['{90C9BDF0-E2CC-4369-BA0B-96C46A70A19C}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;

    function GetIsInited: Boolean;
    property IsInited: Boolean read GetIsInited;

    procedure Clear;
    procedure FullFill(const AFillColor: TColor32);

    procedure FillRect(
      const ARect: TRect;
      const AValue: TColor32
    );
    procedure FrameRect(
      const ARect: TRect;
      const AValue: TColor32
    );
    procedure Line(
      const APoint1, APoint2: TPoint;
      const AValue: TColor32
    );
    procedure SetPixel(
      const APoint: TPoint;
      const AValue: TColor32
    );

    procedure DrawBitmapStatic(const ASource: IBitmap32Static);
    procedure DrawBitmapStaticAt(
      const APosition: TPoint;
      const ASource: IBitmap32Static
    );
    procedure DrawBitmapData(
      const ASize: TPoint;
      const AData: PColor32Array
    );
    procedure DrawBitmapDataAt(
      const APosition: TPoint;
      const ASize: TPoint;
      const AData: PColor32Array
    );
  end;

  IBitmap32StaticBuilder = interface(IBitmap32Surface)
    ['{DD01C179-C0AE-487B-9EE4-809D217559F9}']
    function MakeStaticAndClear: IBitmap32Static;
    function MakeStaticCopy: IBitmap32Static;
  end;

  IBitmap32StaticBuilderFactory = interface
    ['{91C9BDF0-E2CC-4369-BA0B-96C46A70A19C}']
    function BuildEmpty(const ASize: TPoint): IBitmap32StaticBuilder;
    function BuildFillColor(
      const ASize: TPoint;
      const AFillColor: TColor32
    ): IBitmap32StaticBuilder;
    function BuildByData(
      const ASize: TPoint;
      const AData: PColor32Array
    ): IBitmap32StaticBuilder;
    function BuildByBitmap32Static(const ASource: IBitmap32Static): IBitmap32StaticBuilder;
  end;

implementation

end.
