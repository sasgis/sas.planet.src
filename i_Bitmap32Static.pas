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

unit i_Bitmap32Static;

interface

uses
  GR32,
  i_Changeable;

type
  IBitmap32Buffer = interface
    ['{CE710076-F0B6-43BF-A70F-15B40555DBFA}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;
  end;

  // TODO: IBitmap32Buffer should be simple buffer of bitmap data but IBitmap32Static should have hash and should be immutable
  IBitmap32Static = IBitmap32Buffer;

  //TODO for future use
  IBitmap32Surface = interface
    ['{90C9BDF0-E2CC-4369-BA0B-96C46A70A19C}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PColor32Array;
    property Data: PColor32Array read GetData;

    function IsEmpty: Boolean;

    procedure InitByBitmap32Static(const ASource: IBitmap32Static);
    procedure InitByData(const ASize: TPoint; const AData: PColor32Array);
    procedure SetSizeAndClear(const ASize: TPoint; const AFillColor: TColor32);

    procedure Clear;
    procedure FullFill(const AFillColor: TColor32);

    procedure FillRect(const ARect: TRect; const AValue: TColor32);
    procedure DrawBitmapStatic(const ASource: IBitmap32Static);
    // TODO: Add other simple draw methods

    function MakeAndClear: IBitmap32Static;
  end;

  IBitmapChangeable = interface(IChangeable)
    ['{D0735E64-ED1C-42B0-8892-ADFDF9C56BE4}']
    function GetStatic: IBitmap32Static;
  end;

implementation

end.
