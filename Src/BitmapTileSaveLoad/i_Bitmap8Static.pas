{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit i_Bitmap8Static;

interface

uses
  Types,
  t_Bitmap32;

type
  IBitmap8Static = interface
    ['{BCBD7AF8-3785-4FFD-8473-FE3F45F7A372}']
    function GetSize: TPoint;
    property Size: TPoint read GetSize;

    function GetData: PByte;
    property Data: PByte read GetData;

    function GetPalette: PColor32Array;
    property Palette: PColor32Array read GetPalette;

    function GetPaletteSize: Integer;
    property PaletteSize: Integer read GetPaletteSize;
  end;

implementation

end.
