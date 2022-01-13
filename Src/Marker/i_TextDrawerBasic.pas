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

unit i_TextDrawerBasic;

interface

uses
  t_Bitmap32,
  i_Bitmap32Static;

type
  ITextDrawerBasic = interface
    ['{874AF838-4B3D-44AF-BF3E-96A72FE3D8CF}']
    function DrawText(
      const AText: string;
      const AFontSize: Integer;
      const ATextColor: TColor32;
      const ATextBgColor: TColor32;
      const ASolidBgDraw: Boolean
    ): IBitmap32Static;
  end;

implementation

end.
