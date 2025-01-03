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

unit i_TilePostDownloadCropConfig;

interface

uses
  Types;

type
  ITilePostDownloadCropConfigStatic = interface
    ['{D7C63FFC-132A-4E6D-ABD0-CC3935A64D0F}']
    function GetIsCropOnDownload: Boolean;
    property IsCropOnDownload: Boolean read GetIsCropOnDownload;

    function GetCropRect: TRect;
    property CropRect: TRect read GetCropRect;

    // cut downloaded image into multiple images and save all (nonexistent) items
    // crop before cut (if defined)

    function GetIsCutOnDownload: Boolean;
    property IsCutOnDownload: Boolean read GetIsCutOnDownload;

    // count of parts (X and Y)
    function GetCutCount: TPoint;
    property CutCount: TPoint read GetCutCount;

    // or size of every part (X and Y)
    function GetCutSize: TPoint;
    property CutSize: TPoint read GetCutSize;

    // position of requested tile
    function GetCutTile: TPoint;
    property CutTile: TPoint read GetCutTile;

    // skip (for watermarks)
    function CutSkipItem(const AItem, ACount: TPoint): Boolean;
  end;

implementation

end.
