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

unit i_FreeImageFormatIdProvider;

interface

uses
  FreeImage;

type
  IFreeImageFormatIdProvider = interface
    ['{02F307C4-8FA1-419A-9E7C-2B59E6286F53}']
    function GetBmpFormatId: FREE_IMAGE_FORMAT;
    property BmpFormatId: FREE_IMAGE_FORMAT read GetBmpFormatId;

    function GetIcoFormatId: FREE_IMAGE_FORMAT;
    property IcoFormatId: FREE_IMAGE_FORMAT read GetIcoFormatId;

    function GetGifFormatId: FREE_IMAGE_FORMAT;
    property GifFormatId: FREE_IMAGE_FORMAT read GetGifFormatId;

    function GetPngFormatId: FREE_IMAGE_FORMAT;
    property PngFormatId: FREE_IMAGE_FORMAT read GetPngFormatId;

    function GetTiffFormatId: FREE_IMAGE_FORMAT;
    property TiffFormatId: FREE_IMAGE_FORMAT read GetTiffFormatId;

    function GetWebpFormatId: FREE_IMAGE_FORMAT;
    property WebpFormatId: FREE_IMAGE_FORMAT read GetWebpFormatId;
  end;

implementation

end.
