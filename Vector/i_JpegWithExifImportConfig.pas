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

unit i_JpegWithExifImportConfig;

interface

uses
  i_ImportConfig;

type
  IJpegWithExifImportConfig = interface(IImportConfig)
    ['{13CEF43F-633C-4B96-A4E6-B2FFDD0AD9A2}']
    function GetUseThumbnailAsIcon(): Boolean;
    property UseThumbnailAsIcon: Boolean read GetUseThumbnailAsIcon;

    function GetResolutionLimit(): Integer;
    property ResolutionLimit: Integer read GetResolutionLimit;
  end;

implementation

end.
