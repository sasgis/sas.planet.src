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

unit i_ImageTileProvider;

interface

uses
  Types,
  i_NotifierOperation;

type
  IImageTileProvider = interface
    ['{12E225E7-ABD3-4824-96E2-C2448F523C3A}']
    function GetTileSize: TPoint;
    property TileSize: TPoint read GetTileSize;

    function GetBytesPerPixel: Integer;
    property BytesPerPixel: Integer read GetBytesPerPixel;

    function GetTile(
      const AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const APoint: TPoint;
      const AZoom: Integer
    ): Pointer;
  end;

implementation

end.
