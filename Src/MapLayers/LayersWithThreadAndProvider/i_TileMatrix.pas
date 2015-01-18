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

unit i_TileMatrix;

interface

uses
  Types,
  i_Bitmap32Static,
  i_TileRect,
  i_LocalCoordConverter;

type
  ITileMatrixElement = interface
    ['{3F060F92-8F09-4989-AFD8-B7D2B8E5DC20}']
    function GetTile: TPoint;
    property Tile: TPoint read GetTile;

    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetReadyID: Integer;
    property ReadyID: Integer read GetReadyID;

    function GetExpectedID: Integer;
    property ExpectedID: Integer read GetExpectedID;

    procedure IncExpectedID;
    procedure UpdateBitmap(
      AID: Integer;
      const ABitmap: IBitmap32Static
    );

    function GetBitmap: IBitmap32Static;
  end;

  ITileMatrix = interface
    ['{ABC6376A-F0CD-408F-8F19-043633E6D374}']
    function GetTileRect: ITileRect;
    property TileRect: ITileRect read GetTileRect;

    function GetElementByTile(const ATile: TPoint): ITileMatrixElement;

    function GetItem(AX, AY: Integer): ITileMatrixElement;
    property Items[AX, AY: Integer]: ITileMatrixElement read GetItem;
  end;

  ITileMatrixFactory = interface
    ['{63A336D2-751D-4C4F-BC18-CF3791206619}']
    function BuildNewMatrix(
      const ASource: ITileMatrix;
      const ANewTileRect: ITileRect
    ): ITileMatrix;
  end;

implementation

end.
