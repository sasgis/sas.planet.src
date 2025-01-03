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

unit i_GeoTiffCombinerCustomParams;

interface

uses
  Types,
  t_Bitmap32,
  i_Projection,
  i_TileStorage,
  i_MapVersionRequest,
  i_BitmapTileProvider;

type
  IGeoTiffCombinerCustomParams = interface
    ['{ED4575BB-AC6D-498E-8BE1-38098D55DE57}']
    function GetOverviewCount: Integer;
    property OverviewCount: Integer read GetOverviewCount;

    function GetProjection(const AOverviewIndex: Integer): IProjection;
    property Projection[const AOverviewIndex: Integer]: IProjection read GetProjection;

    function GetBitmapTileProvider(const AOverviewIndex: Integer): IBitmapTileProvider;
    procedure SetBitmapTileProvider(const AOverviewIndex: Integer; const AValue: IBitmapTileProvider);
    property BitmapTileProvider[const AOverviewIndex: Integer]: IBitmapTileProvider read GetBitmapTileProvider write SetBitmapTileProvider;

    function GetZoomArray: TByteDynArray;
    function GetOverviewArray: TIntegerDynArray;

    function GetOverviewIndex(const AOverview: Integer): Integer;

    function GetTileStorage: ITileStorage;
    property TileStorage: ITileStorage read GetTileStorage;

    function GetMapVersionRequest: IMapVersionRequest;
    property MapVersionRequest: IMapVersionRequest read GetMapVersionRequest;

    function GetBackgroundColor: TColor32;
    property BackgroundColor: TColor32 read GetBackgroundColor;
  end;

implementation

end.
