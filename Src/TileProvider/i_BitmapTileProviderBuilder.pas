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

unit i_BitmapTileProviderBuilder;

interface

uses
  t_Bitmap32,
  i_BitmapTileProvider,
  i_BitmapLayerProvider,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_Projection;

type
  IBitmapTileProviderBuilder = interface
    ['{C3FFAAD4-E28C-4020-8230-17E5A33E1764}']
    function Build(
      const AUseMarks: Boolean;
      const AUseRecolor: Boolean;
      const AUseFillingMap: Boolean;
      const AUseGrids: Boolean;
      const AUsePreciseCropping: Boolean;
      const ABackGroundColor: TColor32;
      const AEmptyColor: TColor32;
      const ASourceProvider: IBitmapTileUniProvider;
      const APolygon: IGeometryLonLatPolygon;
      const AProjection: IProjection;
      const AProjectedPolygon: IGeometryProjectedPolygon
    ): IBitmapTileProvider;
  end;

implementation

end.
