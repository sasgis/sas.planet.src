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

unit i_KmlLayerConfig;

interface

uses
  i_VectorItemDrawConfig,
  i_MarkerSimpleConfig,
  i_ThreadConfig,
  i_ConfigDataElement;

type
  IKmlLayerConfig = interface(IConfigDataElement)
    ['{6EA3D5D6-3D9D-40DB-AD53-989920190477}']
    function GetDrawConfig: IVectorItemDrawConfig;
    property DrawConfig: IVectorItemDrawConfig read GetDrawConfig;

    function GetPointMarkerConfig: IMarkerSimpleConfig;
    property PointMarkerConfig: IMarkerSimpleConfig read GetPointMarkerConfig;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
