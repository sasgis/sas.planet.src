{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit i_CalcCircleLayerConfig;

interface

uses
  i_ConfigDataElement,
  i_PointCaptionsLayerConfig,
  i_PolyLineLayerConfig,
  i_PolygonLayerConfig;

type
  ICalcCircleLayerConfig = interface(IConfigDataElement)
    ['{F659019C-C6BB-4FFE-839E-7D9D45CEB281}']
    function GetPolygonConfig: IPolygonLayerConfig;
    property PolygonConfig: IPolygonLayerConfig read GetPolygonConfig;

    function GetLineConfig: ILineLayerConfig;
    property LineConfig: ILineLayerConfig read GetLineConfig;

    function GetPointsConfig: IPointsSetLayerConfig;
    property PointsConfig: IPointsSetLayerConfig read GetPointsConfig;

    function GetCaptionConfig: IPointCaptionsLayerConfig;
    property CaptionConfig: IPointCaptionsLayerConfig read GetCaptionConfig;
  end;

implementation

end.
