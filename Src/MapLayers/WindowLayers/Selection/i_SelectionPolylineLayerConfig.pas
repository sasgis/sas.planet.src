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

unit i_SelectionPolylineLayerConfig;

interface

uses
  i_ConfigDataElement,
  i_PolygonLayerConfig,
  i_PolylineLayerConfig;

type
  ISelectionPolylineShadowLayerConfig = interface(IPolygonLayerConfig)
    ['{B53ED0E4-99FA-46F0-B651-47FA443F2849}']
    function GetRadius: Double;
    procedure SetRadius(AValue: Double);
    property Radius: Double read GetRadius write SetRadius;
  end;

  ISelectionPolylineLayerConfig = interface(IConfigDataElement)
    ['{9E4CE106-9322-4A88-915B-CE5AECED03D2}']
    function GetLineConfig: ILineLayerConfig;
    property LineConfig: ILineLayerConfig read GetLineConfig;

    function GetShadowConfig: ISelectionPolylineShadowLayerConfig;
    property ShadowConfig: ISelectionPolylineShadowLayerConfig read GetShadowConfig;

    function GetPointsConfig: IPointsSetLayerConfig;
    property PointsConfig: IPointsSetLayerConfig read GetPointsConfig;
  end;

implementation

end.
