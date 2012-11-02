{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit i_MapLayerGPSMarkerConfig;

interface

uses
  i_ConfigDataElement,
  i_MarkerRingsConfig,
  i_MarkerSimpleConfig;

type
  IMapLayerGPSMarkerConfig = interface(IConfigDataElement)
    ['{A8E08D39-7805-4A4C-AD78-F8CAD66919C7}']
    function GetMinMoveSpeed: Double;
    procedure SetMinMoveSpeed(AValue: Double);
    property MinMoveSpeed: Double read GetMinMoveSpeed write SetMinMoveSpeed;

    function GetMovedMarkerConfig: IMarkerSimpleConfig;
    property MovedMarkerConfig: IMarkerSimpleConfig read GetMovedMarkerConfig;

    function GetStopedMarkerConfig: IMarkerSimpleConfig;
    property StopedMarkerConfig: IMarkerSimpleConfig read GetStopedMarkerConfig;

    function GetMarkerRingsConfig: IMarkerRingsConfig;
    property MarkerRingsConfig: IMarkerRingsConfig read GetMarkerRingsConfig;
  end;

implementation

end.
