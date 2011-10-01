{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit i_SelectionPolylineLayerConfig;

interface

uses
  GR32,
  i_PolylineLayerConfig;

type
  ISelectionPolylineLayerConfig = interface(IPolylineLayerConfig)
    ['{9E4CE106-9322-4A88-915B-CE5AECED03D2}']
    function GetRadius: Double;
    procedure SetRadius(AValue: Double);

    function GetShadowPolygonColor: TColor32;
    procedure SetShadowPolygonColor(AValue: TColor32);
 end;

implementation

end.
