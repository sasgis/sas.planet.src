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

unit i_PolyLineLayerConfig;

interface

uses
  GR32,
  i_ConfigDataElement,
  i_MarkerSimpleConfig;

type
  ILineLayerConfig = interface(IConfigDataElement)
    ['{738384AD-8BD0-42E0-B037-5A24895D64B6}']
    function GetLineColor: TColor32;
    procedure SetLineColor(AValue: TColor32);
    property LineColor: TColor32 read GetLineColor write SetLineColor;

    function GetLineWidth: integer;
    procedure SetLineWidth(AValue: integer);
    property LineWidth: integer read GetLineWidth write SetLineWidth;
  end;


  IPointsSetLayerConfig = interface(IConfigDataElement)
    ['{5B334D74-C1B7-4C5D-96C2-9EA4D02698EF}']
    function GetFirstPointMarker: IMarkerSimpleConfig;
    property FirstPointMarker: IMarkerSimpleConfig read GetFirstPointMarker;

    function GetActivePointMarker: IMarkerSimpleConfig;
    property ActivePointMarker: IMarkerSimpleConfig read GetActivePointMarker;

    function GetNormalPointMarker: IMarkerSimpleConfig;
    property NormalPointMarker: IMarkerSimpleConfig read GetNormalPointMarker;
  end;

implementation

end.
