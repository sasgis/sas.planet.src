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

unit i_MainFormLayersConfig;

interface

uses
  i_ConfigDataElement,
  i_StatBarConfig,
  i_MapLayerGPSMarkerConfig,
  i_MapLayerGPSTrackConfig,
  i_MapLayerNavToPointMarkerConfig,
  i_MarksLayerConfig,
  i_KmlLayerConfig,
  i_MiniMapLayerConfig,
  i_CenterScaleConfig,
  i_ScaleLineConfig,
  i_LastSelectionLayerConfig,
  i_CalcLineLayerConfig,
  i_SelectionRectLayerConfig,
  i_SelectionPolygonLayerConfig,
  i_SelectionPolylineLayerConfig,
  i_MarkPolygonLayerConfig,
  i_MarkPolyLineLayerConfig,
  i_FillingMapLayerConfig,
  i_GotoLayerConfig,
  i_FullMapMouseCursorLayerConfig,
  i_MainMapLayerConfig,
  i_MapLayerGridsConfig;

type
  IMainFormLayersConfig = interface(IConfigDataElement)
    ['{02A323E8-25E4-43E5-BE24-AABDF9B331EC}']
    function GetMainMapLayerConfig: IMainMapLayerConfig;
    property MainMapLayerConfig: IMainMapLayerConfig read GetMainMapLayerConfig;

    function GetMapLayerGridsConfig: IMapLayerGridsConfig;
    property MapLayerGridsConfig: IMapLayerGridsConfig read GetMapLayerGridsConfig;

    function GetStatBar: IStatBarConfig;
    property StatBar: IStatBarConfig read GetStatBar;

    function GetGPSMarker: IMapLayerGPSMarkerConfig;
    property GPSMarker: IMapLayerGPSMarkerConfig read GetGPSMarker;

    function GetGPSTrackConfig: IMapLayerGPSTrackConfig;
    property GPSTrackConfig: IMapLayerGPSTrackConfig read GetGPSTrackConfig;

    function GetNavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig;
    property NavToPointMarkerConfig: IMapLayerNavToPointMarkerConfig read GetNavToPointMarkerConfig;

    function GetMarksLayerConfig: IMarksLayerConfig;
    property MarksLayerConfig: IMarksLayerConfig read GetMarksLayerConfig;

    function GetKmlLayerConfig: IKmlLayerConfig;
    property KmlLayerConfig: IKmlLayerConfig read GetKmlLayerConfig;

    function GetMiniMapLayerConfig: IMiniMapLayerConfig;
    property MiniMapLayerConfig: IMiniMapLayerConfig read GetMiniMapLayerConfig;

    function GetCenterScaleConfig: ICenterScaleConfig;
    property CenterScaleConfig: ICenterScaleConfig read GetCenterScaleConfig;

    function GetScaleLineConfig: IScaleLineConfig;
    property ScaleLineConfig: IScaleLineConfig read GetScaleLineConfig;

    function GetLastSelectionLayerConfig: ILastSelectionLayerConfig;
    property LastSelectionLayerConfig: ILastSelectionLayerConfig read GetLastSelectionLayerConfig;

    function GetCalcLineLayerConfig: ICalcLineLayerConfig;
    property CalcLineLayerConfig: ICalcLineLayerConfig read GetCalcLineLayerConfig;

    function GetSelectionRectLayerConfig: ISelectionRectLayerConfig;
    property SelectionRectLayerConfig: ISelectionRectLayerConfig read GetSelectionRectLayerConfig;

    function GetSelectionPolygonLayerConfig: ISelectionPolygonLayerConfig;
    property SelectionPolygonLayerConfig: ISelectionPolygonLayerConfig read GetSelectionPolygonLayerConfig;

    function GetSelectionPolylineLayerConfig: ISelectionPolylineLayerConfig;
    property SelectionPolylineLayerConfig: ISelectionPolylineLayerConfig read GetSelectionPolylineLayerConfig;

    function GetMarkPolygonLayerConfig: IMarkPolygonLayerConfig;
    property MarkPolygonLayerConfig: IMarkPolygonLayerConfig read GetMarkPolygonLayerConfig;

    function GetMarkPolyLineLayerConfig: IMarkPolyLineLayerConfig;
    property MarkPolyLineLayerConfig: IMarkPolyLineLayerConfig read GetMarkPolyLineLayerConfig;

    function GetFillingMapLayerConfig: IFillingMapLayerConfig;
    property FillingMapLayerConfig: IFillingMapLayerConfig read GetFillingMapLayerConfig;

    function GetGotoLayerConfig: IGotoLayerConfig;
    property GotoLayerConfig: IGotoLayerConfig read GetGotoLayerConfig;

    function GetFullMapMouseCursorLayerConfig: IFullMapMouseCursorLayerConfig;
    property FullMapMouseCursorLayerConfig: IFullMapMouseCursorLayerConfig read GetFullMapMouseCursorLayerConfig;
  end;

implementation

end.
