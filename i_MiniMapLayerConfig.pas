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

unit i_MiniMapLayerConfig;

interface

uses
  i_ThreadConfig,
  i_ActiveMapsConfig,
  i_UseTilePrevZoomConfig,
  i_ConfigDataElement;

type
  IMiniMapLayerLocationConfigStatic = interface
    ['{BB6946B2-DC9B-493C-9C7D-B77A6D259345}']
    function GetVisible: Boolean;
    property Visible: Boolean read GetVisible;

    function GetWidth: Integer;
    property Width: Integer read GetWidth;

    function GetBottomMargin: Integer;
    property BottomMargin: Integer read GetBottomMargin;

    function GetZoomDelta: Integer;
    property ZoomDelta: Integer read GetZoomDelta;
  end;

  IMiniMapLayerLocationConfig = interface(IConfigDataElement)
    ['{7AC06EAF-95D9-4D34-98CA-AF0FF25234E3}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    property Width: Integer read GetWidth write SetWidth;

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;

    function GetZoomDelta: Integer;
    procedure SetZoomDelta(AValue: Integer);
    property ZoomDelta: Integer read GetZoomDelta write SetZoomDelta;

    function GetStatic: IMiniMapLayerLocationConfigStatic;
  end;

  IMiniMapLayerConfig = interface(IConfigDataElement)
    ['{52CF4419-A937-47E1-9A07-966736ACAA86}']
    function GetMasterAlpha: Integer;
    procedure SetMasterAlpha(AValue: Integer);
    property MasterAlpha: Integer read GetMasterAlpha write SetMasterAlpha;

    function GetLocationConfig: IMiniMapLayerLocationConfig;
    property LocationConfig: IMiniMapLayerLocationConfig read GetLocationConfig;

    function GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    property UseTilePrevZoomConfig: IUseTilePrevZoomConfig read GetUseTilePrevZoomConfig;

    function GetMapsConfig: IActivMapWithLayers;
    property MapsConfig: IActivMapWithLayers read GetMapsConfig;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
