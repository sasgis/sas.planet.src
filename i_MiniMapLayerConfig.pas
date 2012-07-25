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
  IMiniMapLayerConfig = interface(IConfigDataElement)
    ['{52CF4419-A937-47E1-9A07-966736ACAA86}']
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    property Width: Integer read GetWidth write SetWidth;

    function GetZoomDelta: Integer;
    procedure SetZoomDelta(AValue: Integer);
    property ZoomDelta: Integer read GetZoomDelta write SetZoomDelta;

    function GetMasterAlpha: Integer;
    procedure SetMasterAlpha(AValue: Integer);
    property MasterAlpha: Integer read GetMasterAlpha write SetMasterAlpha;

    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;

    function GetUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    property UseTilePrevZoomConfig: IUseTilePrevZoomConfig read GetUseTilePrevZoomConfig;

    function GetMapsConfig: IActivMapWithLayers;
    property MapsConfig: IActivMapWithLayers read GetMapsConfig;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;
  end;

implementation

end.
