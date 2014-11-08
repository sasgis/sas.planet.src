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

unit i_StatBarConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IStatBarConfig = interface(IConfigDataElement)
    ['{473782BB-AD89-4745-8CBA-93B38EA851E6}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    property Height: Integer read GetHeight write SetHeight;

    function GetMinUpdateTickCount: Cardinal;
    procedure SetMinUpdateTickCount(AValue: Cardinal);
    property MinUpdateTickCount: Cardinal read GetMinUpdateTickCount write SetMinUpdateTickCount;

    function GetBgColor: TColor32;
    procedure SetBgColor(AValue: TColor32);
    property BgColor: TColor32 read GetBgColor write SetBgColor;

    function GetTextColor: TColor32;
    procedure SetTextColor(AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    property FontName: string read GetFontName write SetFontName;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetViewZoomInfo: Boolean;
    procedure SetViewZoomInfo(AValue: Boolean);
    property ViewZoomInfo: Boolean read GetViewZoomInfo write SetViewZoomInfo;

    function GetViewLonLatInfo: Boolean;
    procedure SetViewLonLatInfo(AValue: Boolean);
    property ViewLonLatInfo: Boolean read GetViewLonLatInfo write SetViewLonLatInfo;

    function GetViewMetrPerPixInfo: Boolean;
    procedure SetViewMetrPerPixInfo(AValue: Boolean);
    property ViewMetrPerPixInfo: Boolean read GetViewMetrPerPixInfo write SetViewMetrPerPixInfo;

    function GetViewTimeZoneTimeInfo: Boolean;
    procedure SetViewTimeZoneTimeInfo(AValue: Boolean);
    property ViewTimeZoneTimeInfo: Boolean read GetViewTimeZoneTimeInfo write SetViewTimeZoneTimeInfo;

    function GetTimeZoneInfoAvailable: Boolean;
    procedure SetTimeZoneInfoAvailable(AValue: Boolean);
    property TimeZoneInfoAvailable: Boolean read GetTimeZoneInfoAvailable write SetTimeZoneInfoAvailable;

    function GetViewDownloadedInfo: Boolean;
    procedure SetViewDownloadedInfo(AValue: Boolean);
    property ViewDownloadedInfo: Boolean read GetViewDownloadedInfo write SetViewDownloadedInfo;

    function GetViewHttpQueueInfo: Boolean;
    procedure SetViewHttpQueueInfo(AValue: Boolean);
    property ViewHttpQueueInfo: Boolean read GetViewHttpQueueInfo write SetViewHttpQueueInfo;

    function GetViewTilePathInfo: Boolean;
    procedure SetViewTilePathInfo(AValue: Boolean);
    property ViewTilePathInfo: Boolean read GetViewTilePathInfo write SetViewTilePathInfo;
  end;

implementation

end.
