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

unit i_FillingMapLayerConfig;

interface

uses
  t_Bitmap32,
  t_FillingMapModes,
  i_ThreadConfig,
  i_ActiveMapsConfig,
  i_ConfigDataElement;

type
  IFillingMapLayerConfigStatic = interface
    ['{6D257213-D59E-45D8-A632-6499B2549C64}']
    function GetVisible: Boolean;
    property Visible: Boolean read GetVisible;

    function GetSelectedMap: TGUID;
    property SelectedMap: TGUID read GetSelectedMap;

    function GetUseRelativeZoom: Boolean;
    property UseRelativeZoom: Boolean read GetUseRelativeZoom;

    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;

    function GetNoTileColor: TColor32;
    property NoTileColor: TColor32 read GetNoTileColor;

    function GetShowTNE: Boolean;
    property ShowTNE: Boolean read GetShowTNE;

    function GetTNEColor: TColor32;
    property TNEColor: TColor32 read GetTNEColor;

    function GetFillMode: TFillMode;
    property FillMode: TFillMode read GetFillMode;

    function GetFilterMode: Boolean;
    property FilterMode: Boolean read GetFilterMode;

    function GetFillFirstDay: TDateTime;
    property FillFirstDay: TDateTime read GetFillFirstDay;

    function GetFillLastDay: TDateTime;
    property FillLastDay: TDateTime read GetFillLastDay;
  end;

  IFillingMapLayerConfig = interface(IConfigDataElement)
    ['{5A89A65C-7145-4063-8B8E-357DEEF9DC66}']
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(const AValue: Boolean);
    property UseRelativeZoom: Boolean read GetUseRelativeZoom write SetUseRelativeZoom;

    function GetZoom: Byte;
    procedure SetZoom(const AValue: Byte);
    property Zoom: Byte read GetZoom write SetZoom;

    function GetNoTileColor: TColor32;
    procedure SetNoTileColor(const AValue: TColor32);
    property NoTileColor: TColor32 read GetNoTileColor write SetNoTileColor;

    function GetShowTNE: Boolean;
    procedure SetShowTNE(const AValue: Boolean);
    property ShowTNE: Boolean read GetShowTNE write SetShowTNE;

    function GetTNEColor: TColor32;
    procedure SetTNEColor(const AValue: TColor32);
    property TNEColor: TColor32 read GetTNEColor write SetTNEColor;

    function GetFillMode: TFillMode;
    procedure SetFillMode(const AValue: TFillMode);
    property FillMode: TFillMode read GetFillMode write SetFillMode;

    function GetFilterMode: Boolean;
    procedure SetFilterMode(const AValue: Boolean);
    property FilterMode: Boolean read GetFilterMode write SetFilterMode;

    function GetFillFirstDay: TDateTime;
    procedure SetFillFirstDay(const AValue: TDateTime);
    property FillFirstDay: TDateTime read GetFillFirstDay write SetFillFirstDay;

    function GetFillLastDay: TDateTime;
    procedure SetFillLastDay(const AValue: TDateTime);
    property FillLastDay: TDateTime read GetFillLastDay write SetFillLastDay;

    function GetThreadConfig: IThreadConfig;
    property ThreadConfig: IThreadConfig read GetThreadConfig;

    function GetSourceMap: IActiveMapConfig;
    property SourceMap: IActiveMapConfig read GetSourceMap;

    function GetStatic: IFillingMapLayerConfigStatic;
  end;

implementation

end.
