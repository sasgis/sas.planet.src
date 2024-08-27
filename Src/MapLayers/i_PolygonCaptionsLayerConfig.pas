{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_PolygonCaptionsLayerConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IPolygonCaptionsLayerConfigStatic = interface
    ['{59FA17BD-85C7-4CD4-AEEA-2C5D79C6AAC2}']
    function GetVisible: Boolean;
    property Visible: Boolean read GetVisible;

    function GetShowPerimeter: Boolean;
    property ShowPerimeter: Boolean read GetShowPerimeter;

    function GetShowArea: Boolean;
    property ShowArea: Boolean read GetShowArea;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;

    function GetFontName: string;
    property FontName: string read GetFontName;

    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBGColor: TColor32;
    property TextBGColor: TColor32 read GetTextBGColor;
  end;

  IPolygonCaptionsLayerConfig = interface(IConfigDataElement)
    ['{62C52B3C-BC6E-4CC0-BEA6-E20FA5A82A0F}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetShowPerimeter: Boolean;
    procedure SetShowPerimeter(const AValue: Boolean);
    property ShowPerimeter: Boolean read GetShowPerimeter write SetShowPerimeter;

    function GetShowArea: Boolean;
    procedure SetShowArea(const AValue: Boolean);
    property ShowArea: Boolean read GetShowArea write SetShowArea;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    property FontName: string read GetFontName write SetFontName;

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);
    property TextBGColor: TColor32 read GetTextBGColor write SetTextBGColor;

    function GetStatic: IPolygonCaptionsLayerConfigStatic;
  end;

implementation

end.
