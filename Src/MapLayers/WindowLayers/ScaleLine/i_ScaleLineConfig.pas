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

unit i_ScaleLineConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  TScaleLegendNumbersFormat = (slnfNice = 0, slnfScienceRound = 1, slnfScience = 2);

  IScaleLineConfig = interface(IConfigDataElement)
    ['{C8AAEDF7-D20D-4ECA-919A-76EF8E60EAE8}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetExtended: Boolean;
    procedure SetExtended(AValue: Boolean);
    property Extended: Boolean read GetExtended write SetExtended;

    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    property Width: Integer read GetWidth write SetWidth;

    function GetColor: TColor32;
    procedure SetColor(AValue: TColor32);
    property Color: TColor32 read GetColor write SetColor;

    function GetOutLineColor: TColor32;
    procedure SetOutLineColor(AValue: TColor32);
    property OutLineColor: TColor32 read GetOutLineColor write SetOutLineColor;

    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    property FontName: string read GetFontName write SetFontName;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetNumbersFormat: TScaleLegendNumbersFormat;
    procedure SetNumbersFormat(AValue: TScaleLegendNumbersFormat);
    property NumbersFormat: TScaleLegendNumbersFormat read GetNumbersFormat write SetNumbersFormat;

    function GetBottomMargin: Integer;
    procedure SetBottomMargin(AValue: Integer);
    property BottomMargin: Integer read GetBottomMargin write SetBottomMargin;
  end;

implementation

end.
