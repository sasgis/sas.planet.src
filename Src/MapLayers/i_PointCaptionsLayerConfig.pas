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

unit i_PointCaptionsLayerConfig;

interface

uses
  t_Bitmap32,
  i_ConfigDataElement;

type
  IPointCaptionsLayerConfigStatic = interface
    ['{E826C149-C1AC-490E-BA46-63E74289B7DF}']
    function GetVisible: Boolean;
    property Visible: Boolean read GetVisible;

    function GetShowAzimuth: Boolean;
    property ShowAzimuth: Boolean read GetShowAzimuth;

    function GetShowIntermediateDist: Boolean;
    property ShowIntermediateDist: Boolean read GetShowIntermediateDist;

    function GetShowDistIncrement: Boolean;
    property ShowDistIncrement: Boolean read GetShowDistIncrement;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;

    function GetLastPointFontSize: Integer;
    property LastPointFontSize: Integer read GetLastPointFontSize;

    function GetFontName: string;
    property FontName: string read GetFontName;

    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBGColor: TColor32;
    property TextBGColor: TColor32 read GetTextBGColor;
  end;

  IPointCaptionsLayerConfig = interface(IConfigDataElement)
    ['{7B3B1D25-519A-43AE-9FFA-B27982DA37D7}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;

    function GetShowAzimuth: Boolean;
    procedure SetShowAzimuth(AValue: Boolean);
    property ShowAzimuth: Boolean read GetShowAzimuth write SetShowAzimuth;

    function GetShowIntermediateDist: Boolean;
    procedure SetShowIntermediateDist(const AValue: Boolean);
    property ShowIntermediateDist: Boolean read GetShowIntermediateDist write SetShowIntermediateDist;

    function GetShowDistIncrement: Boolean;
    procedure SetShowDistIncrement(const AValue: Boolean);
    property ShowDistIncrement: Boolean read GetShowDistIncrement write SetShowDistIncrement;

    function GetFontSize: Integer;
    procedure SetFontSize(AValue: Integer);
    property FontSize: Integer read GetFontSize write SetFontSize;

    function GetLastPointFontSize: Integer;
    procedure SetLastPointFontSize(AValue: Integer);
    property LastPointFontSize: Integer read GetLastPointFontSize write SetLastPointFontSize;

    function GetFontName: string;
    procedure SetFontName(const AValue: string);
    property FontName: string read GetFontName write SetFontName;

    function GetTextColor: TColor32;
    procedure SetTextColor(const AValue: TColor32);
    property TextColor: TColor32 read GetTextColor write SetTextColor;

    function GetTextBGColor: TColor32;
    procedure SetTextBGColor(const AValue: TColor32);
    property TextBGColor: TColor32 read GetTextBGColor write SetTextBGColor;

    function GetStatic: IPointCaptionsLayerConfigStatic;
  end;

implementation

end.
