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

unit i_AppearanceHelper;

interface

uses
  t_Bitmap32,
  i_MarkPicture,
  i_Appearance;

type
  IColorSetHelper = interface
    ['{7D35BEEA-3C43-4116-9BB2-6D7C3E64DDA7}']
    procedure Reset;

    function GetColor: TColor32;
    procedure SetColor(AValue: TColor32);
    property Color: TColor32 read GetColor write SetColor;

    function GetFound: Boolean;
    property Found: Boolean read GetFound;

    function SetGPXColorName(const AName: string): Boolean;
    procedure SetKMLColorValue(const AValue: LongWord);
  end;

  IIntegerSetHelper = interface
    ['{9888B573-FF22-4D4E-A85A-D6A19487B5ED}']
    procedure Reset;

    function GetValue: Integer;
    procedure SetValue(AValue: Integer);
    property Value: Integer read GetValue write SetValue;

    function GetFound: Boolean;
    property Found: Boolean read GetFound;
  end;

  IIconSetHelper = interface
    ['{6DCC5E22-BD64-4EC0-AD11-C05E88F63A79}']
    procedure Reset;

    function GetIcon: IMarkPicture;
    procedure SetIcon(const AIcon: IMarkPicture);
    property Icon: IMarkPicture read GetIcon write SetIcon;

    function GetFound: Boolean;
    property Found: Boolean read GetFound;

    function SetByName(const AName: String): Boolean;
  end;

  IAppearanceHelper = interface
    ['{75208191-C955-44A0-B9A0-A9215BC6712C}']
    procedure Reset;

    function GetTextColor: IColorSetHelper;
    property TextColor: IColorSetHelper read GetTextColor;

    function GetLineColor: IColorSetHelper;
    property LineColor: IColorSetHelper read GetLineColor;

    function GetFillColor: IColorSetHelper;
    property FillColor: IColorSetHelper read GetFillColor;

    function GetLineWidth: IIntegerSetHelper;
    property LineWidth: IIntegerSetHelper read GetLineWidth;

    function GetTextSize: IIntegerSetHelper;
    property TextSize: IIntegerSetHelper read GetTextSize;

    function GetIconSize: IIntegerSetHelper;
    property IconSize: IIntegerSetHelper read GetIconSize;

    function GetIcon: IIconSetHelper;
    property Icon: IIconSetHelper read GetIcon;


    function GetHasPointAppearance: Boolean;
    property HasPointAppearance: Boolean read GetHasPointAppearance;

    function GetHasLineAppearance: Boolean;
    property HasLineAppearance: Boolean read GetHasLineAppearance;

    function GetHasPolygonAppearance: Boolean;
    property HasPolygonAppearance: Boolean read GetHasPolygonAppearance;


    function RedefinePointAppearance: IAppearance;
    function RedefineLineAppearance: IAppearance;
    function RedefinePolygonAppearance: IAppearance;
  end;
  
implementation

end.
