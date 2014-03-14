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

unit i_AppearanceOfVectorItem;

interface

uses
  GR32,
  i_MarkPicture,
  i_Appearance;

type
  IAppearancePointCaption = interface(IAppearance)
    ['{0AC73A92-23B1-4D92-AE97-BEE965944424}']
    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBgColor: TColor32;
    property TextBgColor: TColor32 read GetTextBgColor;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;
  end;

  IAppearancePointIcon = interface(IAppearance)
    ['{537841F9-C492-4E42-BD96-66A1E78C65DC}']
    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetPicName: string;
    property PicName: string read GetPicName;

    function GetPic: IMarkPicture;
    property Pic: IMarkPicture read GetPic;
  end;

  IAppearanceLine = interface(IAppearance)
    ['{97FA7D16-A05E-4041-8DCF-0295279CD941}']
    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IAppearancePolygonBorder = interface(IAppearance)
    ['{B38E37EB-FBD9-46F6-9ED1-A716AE5D155A}']
    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IAppearancePolygonFill = interface(IAppearance)
    ['{006057E4-EE9A-4135-A286-327B1D2E8733}']
    function GetFillColor: TColor32;
    property FillColor: TColor32 read GetFillColor;
  end;

implementation

end.
