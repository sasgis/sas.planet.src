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

unit i_MarkTemplate;

interface

uses
  GR32,
  i_Category;

type
  IMarkTemplate = interface
    ['{2D6A0C13-754C-4BC1-9003-361CA28D311E}']
    function GetNewName: string;

    function GetCategory: ICategory;
    property Category: ICategory read GetCategory;
  end;

  IMarkTemplatePoint = interface(IMarkTemplate)
    ['{B36731B8-7D98-4D56-996F-E6B77AA6FAB3}']
    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBgColor: TColor32;
    property TextBgColor: TColor32 read GetTextBgColor;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;

    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;

    function GetPicName: string;
    property PicName: string read GetPicName;

    function IsSame(const ATemplate: IMarkTemplatePoint): Boolean;
  end;

  IMarkTemplateLine = interface(IMarkTemplate)
    ['{BF4FF116-98E1-43C5-A7FD-DCE3BF26E8D4}']
    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;

    function IsSame(const ATemplate: IMarkTemplateLine): Boolean;
  end;

  IMarkTemplatePoly = interface(IMarkTemplate)
    ['{81CB621A-112D-4914-B801-BBBAAE11C797}']
    function GetBorderColor: TColor32;
    property BorderColor: TColor32 read GetBorderColor;

    function GetFillColor: TColor32;
    property FillColor: TColor32 read GetFillColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;

    function IsSame(const ATemplate: IMarkTemplatePoly): Boolean;
  end;

implementation

end.
