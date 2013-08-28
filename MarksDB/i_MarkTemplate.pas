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
  i_Appearance,
  i_AppearanceOfVectorItem,
  i_Category;

type
  IMarkTemplate = interface
    ['{2D6A0C13-754C-4BC1-9003-361CA28D311E}']
    function GetNewName: string;

    function GetAppearance: IAppearance;
    property Appearance: IAppearance read GetAppearance;

    function GetCategory: ICategory;
    property Category: ICategory read GetCategory;
  end;

  IMarkTemplatePoint = interface(IMarkTemplate)
    ['{B36731B8-7D98-4D56-996F-E6B77AA6FAB3}']
    function GetCaptionAppearance: IAppearancePointCaption;
    property CaptionAppearance: IAppearancePointCaption read GetCaptionAppearance;

    function GetIconAppearance: IAppearancePointIcon;
    property IconAppearance: IAppearancePointIcon read GetIconAppearance;

    function IsSame(const ATemplate: IMarkTemplatePoint): Boolean;
  end;

  IMarkTemplateLine = interface(IMarkTemplate)
    ['{BF4FF116-98E1-43C5-A7FD-DCE3BF26E8D4}']
    function GetLineAppearance: IAppearanceLine;
    property LineAppearance: IAppearanceLine read GetLineAppearance;

    function IsSame(const ATemplate: IMarkTemplateLine): Boolean;
  end;

  IMarkTemplatePoly = interface(IMarkTemplate)
    ['{81CB621A-112D-4914-B801-BBBAAE11C797}']
    function GetBorderAppearance: IAppearancePolygonBorder;
    property BorderAppearance: IAppearancePolygonBorder read GetBorderAppearance;

    function GetFillAppearance: IAppearancePolygonFill;
    property FillAppearance: IAppearancePolygonFill read GetFillAppearance;

    function IsSame(const ATemplate: IMarkTemplatePoly): Boolean;
  end;

implementation

end.
