{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit i_MarksExplorerConfig;

interface

uses
  Classes,
  i_ConfigDataElement,
  i_WindowPositionConfig;

type
  IMarksExplorerConfig = interface(IConfigDataElement)
    ['{19EA0E1B-137F-41B2-865A-8654D3A72094}']
    function GetWindowPositionConfig: IWindowPositionConfig;
    property WindowPositionConfig: IWindowPositionConfig read GetWindowPositionConfig;

    function GetCategoriesWidth: Integer;
    procedure SetCategoriesWidth(const AValue: Integer);
    property CategoriesWidth: Integer read GetCategoriesWidth write SetCategoriesWidth;

    function GetExpandedCategories: AnsiString;
    procedure SetExpandedCategories(const AValue: AnsiString);
    property ExpandedCategories: AnsiString read GetExpandedCategories write SetExpandedCategories;

    function GetSelectedCategory: AnsiString;
    procedure SetSelectedCategory(const AValue: AnsiString);
    property SelectedCategory: AnsiString read GetSelectedCategory write SetSelectedCategory;
  end;

implementation

end.
