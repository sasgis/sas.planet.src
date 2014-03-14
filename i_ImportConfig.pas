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

unit i_ImportConfig;

interface

uses
  i_Category,
  i_Appearance,
  i_AppearanceOfVectorItem;

type
  IImportPointParams = interface
    ['{2042149A-3C2C-44A7-8939-CB14EDD53078}']
    function GetAppearance: IAppearance;
    property Appearance: IAppearance read GetAppearance;

    function GetCaptionAppearance: IAppearancePointCaption;
    property CaptionAppearance: IAppearancePointCaption read GetCaptionAppearance;

    function GetIconAppearance: IAppearancePointIcon;
    property IconAppearance: IAppearancePointIcon read GetIconAppearance;

    function GetIsForceTextColor: Boolean;
    property IsForceTextColor: Boolean read GetIsForceTextColor;

    function GetIsForceTextBgColor: Boolean;
    property IsForceTextBgColor: Boolean read GetIsForceTextBgColor;

    function GetIsForceFontSize: Boolean;
    property IsForceFontSize: Boolean read GetIsForceFontSize;

    function GetIsForceMarkerSize: Boolean;
    property IsForceMarkerSize: Boolean read GetIsForceMarkerSize;

    function GetIsForcePicName: Boolean;
    property IsForcePicName: Boolean read GetIsForcePicName;
  end;

  IImportLineParams = interface
    ['{99AFF863-CB3E-4774-9994-A3EE9F445A44}']
    function GetAppearance: IAppearance;
    property Appearance: IAppearance read GetAppearance;

    function GetLineAppearance: IAppearanceLine;
    property LineAppearance: IAppearanceLine read GetLineAppearance;

    function GetIsForceLineColor: Boolean;
    property IsForceLineColor: Boolean read GetIsForceLineColor;

    function GetIsForceLineWidth: Boolean;
    property IsForceLineWidth: Boolean read GetIsForceLineWidth;
  end;

  IImportPolyParams = interface
    ['{FB94F289-5FE9-48D8-9FC9-9E2A05BD18F7}']
    function GetAppearance: IAppearance;
    property Appearance: IAppearance read GetAppearance;

    function GetBorderAppearance: IAppearancePolygonBorder;
    property BorderAppearance: IAppearancePolygonBorder read GetBorderAppearance;

    function GetFillAppearance: IAppearancePolygonFill;
    property FillAppearance: IAppearancePolygonFill read GetFillAppearance;

    function GetIsForceLineColor: Boolean;
    property IsForceLineColor: Boolean read GetIsForceLineColor;

    function GetIsForceLineWidth: Boolean;
    property IsForceLineWidth: Boolean read GetIsForceLineWidth;

    function GetIsForceFillColor: Boolean;
    property IsForceFillColor: Boolean read GetIsForceFillColor;
  end;

  IImportCategoryParams = interface
    ['{2C97B7DE-52B9-4404-B151-79FE7E799847}']
    function GetIsAddAllInRootCategory: Boolean;
    property IsAddAllInRootCategory: Boolean read GetIsAddAllInRootCategory;

    function GetIsIgnoreMarkIfSubCategotyNotEixts: Boolean;
    property IsIgnoreMarkIfSubCategotyNotEixts: Boolean read GetIsIgnoreMarkIfSubCategotyNotEixts;

    function GetIsCreateSubCategory: Boolean;
    property IsCreateSubCategory: Boolean read GetIsCreateSubCategory;

    function GetIsIgnoreMarkIfExistsWithSameNameInCategory: Boolean;
    property IsIgnoreMarkIfExistsWithSameNameInCategory: Boolean read GetIsIgnoreMarkIfExistsWithSameNameInCategory;
  end;

  IImportConfig = interface
    ['{95479381-A0D7-4FE3-86FB-11C5ED532FD2}']
    function GetRootCategory: ICategory;
    property RootCategory: ICategory read GetRootCategory;

    function GetCategoryParams: IImportCategoryParams;
    property CategoryParams: IImportCategoryParams read GetCategoryParams;

    function GetPointParams: IImportPointParams;
    property PointParams: IImportPointParams read GetPointParams;

    function GetLineParams: IImportLineParams;
    property LineParams: IImportLineParams read GetLineParams;

    function GetPolyParams: IImportPolyParams;
    property PolyParams: IImportPolyParams read GetPolyParams;
  end;

implementation

end.
