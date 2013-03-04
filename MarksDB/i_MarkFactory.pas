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

unit i_MarkFactory;

interface

uses
  GR32,
  t_GeoTypes,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_ImportConfig,
  i_Category,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkTemplate,
  i_MarksSimple;

type
  IMarkFactory = interface
    ['{725CB1AC-1393-4889-B621-64C3B4348331}']
    function CreateNewPoint(
      const APoint: TDoublePoint;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoint = nil
    ): IMarkPoint;
    function CreateNewLine(
      const ALine: ILonLatPath;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplateLine = nil
    ): IMarkLine;
    function CreateNewPoly(
      const ALine: ILonLatPolygon;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoly = nil
    ): IMarkPoly;

    function ReplaceCategory(
      const AMark: IMark;
      const ACategory: ICategory
    ): IMark;

    function ModifyPoint(
      const ASource: IMarkPoint;
      const AName: string;
      const APic: IMarkPicture;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: TDoublePoint;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
    ): IMarkPoint;
    function ModifyLine(
      const ASource: IMarkLine;
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function ModifyPoly(
      const ASource: IMarkPoly;
      const AName: string;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPolygon;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkPoly;

    function SimpleModifyPoint(
      const ASource: IMarkPoint;
      const ALonLat: TDoublePoint
    ): IMarkPoint;
    function SimpleModifyLine(
      const ASource: IMarkLine;
      const ALine: ILonLatPath;
      const ADesc: string
    ): IMarkLine;
    function SimpleModifyPoly(
      const ASource: IMarkPoly;
      const ALine: ILonLatPolygon
    ): IMarkPoly;

    function PreparePoint(
      const AItem: IVectorDataItemPoint;
      const AName: string;
      const AParams: IImportPointParams;
      const ACategory: ICategory;
      const ADefaultPic: IMarkPicture
    ): IMarkPoint;
    function PrepareLine(
      const AItem: IVectorDataItemLine;
      const AName: string;
      const AParams: IImportLineParams;
      const ACategory: ICategory
    ): IMarkLine;
    function PreparePoly(
      const AItem: IVectorDataItemPoly;
      const AName: string;
      const AParams: IImportPolyParams;
      const ACategory: ICategory
    ): IMarkPoly;

    function GetMarkPictureList: IMarkPictureList;
    property MarkPictureList: IMarkPictureList read GetMarkPictureList;

    function GetConfig: IMarksFactoryConfig;
    property Config: IMarksFactoryConfig read GetConfig;
  end;

implementation

end.
