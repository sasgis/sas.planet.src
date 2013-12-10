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
  t_GeoTypes,
  i_Appearance,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_ImportConfig,
  i_Category,
  i_MarkPicture,
  i_MarkFactoryConfig,
  i_MarkTemplate;

type
  IMarkFactory = interface
    ['{725CB1AC-1393-4889-B621-64C3B4348331}']
    function CreateNewPoint(
      const APoint: IGeometryLonLatPoint;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoint = nil
    ): IVectorDataItemPoint;
    function CreateNewLine(
      const ALine: IGeometryLonLatMultiLine;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplateLine = nil
    ): IVectorDataItemLine;
    function CreateNewPoly(
      const ALine: IGeometryLonLatMultiPolygon;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplatePoly = nil
    ): IVectorDataItemPoly;

    function ReplaceCategory(
      const AMark: IVectorDataItemSimple;
      const ACategory: ICategory
    ): IVectorDataItemSimple;

    function CreatePoint(
      const APoint: IGeometryLonLatPoint;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemPoint;
    function CreateLine(
      const ALine: IGeometryLonLatMultiLine;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemLine;
    function CreatePoly(
      const ALine: IGeometryLonLatMultiPolygon;
      const ADesc: string;
      const AName: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItemPoly;

    function SimpleModifyPoint(
      const ASource: IVectorDataItemPoint;
      const ALonLat: IGeometryLonLatPoint
    ): IVectorDataItemPoint;
    function SimpleModifyLine(
      const ASource: IVectorDataItemLine;
      const ALine: IGeometryLonLatMultiLine;
      const ADesc: string
    ): IVectorDataItemLine;
    function SimpleModifyPoly(
      const ASource: IVectorDataItemPoly;
      const ALine: IGeometryLonLatMultiPolygon
    ): IVectorDataItemPoly;

    function PreparePoint(
      const AItem: IVectorDataItemPoint;
      const AName: string;
      const AParams: IImportPointParams;
      const ACategory: ICategory
    ): IVectorDataItemPoint;
    function PrepareLine(
      const AItem: IVectorDataItemLine;
      const AName: string;
      const AParams: IImportLineParams;
      const ACategory: ICategory
    ): IVectorDataItemLine;
    function PreparePoly(
      const AItem: IVectorDataItemPoly;
      const AName: string;
      const AParams: IImportPolyParams;
      const ACategory: ICategory
    ): IVectorDataItemPoly;

    function GetMarkPictureList: IMarkPictureList;
    property MarkPictureList: IMarkPictureList read GetMarkPictureList;

    function GetConfig: IMarkFactoryConfig;
    property Config: IMarkFactoryConfig read GetConfig;
  end;

implementation

end.
