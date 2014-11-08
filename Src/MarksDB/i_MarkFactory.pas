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

unit i_MarkFactory;

interface

uses
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
    function CreateNewMark(
      const AGeometry: IGeometryLonLat;
      const AName: string;
      const ADesc: string;
      const ATemplate: IMarkTemplate = nil
    ): IVectorDataItem;

    function ReplaceCategory(
      const AMark: IVectorDataItem;
      const ACategory: ICategory
    ): IVectorDataItem;

    function CreateMark(
      const AGeometry: IGeometryLonLat;
      const AName: string;
      const ADesc: string;
      const ACategory: ICategory;
      const AAppearance: IAppearance
    ): IVectorDataItem;

    function ModifyGeometry(
      const ASource: IVectorDataItem;
      const AGeometry: IGeometryLonLat;
      const ADesc: string = ''
    ): IVectorDataItem;

    function PrepareMark(
      const AItem: IVectorDataItem;
      const AName: string;
      const AParams: IImportMarkParams;
      const ACategory: ICategory
    ): IVectorDataItem;

    function GetMarkPictureList: IMarkPictureList;
    property MarkPictureList: IMarkPictureList read GetMarkPictureList;

    function GetConfig: IMarkFactoryConfig;
    property Config: IMarkFactoryConfig read GetConfig;
  end;

implementation

end.
