{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  i_MarkCategory,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkTemplate,
  i_MarksSimple;

type
  IMarkFactory = interface
    ['{725CB1AC-1393-4889-B621-64C3B4348331}']
    function CreateNewPoint(
      APoint: TDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoint = nil
    ): IMarkPoint;
    function CreateNewLine(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplateLine = nil
    ): IMarkLine;
    function CreateNewPoly(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoly = nil
    ): IMarkPoly;

    function ModifyPoint(
      ASource: IMarkPoint;
      AName: string;
      AVisible: Boolean;
      APic: IMarkPicture;
      ACategory: ICategory;
      ADesc: string;
      APoint: TDoublePoint;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
    ): IMarkPoint;
    function ModifyLine(
      ASource: IMarkLine;
      AName: string;
      AVisible: Boolean;
      ACategory: ICategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function ModifyPoly(
      ASource: IMarkPoly;
      AName: string;
      AVisible: Boolean;
      ACategory: ICategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkPoly;

    function SimpleModifyLine(
      ASource: IMarkLine;
      APoints: TArrayOfDoublePoint;
      ADesc: string
    ): IMarkLine;
    function SimpleModifyPoly(
      ASource: IMarkPoly;
      APoints: TArrayOfDoublePoint
    ): IMarkPoly;

    function GetConfig: IMarksFactoryConfig;
    property Config: IMarksFactoryConfig read GetConfig;
  end;

implementation

end.
