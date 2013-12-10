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

unit i_VectorDataItemSimple;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_Category,
  i_GeometryLonLat;

type
  IVectorDataItemSimple = interface
    ['{1242B43D-C878-4AC9-9F29-0A3E258F4670}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetName: string;
    property Name: string read GetName;

    function GetDesc: string;
    property Desc: string read GetDesc;

    function GetLLRect: ILonLatRect;
    property LLRect: ILonLatRect read GetLLRect;

    function GetAppearance: IAppearance;
    property Appearance: IAppearance read GetAppearance;


    function IsEqual(const AItem: IVectorDataItemSimple): Boolean;
    function GetGoToLonLat: TDoublePoint;
    function GetHintText: string;
    function GetInfoUrl: string;
    function GetInfoHTML: string;
    function GetInfoCaption: string;
  end;

  IVectorDataItemWithCategory = interface
    ['{4B83C411-647D-442C-B289-E6F65A65B9F5}']
    function GetCategory: ICategory;
    property Category: ICategory read GetCategory;
  end;

  IVectorDataItemPoint = interface(IVectorDataItemSimple)
    ['{C4EF133D-831F-4F8F-BF51-D5B9C89C87D7}']
    function GetPoint: IGeometryLonLatPoint;
    property Point: IGeometryLonLatPoint read GetPoint;
  end;

  IVectorDataItemLine = interface(IVectorDataItemSimple)
    ['{6EF44536-9F01-4053-AF77-B83F7574773E}']
    function GetLine: IGeometryLonLatMultiLine;
    property Line: IGeometryLonLatMultiLine read GetLine;
  end;

  IVectorDataItemPoly = interface(IVectorDataItemSimple)
    ['{8693C9BF-C424-4223-AAD2-8DDEAD2344A1}']
    function GetLine: IGeometryLonLatMultiPolygon;
    property Line: IGeometryLonLatMultiPolygon read GetLine;
  end;

implementation

end.
