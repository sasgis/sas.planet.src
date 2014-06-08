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

unit i_VectorDataItemSimple;

interface

uses
  t_Hash,
  i_Appearance,
  i_Category,
  i_GeometryLonLat;

type
  IVectorDataItemMainInfo = interface
    ['{0B2B2DFE-E0FB-462B-9C0C-DAE934D59B68}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetName: string;
    property Name: string read GetName;

    function GetDesc: string;
    property Desc: string read GetDesc;

    function IsEqual(const AItem: IVectorDataItemMainInfo): Boolean;

    function GetHintText: string;
    function GetInfoUrl: string;
    function GetInfoHTML: string;
    function GetInfoCaption: string;
  end;

  IVectorDataItemSimple = interface
    ['{1242B43D-C878-4AC9-9F29-0A3E258F4670}']
    function GetHash: THashValue;
    property Hash: THashValue read GetHash;

    function GetMainInfo: IVectorDataItemMainInfo;
    property MainInfo: IVectorDataItemMainInfo read GetMainInfo;

    function GetName: string;
    property Name: string read GetName;

    function GetDesc: string;
    property Desc: string read GetDesc;

    function GetGeometry: IGeometryLonLat;
    property Geometry: IGeometryLonLat read GetGeometry;

    function GetAppearance: IAppearance;
    property Appearance: IAppearance read GetAppearance;

    function IsEqual(const AItem: IVectorDataItemSimple): Boolean;
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

implementation

end.
