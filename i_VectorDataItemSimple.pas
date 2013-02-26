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
  GR32,
  t_GeoTypes,
  i_LonLatRect,
  i_MarkPicture,
  i_Category,
  i_VectorItemLonLat;

type
  IVectorDataItemSimple = interface
    ['{1242B43D-C878-4AC9-9F29-0A3E258F4670}']
    function GetName: string;
    property Name: string read GetName;

    function GetDesc: string;
    property Desc: string read GetDesc;

    function GetLLRect: ILonLatRect;
    property LLRect: ILonLatRect read GetLLRect;

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
    function GetPoint: TDoublePoint;
    property Point: TDoublePoint read GetPoint;
  end;

  IVectorDataItemPointWithIconParams = interface
    ['{D96343C5-9F7C-4276-BB41-6AA5AD4DE948}']
    function GetPicName: string;
    property PicName: string read GetPicName;

    function GetPic: IMarkPicture;
    property Pic: IMarkPicture read GetPic;

    function GetMarkerSize: Integer;
    property MarkerSize: Integer read GetMarkerSize;
  end;

  IVectorDataItemPointWithCaptionParams = interface
    ['{CBF01A7B-F6CE-4EE2-BEF6-740B624AE41C}']
    function GetTextColor: TColor32;
    property TextColor: TColor32 read GetTextColor;

    function GetTextBgColor: TColor32;
    property TextBgColor: TColor32 read GetTextBgColor;

    function GetFontSize: Integer;
    property FontSize: Integer read GetFontSize;
  end;

  IVectorDataItemLine = interface(IVectorDataItemSimple)
    ['{6EF44536-9F01-4053-AF77-B83F7574773E}']
    function GetLine: ILonLatPath;
    property Line: ILonLatPath read GetLine;
  end;

  IVectorDataItemWithLineParams = interface
    ['{DD90D0C7-3855-4367-ADCA-1548280E0512}']
    function GetLineColor: TColor32;
    property LineColor: TColor32 read GetLineColor;

    function GetLineWidth: Integer;
    property LineWidth: Integer read GetLineWidth;
  end;

  IVectorDataItemPoly = interface(IVectorDataItemSimple)
    ['{8693C9BF-C424-4223-AAD2-8DDEAD2344A1}']
    function GetLine: ILonLatPolygon;
    property Line: ILonLatPolygon read GetLine;
  end;

  IVectorDataItemPolyWithFillParams = interface
    ['{29762CED-12E5-4382-99B9-25FAD102071E}']
    function GetFillColor: TColor32;
    property FillColor: TColor32 read GetFillColor;
  end;

implementation

end.
