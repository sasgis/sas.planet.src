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

unit i_RegionProcessParamsFrame;

interface

uses
  Types,
  t_Bitmap32,
  t_CommonTypes,
  t_GeoTIFF,
  i_GeometryLonLat,
  i_BitmapLayerProvider,
  i_MapCalibration,
  i_Projection,
  i_PredicateByTileInfo,
  i_MapType;

type
  IRegionProcessParamsFrameBase = interface
    ['{F5346D9B-766C-4B3B-AC4B-9AC71FF62F05}']
    procedure Init(
      const AZoom: byte;
      const APolygon: IGeometryLonLatPolygon
    );
    function Validate: Boolean;
  end;

  IRegionProcessParamsFrameOneMap = interface(IRegionProcessParamsFrameBase)
    ['{240B7587-DDC0-4471-BDF4-AD2EE0040526}']
    function GetMapType: IMapType;
    property MapType: IMapType read GetMapType;
  end;

  IRegionProcessParamsFrameMarksState = interface(IRegionProcessParamsFrameBase)
    ['{97F8B47B-44D4-473A-B841-F23FCBFBC4D5}']
    function MarksState: Byte;
    property GetMarksState: Byte read MarksState;

    function DeleteHiddenMarks: Boolean;
    property GetDeleteHiddenMarks: Boolean read DeleteHiddenMarks;
  end;

  IRegionProcessParamsFrameOneZoom = interface(IRegionProcessParamsFrameBase)
    ['{A1A9D2C3-4C9F-4205-B19C-5A768E938808}']
    function GetZoom: Byte;
    property Zoom: Byte read GetZoom;
  end;

  IRegionProcessParamsFrameZoomArray = interface(IRegionProcessParamsFrameBase)
    ['{9DB542F9-7F4E-4DFF-8957-E0E81B8A9096}']
    function GetZoomArray: TByteDynArray;
    property ZoomArray: TByteDynArray read GetZoomArray;
  end;

  IRegionProcessParamsFrameTargetProjection = interface(IRegionProcessParamsFrameBase)
    ['{F0FACC2E-C686-4282-99A1-E5E2F1F5CE2D}']
    function GetProjection: IProjection;
    property Projection: IProjection read GetProjection;
  end;

  IRegionProcessParamsFrameMapCalibrationList = interface(IRegionProcessParamsFrameBase)
    ['{41A9899D-D431-4D12-8DC4-1F65B36A8CAB}']
    function GetMapCalibrationList: IMapCalibrationList;
    property MapCalibrationList: IMapCalibrationList read GetMapCalibrationList;
  end;

  IRegionProcessParamsFrameImageProvider = interface(IRegionProcessParamsFrameBase)
    ['{98A4BE9B-AF50-45F5-8E26-0DBF0F094C0B}']
    function GetProvider: IBitmapTileUniProvider;
    property Provider: IBitmapTileUniProvider read GetProvider;
  end;

  IRegionProcessParamsFrameProcessPredicate = interface(IRegionProcessParamsFrameBase)
    ['{DF8D4BBB-BA83-412A-BA70-3A1E454AD3C3}']
    function GetPredicate: IPredicateByTileInfo;
    property Predicate: IPredicateByTileInfo read GetPredicate;
  end;

  IRegionProcessParamsFrameTargetPath = interface(IRegionProcessParamsFrameBase)
    ['{A0510824-7E26-430F-9C04-AE71EBAD65FF}']
    function GetPath: string;
    property Path: string read GetPath;
  end;

  IMapCombineCustomOptions = interface
    ['{5E584132-7A0B-4A8A-A79E-005329AF1821}']
    function GetQuality: Integer;
    property Quality: Integer read GetQuality;

    function GetIsSaveGeoRefInfoToExif: Boolean;
    property IsSaveGeoRefInfoToExif: Boolean read GetIsSaveGeoRefInfoToExif;

    function GetThreadCount: Integer;
    property ThreadCount: Integer read GetThreadCount;

    function GetIsSaveAlfa: Boolean;
    property IsSaveAlfa: Boolean read GetIsSaveAlfa;

    function GetGeoTiffCompression: TGeoTiffCompression;
    property GeoTiffCompression: TGeoTiffCompression read GetGeoTiffCompression;

    function GetGeoTiffFormat: TGeoTiffFileFormat;
    property GeoTiffFormat: TGeoTiffFileFormat read GetGeoTiffFormat;
  end;

  IRegionProcessParamsFrameMapCombine = interface(IRegionProcessParamsFrameBase)
    ['{6771DEDD-F33C-4152-B4AB-47E6A0B032E1}']
    function GetUseMarks: Boolean;
    property UseMarks: Boolean read GetUseMarks;

    function GetUseGrids: Boolean;
    property UseGrids: Boolean read GetUseGrids;

    function GetUseFillingMap: Boolean;
    property UseFillingMap: Boolean read GetUseFillingMap;

    function GetUseRecolor: Boolean;
    property UseRecolor: Boolean read GetUseRecolor;

    function GetSplitCount: TPoint;
    property SplitCount: TPoint read GetSplitCount;

    function GetSkipExistingFiles: Boolean;
    property SkipExistingFiles: Boolean read GetSkipExistingFiles;

    function GetBGColor: TColor32;
    property BGColor: TColor32 read GetBGColor;

    function GetCustomOptions: IMapCombineCustomOptions;
    property CustomOptions: IMapCombineCustomOptions read GetCustomOptions;
  end;

implementation

end.
