{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit i_BitmapMapCombiner;

interface

uses
  Types,
  t_CommonTypes,
  t_MapCombineOptions,
  i_NotifierOperation,
  i_GeometryLonLat,
  i_GeometryProjected,
  i_RegionProcessParamsFrame,
  i_BitmapTileProvider;

type
  IBitmapCombineProgressUpdate = interface
    ['{79F63F0C-4B11-44AB-BCA0-E242E58FCE6B}']
    procedure Update(AProgress: Double);
  end;

  IBitmapMapCombiner = interface
    ['{AAE17956-FD14-4426-AD21-BB02A412FF4B}']
    procedure SaveRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AFileName: string;
      const AImageProvider: IBitmapTileProvider;
      const APolygon: IGeometryProjectedPolygon;
      const AMapRect: TRect
    );
  end;

  IBitmapMapCombinerFactory = interface
    ['{52A37CEB-7A4B-4C6D-A757-E65BFD1BC8D9}']

    function GetMinPartSize: TPoint;
    property MinPartSize: TPoint read GetMinPartSize;

    function GetMaxPartSize: TPoint;
    property MaxPartSize: TPoint read GetMaxPartSize;

    function GetCombinePathStringTypeSupport: TStringTypeSupport;
    property CombinePathStringTypeSupport: TStringTypeSupport read GetCombinePathStringTypeSupport;

    function GetDefaultExt: string;
    property DefaultExt: string read GetDefaultExt;

    function GetFormatName: string;
    property FormatName: string read GetFormatName;

    function GetOptionsSet: TMapCombineOptionsSet;
    property OptionsSet: TMapCombineOptionsSet read GetOptionsSet;

    function Validate(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const APolygon: IGeometryLonLatPolygon
    ): Boolean;

    function PrepareMapCombiner(
      const AParams: IRegionProcessParamsFrameMapCombine;
      const AProgressInfo: IBitmapCombineProgressUpdate
    ): IBitmapMapCombiner;
  end;

implementation

end.
