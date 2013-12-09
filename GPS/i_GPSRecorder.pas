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

unit i_GPSRecorder;

interface

uses
  t_GeoTypes,
  i_GPS,
  i_GeometryLonLat,
  i_Changeable;

type
  TGPSTrackPoint = record
    Point: TDoublePoint;
    Speed: Double;
    Time: TDateTime;
  end;
  PTrackPointArray = ^TTrackPointArray;
  TTrackPointArray = array [0..0] of TGPSTrackPoint;

  IEnumGPSTrackPoint = interface
    ['{9BD74D0A-BB44-4A63-A689-748F082CC3A1}']
    function Next(out APoint: TGPSTrackPoint): Boolean;
  end;

  IGpsTrackRecorder = interface(IChangeable)
    ['{DB8E0A07-EF6B-40A5-AF00-5A5028025CEE}']
    procedure ClearTrack;
    function IsEmpty: Boolean;
    function LastPoints(const AMaxCount: Integer): IEnumGPSTrackPoint;
    function GetAllPoints: IGeometryLonLatMultiLine;
  end;

  IGpsTrackRecorderInternal = interface(IGpsTrackRecorder)
    procedure AddPoint(const APosition: IGPSPosition);
    procedure AddEmptyPoint;
    procedure Save;
    procedure Load;
  end;

  IGPSRecorder = interface(IChangeable)
    ['{E8525CFD-243B-4454-82AA-C66108A74B8F}']
    function GetOdometer1: Double;
    property Odometer1: Double read GetOdometer1;
    procedure ResetOdometer1;

    function GetOdometer2: Double;
    property Odometer2: Double read GetOdometer2;
    procedure ResetOdometer2;

    function GetDist: Double;
    property Dist: Double read GetDist;
    procedure ResetDist;

    function GetMaxSpeed: Double;
    property MaxSpeed: Double read GetMaxSpeed;
    procedure ResetMaxSpeed;

    function GetAvgSpeed: Double;
    property AvgSpeed: Double read GetAvgSpeed;
    procedure ResetAvgSpeed;

    function GetLastSpeed: Double;
    property LastSpeed: Double read GetLastSpeed;

    function GetLastAltitude: Double;
    property LastAltitude: Double read GetLastAltitude;

    function GetLastHeading: Double;
    property LastHeading: Double read GetLastHeading;

    function GetLastPosition: TDoublePoint;
    property LastPosition: TDoublePoint read GetLastPosition;

    function GetCurrentPosition: IGPSPosition;
    property CurrentPosition: IGPSPosition read GetCurrentPosition;
  end;

  IGPSRecorderInternal = interface(IGPSRecorder)
    procedure AddPoint(const APosition: IGPSPosition);
    procedure AddEmptyPoint;
    procedure Save;
    procedure Load;
  end;


implementation

end.
