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

unit u_GPSPositionFactory;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_GPSPositionFactory,
  i_GPS;

type
  TGPSPositionFactory = class(TInterfacedObject, IGPSPositionFactory)
  private
    FSatellitesInViewEmpty: IGPSSatellitesInView;
    FPositionEmpty: IGPSPosition;
  protected
    function BuildSatelliteInfo(
      const APseudoRandomCode: Integer;
      const AElevation: Integer;
      const AAzimuth: Integer;
      const ASignalToNoiseRatio: Integer;
      const AIsFix: Boolean
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      const AFixCount: Integer;
      const AItemsCount: Integer;
      const AItems: PUnknownList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
      const APosition: TDoublePoint;
      const AAltitude: Double;
      const ASpeed_KMH: Double;
      const AHeading: Double;
      const AUTCDateTime: TDateTime;
      const ALocalDateTime: TDateTime;
      const AIsFix: Word;
      const AHDOP: Double;
      const AVDOP: Double;
      const APDOP: Double;
      const ASatellites: IGPSSatellitesInView
    ): IGPSPosition;
  public
    constructor Create;
  end;

implementation

uses
  u_GPSPositionStatic,
  u_GPSSatellitesInView,
  u_GPSSatelliteInfo;

{ TGPSPositionFactory }

constructor TGPSPositionFactory.Create;
var
  VPoint: TDoublePoint;
begin
  FSatellitesInViewEmpty := TGPSSatellitesInView.Create(0, 0, nil);

  VPoint.X := 0;
  VPoint.Y := 0;
  FPositionEmpty :=
    TGPSPositionStatic.Create(
      VPoint, 0, 0, 0, 0, 0, 0, 0, 0, 0, FSatellitesInViewEmpty
    );
end;

function TGPSPositionFactory.BuildPosition(
  const APosition: TDoublePoint;
  const AAltitude, ASpeed_KMH, AHeading: Double;
  const AUTCDateTime, ALocalDateTime: TDateTime;
  const AIsFix: Word;
  const AHDOP, AVDOP, APDOP: Double;
  const ASatellites: IGPSSatellitesInView
): IGPSPosition;
begin
  Result :=
    TGPSPositionStatic.Create(
      APosition,
      AAltitude,
      ASpeed_KMH,
      AHeading,
      AUTCDateTime,
      ALocalDateTime,
      AIsFix,
      AHDOP,
      AVDOP,
      APDOP,
      ASatellites
    );
end;

function TGPSPositionFactory.BuildPositionEmpty: IGPSPosition;
begin
  Result := FPositionEmpty;
end;

function TGPSPositionFactory.BuildSatelliteInfo(
  const APseudoRandomCode, AElevation, AAzimuth, ASignalToNoiseRatio: Integer;
  const AIsFix: Boolean
): IGPSSatelliteInfo;
begin
  Result :=
    TGPSSatelliteInfo.Create(
      APseudoRandomCode,
      AElevation,
      AAzimuth,
      ASignalToNoiseRatio,
      AIsFix
    );
end;

function TGPSPositionFactory.BuildSatellitesInView(
  const AFixCount, AItemsCount: Integer;
  const AItems: PUnknownList
): IGPSSatellitesInView;
begin
  Result :=
    TGPSSatellitesInView.Create(
      AFixCount,
      AItemsCount,
      AItems
    )
end;

function TGPSPositionFactory.BuildSatellitesInViewEmpty: IGPSSatellitesInView;
begin
  Result := FSatellitesInViewEmpty;
end;

end.
