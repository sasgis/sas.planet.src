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

unit u_GPSPositionFactory;

interface

uses
  i_GPSPositionFactory,
  i_GPS,
  vsagps_public_base,
  vsagps_public_position,
  u_BaseInterfacedObject;

type
  TGPSPositionFactory = class(TBaseInterfacedObject, IGPSPositionFactory)
  private
    FSatellitesInViewEmpty: IGPSSatellitesInView;
    FPositionEmpty: IGPSPosition;
  private
    function BuildSatelliteInfo(
      const AData: PSingleSatFixibilityData;
      const ASky: PSingleSatSkyData
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      const AItemsGP, AItemsGL: IGPSSatelliteInfoList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
      const ASingleGPSData: PSingleGPSData;
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
begin
  inherited Create;
  FSatellitesInViewEmpty := TGPSSatellitesInView.Create(nil, nil);
  FPositionEmpty :=
    TGPSPositionStatic.Create(nil, FSatellitesInViewEmpty);
end;

function TGPSPositionFactory.BuildPosition(
  const ASingleGPSData: PSingleGPSData;
  const ASatellites: IGPSSatellitesInView
): IGPSPosition;
begin
  Result :=
    TGPSPositionStatic.Create(
      ASingleGPSData,
      ASatellites
    );
end;

function TGPSPositionFactory.BuildPositionEmpty: IGPSPosition;
begin
  Result := FPositionEmpty;
end;

function TGPSPositionFactory.BuildSatelliteInfo(
  const AData: PSingleSatFixibilityData;
  const ASky: PSingleSatSkyData
): IGPSSatelliteInfo;
begin
  Result :=
    TGPSSatelliteInfo.Create(
      AData,
      ASky
    );
end;

function TGPSPositionFactory.BuildSatellitesInView(
  const AItemsGP, AItemsGL: IGPSSatelliteInfoList
): IGPSSatellitesInView;
begin
  Result :=
    TGPSSatellitesInView.Create(
      AItemsGP,
      AItemsGL
    );
end;

function TGPSPositionFactory.BuildSatellitesInViewEmpty: IGPSSatellitesInView;
begin
  Result := FSatellitesInViewEmpty;
end;

end.
