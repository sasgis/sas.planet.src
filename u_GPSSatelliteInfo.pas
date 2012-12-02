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

unit u_GPSSatelliteInfo;

interface

uses
  i_GPS,
  vsagps_public_base,
  u_BaseInterfacedObject;

type
  TGPSSatelliteInfo = class(TBaseInterfacedObject, IGPSSatelliteInfo)
  private
    FSingleSatFixibilityData: TSingleSatFixibilityData;
    FSingleSatSkyData: TSingleSatSkyData;
  private
    procedure GetBaseSatelliteParams(AParams: PSingleSatFixibilityData); stdcall;
    procedure GetSkySatelliteParams(AParams: PSingleSatSkyData); stdcall;
  public
    constructor Create(
      const ASingleSatFixibilityData: PSingleSatFixibilityData;
      const ASingleSatSkyData: PSingleSatSkyData
    );
    destructor Destroy; override;
  end;


implementation

{ TGPSSatelliteInfo }

constructor TGPSSatelliteInfo.Create(
  const ASingleSatFixibilityData: PSingleSatFixibilityData;
  const ASingleSatSkyData: PSingleSatSkyData
);
begin
  inherited Create;
  FSingleSatFixibilityData := ASingleSatFixibilityData^;
  FSingleSatSkyData := ASingleSatSkyData^;
end;

destructor TGPSSatelliteInfo.Destroy;
begin
  FSingleSatFixibilityData.sat_info.svid := 0;
  inherited;
end;

procedure TGPSSatelliteInfo.GetBaseSatelliteParams(AParams: PSingleSatFixibilityData);
begin
  AParams^ := FSingleSatFixibilityData;
end;

procedure TGPSSatelliteInfo.GetSkySatelliteParams(AParams: PSingleSatSkyData);
begin
  AParams^ := FSingleSatSkyData;
end;

end.
