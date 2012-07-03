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

unit u_GPSPositionStatic;

interface

uses
  vsagps_public_base,
  vsagps_public_position,
  i_GPS;

type
  TGPSPositionStatic = class(TInterfacedObject, IGPSPosition)
  private
    FSingleGPSData: TSingleGPSData;
    FSatellites: IGPSSatellitesInView;
  private
    function GetPosParams: PSingleGPSData; stdcall;

    function GetTracksParams(
      var pPos: PSingleGPSData;
      var pSatFixAll: PVSAGPS_FIX_ALL
    ): Boolean; stdcall;

    function GetSatellites: IGPSSatellitesInView; stdcall;
  public
    constructor Create(
      const ASingleGPSData: PSingleGPSData;
      const ASatellites: IGPSSatellitesInView
    );
    destructor Destroy; override;
  end;

implementation

{ TGPSPosition }

constructor TGPSPositionStatic.Create(
  const ASingleGPSData: PSingleGPSData;
  const ASatellites: IGPSSatellitesInView
);
begin
  inherited Create;
  if (nil = ASingleGPSData) then begin
    InitSingleGPSData(@FSingleGPSData);
  end else begin
    FSingleGPSData := ASingleGPSData^;
  end;

  FSatellites := ASatellites;
end;

destructor TGPSPositionStatic.Destroy;
begin
  FSatellites := nil;
  inherited;
end;

function TGPSPositionStatic.GetPosParams: PSingleGPSData;
begin
  Result := @FSingleGPSData;
end;

function TGPSPositionStatic.GetSatellites: IGPSSatellitesInView;
begin
  Result := FSatellites;
end;

function TGPSPositionStatic.GetTracksParams(
  var pPos: PSingleGPSData;
  var pSatFixAll: PVSAGPS_FIX_ALL
): Boolean;
begin
  Result := TRUE;
  pPos := @FSingleGPSData;
  if Assigned(FSatellites) then begin
    pSatFixAll := FSatellites.GetFixedSats;
  end else begin
    pSatFixAll := nil;
  end;
end;

end.
