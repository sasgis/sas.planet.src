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
  t_GeoTypes,
  i_GPS,
  u_BaseInterfacedObject;

type
  TGPSPositionStatic = class(TBaseInterfacedObject, IGPSPosition)
  private
    FSingleGPSData: TSingleGPSData;
    FSatellites: IGPSSatellitesInView;
  private
    function GetLonLat: TDoublePoint;
    function GetAltitude: Double;
    function GetGeoidHeight: Double;
    function GetSpeed_KMH: Double;   // in km/h
    function GetHeading: Double;     // true
    function GetUTCTime: TDateTime;
    function GetHDOP: Double;
    function GetVDOP: Double;
    function GetPDOP: Double;
    function GetDGPS: string;
    function GetPositionOK: Boolean;
    function GetUTCTimeOK: Boolean;
    function GetSpeedOK: Boolean;
    function GetSatellites: IGPSSatellitesInView; stdcall;
  public
    constructor Create(
      const ASingleGPSData: PSingleGPSData;
      const ASatellites: IGPSSatellitesInView
    );
  end;

implementation

uses
  SysUtils,
  Math,
  u_GeoToStrFunc;

{ TGPSPosition }

constructor TGPSPositionStatic.Create(
  const ASingleGPSData: PSingleGPSData;
  const ASatellites: IGPSSatellitesInView
);
begin
  inherited Create;
  if (ASingleGPSData = nil) then begin
    FSingleGPSData.Init;
  end else begin
    FSingleGPSData := ASingleGPSData^;
  end;

  FSatellites := ASatellites;

  FSingleGPSData.PositionOK :=
    FSingleGPSData.PositionOK and
    not NoData_Float64(FSingleGPSData.PositionLon) and 
    not NoData_Float64(FSingleGPSData.PositionLat);
    
  if not FSingleGPSData.PositionOK then begin
    FSingleGPSData.PositionLon := NaN;
    FSingleGPSData.PositionLat := NaN;
  end;

  FSingleGPSData.VSpeedOK :=
    not NoData_Float64(FSingleGPSData.Speed_KMH)and
    not NoData_Float64(FSingleGPSData.Heading);

  if not FSingleGPSData.VSpeedOK then begin
    FSingleGPSData.Speed_KMH := NaN;
    FSingleGPSData.Heading := NaN;
  end;

  FSingleGPSData.UTCTimeOK := 
    FSingleGPSData.UTCTimeOK and
    FSingleGPSData.UTCDateOK and
    not NoData_Float64(FSingleGPSData.UTCDate)and
    not NoData_Float64(FSingleGPSData.UTCTime);

  if FSingleGPSData.UTCTimeOK then begin
    FSingleGPSData.UTCTime := FSingleGPSData.UTCDate + FSingleGPSData.UTCTime;
  end else begin
    FSingleGPSData.UTCTime := 0;
  end;

  if NoData_Float64(FSingleGPSData.Altitude) then begin
    FSingleGPSData.Altitude := NaN;
  end;
  
  if NoData_Float64(FSingleGPSData.GeoidHeight) then begin
    FSingleGPSData.GeoidHeight := NaN;
  end;

  if NoData_Float64(FSingleGPSData.HDOP) then begin
    FSingleGPSData.HDOP := NaN;
  end;

  if NoData_Float64(FSingleGPSData.VDOP) then begin
    FSingleGPSData.VDOP := NaN;
  end;

  if NoData_Float64(FSingleGPSData.PDOP) then begin
    FSingleGPSData.PDOP := NaN;
  end;
end;

function TGPSPositionStatic.GetAltitude: Double;
begin
  Result := FSingleGPSData.Altitude;
end;

function TGPSPositionStatic.GetDGPS: string;
begin
  with FSingleGPSData.DGPS do begin
    case Nmea23_Mode of
      'A': begin
        Result := 'A';
      end; //'Autonomous';
      'D': begin
        Result := 'DGPS';
      end; //'DGPS';
      'E': begin
        Result := 'DR';
      end; //'Dead Reckoning';
      'R': begin
        Result := 'CP';
      end; //'Coarse Position';
      'P': begin
        Result := 'PPS';
      end; //'PPS';
    else begin
      Result := 'N';
    end; //#0 if no data or 'N' = Not Valid
    end;

    if (Dimentions > 1) then begin
      Result := Result + ' (' + IntToStr(Dimentions) + 'D)';
    end;

    if (not NoData_Float32(DGPS_Age_Second)) then begin
      if (DGPS_Age_Second > 0) then begin
        Result := Result + ': ' + RoundEx(DGPS_Age_Second, 2);
      end;
      if (DGPS_Station_ID > 0) then begin
        Result := Result + ' #' + IntToStr(DGPS_Station_ID);
      end;
    end;
  end;
end;

function TGPSPositionStatic.GetGeoidHeight: Double;
begin
  Result := FSingleGPSData.GeoidHeight;
end;

function TGPSPositionStatic.GetHDOP: Double;
begin
  Result := FSingleGPSData.HDOP;
end;

function TGPSPositionStatic.GetHeading: Double;
begin
  Result := FSingleGPSData.Heading;
end;

function TGPSPositionStatic.GetLonLat: TDoublePoint;
begin
  Result.X := FSingleGPSData.PositionLon;
  Result.Y := FSingleGPSData.PositionLat;
end;

function TGPSPositionStatic.GetPDOP: Double;
begin
  Result := FSingleGPSData.PDOP;
end;

function TGPSPositionStatic.GetPositionOK: Boolean;
begin
  Result := FSingleGPSData.PositionOK;
end;

function TGPSPositionStatic.GetSatellites: IGPSSatellitesInView;
begin
  Result := FSatellites;
end;

function TGPSPositionStatic.GetSpeedOK: Boolean;
begin
  Result := FSingleGPSData.VSpeedOK;
end;

function TGPSPositionStatic.GetSpeed_KMH: Double;
begin
  Result := FSingleGPSData.Speed_KMH;
end;

function TGPSPositionStatic.GetUTCTime: TDateTime;
begin
  Result := FSingleGPSData.UTCTime;
end;

function TGPSPositionStatic.GetUTCTimeOK: Boolean;
begin
  Result := FSingleGPSData.UTCTimeOK and FSingleGPSData.UTCDateOK;
end;

function TGPSPositionStatic.GetVDOP: Double;
begin
  Result := FSingleGPSData.VDOP;
end;

end.
