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

unit u_GPSSatellitesInView;

interface

uses
  Windows,
  vsagps_public_base,
  i_InterfaceListStatic,
  i_GPS,
  u_BaseInterfacedObject;

type
  TGPSSatellitesInView = class(TBaseInterfacedObject, IGPSSatellitesInView)
  private
    FItemsGP: IInterfaceListStatic;
    FItemsGL: IInterfaceListStatic;
    FFixSatsALL: TVSAGPS_FIX_ALL;
    function InternalCreateItems(
      const ASource: IGPSSatelliteInfoList
    ): IInterfaceListStatic;
  private
    function GetCount(const ATalkerID: AnsiString): Byte; stdcall;
    function GetFixCount(const ATalkerID: AnsiString): Byte; stdcall;
    function GetItem(
      const ATalkerID: AnsiString;
      const AIndex: Byte
    ): IGPSSatelliteInfo; stdcall;

    procedure SetFixedSats(AFixSatsALL: PVSAGPS_FIX_ALL); stdcall;
    function GetFixedSats: PVSAGPS_FIX_ALL; stdcall;

    function GetAllSatelliteParams(
      const AIndex: Byte;
      const ATalkerID: AnsiString;
      var AFixed: Boolean;
      AParams: PSingleSatFixibilityData;
      ASky: PSingleSatSkyData = nil
    ): Boolean; stdcall;

    function EnumerateTalkerID(var ATalkerID: AnsiString): Boolean; stdcall;
    function GetCountForAllTalkerIDs(const AOnlyForFixed: Boolean): Byte; stdcall;
  public
    constructor Create(
      const AItemsGP, AItemsGL: IGPSSatelliteInfoList
    );
  end;

implementation

uses
  ALString,
  i_InterfaceListSimple,
  u_InterfaceListSimple;

{ TGPSSatellitesInView }

constructor TGPSSatellitesInView.Create(
  const AItemsGP, AItemsGL: IGPSSatelliteInfoList
);
begin
  inherited Create;
  // init
  SetFixedSats(nil);
  // make
  FItemsGP := InternalCreateItems(AItemsGP);
  FItemsGL := InternalCreateItems(AItemsGL);
end;

function TGPSSatellitesInView.EnumerateTalkerID(var ATalkerID: AnsiString): Boolean;
begin
  if (0 = Length(ATalkerID)) then begin
    // get first talker_id

    // check gps
    if (nil <> FItemsGP) and (0 < FItemsGP.Count) then begin
      // GPS
      ATalkerID := nmea_ti_GPS;
      Result := TRUE;
      Exit;
    end;

    // check glonass
    if (nil <> FItemsGL) and (0 < FItemsGL.Count) then begin
      // GPS
      ATalkerID := nmea_ti_GLONASS;
      Result := TRUE;
      Exit;
    end;
  end else begin
    // get next talker_id

    // check glonass after gps (the only)
    if ALSameText(ATalkerID, nmea_ti_GPS) then begin
      // check for glonass
      if (nil <> FItemsGL) and (0 < FItemsGL.Count) then begin
        // GPS
        ATalkerID := nmea_ti_GLONASS;
        Result := TRUE;
        Exit;
      end;
    end;
  end;

  // nothing
  Result := FALSE;
end;

function TGPSSatellitesInView.GetCount(const ATalkerID: AnsiString): Byte;
begin
  if ALSameText(ATalkerID, nmea_ti_GLONASS) then begin
    // glonass
    if FItemsGL <> nil then begin
      Result := FItemsGL.Count;
    end else begin
      Result := 0;
    end;
  end else begin
    // gps
    if FItemsGP <> nil then begin
      Result := FItemsGP.Count;
    end else begin
      Result := 0;
    end;
  end;
end;

function TGPSSatellitesInView.GetCountForAllTalkerIDs(const AOnlyForFixed: Boolean): Byte;
begin
  if AOnlyForFixed then begin
    // all fixed satellites
    Result := Get_PVSAGPS_FIX_SATS_FixCount(@(FFixSatsALL.gp)) + Get_PVSAGPS_FIX_SATS_FixCount(@(FFixSatsALL.gl));
  end else begin
    // all satelites
    Result := 0;
    if (nil <> FItemsGP) then begin
      Inc(Result, FItemsGP.Count);
    end;
    if (nil <> FItemsGL) then begin
      Inc(Result, FItemsGL.Count);
    end;
  end;
end;

function TGPSSatellitesInView.GetFixCount(const ATalkerID: AnsiString): Byte;
var
  p: PVSAGPS_FIX_SATS;
begin
  p := Select_PVSAGPS_FIX_SATS_from_ALL(@FFixSatsALL, ATalkerID);
  Result := Get_PVSAGPS_FIX_SATS_FixCount(p);
end;

function TGPSSatellitesInView.GetFixedSats: PVSAGPS_FIX_ALL;
begin
  Result := @FFixSatsALL;
end;

function TGPSSatellitesInView.GetAllSatelliteParams(
  const AIndex: Byte;
  const ATalkerID: AnsiString;
  var AFixed: Boolean;
  AParams: PSingleSatFixibilityData;
  ASky: PSingleSatSkyData = nil
): Boolean;
var
  VItem: IGPSSatelliteInfo;
  VSat: TVSAGPS_FIX_SAT;
  Vresult_index: ShortInt;
begin
  Result := FALSE;
  VItem := GetItem(ATalkerID, AIndex);
  if Assigned(VItem) then begin
    VItem.GetBaseSatelliteParams(AParams);
    VSat := AParams.sat_info;
    AFixed := GetSatNumberIndexEx(Select_PVSAGPS_FIX_SATS_from_ALL(@FFixSatsALL, ATalkerID), @(VSat), Vresult_index);
    if (nil <> ASky) then begin
      VItem.GetSkySatelliteParams(ASky);
    end;
    Result := TRUE;
  end;
end;

function TGPSSatellitesInView.GetItem(
  const ATalkerID: AnsiString;
  const AIndex: Byte
): IGPSSatelliteInfo;
begin
  if ALSameText(ATalkerID, nmea_ti_GLONASS) then begin
    // glonass
    if FItemsGL <> nil then begin
      Result := IGPSSatelliteInfo(FItemsGL[AIndex]);
    end else begin
      Result := nil;
    end;
  end else begin
    // gps
    if FItemsGP <> nil then begin
      Result := IGPSSatelliteInfo(FItemsGP[AIndex]);
    end else begin
      Result := nil;
    end;
  end;
end;

function TGPSSatellitesInView.InternalCreateItems(
  const ASource: IGPSSatelliteInfoList
): IInterfaceListStatic;
var
  i: Integer;
  VItemCount: Integer;
  VItem: IGPSSatelliteInfo;
  VList: IInterfaceListSimple;
begin
  Result := nil;

  if (nil=ASource) then
    Exit;

  VItemCount := ASource.Count;
  if (0=VItemCount) then
    Exit;

  if VItemCount > cNmea_max_sat_count then begin
    VItemCount := cNmea_max_sat_count;
  end;

  VList := TInterfaceListSimple.Create;
  VList.Capacity := VItemCount;

  for i := 0 to VItemCount - 1 do begin
    VItem := ASource[i];
    VList.Add(VItem);
  end;
  Result := VList.MakeStaticAndClear;
end;

procedure TGPSSatellitesInView.SetFixedSats(AFixSatsALL: PVSAGPS_FIX_ALL);
begin
  if (nil = AFixSatsALL) then begin
    ZeroMemory(@FFixSatsALL, sizeof(FFixSatsALL));
  end else begin
    FFixSatsALL := AFixSatsALL^;
  end;
end;

end.
