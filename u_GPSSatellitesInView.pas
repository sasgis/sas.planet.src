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

unit u_GPSSatellitesInView;

interface

uses
  Windows,
  ActiveX,
  Classes,
  SysUtils,
  i_GPS,
  vsagps_public_base;

type
  TGPSSatellitesInView = class(TInterfacedObject, IGPSSatellitesInView)
  private
    FItemsGP: IInterfaceList;
    FItemsGL: IInterfaceList;
    FFixSatsALL: TVSAGPS_FIX_ALL;
    procedure InternalCreateItems(const AItemsCountTI: Integer;
                                  AItemsTI: PUnknownList;
                                  var AItemsIfaceTI: IInterfaceList);
  protected
    function GetCount(const ATalkerID: String): Byte; stdcall;
    function GetFixCount(const ATalkerID: String): Byte; stdcall;
    function GetItem(const ATalkerID: String; const AIndex: Byte): IGPSSatelliteInfo; stdcall;

    procedure SetFixedSats(AFixSatsALL: PVSAGPS_FIX_ALL); stdcall;
    function GetFixedSats: PVSAGPS_FIX_ALL; stdcall;

    function GetAllSatelliteParams(const AIndex: Byte;
                                   const ATalkerID: String;
                                   var AFixed: Boolean;
                                   AParams: PSingleSatFixibilityData;
                                   ASky: PSingleSatSkyData = nil): Boolean; stdcall;

    function GetPreferredTalkerID: String; stdcall;
  public
    constructor Create(
      const AItemsCountGP: Integer;
      AItemsGP: PUnknownList;
      const AItemsCountGL: Integer;
      AItemsGL: PUnknownList
    );
    destructor Destroy; override;
  end;

implementation

{ TGPSSatellitesInView }

constructor TGPSSatellitesInView.Create(
  const AItemsCountGP: Integer;
  AItemsGP: PUnknownList;
  const AItemsCountGL: Integer;
  AItemsGL: PUnknownList
);
begin
  // init
  SetFixedSats(nil);
  // make
  InternalCreateItems(AItemsCountGP, AItemsGP, FItemsGP);
  InternalCreateItems(AItemsCountGL, AItemsGL, FItemsGL);
end;

destructor TGPSSatellitesInView.Destroy;
begin
  FItemsGP := nil;
  FItemsGL := nil;
  inherited;
end;

function TGPSSatellitesInView.GetCount(const ATalkerID: String): Byte;
begin
  if SameText(ATalkerID, nmea_ti_GLONASS) then begin
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

function TGPSSatellitesInView.GetFixCount(const ATalkerID: String): Byte;
var
  p: PVSAGPS_FIX_SATS;
begin
  p:=Select_PVSAGPS_FIX_SATS_from_ALL(@FFixSatsALL, ATalkerID);
  Result:=Get_PVSAGPS_FIX_SATS_FixCount(p);
end;

function TGPSSatellitesInView.GetFixedSats: PVSAGPS_FIX_ALL;
begin
  Result:=@FFixSatsALL;
end;

function TGPSSatellitesInView.GetAllSatelliteParams(const AIndex: Byte;
                                                    const ATalkerID: String;
                                                    var AFixed: Boolean;
                                                    AParams: PSingleSatFixibilityData;
                                                    ASky: PSingleSatSkyData = nil): Boolean;
var
  VItem: IGPSSatelliteInfo;
  VSat: TVSAGPS_FIX_SAT;
begin
  Result:=FALSE;
  VItem:=GetItem(ATalkerID, AIndex);
  if Assigned(VItem) then begin
    VItem.GetBaseSatelliteParams(AParams);
    VSat:=AParams.sat_info;
    AFixed:=(GetSatNumberIndex(@FFixSatsALL, @(VSat))>=0);
    if (nil<>ASky) then
      VItem.GetSkySatelliteParams(ASky);
    Result:=TRUE;
  end;
end;

function TGPSSatellitesInView.GetItem(const ATalkerID: String; const AIndex: Byte): IGPSSatelliteInfo;
begin
  if SameText(ATalkerID, nmea_ti_GLONASS) then begin
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

function TGPSSatellitesInView.GetPreferredTalkerID: String;
begin
  if FFixSatsALL.gl.fix_count>FFixSatsALL.gp.fix_count then
    Result:=nmea_ti_GLONASS
  else
    Result:=nmea_ti_GPS;
end;

procedure TGPSSatellitesInView.InternalCreateItems(
  const AItemsCountTI: Integer;
  AItemsTI: PUnknownList;
  var AItemsIfaceTI: IInterfaceList
  );
var
  i: Integer;
  VItemCount: Integer;
  VItem: IGPSSatelliteInfo;
begin
  AItemsIfaceTI:=nil;
  
  if (AItemsCountTI > 0) and (AItemsTI <> nil) then begin
    VItemCount := AItemsCountTI;
    if VItemCount > cNmea_max_sat_count then begin
      VItemCount := cNmea_max_sat_count;
    end;

    AItemsIfaceTI := TInterfaceList.Create;
    AItemsIfaceTI.Capacity := VItemCount;

    for i := 0 to VItemCount - 1 do begin
      VItem := IGPSSatelliteInfo(AItemsTI^[i]);
      AItemsIfaceTI.Add(VItem);
    end;
  end;
end;

procedure TGPSSatellitesInView.SetFixedSats(AFixSatsALL: PVSAGPS_FIX_ALL);
begin
  if (nil=AFixSatsALL) then
    ZeroMemory(@FFixSatsALL, sizeof(FFixSatsALL))
  else
    FFixSatsALL := AFixSatsALL^;
end;

end.
