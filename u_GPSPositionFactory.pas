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

unit u_GPSPositionFactory;

interface

uses
  ActiveX,
  i_GPSPositionFactory,
  i_GPS,
  vsagps_public_base,
  vsagps_public_position,
  vsagps_public_events,
  u_BaseInterfacedObject;

type
  TGPSPositionFactory = class(TBaseInterfacedObject, IGPSPositionFactory)
  private
    FSatellitesInViewEmpty: IGPSSatellitesInView;
    FPositionEmpty: IGPSPosition;
    FExecuteGPSCommandEvent: TExecuteGPSCommandEvent;
    FGPSUnitInfoChangedEvent: TVSAGPS_UNIT_INFO_Changed_Event;
  private
    function BuildSatelliteInfo(
      const AData: PSingleSatFixibilityData;
      const ASky: PSingleSatSkyData
    ): IGPSSatelliteInfo;

    function BuildSatellitesInViewEmpty: IGPSSatellitesInView;
    function BuildSatellitesInView(
      const AItemsCountGP: Integer;
      const AItemsGP: PUnknownList;
      const AItemsCountGL: Integer;
      const AItemsGL: PUnknownList
    ): IGPSSatellitesInView;

    function BuildPositionEmpty: IGPSPosition;
    function BuildPosition(
      const ASingleGPSData: PSingleGPSData;
      const ASatellites: IGPSSatellitesInView
    ): IGPSPosition;

    function ExecuteGPSCommand(
      Sender: TObject;
      const AUnitIndex: Byte;
      const ACommand: LongInt;
      const APointer: Pointer
    ): AnsiString;

    procedure SetExecuteGPSCommandHandler(AExecuteGPSCommandEvent: TExecuteGPSCommandEvent);

    procedure SetGPSUnitInfoChangedHandler(AGPSUnitInfoChangedEvent: TVSAGPS_UNIT_INFO_Changed_Event);
    function GetGPSUnitInfoChangedHandler: TVSAGPS_UNIT_INFO_Changed_Event;
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
  FExecuteGPSCommandEvent := nil;
  FGPSUnitInfoChangedEvent := nil;
  FSatellitesInViewEmpty := TGPSSatellitesInView.Create(0, nil, 0, nil);
  FPositionEmpty :=
    TGPSPositionStatic.Create(nil, FSatellitesInViewEmpty);
end;

function TGPSPositionFactory.ExecuteGPSCommand(
  Sender: TObject;
  const AUnitIndex: Byte;
  const ACommand: Integer;
  const APointer: Pointer
): AnsiString;
begin
  if Assigned(FExecuteGPSCommandEvent) then begin
    Result := FExecuteGPSCommandEvent(Sender, AUnitIndex, ACommand, APointer);
  end else begin
    Result := '';
  end;
end;

function TGPSPositionFactory.GetGPSUnitInfoChangedHandler: TVSAGPS_UNIT_INFO_Changed_Event;
begin
  Result := FGPSUnitInfoChangedEvent;
end;

procedure TGPSPositionFactory.SetExecuteGPSCommandHandler(AExecuteGPSCommandEvent: TExecuteGPSCommandEvent);
begin
  FExecuteGPSCommandEvent := AExecuteGPSCommandEvent;
end;

procedure TGPSPositionFactory.SetGPSUnitInfoChangedHandler(AGPSUnitInfoChangedEvent: TVSAGPS_UNIT_INFO_Changed_Event);
begin
  FGPSUnitInfoChangedEvent := AGPSUnitInfoChangedEvent;
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
  const AItemsCountGP: Integer;
  const AItemsGP: PUnknownList;
  const AItemsCountGL: Integer;
  const AItemsGL: PUnknownList
): IGPSSatellitesInView;
begin
  Result :=
    TGPSSatellitesInView.Create(
      AItemsCountGP,
      AItemsGP,
      AItemsCountGL,
      AItemsGL
    );
end;

function TGPSPositionFactory.BuildSatellitesInViewEmpty: IGPSSatellitesInView;
begin
  Result := FSatellitesInViewEmpty;
end;

end.
