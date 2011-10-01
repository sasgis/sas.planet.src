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
  ActiveX,
  Classes,
  SysUtils,
  i_GPS;

type
  TGPSSatellitesInView = class(TInterfacedObject, IGPSSatellitesInView)
  private
    FFixCount: Integer;
    FItems: IInterfaceList;
  protected
    function GetCount: Integer; stdcall;
    function GetFixCount: Integer; stdcall;
    function GetItem(AIndex: Integer): IGPSSatelliteInfo; stdcall;
  public
    constructor Create(
      AFixCount: Integer;
      AItemsCount: Integer;
      AItems: PUnknownList
    );
    destructor Destroy; override;
  end;

implementation

const
  CMaxSatellitesCount = 32;

{ TGPSSatellitesInView }

constructor TGPSSatellitesInView.Create(
  AFixCount: Integer;
  AItemsCount: Integer;
  AItems: PUnknownList
);
var
  i: Integer;
  VItemCount: Integer;
  VItem: IGPSSatelliteInfo;
begin
  if (AItemsCount > 0) and (AItems <> nil) then begin
    VItemCount := AItemsCount;
    if VItemCount > CMaxSatellitesCount then begin
      VItemCount := CMaxSatellitesCount;
    end;

    FItems := TInterfaceList.Create;
    FItems.Capacity := VItemCount;

    for i := 0 to VItemCount - 1 do begin
      VItem := IGPSSatelliteInfo(AItems^[i]);
      FItems.Add(VItem);
    end;
    FFixCount := AFixCount;
    if FItems.Count < FFixCount then begin
      FFixCount := FItems.Count;
    end;
  end else begin
    FItems := nil;
    FFixCount := 0;
  end;
end;

destructor TGPSSatellitesInView.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TGPSSatellitesInView.GetCount: Integer;
begin
  if FItems <> nil then begin
    Result := FItems.Count;
  end else begin
    Result := 0;
  end;
end;

function TGPSSatellitesInView.GetFixCount: Integer;
begin
  Result := FFixCount;
end;

function TGPSSatellitesInView.GetItem(AIndex: Integer): IGPSSatelliteInfo;
begin
  if FItems <> nil then begin
    Result := IGPSSatelliteInfo(FItems[AIndex]);
  end else begin
    Result := nil;
  end;
end;

end.
