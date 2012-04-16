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

unit u_GeoCodePlacemark;

interface

uses
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCodePlacemark = class(TInterfacedObject, IGeoCodePlacemark)
  private
    FPoint: TDoublePoint;
    FAddress: WideString;
    FDesc: WideString;
    FFullDesc: WideString;
    FAccuracy: Integer;
    function GetPoint: TDoublePoint; safecall;
    function GetAddress: WideString; safecall;
    function GetDesc: WideString; safecall;
    function GetFullDesc: WideString; safecall;
    function GetAccuracy: Integer; safecall;
  public
    constructor Create(
      const APoint: TDoublePoint;
      const AAddress: WideString;
      const ADesc: WideString;
      const AFullDesc: WideString;
      AAccuracy: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGeoCodePlacemark }

constructor TGeoCodePlacemark.Create(
  const APoint: TDoublePoint;
  const AAddress: WideString;
  const ADesc: WideString;
  const AFullDesc: WideString;
  AAccuracy: Integer
);
begin
  inherited Create;
  FAddress := AAddress;
  FDesc := ADesc;
  FFullDesc := AFullDesc;
  FPoint := APoint;
  FAccuracy := AAccuracy;
end;

destructor TGeoCodePlacemark.Destroy;
begin
  FAddress := '';
  inherited;
end;

function TGeoCodePlacemark.GetAccuracy: Integer;
begin
  Result := FAccuracy;
end;

function TGeoCodePlacemark.GetAddress: WideString;
begin
  Result := FAddress;
end;

function TGeoCodePlacemark.GetDesc: WideString;
begin
  Result := FDesc;
end;

function TGeoCodePlacemark.GetFullDesc: WideString;
begin
  Result := FFullDesc;
end;

function TGeoCodePlacemark.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

end.
