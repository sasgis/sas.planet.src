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
  i_LonLatRect,
  i_GeoCoder,
  u_BaseInterfacedObject;

type
  TGeoCodePlacemark = class(TBaseInterfacedObject, IGeoCodePlacemark)
  private
    FLLRect: ILonLatRect;
    FAddress: WideString;
    FDesc: WideString;
    FFullDesc: WideString;
    FAccuracy: Integer;
  private
    function GetPoint: TDoublePoint;
    function GetName: string;
    function GetDesc: string;
    function GetLLRect: ILonLatRect;
    function GetHintText: string;
    function GetInfoHTML: string;
    function GetInfoUrl: string;
    function GetInfoCaption: string;
  private
    function GetAccuracy: Integer; safecall;
  public
    constructor Create(
      const APoint: TDoublePoint;
      const AAddress: WideString;
      const ADesc: WideString;
      const AFullDesc: WideString;
      AAccuracy: Integer
    );
  end;

implementation

uses
  u_LonLatRectByPoint;

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
  FLLRect := TLonLatRectByPoint.Create(APoint);
  FAccuracy := AAccuracy;
end;

function TGeoCodePlacemark.GetAccuracy: Integer;
begin
  Result := FAccuracy;
end;

function TGeoCodePlacemark.GetName: string;
begin
  Result := FAddress;
end;

function TGeoCodePlacemark.GetDesc: string;
begin
  Result := FDesc;
end;

function TGeoCodePlacemark.GetHintText: string;
begin
  Result := FDesc;
end;

function TGeoCodePlacemark.GetInfoCaption: string;
begin
  Result := FAddress;
end;

function TGeoCodePlacemark.GetInfoHTML: string;
begin
  Result := FFullDesc;
end;

function TGeoCodePlacemark.GetInfoUrl: string;
begin
  Result := '';
end;

function TGeoCodePlacemark.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

function TGeoCodePlacemark.GetPoint: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

end.
