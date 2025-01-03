{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_DoublePointsMeta;

interface

uses
  t_GeoTypes,
  i_DoublePointsMeta,
  u_BaseInterfacedObject;

type
  TDoublePointsMetaImpl = class(TBaseInterfacedObject, IDoublePointsMeta)
  private
    FMeta: PDoublePointsMeta;
    FCount: Integer;
  private
    { IDoublePointsMeta }
    function GetCount: Integer;
    function GetMeta: PDoublePointsMeta;
  public
    constructor Create(
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );
    constructor CreateWithOwn(
      const AMeta: PDoublePointsMeta;
      const ACount: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_DoublePointsMetaFunc;

{ TDoublePointsMetaImpl }

constructor TDoublePointsMetaImpl.Create(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  inherited Create;

  FMeta := CopyMeta(AMeta, ACount);
  FCount := ACount;
end;

constructor TDoublePointsMetaImpl.CreateWithOwn(
  const AMeta: PDoublePointsMeta;
  const ACount: Integer
);
begin
  inherited Create;

  FMeta := AMeta;
  FCount := ACount;
end;

destructor TDoublePointsMetaImpl.Destroy;
begin
  FreeAndNilMeta(FMeta);
  inherited Destroy;
end;

function TDoublePointsMetaImpl.GetCount: Integer;
begin
  Result := FCount;
end;

function TDoublePointsMetaImpl.GetMeta: PDoublePointsMeta;
begin
  Result := FMeta;
end;

end.
