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

unit u_StickToGridDegree;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_MapLayerGridsConfig,
  i_StickToGrid,
  u_BaseInterfacedObject;

type
  TStickToGridDegree = class(TBaseInterfacedObject, IStickToGrid)
  private
    FConfig: IDegreeGridConfig;
  private
    function PointStick(
      const AProjection: IProjection;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint;
    function RectStick(
      const AProjection: IProjection;
      const ASourceRect: TDoubleRect
    ): TDoubleRect; 
  public
    constructor Create(const AConfig: IDegreeGridConfig);
  end;

implementation

uses
  u_GeoFunc;

const
  GSHprec = 100000000;

{ TStickToGridDegree }

constructor TStickToGridDegree.Create(const AConfig: IDegreeGridConfig);
begin
  Assert(Assigned(AConfig));
  inherited Create;
  FConfig := AConfig;
end;

function TStickToGridDegree.PointStick(
  const AProjection: IProjection;
  const ASourceLonLat: TDoublePoint
): TDoublePoint;
var
  VScale: Double;
  VVisible: Boolean;
  z: TDoublePoint;
begin
  FConfig.LockRead;
  try
    VVisible := FConfig.Visible;
    VScale := FConfig.Scale;
  finally
    FConfig.UnlockRead;
  end;
  Result := ASourceLonLat;
  if VVisible then begin
    z := GetDegBordersStepByScale(VScale, AProjection.Zoom);
    Result.X := Result.X - (round(Result.X * GSHprec) mod round(z.X * GSHprec)) / GSHprec;
    if Result.X < 0 then begin
      Result.X := Result.X - z.X;
    end;

    Result.Y := Result.Y - (round(Result.Y * GSHprec) mod round(z.Y * GSHprec)) / GSHprec;
    if Result.Y > 0 then begin
      Result.Y := Result.Y + z.Y;
    end;
  end;
end;

function TStickToGridDegree.RectStick(
  const AProjection: IProjection;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VScale: Double;
  VVisible: Boolean;
  z: TDoublePoint;
begin
  FConfig.LockRead;
  try
    VVisible := FConfig.GetVisible;
    VScale := FConfig.Scale;
  finally
    FConfig.UnlockRead;
  end;
  Result := ASourceRect;
  if VVisible then begin
    z := GetDegBordersStepByScale(VScale, AProjection.Zoom);
    Result.Left := Result.Left - (round(Result.Left * GSHprec) mod round(z.X * GSHprec)) / GSHprec;
    if Result.Left < 0 then begin
      Result.Left := Result.Left - z.X;
    end;

    Result.Top := Result.Top - (round(Result.Top * GSHprec) mod round(z.Y * GSHprec)) / GSHprec;
    if Result.Top > 0 then begin
      Result.Top := Result.Top + z.Y;
    end;

    Result.Right := Result.Right - (round(Result.Right * GSHprec) mod round(z.X * GSHprec)) / GSHprec;
    if Result.Right >= 0 then begin
      Result.Right := Result.Right + z.X;
    end;

    Result.Bottom := Result.Bottom - (round(Result.Bottom * GSHprec) mod round(z.Y * GSHprec)) / GSHprec;
    if Result.Bottom <= 0 then begin
      Result.Bottom := Result.Bottom - z.Y;
    end;
  end;
end;

end.
