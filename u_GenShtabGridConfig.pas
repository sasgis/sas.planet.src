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

unit u_GenShtabGridConfig;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TGenShtabGridConfig = class(TBaseGridConfig, IGenShtabGridConfig)
  private
    FScale: Integer;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetPointStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint; override;
    function GetRectStickToGrid(
      const ALocalConverter: ILocalCoordConverter;
      const ASourceRect: TDoubleRect
    ): TDoubleRect; override;
  private
    function GetScale: Integer;
    procedure SetScale(AValue: Integer);
  public
    constructor Create;
  end;

implementation

uses
  u_GeoFunc;

const
  GSHprec = 100000000;

{ TGenShtabGridConfig }

constructor TGenShtabGridConfig.Create;
begin
  inherited;
  FScale := 0;
end;

procedure TGenShtabGridConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetScale(AConfigData.ReadInteger('Scale', FScale));
  end;
end;

procedure TGenShtabGridConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteInteger('Scale', FScale);
end;

function TGenShtabGridConfig.GetPointStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceLonLat: TDoublePoint): TDoublePoint;
var
  VScale: Integer;
  VVisible: Boolean;
  z: TDoublePoint;
begin
  LockRead;
  try
    VVisible := GetVisible;
    VScale := FScale;
  finally
    UnlockRead;
  end;
  Result := ASourceLonLat;
  if VVisible and (VScale > 0) then begin
    z := GetGhBordersStepByScale(VScale, ALocalConverter.Getzoom);

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

function TGenShtabGridConfig.GetRectStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VScale: Integer;
  VVisible: Boolean;
  z: TDoublePoint;
begin
  LockRead;
  try
    VVisible := GetVisible;
    VScale := FScale;
  finally
    UnlockRead;
  end;
  Result := ASourceRect;
  if VVisible and (VScale > 0) then begin
    z := GetGhBordersStepByScale(VScale, ALocalConverter.Getzoom);

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

function TGenShtabGridConfig.GetScale: Integer;
begin
  LockRead;
  try
    Result := FScale;
  finally
    UnlockRead;
  end;
end;

procedure TGenShtabGridConfig.SetScale(AValue: Integer);
var
  VScale: Integer;
begin
  VScale := AValue;
  if VScale >= 1000000 then begin
    VScale := 1000000;
  end else if VScale >= 500000 then begin
    VScale := 500000;
  end else if VScale >= 200000 then begin
    VScale := 200000;
  end else if VScale >= 100000 then begin
    VScale := 100000;
  end else if VScale >= 50000 then begin
    VScale := 50000;
  end else if VScale >= 25000 then begin
    VScale := 25000;
  end else if VScale >= 10000 then begin
    VScale := 10000;
  end else begin
    VScale := AValue;
  end;
  LockWrite;
  try
    if FScale <> VScale then begin
      FScale := VScale;
      if FScale = 0 then begin
        SetVisible(False);
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
