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

unit u_DegreeGridConfig;

interface

uses
  t_GeoTypes,
  i_LocalCoordConverter,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerGridsConfig,
  u_BaseGridConfig;

type
  TDegreeGridConfig = class(TBaseGridConfig, IDegreeGridConfig)
  private
    FScale: Double;
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
    function GetScale: Double;
    procedure SetScale(AValue: Double);
  public
    constructor Create;
  end;

implementation

uses
  u_GeoFunc;

const
  GSHprec = 100000000;

{ TDegreeGridConfig }

constructor TDegreeGridConfig.Create;
begin
  inherited;
  FScale := 0;
end;

procedure TDegreeGridConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetScale(AConfigData.ReadFloat('Scale', FScale));
  end;
end;

procedure TDegreeGridConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteFloat('Scale', FScale);
end;

function TDegreeGridConfig.GetPointStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceLonLat: TDoublePoint): TDoublePoint;
var
  VScale: Double;
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
  if VVisible then begin
    z := GetDegBordersStepByScale(VScale, ALocalConverter.Getzoom);
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

function TDegreeGridConfig.GetRectStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VScale: Double;
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
  if VVisible then begin
    z := GetDegBordersStepByScale(VScale, ALocalConverter.Getzoom);
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

function TDegreeGridConfig.GetScale: Double;
begin
  LockRead;
  try
    Result := FScale;
  finally
    UnlockRead;
  end;
end;

procedure TDegreeGridConfig.SetScale(AValue: Double);
var
  VScale: Double;
begin
  VScale := AValue;
  if VScale >= 1000000000 then begin
    VScale := 1000000000;
  end;

  LockWrite;
  try
    if (FScale <> VScale) then begin
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
