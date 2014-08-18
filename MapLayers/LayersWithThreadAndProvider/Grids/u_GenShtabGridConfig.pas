{******************************************************************************}
{* SAS.Planet (SAS.ѕланета)                                                   *}
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
  WGS84_SK42,
  u_GeoFunc;

const
  GSHprec = 10000000000000000;

function WGS84_To_SK42(const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := SK42_WGS84_Long(APoint.Y, APoint.X, 0);
  Result.Y := SK42_WGS84_Lat(APoint.Y, APoint.X, 0);
end;

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
  VStep: TDoublePoint;
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
    VStep := GetGhBordersStepByScale(VScale, ALocalConverter.Getzoom);

    Result.X := Result.X - (Round(Result.X * GSHprec) mod Round(VStep.X * GSHprec)) / GSHprec;
    if Result.X < 0 then begin
      Result.X := Result.X - VStep.X;
    end;

    Result.Y := Result.Y - (Round(Result.Y * GSHprec) mod Round(VStep.Y * GSHprec)) / GSHprec;
    if Result.Y > 0 then begin
      Result.Y := Result.Y + VStep.Y;
    end;

    Result := WGS84_To_SK42(Result);
  end;
end;

function TGenShtabGridConfig.GetRectStickToGrid(
  const ALocalConverter: ILocalCoordConverter;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VScale: Integer;
  VVisible: Boolean;
  VStep: TDoublePoint;
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
    VStep := GetGhBordersStepByScale(VScale, ALocalConverter.Getzoom);

    Result.Left := Result.Left - (Round(Result.Left * GSHprec) mod Round(VStep.X * GSHprec)) / GSHprec;
    if Result.Left < 0 then begin
      Result.Left := Result.Left - VStep.X;
    end;

    Result.Top := Result.Top - (Round(Result.Top * GSHprec) mod Round(VStep.Y * GSHprec)) / GSHprec;
    if Result.Top > 0 then begin
      Result.Top := Result.Top + VStep.Y;
    end;

    Result.Right := Result.Right - (Round(Result.Right * GSHprec) mod Round(VStep.X * GSHprec)) / GSHprec;
    if Result.Right >= 0 then begin
      Result.Right := Result.Right + VStep.X;
    end;

    Result.Bottom := Result.Bottom - (Round(Result.Bottom * GSHprec) mod Round(VStep.Y * GSHprec)) / GSHprec;
    if Result.Bottom <= 0 then begin
      Result.Bottom := Result.Bottom - VStep.Y;
    end;

    // !сетка генштаба не пр€моугольна€, поэтому верхн€€ права€ и нижн€€ лева€ точки будут показывать абы-куда
    Result.TopLeft := WGS84_To_SK42(Result.TopLeft);
    Result.BottomRight := WGS84_To_SK42(Result.BottomRight);
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
