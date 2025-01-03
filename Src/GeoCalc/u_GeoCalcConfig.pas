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

unit u_GeoCalcConfig;

interface

uses
  i_GeoCalcConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TGeoCalcConfig = class(TConfigDataElementBase, IGeoCalcConfig)
  private
    FDatumSource: TGeoCalcDatumSource;
  private
    { IGeoCalcConfig }
    function GetDatumSource: TGeoCalcDatumSource;
    procedure SetDatumSource(const AValue: TGeoCalcDatumSource);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

{ TGeoCalcConfig }

constructor TGeoCalcConfig.Create;
begin
  inherited Create;
  FDatumSource := dsWGS84;
end;

procedure TGeoCalcConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;

  if not Assigned(AConfigData) then begin
    Exit;
  end;

  LockWrite;
  try
    FDatumSource := TGeoCalcDatumSource(AConfigData.ReadInteger('DatumSource', Integer(FDatumSource)));
    if not (FDatumSource in [dsWGS84, dsZMP]) then begin
      FDatumSource := dsWGS84;
    end;

    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TGeoCalcConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;

  LockRead;
  try
    AConfigData.WriteInteger('DatumSource', Integer(FDatumSource));
  finally
    UnlockRead;
  end;
end;

function TGeoCalcConfig.GetDatumSource: TGeoCalcDatumSource;
begin
  LockRead;
  try
    Result := FDatumSource;
  finally
    UnlockRead;
  end;
end;

procedure TGeoCalcConfig.SetDatumSource(const AValue: TGeoCalcDatumSource);
begin
  LockWrite;
  try
    if AValue <> FDatumSource then begin
      FDatumSource := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
