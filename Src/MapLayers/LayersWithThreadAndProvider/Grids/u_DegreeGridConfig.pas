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

unit u_DegreeGridConfig;

interface

uses
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
  private
    function GetScale: Double;
    procedure SetScale(AValue: Double);
  public
    constructor Create;
  end;

implementation



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
