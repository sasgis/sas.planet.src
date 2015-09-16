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

unit u_GenShtabGridConfig;

interface

uses
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
  private
    function GetScale: Integer;
    procedure SetScale(AValue: Integer);
  public
    constructor Create;
  end;

implementation

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
