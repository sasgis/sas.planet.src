{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2020, SAS.Planet development team.                      *}
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

unit u_PathDetalizeConfig;

interface

uses
  i_PathDetalizeConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TPathDetalizeConfig = class(TConfigDataElementBase, IPathDetalizeConfig)
  private
    FEnableAutomaticRouting: Boolean;
    FDefaultProvider: TGUID;
  private
    { IPathDetalizeConfig }
    function GetEnableAutomaticRouting: Boolean;
    procedure SetEnableAutomaticRouting(const AValue: Boolean);

    function GetDefaultProvider: TGUID;
    procedure SetDefaultProvider(const AValue: TGUID);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  c_PathDetalizeProvidersGUID,
  u_ConfigProviderHelpers;

{ TPathDetalizeConfig }

constructor TPathDetalizeConfig.Create;
begin
  inherited Create;

  FEnableAutomaticRouting := False;
  FDefaultProvider := CPathDetalizeProviderYourNavigationFastestByCar;
end;

procedure TPathDetalizeConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FEnableAutomaticRouting := AConfigData.ReadBool('EnableAutomaticRouting', FEnableAutomaticRouting);
    FDefaultProvider := ReadGUID(AConfigData, 'DefaultProvider', FDefaultProvider);
    SetChanged;
  end;
end;

procedure TPathDetalizeConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('EnableAutomaticRouting', FEnableAutomaticRouting);
  AConfigData.WriteString('DefaultProvider', GUIDToString(FDefaultProvider));
end;

function TPathDetalizeConfig.GetDefaultProvider: TGUID;
begin
  LockRead;
  try
    Result := FDefaultProvider;
  finally
    UnlockRead;
  end;
end;

function TPathDetalizeConfig.GetEnableAutomaticRouting: Boolean;
begin
  LockRead;
  try
    Result := FEnableAutomaticRouting;
  finally
    UnlockRead;
  end;
end;

procedure TPathDetalizeConfig.SetDefaultProvider(const AValue: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FDefaultProvider, AValue) then begin
      FDefaultProvider := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TPathDetalizeConfig.SetEnableAutomaticRouting(const AValue: Boolean);
begin
  LockWrite;
  try
    if FEnableAutomaticRouting <> AValue then begin
      FEnableAutomaticRouting := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
