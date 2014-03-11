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

unit u_TerrainConfig;

interface

uses
  i_TerrainConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TTerrainConfig = class(TConfigDataElementBase, ITerrainConfig)
  private
    FShowInStatusBar: Boolean;
    FAvailable: Boolean;
    FTrySecondaryProviders: Boolean;
    FPrimaryProvider: TGUID;
    FLastActualProviderWithElevationData: TGUID;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetShowInStatusBar: Boolean;
    procedure SetShowInStatusBar(const AValue: Boolean);

    function GetElevationInfoAvailable: Boolean;
    procedure SetElevationInfoAvailable(const AValue: Boolean);

    function GetElevationPrimaryProvider: TGUID;
    procedure SetElevationPrimaryProvider(const AValue: TGUID);

    function GetLastActualProviderWithElevationData: TGUID;
    procedure SetLastActualProviderWithElevationData(const AValue: TGUID);

    function GetTrySecondaryElevationProviders: Boolean;
    procedure SetTrySecondaryElevationProviders(const AValue: Boolean);
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  c_TerrainProviderGUID;

{ TTerrainConfig }

constructor TTerrainConfig.Create;
begin
  inherited Create;
  FShowInStatusBar := True;
  FAvailable := False;
  FTrySecondaryProviders := True;
  FPrimaryProvider := cTerrainProviderGoogleEarthGUID;
end;

procedure TTerrainConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowInStatusBar := AConfigData.ReadBool('ShowInStatusBar', FShowInStatusBar);
    FTrySecondaryProviders := AConfigData.ReadBool('TrySecondaryProviders', FTrySecondaryProviders);
    FPrimaryProvider := StringToGUID(AConfigData.ReadString('PrimaryProvider', GUIDToString(FPrimaryProvider)));
    FLastActualProviderWithElevationData := FPrimaryProvider;
    SetChanged;
  end;
end;

procedure TTerrainConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('ShowInStatusBar', FShowInStatusBar);
  AConfigData.WriteBool('TrySecondaryProviders', FTrySecondaryProviders);
  AConfigData.WriteString('PrimaryProvider', GUIDToString(FPrimaryProvider));
end;

function TTerrainConfig.GetShowInStatusBar: Boolean;
begin
  LockRead;
  try
    Result := FShowInStatusBar;
  finally
    UnlockRead;
  end;
end;

function TTerrainConfig.GetElevationInfoAvailable: Boolean;
begin
  LockRead;
  try
    Result := FAvailable;
  finally
    UnlockRead;
  end;
end;

function TTerrainConfig.GetElevationPrimaryProvider: TGUID;
begin
  LockRead;
  try
    Result := FPrimaryProvider;
  finally
    UnlockRead;
  end;
end;

function TTerrainConfig.GetLastActualProviderWithElevationData: TGUID;
begin
  LockRead;
  try
    Result := FLastActualProviderWithElevationData;
  finally
    UnlockRead;
  end;
end;

function TTerrainConfig.GetTrySecondaryElevationProviders: Boolean;
begin
  LockRead;
  try
    Result := FTrySecondaryProviders;
  finally
    UnlockRead;
  end;
end;

procedure TTerrainConfig.SetShowInStatusBar(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowInStatusBar <> AValue then begin
      FShowInStatusBar := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTerrainConfig.SetElevationInfoAvailable(const AValue: Boolean);
begin
  LockWrite;
  try
    if FAvailable <> AValue then begin
      FAvailable := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTerrainConfig.SetElevationPrimaryProvider(const AValue: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FPrimaryProvider, AValue) then begin
      FPrimaryProvider := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTerrainConfig.SetLastActualProviderWithElevationData(const AValue: TGUID);
begin
  LockWrite;
  try
    if not IsEqualGUID(FLastActualProviderWithElevationData, AValue) then begin
      FLastActualProviderWithElevationData := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TTerrainConfig.SetTrySecondaryElevationProviders(const AValue: Boolean);
begin
  LockWrite;
  try
    if FTrySecondaryProviders <> AValue then begin
      FTrySecondaryProviders := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
