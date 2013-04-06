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

unit u_MapVersionConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapVersionInfo,
  i_MapVersionConfig,
  u_ConfigDataElementBase;

type
  TMapVersionConfig = class(TConfigDataElementBase, IMapVersionConfig)
  private
    FDefConfig: IMapVersionInfo;
    FShowPrevVersion: Boolean;
    FVersionFactory: IMapVersionFactory;
    FVersion: IMapVersionInfo;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVersionFactory: IMapVersionFactory;

    function GetVersion: IMapVersionInfo;
    procedure SetVersion(const AValue: IMapVersionInfo);

    function GetShowPrevVersion: Boolean;
    procedure SetShowPrevVersion(const AValue: Boolean);
  public
    constructor Create(
      const ADefConfig: IMapVersionInfo;
      const AMapVersionFactory: IMapVersionFactory
    );
  end;


implementation

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(
  const ADefConfig: IMapVersionInfo;
  const AMapVersionFactory: IMapVersionFactory
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FShowPrevVersion := ADefConfig.ShowPrevVersion;
  FVersionFactory := AMapVersionFactory;
  FVersion := FVersionFactory.CreateByMapVersion(FDefConfig, FShowPrevVersion);
end;

procedure TMapVersionConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VStoreString: string;
  VShowPrevVersion: Boolean;
begin
  inherited;
  if AConfigData <> nil then begin
    VStoreString := AConfigData.ReadString('Version', FVersion.StoreString);
    if VStoreString <> FVersion.StoreString then begin
      SetVersion(FVersionFactory.CreateByStoreString(VStoreString));
    end;
    VShowPrevVersion := AConfigData.ReadBool('ShowPrevVersion', FShowPrevVersion);
    if VShowPrevVersion <> FShowPrevVersion then begin
      SetShowPrevVersion(VShowPrevVersion);
    end;
  end;
end;

procedure TMapVersionConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VStoreString: string;
  VShowPrevVersion: Boolean;
begin
  inherited;
  VStoreString := FVersion.StoreString;
  if VStoreString <> FDefConfig.StoreString then begin
    AConfigData.WriteString('Version', VStoreString);
  end else begin
    AConfigData.DeleteValue('Version');
  end;
  VShowPrevVersion := FShowPrevVersion;
  if VShowPrevVersion <> FDefConfig.ShowPrevVersion then begin
    AConfigData.WriteBool('ShowPrevVersion', VShowPrevVersion);
  end else begin
    AConfigData.DeleteValue('ShowPrevVersion');
  end;
end;

function TMapVersionConfig.GetShowPrevVersion: Boolean;
begin
  LockRead;
  try
    Result := FShowPrevVersion;
  finally
    UnlockRead;
  end;
end;

function TMapVersionConfig.GetVersion: IMapVersionInfo;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

function TMapVersionConfig.GetVersionFactory: IMapVersionFactory;
begin
  LockRead;
  try
    Result := FVersionFactory;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionConfig.SetShowPrevVersion(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowPrevVersion <> AValue then begin
      FShowPrevVersion := AValue;
      FVersion := FVersionFactory.CreateByMapVersion(FVersion, FShowPrevVersion);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapVersionConfig.SetVersion(const AValue: IMapVersionInfo);
var
  VValue: IMapVersionInfo;
begin
  LockWrite;
  try
    if FVersion <> AValue then begin
      VValue := FVersionFactory.CreateByMapVersion(AValue, FShowPrevVersion);
      if FVersion <> VValue then begin
        FVersion := VValue;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
