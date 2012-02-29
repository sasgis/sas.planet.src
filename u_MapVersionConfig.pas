{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
    FVersionFactory: IMapVersionFactory;
    FVersion: IMapVersionInfo;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVersionFactory: IMapVersionFactory;
    procedure SetVersionFactory(AValue: IMapVersionFactory);

    function GetVersion: IMapVersionInfo;
    procedure SetVersion(const AValue: IMapVersionInfo);
  public
    constructor Create(ADefConfig: IMapVersionInfo);
  end;


implementation

uses
  u_MapVersionFactorySimpleString;

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(ADefConfig: IMapVersionInfo);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FVersionFactory := TMapVersionFactorySimpleString.Create;
  FVersion := FVersionFactory.CreateByMapVersion(FDefConfig);
end;

procedure TMapVersionConfig.DoReadConfig(AConfigData: IConfigDataProvider);
var
  VStoreString: string;
begin
  inherited;
  if AConfigData <> nil then begin
    VStoreString := AConfigData.ReadString('Version', FVersion.StoreString);
    if VStoreString <> FVersion.StoreString then begin
      SetVersion(FVersionFactory.CreateByStoreString(VStoreString));
    end;
  end;
end;

procedure TMapVersionConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  VStoreString: string;
begin
  inherited;
  VStoreString := FVersion.StoreString;
  if VStoreString <> FDefConfig.StoreString then begin
    AConfigData.WriteString('Version', VStoreString);
  end else begin
    AConfigData.DeleteValue('Version');
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

procedure TMapVersionConfig.SetVersion(const AValue: IMapVersionInfo);
var
  VValue: IMapVersionInfo;
begin
  LockWrite;
  try
    if FVersion <> AValue then begin
      VValue := FVersionFactory.CreateByMapVersion(AValue);
      if FVersion <> VValue then begin
        FVersion := VValue;
        SetChanged;
      end;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapVersionConfig.SetVersionFactory(AValue: IMapVersionFactory);
begin
  LockWrite;
  try
    if FVersionFactory <> AValue then begin
      FVersionFactory := AValue;
      FVersion := FVersionFactory.CreateByMapVersion(FVersion);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
