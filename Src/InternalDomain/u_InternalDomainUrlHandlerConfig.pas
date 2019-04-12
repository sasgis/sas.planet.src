{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_InternalDomainUrlHandlerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_InternalDomainUrlHandlerConfig,
  u_ConfigDataElementComplexBase;

type
  TInternalDomainUrlHandlerConfig = class(TConfigDataElementComplexBase, IInternalDomainUrlHandlerConfig)
  private
    FAllowedExt: string;
    FUserAppsConfig: IUserAppsConfig;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    { IInternalDomainUrlHandlerConfig }
    function GetAllowedExt: string;
    function GetUserAppsConfig: IUserAppsConfig;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  i_StringListStatic,
  u_ConfigDataElementBase,
  u_ConfigSaveLoadStrategyBasicProviderSubItem;

type
  TUserAppsConfig = class(TConfigDataElementBase, IUserAppsConfig)
  private
    FUserApps: TUserAppArray;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetUserApps: TUserAppArray;
  end;

{ TUserAppsConfig }

procedure TUserAppsConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  I, J: Integer;
  VIdent, VValue: string;
  VList: IStringListStatic;
begin
  inherited;
  if AConfigData <> nil then begin
    J := 0;
    VList := AConfigData.ReadValuesList;
    SetLength(FUserApps, VList.Count);
    for I := 0 to VList.Count - 1 do begin
      VIdent := VList.Items[I];
      VValue := AConfigData.ReadString(VIdent, '');
      if VValue <> '' then begin
        FUserApps[J].ID := LowerCase(VIdent);
        FUserApps[J].Path := VValue;
        Inc(J);
      end;
    end;
    SetLength(FUserApps, J);
    SetChanged;
  end;
end;

procedure TUserAppsConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
end;

function TUserAppsConfig.GetUserApps: TUserAppArray;
begin
  LockRead;
  try
    Result := Copy(FUserApps);
  finally
    UnlockRead;
  end;
end;

{ TInternalDomainUrlHandlerConfig }

constructor TInternalDomainUrlHandlerConfig.Create;
begin
  inherited Create;
  FAllowedExt := ''; // allow all
  FUserAppsConfig := TUserAppsConfig.Create;
  Add(FUserAppsConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('UserApps'));
end;

procedure TInternalDomainUrlHandlerConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    FAllowedExt := AConfigData.ReadString('AllowedExt', FAllowedExt);
    SetChanged;
  end;
end;

procedure TInternalDomainUrlHandlerConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  AConfigData.WriteString('AllowedExt', FAllowedExt);
end;

function TInternalDomainUrlHandlerConfig.GetAllowedExt: string;
begin
  LockRead;
  try
    Result := FAllowedExt;
  finally
    UnlockRead;
  end;
end;

function TInternalDomainUrlHandlerConfig.GetUserAppsConfig: IUserAppsConfig;
begin
  Result := FUserAppsConfig;
end;

end.
