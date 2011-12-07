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
    FVersion: Variant;
    FStatic: IMapVersionInfo;
    function CreateStatic: IMapVersionInfo;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVersion: Variant;
    procedure SetVersion(const AValue: Variant);

    function GetStatic: IMapVersionInfo;
  public
    constructor Create(ADefConfig: IMapVersionInfo);
  end;


implementation

uses
  u_MapVersionInfo;

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(ADefConfig: IMapVersionInfo);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FVersion := FDefConfig.Version;
  FStatic := CreateStatic;
end;

function TMapVersionConfig.CreateStatic: IMapVersionInfo;
begin
  Result := TMapVersionInfo.Create(FVersion);
end;

procedure TMapVersionConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TMapVersionConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetVersion(AConfigData.ReadString('Version', FVersion));
    SetChanged;
  end;
end;

procedure TMapVersionConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  if FVersion <> FDefConfig.Version then begin
    AConfigData.WriteString('Version', FVersion);
  end else begin
    AConfigData.DeleteValue('Version');
  end;
end;

function TMapVersionConfig.GetStatic: IMapVersionInfo;
begin
  Result := FStatic;
end;

function TMapVersionConfig.GetVersion: Variant;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionConfig.SetVersion(const AValue: Variant);
begin
  LockWrite;
  try
    if FVersion <> AValue then begin
      FVersion := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
