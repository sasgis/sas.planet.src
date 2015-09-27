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

unit u_MapVersionConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapVersionConfig,
  u_ConfigDataElementBase;

type
  TMapVersionConfig = class(TConfigDataElementBase, IMapVersionConfig)
  private
    FVersion: string;
    FVersionDef: string;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetVersion: string;
    procedure SetVersion(const AValue: string);
  public
    constructor Create(
      const AVersion: string
    );
  end;

implementation

{ TMapVersionConfig }

constructor TMapVersionConfig.Create(
  const AVersion: string
);
begin
  inherited Create;
  FVersion := AVersion;
  FVersionDef := AVersion;
end;

procedure TMapVersionConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VStoreString: string;
begin
  inherited;
  if AConfigData <> nil then begin
    VStoreString := AConfigData.ReadString('Version', FVersion);
    SetVersion(VStoreString);
  end;
end;

procedure TMapVersionConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if FVersion <> FVersionDef then begin
    AConfigData.WriteString('Version', FVersion);
  end else begin
    AConfigData.DeleteValue('Version');
  end;
end;

function TMapVersionConfig.GetVersion: string;
begin
  LockRead;
  try
    Result := FVersion;
  finally
    UnlockRead;
  end;
end;

procedure TMapVersionConfig.SetVersion(const AValue: string);
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
