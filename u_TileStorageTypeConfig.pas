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

unit u_TileStorageTypeConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_TileStorageTypeConfig,
  u_ConfigDataElementBase;

type
  TTileStorageTypeConfig = class(TConfigDataElementBase, ITileStorageTypeConfig)
  private
    FBasePath: string;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetBasePath: string;
    procedure SetBasePath(AValue: string);
  public
    constructor Create(ABasePath: string);
  end;

implementation

{ TTileStorageTypeConfig }

constructor TTileStorageTypeConfig.Create(ABasePath: string);
begin
  inherited Create;
  FBasePath := ABasePath;
end;

procedure TTileStorageTypeConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FBasePath := AConfigData.ReadString('Path', FBasePath);
    SetChanged;
  end;
end;

procedure TTileStorageTypeConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('Path', FBasePath);
end;

function TTileStorageTypeConfig.GetBasePath: string;
begin
  LockRead;
  try
    Result := FBasePath;
  finally
    UnlockRead;
  end;
end;

procedure TTileStorageTypeConfig.SetBasePath(AValue: string);
begin
  LockWrite;
  try
    if FBasePath <> AValue then begin
      FBasePath := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
