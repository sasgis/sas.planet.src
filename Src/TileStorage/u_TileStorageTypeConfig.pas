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

unit u_TileStorageTypeConfig;

interface

uses
  i_PathConfig,
  i_TileStorageTypeConfig,
  u_ConfigDataElementComplexBase;

type
  TTileStorageTypeConfig = class(TConfigDataElementComplexBase, ITileStorageTypeConfig)
  private
    FPath: IPathConfig;
  private
    function GetBasePath: IPathConfig;
  public
    constructor Create(
      const APath: IPathConfig
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider;

{ TTileStorageTypeConfig }

constructor TTileStorageTypeConfig.Create(
  const APath: IPathConfig
);
begin
  inherited Create;
  FPath := APath;
  if Assigned(FPath) then begin
    Add(FPath, TConfigSaveLoadStrategyBasicUseProvider.Create);
  end;
end;

function TTileStorageTypeConfig.GetBasePath: IPathConfig;
begin
  Result := FPath;
end;

end.
