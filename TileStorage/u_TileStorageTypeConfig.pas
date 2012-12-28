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
      const ABasePath: IPathConfig;
      const ADefaultPath: string
    );
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_PathConfig;

{ TTileStorageTypeConfig }

constructor TTileStorageTypeConfig.Create(
  const ABasePath: IPathConfig;
  const ADefaultPath: string
);
begin
  inherited Create;
  FPath := TPathConfig.Create('Path', ADefaultPath, ABasePath);
  Add(FPath, TConfigSaveLoadStrategyBasicUseProvider.Create);
end;

function TTileStorageTypeConfig.GetBasePath: IPathConfig;
begin
  LockRead;
  try
    Result := FPath;
  finally
    UnlockRead;
  end;
end;

end.
