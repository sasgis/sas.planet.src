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

unit u_ConfigDataProviderZmpComplex;

interface

uses
  i_ConfigDataProvider,
  u_ConfigDataProviderWithLocal;

type
  TConfigDataProviderZmpComplex = class(TConfigDataProviderWithLocal)
  public
    constructor Create(
      const AZmpMapConfig: IConfigDataProvider;
      const ALocalMapConfig: IConfigDataProvider
    );
  end;

implementation

uses
  Classes,
  u_ConfigDataProviderWithUseDepreciated,
  u_ConfigDataProviderVirtualWithSubItem,
  u_ConfigDataProviderWithReplacedSubItem;

{ TConfigDataProviderZmpComplex }

constructor TConfigDataProviderZmpComplex.Create(
  const AZmpMapConfig, ALocalMapConfig: IConfigDataProvider
);
var
  VConfig: IConfigDataProvider;
  VParamsTXT: IConfigDataProvider;
  VParams: IConfigDataProvider;
  VRenamesList: TStringList;
  VSubProvider: IConfigDataProvider;
  VLocalMapConfig: IConfigDataProvider;
begin
  VConfig := AZmpMapConfig;
  VParamsTXT := VConfig.GetSubItem('params.txt');
  VParams := VParamsTXT.GetSubItem('PARAMS');

  VRenamesList := TStringList.Create;
  try
    VRenamesList.Values['URLBase'] := 'DefURLBase';
    VRenamesList.Values['HotKey'] := 'DefHotKey';
    VParams := TConfigDataProviderWithUseDepreciated.Create(VParams, VRenamesList);
  finally
    VRenamesList.Free;
  end;
  VParamsTXT := TConfigDataProviderWithReplacedSubItem.Create(VParamsTXT, 'PARAMS', VParams);
  VConfig := TConfigDataProviderWithReplacedSubItem.Create(VConfig, 'params.txt', VParamsTXT);
  VSubProvider := TConfigDataProviderVirtualWithSubItem.Create('PARAMS', ALocalMapConfig);
  VLocalMapConfig := TConfigDataProviderVirtualWithSubItem.Create('params.txt', VSubProvider);

  inherited Create(VConfig, VLocalMapConfig);
end;

end.
