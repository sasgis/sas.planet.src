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

unit u_SASMainConfigProvider;

interface

uses
  IniFiles,
  u_ConfigDataWriteProviderWithGlobal;

type
  TSASMainConfigProvider = class(TConfigDataWriteProviderWithGlobal)
  private
    function GetMainConfigFileName(const ABasePath, AExeFileName: string): string;
  public
    constructor Create(
      const ABasePath, AExeFileName: string;
      AHandle: THandle
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataWriteProviderByIniFile,
  u_ConfigDataProviderVirtualWithSubItem,
  u_ConfigDataProviderByResources;

{ TSASMainConfigProvider }

constructor TSASMainConfigProvider.Create(
  const ABasePath, AExeFileName: string;
  AHandle: THandle
);
var
  VResourceProvider: IConfigDataProvider;
  VGlobalProvider: IConfigDataProvider;
  VMainProvider: IConfigDataWriteProvider;
  VIniFile: TMemIniFile;
begin
  VResourceProvider := TConfigDataProviderByResources.Create(AHandle);
  VGlobalProvider := TConfigDataProviderVirtualWithSubItem.Create('Resource', VResourceProvider);
  VIniFile := TMeminifile.Create(GetMainConfigFileName(ABasePath, AExeFileName));
  try
    VMainProvider := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
    VIniFile := nil;
  finally
    VIniFile.Free;
  end;
  inherited Create(VMainProvider, 'sas:\', VGlobalProvider);
end;

function TSASMainConfigProvider.GetMainConfigFileName(const ABasePath, AExeFileName: string): string;
var
  VPos: Integer;
begin
  VPos := Pos('.', AExeFileName);
  Result := IncludeTrailingPathDelimiter(ABasePath) + LeftStr(AExeFileName, VPos - 1) + '.ini';
end;

end.
