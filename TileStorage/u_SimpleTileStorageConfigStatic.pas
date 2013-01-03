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

unit u_SimpleTileStorageConfigStatic;

interface

uses
  i_CoordConverter,
  i_SimpleTileStorageConfig,
  u_BaseInterfacedObject;

type
  TSimpleTileStorageConfigStatic = class(TBaseInterfacedObject, ISimpleTileStorageConfigStatic)
  private
    FCoordConverter: ICoordConverter;
    FCacheTypeCode: Integer;
    FNameInCache: string;
    FTileFileExt: string;
    FIsReadOnly: Boolean;
    FAllowDelete: Boolean;
    FAllowAdd: Boolean;
    FAllowReplace: Boolean;
    FUseMemCache: Boolean;
    FMemCacheCapacity: Integer;
    FMemCacheTTL: Cardinal;
    FMemCacheClearStrategy: Integer;
  private
    function GetCoordConverter: ICoordConverter;
    function GetCacheTypeCode: Integer;
    function GetNameInCache: string;
    function GetTileFileExt: string;
    function GetIsReadOnly: Boolean;
    function GetAllowDelete: Boolean;
    function GetAllowAdd: Boolean;
    function GetAllowReplace: Boolean;
    function GetUseMemCache: Boolean;
    function GetMemCacheCapacity: Integer;
    function GetMemCacheTTL: Cardinal;
    function GetMemCacheClearStrategy: Integer;
  public
    constructor Create(
      const ACoordConverter: ICoordConverter;
      const ACacheTypeCode: Integer;
      const ANameInCache: string;
      const ATileFileExt: string;
      const AIsReadOnly: Boolean;
      const AAllowDelete: Boolean;
      const AAllowAdd: Boolean;
      const AAllowReplace: Boolean;
      const AUseMemCache: Boolean;
      const AMemCacheCapacity: Integer;
      const AMemCacheTTL: Cardinal;
      const AMemCacheClearStrategy: Integer
    );
  end;

implementation

{ TSimpleTileStorageConfigStatic }

constructor TSimpleTileStorageConfigStatic.Create(
  const ACoordConverter: ICoordConverter;
  const ACacheTypeCode: Integer;
  const ANameInCache: string;
  const ATileFileExt: string;
  const AIsReadOnly: Boolean;
  const AAllowDelete: Boolean;
  const AAllowAdd: Boolean;
  const AAllowReplace: Boolean;
  const AUseMemCache: Boolean;
  const AMemCacheCapacity: Integer;
  const AMemCacheTTL: Cardinal;
  const AMemCacheClearStrategy: Integer
);
begin
  inherited Create;
  FCoordConverter := ACoordConverter;
  FCacheTypeCode := ACacheTypeCode;
  FNameInCache := ANameInCache;
  FTileFileExt := ATileFileExt;
  FIsReadOnly := AIsReadOnly;
  FAllowDelete := AAllowDelete;
  FAllowAdd := AAllowAdd;
  FAllowReplace := AAllowReplace;
  FUseMemCache := AUseMemCache;
  FMemCacheCapacity := AMemCacheCapacity;
  FMemCacheTTL := AMemCacheTTL;
  FMemCacheClearStrategy := AMemCacheClearStrategy;
end;

function TSimpleTileStorageConfigStatic.GetAllowAdd: Boolean;
begin
  Result := FAllowAdd;
end;

function TSimpleTileStorageConfigStatic.GetAllowDelete: Boolean;
begin
  Result := FAllowDelete;
end;

function TSimpleTileStorageConfigStatic.GetAllowReplace: Boolean;
begin
  Result := FAllowReplace;
end;

function TSimpleTileStorageConfigStatic.GetCacheTypeCode: Integer;
begin
  Result := FCacheTypeCode;
end;

function TSimpleTileStorageConfigStatic.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TSimpleTileStorageConfigStatic.GetIsReadOnly: Boolean;
begin
  Result := FIsReadOnly;
end;

function TSimpleTileStorageConfigStatic.GetNameInCache: string;
begin
  Result := FNameInCache;
end;

function TSimpleTileStorageConfigStatic.GetTileFileExt: string;
begin
  Result := FTileFileExt;
end;

function TSimpleTileStorageConfigStatic.GetUseMemCache: Boolean;
begin
  Result := FUseMemCache;
end;

function TSimpleTileStorageConfigStatic.GetMemCacheCapacity: Integer;
begin
  Result := FMemCacheCapacity;
end;

function TSimpleTileStorageConfigStatic.GetMemCacheTTL: Cardinal;
begin
  Result := FMemCacheTTL;
end;

function TSimpleTileStorageConfigStatic.GetMemCacheClearStrategy: Integer;
begin
  Result := FMemCacheClearStrategy;
end;

end.
