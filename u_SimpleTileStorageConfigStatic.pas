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
  i_SimpleTileStorageConfig;

type
  TSimpleTileStorageConfigStatic = class(TInterfacedObject, ISimpleTileStorageConfigStatic)
  private
    FCoordConverter: ICoordConverter;
    FCacheTypeCode: Integer;
    FNameInCache: string;
    FTileFileExt: string;
    FIsStoreFileCache: Boolean;
    FIsReadOnly: boolean;
    FAllowDelete: boolean;
    FAllowAdd: boolean;
    FAllowReplace: boolean;
  private
    function GetCoordConverter: ICoordConverter;
    function GetCacheTypeCode: Integer;
    function GetNameInCache: string;
    function GetTileFileExt: string;
    function GetIsStoreFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowAdd: boolean;
    function GetAllowReplace: boolean;
  public
    constructor Create(
      const ACoordConverter: ICoordConverter;
      ACacheTypeCode: Integer;
      const ANameInCache: string;
      const ATileFileExt: string;
      AIsStoreFileCache: Boolean;
      AIsReadOnly: boolean;
      AAllowDelete: boolean;
      AAllowAdd: boolean;
      AAllowReplace: boolean
    );
  end;

implementation

{ TSimpleTileStorageConfigStatic }

constructor TSimpleTileStorageConfigStatic.Create(
  const ACoordConverter: ICoordConverter;
  ACacheTypeCode: Integer;
  const ANameInCache: string;
  const ATileFileExt: string;
  AIsStoreFileCache, AIsReadOnly, AAllowDelete, AAllowAdd, AAllowReplace: boolean
);
begin
  inherited Create;
  FCoordConverter := ACoordConverter;
  FCacheTypeCode := ACacheTypeCode;
  FNameInCache := ANameInCache;
  FTileFileExt := ATileFileExt;
  FIsStoreFileCache := AIsStoreFileCache;
  FIsReadOnly := AIsReadOnly;
  FAllowDelete := AAllowDelete;
  FAllowAdd := AAllowAdd;
  FAllowReplace := AAllowReplace;
end;

function TSimpleTileStorageConfigStatic.GetAllowAdd: boolean;
begin
  Result := FAllowAdd;
end;

function TSimpleTileStorageConfigStatic.GetAllowDelete: boolean;
begin
  Result := FAllowDelete;
end;

function TSimpleTileStorageConfigStatic.GetAllowReplace: boolean;
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

function TSimpleTileStorageConfigStatic.GetIsReadOnly: boolean;
begin
  Result := FIsReadOnly;
end;

function TSimpleTileStorageConfigStatic.GetIsStoreFileCache: Boolean;
begin
  Result := FIsStoreFileCache;
end;

function TSimpleTileStorageConfigStatic.GetNameInCache: string;
begin
  Result := FNameInCache;
end;

function TSimpleTileStorageConfigStatic.GetTileFileExt: string;
begin
  Result := FTileFileExt;
end;

end.
