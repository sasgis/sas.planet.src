{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_TileStorageImporterListSimple;

interface

uses
  i_Notifier,
  i_ArchiveReadWriteFactory,
  i_ProjectionSetFactory,
  i_ContentTypeManager,
  i_TileStorageImporter,
  i_TileStorageImporterList,
  u_BaseInterfacedObject;

type
  TTileStorageImporterListSimple = class(TBaseInterfacedObject, ITileStorageImporterListChangeable)
  private
    FNotifierFake: INotifier;
    FList: ITileStorageImporterListStatic;
  private
    { IChangeable}
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    { ITileStorageImporterListChangeable }
    function GetStatic: ITileStorageImporterListStatic;
  public
    constructor Create(
      const AContentTypeManager: IContentTypeManager;
      const AProjectionSetFactory: IProjectionSetFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory
    );
  end;

implementation

uses
  i_InterfaceListSimple,
  u_Notifier,
  u_InterfaceListSimple,
  u_TileStorageImporterList,
  u_TileStorageSQLiteFileImporter;

{ TTileStorageImporterListSimple }

constructor TTileStorageImporterListSimple.Create(
  const AContentTypeManager: IContentTypeManager;
  const AProjectionSetFactory: IProjectionSetFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory
);
var
  VList: IInterfaceListSimple;
  VImporter: ITileStorageImporter;
  VItem: ITileStorageImporterListItem;
begin
  inherited Create;

  FNotifierFake := TNotifierFaked.Create;
  VList := TInterfaceListSimple.Create;

  // SQLiteFile: MBTiles, OsmAnd, Locus, RMaps, OruxMaps
  VImporter :=
    TTileStorageSQLiteFileImporter.Create(
      AContentTypeManager,
      AProjectionSetFactory,
      AArchiveReadWriteFactory
    );

  // MBTiles
  VItem :=
    TTileStorageImporterListItem.Create(
      VImporter,
      ['mbtiles'],
      'MBTiles (SQLite3)'
    );
  VList.Add(VItem);

  // OsmAnd, Locus, RMaps
  VItem :=
    TTileStorageImporterListItem.Create(
      VImporter,
      ['sqlitedb', 'rmap'],
      'OsmAnd, Locus, RMaps (SQLite3)'
    );
  VList.Add(VItem);

  // OruxMaps
  VItem :=
    TTileStorageImporterListItem.Create(
      VImporter,
      ['db'],
      'OruxMaps (SQLite3)'
    );
  VList.Add(VItem);

  FList := TTileStorageImporterListStatic.Create(VList.MakeStaticAndClear);
end;

function TTileStorageImporterListSimple.GetAfterChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TTileStorageImporterListSimple.GetBeforeChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TTileStorageImporterListSimple.GetChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TTileStorageImporterListSimple.GetStatic: ITileStorageImporterListStatic;
begin
  Result := FList;
end;

end.
