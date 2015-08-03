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

unit u_VectorItemTreeExporterListSimple;

interface

uses
  i_Notifier,
  i_ArchiveReadWriteFactory,
  i_VectorItemTreeExporter,
  i_VectorItemTreeExporterList,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkCategoryFactory,
  i_MarkSystemImplFactory,
  u_BaseInterfacedObject;

type
  TVectorItemTreeExporterListSimple = class(TBaseInterfacedObject, IVectorItemTreeExporterListChangeable)
  private
    FNotifierFake: INotifier;
    FList: IVectorItemTreeExporterListStatic;
  private
    function GetBeforeChangeNotifier: INotifier;
    function GetChangeNotifier: INotifier;
    function GetAfterChangeNotifier: INotifier;
    function GetStatic: IVectorItemTreeExporterListStatic;
  public
    constructor Create(
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryFactory: IMarkCategoryFactory;
      const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic
    );
  end;

implementation

uses
  c_MarkSystem,
  i_InterfaceListSimple,
  u_Notifier,
  u_InterfaceListSimple,
  u_VectorItemTreeMarksDb,
  u_VectorItemTreeExporterList,
  u_VectorItemTreeExporterKmlKmz,
  u_VectorItemTreeExporterGPX;

{ TVectorItemTreeExporterListSimple }

constructor TVectorItemTreeExporterListSimple.Create(
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryFactory: IMarkCategoryFactory;
  const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic
);
var
  VList: IInterfaceListSimple;
  VExporter: IVectorItemTreeExporter;
  VItem: IVectorItemTreeExporterListItem;
begin
  inherited Create;
  FNotifierFake := TNotifierFaked.Create;
  VList := TInterfaceListSimple.Create;

  VExporter := TVectorItemTreeExporterKmlKmz.Create(AArchiveReadWriteFactory);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'kmz',
      'Google KMZ file'
    );
  VList.Add(VItem);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'kml',
      'Google KML file'
    );
  VList.Add(VItem);

  VExporter := TVectorItemTreeExporterGPX.Create(AArchiveReadWriteFactory);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'gpx',
      'GPS Exchange files'
    );
  VList.Add(VItem);

  VExporter := TVectorItemTreeMarksDb.Create(
    cSMLMarksDbGUID,
    AMarkFactory,
    ACategoryFactory,
    AAppearanceOfMarkFactory,
    AMarkSystemImplFactoryListStatic
  );
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'sml',
      'SAS.Planet Marks Database in XML format'
    );
  VList.Add(VItem);

  VExporter := TVectorItemTreeMarksDb.Create(
    cORMSQLiteMarksDbGUID,
    AMarkFactory,
    ACategoryFactory,
    AAppearanceOfMarkFactory,
    AMarkSystemImplFactoryListStatic
  );
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      'db3',
      'SAS.Planet Marks Database in SQLite3 format'
    );
  VList.Add(VItem);

  FList := TVectorItemTreeExporterListStatic.Create(VList.MakeStaticAndClear);
end;

function TVectorItemTreeExporterListSimple.GetAfterChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeExporterListSimple.GetBeforeChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeExporterListSimple.GetChangeNotifier: INotifier;
begin
  Result := FNotifierFake;
end;

function TVectorItemTreeExporterListSimple.GetStatic: IVectorItemTreeExporterListStatic;
begin
  Result := FList;
end;

end.
