{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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

unit u_VectorItemTreeExporterListSimple;

interface

uses
  i_Notifier,
  i_ArchiveReadWriteFactory,
  i_GeoCalc,
  i_BuildInfo,
  i_VectorItemTreeExporter,
  i_VectorItemTreeExporterList,
  i_AppearanceOfMarkFactory,
  i_MarkFactory,
  i_MarkCategoryFactory,
  i_MarkSystemImplFactory,
  i_ExportMarks2KMLConfig,
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
      const AGeoCalc: IGeoCalcChangeable;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
      const AMarkFactory: IMarkFactory;
      const ACategoryFactory: IMarkCategoryFactory;
      const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic;
      const AExportMarks2KMLConfig: IExportMarks2KMLConfig;
      const ABuildInfo: IBuildInfo
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
  u_VectorItemTreeExporterGPX,
  u_VectorItemTreeExporterTCX;

{ TVectorItemTreeExporterListSimple }

constructor TVectorItemTreeExporterListSimple.Create(
  const AGeoCalc: IGeoCalcChangeable;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const AAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
  const AMarkFactory: IMarkFactory;
  const ACategoryFactory: IMarkCategoryFactory;
  const AMarkSystemImplFactoryListStatic: IMarkSystemImplFactoryListStatic;
  const AExportMarks2KMLConfig: IExportMarks2KMLConfig;
  const ABuildInfo: IBuildInfo
);
var
  VList: IInterfaceListSimple;
  VExporter: IVectorItemTreeExporter;
  VItem: IVectorItemTreeExporterListItem;
begin
  inherited Create;

  FNotifierFake := TNotifierFaked.Create;
  VList := TInterfaceListSimple.Create;

  VExporter :=
    TVectorItemTreeExporterKmlKmz.Create(
      AArchiveReadWriteFactory,
      AExportMarks2KMLConfig
    );
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      AExportMarks2KMLConfig,
      [elioSaveToFile, elioAllowSeparateFiles],
      'kmz',
      'Google KMZ file'
    );
  VList.Add(VItem);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      AExportMarks2KMLConfig,
      [elioSaveToFile, elioAllowSeparateFiles],
      'kml',
      'Google KML file'
    );
  VList.Add(VItem);

  VExporter := TVectorItemTreeExporterGPX.Create(AGeoCalc.GpsCalc, ABuildInfo, True);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      nil,
      [elioSaveToFile, elioAllowSeparateFiles],
      'gpx',
      'GPS Exchange format (GPX track)'
    );
  VList.Add(VItem);

  VExporter := TVectorItemTreeExporterGPX.Create(AGeoCalc.GpsCalc, ABuildInfo, False);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      nil,
      [elioSaveToFile, elioAllowSeparateFiles],
      'gpx',
      'GPS Exchange format (GPX route)'
    );
  VList.Add(VItem);

  VExporter := TVectorItemTreeExporterTCX.Create(AGeoCalc.GpsCalc, ABuildInfo);
  VItem :=
    TVectorItemTreeExporterListItem.Create(
      VExporter,
      nil,
      [elioSaveToFile, elioAllowSeparateFiles],
      'tcx',
      'Training Center XML files (TCX)'
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
      nil,
      [elioSaveToDir],
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
      nil,
      [elioSaveToFile],
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
