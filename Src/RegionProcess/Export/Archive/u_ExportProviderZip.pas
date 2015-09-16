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

unit u_ExportProviderZip;

interface

uses
  Forms,
  i_LanguageManager,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_ArchiveReadWriteFactory,
  i_TileStorageTypeList,
  i_TileFileNameGeneratorsList,
  i_RegionProcessProgressInfoInternalFactory,
  u_ExportProviderAbstract,
  fr_MapSelect,
  fr_ExportToFileCont;

type
  TExportProviderZip = class(TExportProviderAbstract)
  private
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FArchiveReadWriteFactory: IArchiveReadWriteFactory;
    FTileStorageTypeList: ITileStorageTypeListStatic;
    FTileNameGenerator: ITileFileNameGeneratorsList;
  protected
    function CreateFrame: TFrame; override;
  protected
    function GetCaption: string; override;
    procedure StartProcess(const APolygon: IGeometryLonLatPolygon); override;
  public
    constructor Create(
      const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
      const ALanguageManager: ILanguageManager;
      const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
      const ATileStorageTypeList: ITileStorageTypeListStatic;
      const ATileNameGenerator: ITileFileNameGeneratorsList
    );
  end;

implementation

uses
  Types,
  Classes,
  SysUtils,
  i_MapType,
  i_RegionProcessParamsFrame,
  i_RegionProcessProgressInfo,
  i_TileFileNameGenerator,
  u_ThreadExportToArchive,
  u_ResStrings;

{ TExportProviderKml }

constructor TExportProviderZip.Create(
  const AProgressFactory: IRegionProcessProgressInfoInternalFactory;
  const ALanguageManager: ILanguageManager;
  const AMapSelectFrameBuilder: IMapSelectFrameBuilder;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const AArchiveReadWriteFactory: IArchiveReadWriteFactory;
  const ATileStorageTypeList: ITileStorageTypeListStatic;
  const ATileNameGenerator: ITileFileNameGeneratorsList
);
begin
  inherited Create(
    AProgressFactory,
    ALanguageManager,
    AMapSelectFrameBuilder
  );
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FArchiveReadWriteFactory := AArchiveReadWriteFactory;
  FTileStorageTypeList := ATileStorageTypeList;
  FTileNameGenerator := ATileNameGenerator;
end;

function TExportProviderZip.CreateFrame: TFrame;
begin
  Result :=
    TfrExportToFileCont.Create(
      Self.LanguageManager,
      Self.MapSelectFrameBuilder,
      FTileNameGenerator,
      FTileStorageTypeList,
      'Zip |*.zip',
      'zip'
    );
  Assert(Supports(Result, IRegionProcessParamsFrameZoomArray));
  Assert(Supports(Result, IRegionProcessParamsFrameOneMap));
  Assert(Supports(Result, IRegionProcessParamsFrameTargetPath));
  Assert(Supports(Result, IRegionProcessParamsFrameExportToFileCont));
end;

function TExportProviderZip.GetCaption: string;
begin
  Result := SAS_STR_ExportZipPackCaption;
end;

procedure TExportProviderZip.StartProcess(const APolygon: IGeometryLonLatPolygon);
var
  VPath: string;
  Zoomarr: TByteDynArray;
  VMapType: IMapType;
  VNameGenerator: ITileFileNameGenerator;
  VProgressInfo: IRegionProcessProgressInfoInternal;
  VThread: TThread;
begin
  inherited;
  Zoomarr := (ParamsFrame as IRegionProcessParamsFrameZoomArray).ZoomArray;
  VPath := (ParamsFrame as IRegionProcessParamsFrameTargetPath).Path;
  VMapType := (ParamsFrame as IRegionProcessParamsFrameOneMap).MapType;
  VNameGenerator := (ParamsFrame as IRegionProcessParamsFrameExportToFileCont).NameGenerator;

  VProgressInfo := ProgressFactory.Build(APolygon);

  VThread :=
    TThreadExportToArchive.Create(
      VProgressInfo,
      FArchiveReadWriteFactory.Zip.WriterFactory.BuildByFileName(VPath),
      FVectorGeometryProjectedFactory,
      APolygon,
      Zoomarr,
      VMapType.TileStorage,
      VNameGenerator
    );
  VThread.Resume;
end;

end.
