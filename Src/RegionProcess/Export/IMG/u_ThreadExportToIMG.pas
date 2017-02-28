{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_ThreadExportToIMG;

interface

uses
  t_GeoTypes,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_GeometryProjectedFactory,
  i_BitmapPostProcessing,
  i_Listener,
  u_ExportToIMGTask,
  u_RegionProcessTaskAbstract;

type
  TThreadExportToIMG = class(TRegionProcessTaskAbstract, IListener)
  private
    FTask: TExportToIMGTask;
    FTargetFileName, FTargetFileExt: string;
    FVectorGeometryProjectedFactory: IGeometryProjectedFactory;
    FBitmapPostProcessing: IBitmapPostProcessing;
    FCancelEvent: THandle;

    FStrPhase1Format: WideString;
    FStrPhase2: WideString;
    FStrPhase3: WideString;
    FStrNoTilesToExport: WideString;
    FStrCompileErrorFormat: WideString;
    FStrCompileErrorNoMessage: WideString;
    FStrCannotStartCompiler: WideString;
    FStrIMGBuildError: WideString;

    // IListener
    procedure Notification(const AMsg: IInterface);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatPolygon;
      const ATask: TExportToIMGTask;
      const ABitmapPostProcessing: IBitmapPostProcessing
    );
    destructor Destroy; override;
  end;

implementation

uses
  Windows,
  Types,
  SysUtils,
  Classes,
  ShLwApi,
  Math,
  ALString,
  CRC32,
  gnugettext,
  i_TileStorage,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_Projection,
  i_Bitmap32Static,
  i_TileIterator,
  i_BinaryData,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_GeometryProjected,
  i_TileStorageAbilities,  
  u_ResStrings,
  u_TileIteratorByPolygon,
  u_GeoFunc;

constructor TThreadExportToIMG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const AVectorGeometryProjectedFactory: IGeometryProjectedFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatPolygon;
  const ATask: TExportToIMGTask;
  const ABitmapPostProcessing: IBitmapPostProcessing
);
begin
  inherited Create(
    AProgressInfo,
    APolygon
  );
  FTargetFileName := ChangeFileExt(ATargetFile, '');
  FTargetFileExt := ExtractFileExt(ATargetFile);
  FVectorGeometryProjectedFactory := AVectorGeometryProjectedFactory;
  FTask := ATask;
  FBitmapPostProcessing := ABitmapPostProcessing;

  FCancelEvent := CreateEvent(Nil, False, False, Nil);

  FStrPhase1Format := _('Phase 1 - saving tiles. %s %d %s');
  FStrNoTilesToExport := _('No tiles to export!');
  FStrPhase2 := _('Phase 2 - compiling the map(s). ');
  FStrCompileErrorFormat := _('Map compilation failed. The map compiler reported the following error: %s');
  FStrCompileErrorNoMessage := _('Map compilation failed. Export aborted.');
  FStrCannotStartCompiler := _('Could not start the map compiler. Export aborted.');
  FStrIMGBuildError := _('Could not build IMG file. Export aborted.');
  FStrPhase3 := _('Phase 3 - cleaning up. ');
end;

destructor TThreadExportToIMG.Destroy;
begin
  CloseHandle(FCancelEvent);

  inherited;
end;

procedure TThreadExportToIMG.Notification(const AMsg: IInterface);
begin
  SetEvent(FCancelEvent);
end;

type
  TSubmapKind = (skFine, skCoarse);

const
  MaxFilePathInMTX = 200;

  CodePages: array [0..12] of String = (
    '        11  874thai                          Thai',
    '         9  932japan                         Japanese',
    '         5  936china                         Chinese (Simplified)',
    '        10  949korea                         Korean',
    '         6  950taiwan                        Chinese (Traditional)',
    '        12 1250anpolish                      Central European',
    '         8 1251ancyrr                        Cyrillic',
    '         7 1252ANSIINTL                      Western European',
    '        13 1253angreek1                      Greek',
    '        14 1254ANTURK                        Turkish',
    '        15 1255ANHEBREW                      Hebrew',
    '        16 1256BLWINAR0                      Arabic',
    '        17 1257ANSIINTL                      Baltic Sort'
  );

  InvariableHeaders: array [1..19] of String = (
    'H4CP1',   'H4CS2',
    'H4CRSAS.Planet development team@2017',
    'H4LL0',   'H4MA  255', 'H4MB1',
    'H4MC  4', 'H4ML  255', 'H4MO0',
    'H4MT1',   'H4NB1',     'H4NT0',
    'H4PF1',   'H4PP0',     'H4HP0',
    'H4PN01',  'H4SP1',     'H4TL150',
    'H4WM0'
  );

  GeneralizationLevels: array [TSubmapKind, 0..7] of String = (
    (
      'H20     24      59724  0 01',        // 5m - 200m
      'H21     35     119423  0 01',        // 300m
      'H22     49     238822  0 01',        // 500m - 800m
      'H23     69     477721  0 01',        // 1.2km
      'H24     98     955420  0 01',        // 2km - 3km
      'H25    138    1910919  0 01',        // 5km
      'H26    195    3821818  0 01',        // 8km - 12km
      'H27    277    7643717  0 01'         // 20km
    ),
    (
      'H20    391   15287416  0 01',        // 30km - 50km
      'H21    553   30574815  0 01',        // 80km
      'H22    782   61149614  0 01',        // 120km - 200km
      'H23   1106  122299213  0 01',        // 300km
      'H24   1564  244598412  0 01',        // 500km - 800km
      '',
      '',
      ''
    )
  );

  // MapFormat:
  //   #1 - OF, TRE/LBL...
  //   #2 - OF, GMP
  //   #3 - NT, GMP
  MapFormatToMF: array [TIMGMapFormat] of Integer = (1, 1, 2);
  MapFormatToMG: array [TIMGMapFormat] of Integer = (0, 1, 1);
  MapFormatSubfileExts: array [TIMGMapFormat, 1..3] of String = (
    ('.tre', '.lbl', '.rgn'),
    ('.gmp', '', ''),
    ('.gmp', '', '')
  );

procedure TThreadExportToIMG.ProcessRegion;
type
  TJPEGFileInfo = record
    Coords: TDoubleRect;
    FilePath: String;
    StartLevel, EndLevel: Integer;
    FileSize: Integer;
  end;
  TVolumeInfo = record
    TileCount: int64;
    SubmapsPresent: array [TSubmapKind] of Boolean;
    SubmapsTileCount: array [TSubmapKind] of int64;
  end;
  TTaskInternalInfo = record
    MapBounds: TDoubleRect;
    AvailableLevels: set of 0..12;
    Volumes: array of TVolumeInfo;
  end;
var
  i, j: Integer;
  sk: TSubmapKind;
  VTempFolder: String;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterators: array of ITileIterator;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VProjectionSet: IProjectionSet;
  VProjection: IProjection;
  VProjectedPolygon: IGeometryProjectedPolygon;
  VTilesToProcess: Int64;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VTileStorage: ITileStorage;
  VVersion: IMapVersionRequest;
  VTileInfo: ITileInfoWithData;
  VContentTypeInfoBitmap: IContentTypeInfoBitmap;
  VCrc32: Cardinal;
  VFileName: String;
  VFile: File;
  VCreatedFiles: TStringList;
  VAbsolutePath: String;
  VRelativePath: String;
  VFileCacheFlagIsValid: Boolean;
  VFileCache: Boolean;
  VUseFileFromCache: Boolean;
  VTaskInternalInfo: TTaskInternalInfo;
  VJPEGFileInfos: array of TJPEGFileInfo;
  VRect: TDoubleRect;
  VMtxFiles: array [TSubmapKind] of TextFile;
  VIndex: Integer;
  VTileHash: String;
  VTileHashTable: TStringList;
  VRunningTotalTileSize: int64;
  VVolumeCount: Integer;
  VGenLevels: array [TSubmapKind] of Integer;
  VSubmapIDs: array [TSubmapKind] of Integer;
  VSubmapMTXNames: array [TSubmapKind] of String;
  VStartLevel, VEndLevel: Integer;
  VStartupInfo: TStartupInfo;
  VProcessInfo: TProcessInformation;
  VCommandLine: String;
  VErrorMessage: String;
  VSearchRec: TSearchRec;
  VSearchRes: Integer;
  VWaitObjects: array [1..2] of THandle;
  VFormatSettings: TFormatSettings;
  VLogFileStringList: TStringList;
  VCoords: ^TDoubleRect;
begin
  inherited;

  SetLength(VTempFolder, MAX_PATH);
  SetLength(VTempFolder, GetTempPath(MAX_PATH, pChar(VTempFolder)));
  VTempFolder := VTempFolder + 'ExportToIMG_' + IntToHex(GetCurrentThreadId, 8) + '\';
  if not DirectoryExists(VTempFolder) then begin
    CreateDir(VTempFolder);
  end;

  VSaver := FTask.FBitmapTileSaver;
  FillChar(VTaskInternalInfo, sizeof(VTaskInternalInfo), 0);

  // Calculating the number of source tiles.
  VTilesToProcess := 0;
  SetLength(VTileIterators, Length(FTask.FItems));
  for i := 0 to Length(FTask.FItems) - 1 do
  begin
    VProjection := FTask.FItems[i].FSourceTileStorage.ProjectionSet.Zooms[FTask.FItems[i].FSourceScale];

    VProjectedPolygon :=
      FVectorGeometryProjectedFactory.CreateProjectedPolygonByLonLatPolygon(
        VProjection,
        PolygLL
      );
    VTileIterators[i] := TTileIteratorByPolygon.Create(VProjection, VProjectedPolygon);
    VTilesToProcess := VTilesToProcess + VTileIterators[i].TilesTotal;

    VRect := VProjection.TileRect2LonLatRect(VTileIterators[i].TilesRect.Rect);
    if i = 0 then begin
      VTaskInternalInfo.MapBounds := VRect;
    end else begin
      VTaskInternalInfo.MapBounds := UnionLonLatRects(VRect, VTaskInternalInfo.MapBounds);
    end;
    VTaskInternalInfo.AvailableLevels := VTaskInternalInfo.AvailableLevels + [FTask.FItems[i].FDeviceZoomStart..FTask.FItems[i].FDeviceZoomEnd];
  end;

  VGenLevels[skFine] := 0;
  VGenLevels[skCoarse] := 0;
  for i:=0 to 12 do begin
    if i in VTaskInternalInfo.AvailableLevels then begin
      if i <= 7 then begin
        inc(VGenLevels[skFine]);
      end else begin
        inc(VGenLevels[skCoarse]);
      end;
    end;
  end;

  VCreatedFiles := TStringList.Create;
  VCreatedFiles.Capacity := VTilesToProcess;
  VCreatedFiles.Sorted := True;

  VTileHashTable := TStringList.Create;
  VTileHashTable.Capacity := VTilesToProcess;
  VTileHashTable.Sorted := True;

  SetLength(VJPEGFileInfos, VTilesToProcess);

  try
    ProgressInfo.SetCaption(SAS_STR_ExportTiles);
    CancelNotifier.AddListener(Self);

    // 1. Writing the tiles to temp folder.
    ProgressInfo.SetFirstLine(Format(FStrPhase1Format, [SAS_STR_AllSaves, VTilesToProcess, SAS_STR_Files]));
    VTilesProcessed := 0;
    ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
    VRunningTotalTileSize := MAXINT;
    VVolumeCount := 0;
    for i := 0 to Length(FTask.FItems) - 1 do begin
      VTileStorage := FTask.FItems[i].FSourceTileStorage;
      VVersion := FTask.FItems[i].FSourceMapVersion;
      VZoom := FTask.FItems[i].FSourceScale;
      VProjectionSet := VTileStorage.ProjectionSet;
      VTileIterator := VTileIterators[i];

      VFileCacheFlagIsValid := False;
      VFileCache := False;

      // If the cache claims the tiles are stored in separate files, we'll need to check if they are
      // plain JPEG files from the first existing tile.
      if VTileStorage.StorageTypeAbilities.StorageClass <> tstcInSeparateFiles then begin
        // Otherwise we have to create our own file copies.
        VFileCacheFlagIsValid := True;
      end;

      while VTileIterator.Next(VTile) do begin
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          exit;
        end;

        if Supports(VTileStorage.GetTileInfoEx(VTile, VZoom, VVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
          VData := Nil;
          VUseFileFromCache := False;

          if Assigned(FBitmapPostProcessing) or not ALSameText(VTileInfo.ContentType.GetContentType, 'image/jpg') then begin
            if Supports(VTileInfo.ContentType, IContentTypeInfoBitmap, VContentTypeInfoBitmap) then begin
              VBitmapTile := VContentTypeInfoBitmap.GetLoader.Load(VTileInfo.TileData);
              if Assigned(FBitmapPostProcessing) then begin
                VBitmapTile := FBitmapPostProcessing.Process(VBitmapTile);
              end;

              if Assigned(VBitmapTile) then begin
                VData := VSaver.Save(VBitmapTile);
              end;
            end;
          end else begin
            VData := VTileInfo.TileData;

            // If the cache is already known to be not based on the file system, skip the file-path manipulations entirely.
            if not (VFileCacheFlagIsValid and not VFileCache) then begin
              // Use the tiles directly from cache whenever possible.
              VAbsolutePath := VTileStorage.GetTileFileName(VTile, VZoom, VVersion.BaseVersion);
              if SameText(ExtractFileExt(VAbsolutePath), '.jpg') and FileExists(VAbsolutePath) then begin
                {$IFDEF UNICODE}
                  if not VFileCacheFlagIsValid then begin
                    // In Unicode build, the cached files can only be used if the cache path is valid in current system locale.
                    // We also need to check if the cache path does not contain any doule-byte symbols.
                    if not FileExists(AnsiString(VAbsolutePath)) or (Length(AnsiString(VAbsolutePath)) <> Length(WideString(VAbsolutePath))) then begin
                      VFileCacheFlagIsValid := True;
                    end;
                  end;
                {$ENDIF}

                // Checking if the cache is a plain, filesystem-based one.
                if not VFileCacheFlagIsValid then begin
                  // Perhaps there's a better way to find our the cache is file-based than to check tile file size.
                  AssignFile(VFile, VAbsolutePath);
                  try
                    Reset(VFile, 1);
                    VFileCacheFlagIsValid := True;
                    VFileCache := FileSize(VFile) = VData.Size;
                  finally
                    CloseFile(VFile);
                  end;
                end;

                // Checking is it's possible to build a relative path to the tile.
                if VFileCacheFlagIsValid and VFileCache then begin
                  SetLength(VRelativePath, MAX_PATH);
                  if PathRelativePathTo(pChar(VRelativePath), pChar(VTempFolder), FILE_ATTRIBUTE_DIRECTORY, pChar(VAbsolutePath), FILE_ATTRIBUTE_NORMAL) then begin
                    VRelativePath := pChar(VRelativePath);
                    VUseFileFromCache := Length(AnsiString(VRelativePath)) <= MaxFilePathInMTX;
                  end;
                end;
              end;
            end;
          end;

          if Assigned(VData) then begin
            VCrc32 := CRC32Buf(VData.Buffer, VData.Size);
            VTileHash := IntToHex(VCrc32, 8) + '_' + IntToHex(VData.Size, 8);
            VIndex := VTileHashTable.IndexOf(VTileHash);
            if VIndex <> -1 then begin
              VIndex := Integer(VTileHashTable.Objects[VIndex]) - 1;
            end;

            // The same tile already saved. Reusing the existing path.
            if VIndex <> -1 then begin
              VFileName := VJPEGFileInfos[VIndex].FilePath;
            end else begin
              if not VUseFileFromCache then begin
                VFileName := VTempFolder + VTileHash + '.jpg';
                if not FileExists(VFileName) then begin
                  AssignFile(VFile, VFileName); Rewrite(VFile, 1);
                  try
                    BlockWrite(VFile, VData.Buffer^, VData.Size);
                  finally
                    CloseFile(VFile);
                  end;
                end;

                VCreatedFiles.Add(VFileName);
                // Stripping temp folder path from the filename.
                VFileName := VTileHash;
              end else begin
                VFileName := VRelativePath;
              end;

              VTileHashTable.AddObject(VTileHash, TObject(VTilesProcessed + 1));
            end;

            VJPEGFileInfos[VTilesProcessed].Coords := VProjectionSet.Zooms[VZoom].TilePos2LonLatRect(VTile);
            VJPEGFileInfos[VTilesProcessed].FilePath := ChangeFileExt(VFileName, '');
            VJPEGFileInfos[VTilesProcessed].StartLevel := FTask.FItems[i].FDeviceZoomStart;
            VJPEGFileInfos[VTilesProcessed].EndLevel := FTask.FItems[i].FDeviceZoomEnd;
            VJPEGFileInfos[VTilesProcessed].FileSize := VData.Size;

            // Adding new map volume when the tile size exceeds the specified limit.
            // TODO: If a tile is references more than once, its size should only be counted for the first time (in each submap independently!).
            if VRunningTotalTileSize + VData.Size > FTask.FVolumeSize then begin
              VRunningTotalTileSize := 0;
              inc(VVolumeCount);

              SetLength(VTaskInternalInfo.Volumes, VVolumeCount);
              FillChar(VTaskInternalInfo.Volumes[VVolumeCount - 1], sizeof(TVolumeInfo), 0);
            end;

            inc(VRunningTotalTileSize, VData.Size);

            inc(VTaskInternalInfo.Volumes[VVolumeCount - 1].TileCount);
            if FTask.FItems[i].FDeviceZoomStart <= 7 then begin
              VTaskInternalInfo.Volumes[VVolumeCount - 1].SubmapsPresent[skFine] := True;
              inc(VTaskInternalInfo.Volumes[VVolumeCount - 1].SubmapsTileCount[skFine]);
            end;
            if FTask.FItems[i].FDeviceZoomEnd > 7 then begin
              VTaskInternalInfo.Volumes[VVolumeCount - 1].SubmapsPresent[skCoarse] := True;
              inc(VTaskInternalInfo.Volumes[VVolumeCount - 1].SubmapsTileCount[skCoarse]);
            end;
          end;
        end;

        inc(VTilesProcessed);
        ProgressFormUpdateOnProgress(VTilesProcessed, VTilesToProcess);
      end;
    end;

    // 2. Making and compiling MTX files.

    // Use '.' as a floating point independently of the user's locale preferences.
    GetLocaleFormatSettings(GetThreadLocale, VFormatSettings);
    VFormatSettings.DecimalSeparator := '.';

    if VVolumeCount = 0 then begin
      raise Exception.Create(FStrNoTilesToExport);
    end else begin
      ProgressInfo.SetFirstLine(FStrPhase2);
      ProgressFormUpdateOnProgress(0, VVolumeCount);
      VTilesProcessed := 0;
      for i:=0 to VVolumeCount - 1 do begin
        if CancelNotifier.IsOperationCanceled(OperationID) then begin
          exit;
        end;

        try
          // Writing MTX Headers
          for sk:=skFine to skCoarse do begin
            if VTaskInternalInfo.Volumes[i].SubmapsPresent[sk] then begin
              VSubmapIDs[sk] := FTask.FMapID;
              VSubmapMTXNames[sk] := IntToHex(FTask.FMapID, 8);
              VFileName := VTempFolder + VSubmapMTXNames[sk] + '.mtx';
              AssignFile(VMtxFiles[sk], VFileName); rewrite(VMtxFiles[sk]);
              VCreatedFiles.Add(VFileName);

              // H0 - Main header
              writeln(VMtxFiles[sk], Format('H0%.4X%10d%10d%10d%10d', [$601, 13 + VGenLevels[sk] + 4 * VTaskInternalInfo.Volumes[i].SubmapsTileCount[sk] + Length(InvariableHeaders), VTaskInternalInfo.Volumes[i].SubmapsTileCount[sk] + 1, 0, 0]));

              // H1 - Map bounds
              VCoords := @VTaskInternalInfo.MapBounds;
              writeln(VMtxFiles[sk],
                Format('H1%12.7f%12.7f%11.7f%11.7f', [VCoords.Left, VCoords.Right, VCoords.Bottom, VCoords.Top], VFormatSettings) +
                Format('%s%2d%5d%1s%1s%1s%1s%5d%s', ['P', 20, 16000, '0', '0', '0', '0', 65280, 'Raster Map'])
              );

              // H2 - Generalization Levels
              for j := 0 to 7 do begin
                if (j + Integer(sk) * 8) in VTaskInternalInfo.AvailableLevels then begin
                  writeln(VMtxFiles[sk], GeneralizationLevels[sk, j]);
                end;
              end;

              // H3 - Feature types
              writeln(VMtxFiles[sk], Format('H3%5d%1d%1d%3d%5d', [23670, 7, 0, 0, 0]));
              writeln(VMtxFiles[sk], Format('H3%5d%1d%1d%3d',    [20122, 7, 0, 1]));

              // H4 - Options
              writeln(VMtxFiles[sk], 'H4LA' + CodePages[FTask.FCodePageIndex]);
              writeln(VMtxFiles[sk], Format('H4TI%10d', [FTask.FMapID]));
              writeln(VMtxFiles[sk], 'H4ID' + IntToStr(FTask.FMapID));
              writeln(VMtxFiles[sk], Format('H4MD%2d', [FTask.FDrawOrder]));
              writeln(VMtxFiles[sk], Format('H4MS%5d%s', [FTask.FMapSeries, 'Raster Map']));
              writeln(VMtxFiles[sk], Format('H4MF%3d', [MapFormatToMF[FTask.FIMGMapFormat]]));
              writeln(VMtxFiles[sk], 'H4MG' + IntToStr(MapFormatToMG[FTask.FIMGMapFormat]));

              for j := Low(InvariableHeaders) to High(InvariableHeaders) do begin
                writeln(VMtxFiles[sk], InvariableHeaders[j]);
              end;

              // DATA_BOUNDS
              writeln(VMtxFiles[sk], 'A3  0      0     462350615020122    0    0');
              writeln(VMtxFiles[sk], Format('C%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s', [VCoords.Left, VCoords.Bottom, 'A', VCoords.Left, VCoords.Top, 'A', VCoords.Right, VCoords.Top, 'A', VCoords.Right, VCoords.Bottom, 'A'], VFormatSettings));

              inc(FTask.FMapID);
            end;
          end;

          // Tiles
          while VTaskInternalInfo.Volumes[i].TileCount > 0 do begin
            if CancelNotifier.IsOperationCanceled(OperationID) then begin
              exit;
            end;

            if VJPEGFileInfos[VTilesProcessed].FilePath <> '' then begin
              for sk:=skFine to skCoarse do begin
                if sk = skFine then begin
                  VStartLevel := Max(VJPEGFileInfos[VTilesProcessed].StartLevel, 0);
                  VEndLevel := Min(VJPEGFileInfos[VTilesProcessed].EndLevel, 7);
                end else begin
                  VStartLevel := Max(VJPEGFileInfos[VTilesProcessed].StartLevel - 8, 0);
                  VEndLevel := VJPEGFileInfos[VTilesProcessed].EndLevel - 8;
                end;

                if VStartLevel <= VEndLevel then begin
                  VCoords := @VJPEGFileInfos[VTilesProcessed].Coords;
                  writeln(VMtxFiles[sk], 'X001  ');
                  writeln(VMtxFiles[sk],
                    Format('X09%12.7f%12.7f%12.7f%12.7f', [VCoords.Left, VCoords.Bottom, VCoords.Right, VCoords.Top], VFormatSettings) +
                    Format('%-200s%2d%2d%3d%3d', [VJPEGFileInfos[VTilesProcessed].FilePath, VStartLevel, VEndLevel, 0, 0]));
                  writeln(VMtxFiles[sk], 'A3  0      0     4      10823670    0    0');
                  writeln(VMtxFiles[sk], Format('C%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s', [VCoords.Left, VCoords.Bottom, 'A', VCoords.Left, VCoords.Top, 'A', VCoords.Right, VCoords.Top, 'A', VCoords.Right, VCoords.Bottom, 'A'], VFormatSettings));
                end;
              end;

              dec(VTaskInternalInfo.Volumes[i].TileCount);
            end;
            inc(VTilesProcessed);
          end;
        finally
          for sk:=skFine to skCoarse do begin
            if VTaskInternalInfo.Volumes[i].SubmapsPresent[sk] then begin
              CloseFile(VMtxFiles[sk]);
            end;
          end;
        end;

        // Compiling MTX files with bld_gmap32.
        for sk:=skFine to skCoarse do begin
          if VTaskInternalInfo.Volumes[i].SubmapsPresent[sk] then begin
            FillChar(VProcessInfo, sizeof(VProcessInfo), 0);
            FillChar(VStartupInfo, sizeof(VStartupInfo), 0);

            VStartupInfo.cb := sizeof(VStartupInfo);
            VStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
            VStartupInfo.wShowWindow := SW_HIDE;

            if FTask.FMapCompilerLicensePath <> '' then begin
              VCommandLine := '/mpc "' + FTask.FMapCompilerLicensePath + '" '
            end else begin
              VCommandLine := '';
            end;

            VCommandLine := '"' + FTask.FMapCompilerPath + '" ' + VCommandLine + '/nep ' + VSubmapMTXNames[sk] + ' .';
            if CreateProcess(Nil, pChar(VCommandLine), Nil, Nil, False, 0, Nil, pChar(VTempFolder), VStartupInfo, VProcessInfo) then begin
              VWaitObjects[1] := VProcessInfo.hProcess;
              VWaitObjects[2] := FCancelEvent;
              try
                if WaitForMultipleObjects(2, @VWaitObjects[1], False, INFINITE) <> WAIT_OBJECT_0 then begin
                  TerminateProcess(VProcessInfo.hProcess, 0);
                  Abort;
                end;
              finally
                CloseHandle(VProcessInfo.hProcess);
                CloseHandle(VProcessInfo.hThread);
              end;

              // Checking if the compilation succeeded.
              // Looking for errors in log file.
              VFileName := VTempFolder + VSubmapMTXNames[sk] + '.log';
              if FileExists(VFileName) then begin
                VCreatedFiles.Add(VFileName);

                VLogFileStringList := TStringList.Create;
                try
                  VLogFileStringList.LoadFromFile(VFileName);
                  if pos('Completion status = FATAL ERROR', VLogFileStringList[VLogFileStringList.Count - 1]) > 0 then begin
                    VErrorMessage := '';
                    // Compilation failed. Finding out the reason.
                    for j:=0 to VLogFileStringList.Count - 1 do begin
                      if pos('ERROR!', VLogFileStringList[j]) > 0 then begin
                        VErrorMessage := VErrorMessage + StringReplace(VLogFileStringList[j], 'ERROR!', #13#10, []);
                      end;
                    end;

                    raise Exception.Create(Format(FStrCompileErrorFormat, [VErrorMessage]));
                  end;
                finally
                  VLogFileStringList.Free;
                end;
              end;

              // Checking if the map parts were made.
              for j:=1 to 3 do begin
                if (MapFormatSubfileExts[FTask.FIMGMapFormat, j] <> '') and not FileExists(VTempFolder + VSubmapMTXNames[sk] + MapFormatSubfileExts[FTask.FIMGMapFormat, j]) then begin
                  raise Exception.Create(FStrCompileErrorNoMessage);
                end
              end;
            end else begin
              raise Exception.Create(FStrCannotStartCompiler);
            end;
          end;
        end;

        // Making IMG file from sub-files.
        VFileName := FTargetFileName;
        if VVolumeCount > 1 then begin
          VFileName := VFileName + '_Part' + IntToStr(i + 1);
        end;
        VFileName := VFileName + FTargetFileExt;

        VCommandLine := '"' + FTask.FGMTPath + '" -j -o "' + VFileName + '" -m "' + FTask.FMapName;
        if VVolumeCount > 1 then begin
          VCommandLine := VCommandLine + ' Part ' + IntToStr(i + 1);
        end;
        VCommandLine := VCommandLine + '"';                      

        for sk:=skFine to skCoarse do begin
          for j:=1 to 3 do begin
            if VTaskInternalInfo.Volumes[i].SubmapsPresent[sk] and (MapFormatSubfileExts[FTask.FIMGMapFormat, j] <> '') then begin
              VCommandLine := VCommandLine + ' ' + VSubmapMTXNames[sk] + MapFormatSubfileExts[FTask.FIMGMapFormat, j];
              VCreatedFiles.Add(VTempFolder + VSubmapMTXNames[sk] + MapFormatSubfileExts[FTask.FIMGMapFormat, j]);
            end;
          end;
        end;

        FillChar(VProcessInfo, sizeof(VProcessInfo), 0);
        FillChar(VStartupInfo, sizeof(VStartupInfo), 0);
        VStartupInfo.cb := sizeof(VStartupInfo);
        VStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
        VStartupInfo.wShowWindow := SW_SHOWNORMAL;

        if CreateProcess(Nil, pChar(VCommandLine), Nil, Nil, False, DETACHED_PROCESS, Nil, pChar(VTempFolder), VStartupInfo, VProcessInfo) then begin
          VWaitObjects[1] := VProcessInfo.hProcess;
          VWaitObjects[2] := FCancelEvent;
          try
            if WaitForMultipleObjects(2, @VWaitObjects[1], False, INFINITE) <> WAIT_OBJECT_0 then begin
              TerminateProcess(VProcessInfo.hProcess, 0);
              Abort;
            end;
          finally
            CloseHandle(VProcessInfo.hProcess);
            CloseHandle(VProcessInfo.hThread);
          end;

          // Checking if the compilation succeeded.
          if not FileExists(VFileName) then begin
            raise Exception.Create(FStrIMGBuildError);
          end;
        end;

        ProgressFormUpdateOnProgress(i, VVolumeCount);
      end;
    end;
  finally
    CancelNotifier.RemoveListener(Self); 

    FreeAndNil(VTileHashTable);

    // 3. Deleting the files from temp folder.
    if not FTask.FKeepTempFiles then begin
      ProgressInfo.SetFirstLine(FStrPhase3);
      if VCreatedFiles.Count > 0 then begin
        ProgressFormUpdateOnProgress(0, VCreatedFiles.Count);
        // Delete files we've created.
        for i:=0 to VCreatedFiles.Count - 1 do begin
          if FileExists(VCreatedFiles[i]) then begin
            DeleteFile(VCreatedFiles[i]);
          end;

          ProgressFormUpdateOnProgress(i + 1, VCreatedFiles.Count);
        end;
      end;

      // Delete any other files.
      VSearchRes := FindFirst(VTempFolder + '*.*', faAnyFile, VSearchRec);
      try
        while VSearchRes = 0 do begin
          if VSearchRec.Attr and faDirectory = 0 then begin
            DeleteFile(VTempFolder + VSearchRec.Name);
          end;

          VSearchRes := FindNext(VSearchRec);
        end;
      finally
        FindClose(VSearchRec);
      end;
    end;

    FreeAndNil(VCreatedFiles);

    // Remove temp folder.
    if not FTask.FKeepTempFiles then begin
      RmDir(VTempFolder);
    end;
  end;
end;

end.
