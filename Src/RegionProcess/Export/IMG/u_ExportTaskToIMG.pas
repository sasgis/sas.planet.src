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

unit u_ExportTaskToIMG;

interface

uses
  Windows,
  SysUtils,
  Classes,
  t_GeoTypes,
  t_ExportToIMGTask,
  i_RegionProcessProgressInfo,
  i_GeometryLonLat,
  i_TileIteratorFactory,
  i_BitmapPostProcessing,
  i_Listener,
  i_TileIterator,
  u_RegionProcessTaskAbstract;

type
  TSubmapKind = (skFine, skCoarse);

  TJpegFileInfo = record
    Coords: TDoubleRect;
    FilePath: string;
    StartLevel: Integer;
    EndLevel: Integer;
    FileSize: Integer;
  end;
  PJpegFileInfo = ^TJpegFileInfo;

  TVolumeInfo = record
    VolumeIndex: Integer;
    TileCount: Int64;
    JPEGFileInfos: array of TJpegFileInfo;
    SubmapsPresent: array [TSubmapKind] of Boolean;
    SubmapsTileCount: array [TSubmapKind] of Int64;
    SubmapMTXNames: array [TSubmapKind] of string;
  end;

  TExportTaskToIMG = class(TRegionProcessTaskAbstract)
  private
    FTask: TExportToIMGTask;
    FTargetFileName, FTargetFileExt: string;
    FBitmapPostProcessing: IBitmapPostProcessing;
    FCancelEvent: THandle;
    FCancelListener: IListener;
    FTempFolder: string;

    // Derived info about the task
    FTilesToProcess: Int64;
    FTileIterators: array of ITileIterator;
    FMapBounds: TDoubleRect;
    FAvailableGeneralizationLevels: set of 0..12;
    FGeneralizationLevelCount: array [TSubmapKind] of Integer;

    FStrPhase1Format: string;
    FStrPhase2: string;
    FStrPhase3: string;
    FStrNoTilesToExport: string;
    FStrCompileErrorFormat: string;
    FStrCompileErrorNoMessage: string;
    FStrCannotStartCompiler: string;
    FStrCannotStartGMT: string;
    FStrIMGBuildError: string;
    FStrNoFreeSpaceErrorFormat: string;

    FFormatSettings: TFormatSettings;

    FTileProcessErrorFmt: string;

    procedure InitializeTaskInternalInfo;
    procedure ClearTempFolder;
    function WriteMTXFiles(var AVolumeInfo: TVolumeInfo): Boolean;
    function CompileMaps(var AVolumeInfo: TVolumeInfo; AAddVolumeSuffix: Boolean): Boolean;

    procedure OnTaskCancel;

    procedure WriteLog(const AMsg: string);
    procedure WriteLogFmt(const AMsg: string; const AFmt: array of const);
  protected
    procedure ProcessRegion; override;
  public
    constructor Create(
      const AProgressInfo: IRegionProcessProgressInfoInternal;
      const ATileIteratorFactory: ITileIteratorFactory;
      const ATargetFile: string;
      const APolygon: IGeometryLonLatPolygon;
      const ATask: TExportToIMGTask;
      const ABitmapPostProcessing: IBitmapPostProcessing
    );
    destructor Destroy; override;
  end;

implementation

uses
  Types,
  ShLwApi,
  Math,
  libcrc32,
  gnugettext,
  i_TileStorage,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  i_ProjectionSet,
  i_Projection,
  i_Bitmap32Static,
  i_BinaryData,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_TileStorageAbilities,
  u_AnsiStr,
  u_Dialogs,
  u_FileSystemFunc,
  u_ListenerByEvent,
  u_ResStrings,
  u_GeoFunc;

constructor TExportTaskToIMG.Create(
  const AProgressInfo: IRegionProcessProgressInfoInternal;
  const ATileIteratorFactory: ITileIteratorFactory;
  const ATargetFile: string;
  const APolygon: IGeometryLonLatPolygon;
  const ATask: TExportToIMGTask;
  const ABitmapPostProcessing: IBitmapPostProcessing
);
begin
  inherited Create(
    AProgressInfo,
    APolygon,
    ATileIteratorFactory
  );

  FTargetFileName := ChangeFileExt(ATargetFile, '');
  FTargetFileExt := ExtractFileExt(ATargetFile);

  FTask := ATask;
  FBitmapPostProcessing := ABitmapPostProcessing;

  FCancelEvent := CreateEvent(nil, False, False, nil);
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnTaskCancel);

  FStrPhase1Format := _('Saving tiles. %s %d %s');
  FStrNoTilesToExport := _('No tiles to export!');
  FStrPhase2 := _('Compiling the map(s). ');
  FStrCompileErrorFormat := _('Map compilation failed. The map compiler reported the following error: %s');
  FStrCompileErrorNoMessage := _('Map compilation failed.');
  FStrCannotStartCompiler := _('Could not start the map compiler: %s.');
  FStrCannotStartGMT := _('Could not start GMapTool: %s.');
  FStrIMGBuildError := _('Could not build IMG file.');
  FStrPhase3 := _('Cleaning up. ');

  FStrNoFreeSpaceErrorFormat := _(
    'There is not enough free space on drive "%0:s"!' + #13#10 +
    'Up to %1:d MB may be needed to store temporary files, but only %2:d MB is available.' + #13#10 +
    'Are you sure you want to continue?'
  );

  FTileProcessErrorFmt := '[IMG] ' + SAS_ERR_TileProcessError;

  // Use '.' as a floating point independently of the user's locale preferences.
  FFormatSettings := TFormatSettings.Create;
  FFormatSettings.DecimalSeparator := '.';
end;

destructor TExportTaskToIMG.Destroy;
begin
  CloseHandle(FCancelEvent);
  FCancelListener := nil;
  inherited;
end;

procedure TExportTaskToIMG.OnTaskCancel;
begin
  SetEvent(FCancelEvent);
end;

const
  MaxFilePathInMTX = 200;

  CodePages: array [0..12] of string = (
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

  InvariableHeaders: array [1..19] of string = (
    'H4CP1',   'H4CS2',
    'H4CRSAS.Planet development team@2026',
    'H4LL0',   'H4MA  255', 'H4MB1',
    'H4MC  4', 'H4ML  255', 'H4MO0',
    'H4MT1',   'H4NB1',     'H4NT0',
    'H4PF1',   'H4PP0',     'H4HP0',
    'H4PN01',  'H4SP1',     'H4TL150',
    'H4WM0'
  );

  GeneralizationLevels: array [TSubmapKind, 0..7] of string = (
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
  MapFormatSubfileExts: array [TIMGMapFormat, 1..3] of string = (
    ('.tre', '.lbl', '.rgn'),
    ('.gmp', '', ''),
    ('.gmp', '', '')
  );

procedure TExportTaskToIMG.WriteLog(const AMsg: string);
var
  VLogFile: TextFile;
  VLogFileName: string;
begin
  try
    VLogFileName := FTempFolder + 'SASPlanet.log';
    AssignFile(VLogFile, VLogFileName);
    try
      if FileExists(VLogFileName) then begin
        Append(VLogFile);
      end else begin
        Rewrite(VLogFile);
      end;
      Writeln(VLogFile, FormatDateTime('hh:nn:ss ', Now) + StringReplace(AMsg, #13#10, ' ', [rfReplaceAll]));
    finally
      CloseFile(VLogFile);
    end;
  except
    {$IFDEF DEBUG}
    on E: Exception do begin
      ShowErrorMessageSync(E.ClassName + ': ' + E.Message);
    end;
    {$ENDIF}
  end;
end;

procedure TExportTaskToIMG.WriteLogFmt(const AMsg: string; const AFmt: array of const);
begin
  Self.WriteLog(Format(AMsg, AFmt));
end;

procedure TExportTaskToIMG.InitializeTaskInternalInfo;
var
  I: Integer;
  VProjection: IProjection;
  VRect: TDoubleRect;
begin
  // Calculating the number of source tiles.
  FTilesToProcess := 0;
  SetLength(FTileIterators, Length(FTask.FItems));
  for I := 0 to Length(FTask.FItems) - 1 do begin
    VProjection := FTask.FItems[I].FSourceTileStorage.ProjectionSet.Zooms[FTask.FItems[I].FSourceScale];
    FTileIterators[I] := Self.MakeTileIterator(VProjection);
    Inc(FTilesToProcess, FTileIterators[I].TilesTotal);

    VRect := VProjection.TileRect2LonLatRect(FTileIterators[I].TilesRect.Rect);
    if I = 0 then begin
      FMapBounds := VRect;
    end else begin
      FMapBounds := UnionLonLatRects(VRect, FMapBounds);
    end;
    FAvailableGeneralizationLevels := FAvailableGeneralizationLevels + [FTask.FItems[I].FDeviceZoomStart..FTask.FItems[I].FDeviceZoomEnd];
  end;

  FGeneralizationLevelCount[skFine] := 0;
  FGeneralizationLevelCount[skCoarse] := 0;
  for I := 0 to 12 do begin
    if I in FAvailableGeneralizationLevels then begin
      if I <= 7 then begin
        Inc(FGeneralizationLevelCount[skFine]);
      end else begin
        Inc(FGeneralizationLevelCount[skCoarse]);
      end;
    end;
  end;
end;

function TExportTaskToIMG.WriteMTXFiles(var AVolumeInfo: TVolumeInfo): Boolean;
var
  I: Integer;
  sk: TSubmapKind;
  VMtxFiles: array [TSubmapKind] of TextFile;
  VFileName: string;
  VCoords: TDoubleRect;
  VTileIndex: Integer;
  VStartLevel: Integer;
  VEndLevel: Integer;
begin
  Result := False;

  try
    VTileIndex := 0;

    // Writing MTX Headers
    for sk := skFine to skCoarse do begin
      if AVolumeInfo.SubmapsPresent[sk] then begin
        AVolumeInfo.SubmapMTXNames[sk] := IntToHex(FTask.FMapID, 8);
        VFileName := FTempFolder + AVolumeInfo.SubmapMTXNames[sk] + '.mtx';
        AssignFile(VMtxFiles[sk], VFileName);
        Rewrite(VMtxFiles[sk]);

        // H0 - Main header
        WriteLn(VMtxFiles[sk], Format('H0%.4X%10d%10d%10d%10d', [$601, 13 + FGeneralizationLevelCount[sk] + 4 * AVolumeInfo.SubmapsTileCount[sk] + Length(InvariableHeaders), AVolumeInfo.SubmapsTileCount[sk] + 1, 0, 0]));

        // H1 - Map bounds
        VCoords := FMapBounds;
        WriteLn(VMtxFiles[sk],
          Format('H1%12.7f%12.7f%11.7f%11.7f', [VCoords.Left, VCoords.Right, VCoords.Bottom, VCoords.Top], FFormatSettings) +
          Format('%s%2d%5d%1s%1s%1s%1s%5d%s', ['P', 20, 16000, '0', '0', '0', '0', 65280, 'Raster Map'])
        );

        // H2 - Generalization Levels
        for I := 0 to 7 do begin
          if (I + Integer(sk) * 8) in FAvailableGeneralizationLevels then begin
            WriteLn(VMtxFiles[sk], GeneralizationLevels[sk, I]);
          end;
        end;

        // H3 - Feature types
        WriteLn(VMtxFiles[sk], Format('H3%5d%1d%1d%3d%5d', [23670, 7, 0, 0, 0]));
        WriteLn(VMtxFiles[sk], Format('H3%5d%1d%1d%3d',    [20122, 7, 0, 1]));

        // H4 - Options
        WriteLn(VMtxFiles[sk], 'H4LA' + CodePages[FTask.FCodePageIndex]);
        WriteLn(VMtxFiles[sk], Format('H4TI%10d', [FTask.FMapID]));
        WriteLn(VMtxFiles[sk], 'H4ID' + IntToStr(FTask.FMapID));
        WriteLn(VMtxFiles[sk], Format('H4MD%2d', [FTask.FDrawOrder]));
        WriteLn(VMtxFiles[sk], Format('H4MS%5d%s', [FTask.FMapSeries, 'Raster Map']));
        WriteLn(VMtxFiles[sk], Format('H4MF%3d', [MapFormatToMF[FTask.FIMGMapFormat]]));
        WriteLn(VMtxFiles[sk], 'H4MG' + IntToStr(MapFormatToMG[FTask.FIMGMapFormat]));

        for I := Low(InvariableHeaders) to High(InvariableHeaders) do begin
          WriteLn(VMtxFiles[sk], InvariableHeaders[I]);
        end;

        // DATA_BOUNDS
        WriteLn(VMtxFiles[sk], 'A3  0      0     462350615020122    0    0');
        WriteLn(VMtxFiles[sk], Format('C%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s', [VCoords.Left, VCoords.Bottom, 'A', VCoords.Left, VCoords.Top, 'A', VCoords.Right, VCoords.Top, 'A', VCoords.Right, VCoords.Bottom, 'A'], FFormatSettings));

        Inc(FTask.FMapID);
      end;
    end;

    // Saving tile info into MTX.
    while AVolumeInfo.TileCount > 0 do begin
      if CancelNotifier.IsOperationCanceled(OperationID) then begin
        Exit;
      end;

      if AVolumeInfo.JPEGFileInfos[VTileIndex].FilePath <> '' then begin
        for sk := skFine to skCoarse do begin
          if sk = skFine then begin
            VStartLevel := Max(AVolumeInfo.JPEGFileInfos[VTileIndex].StartLevel, 0);
            VEndLevel := Min(AVolumeInfo.JPEGFileInfos[VTileIndex].EndLevel, 7);
          end else begin
            VStartLevel := Max(AVolumeInfo.JPEGFileInfos[VTileIndex].StartLevel - 8, 0);
            VEndLevel := AVolumeInfo.JPEGFileInfos[VTileIndex].EndLevel - 8;
          end;

          if VStartLevel <= VEndLevel then begin
            VCoords := AVolumeInfo.JPEGFileInfos[VTileIndex].Coords;
            WriteLn(VMtxFiles[sk], 'X001  ');
            WriteLn(VMtxFiles[sk],
              Format('X09%12.7f%12.7f%12.7f%12.7f', [VCoords.Left, VCoords.Bottom, VCoords.Right, VCoords.Top], FFormatSettings) +
              Format('%-200s%2d%2d%3d%3d', [AVolumeInfo.JPEGFileInfos[VTileIndex].FilePath, VStartLevel, VEndLevel, 0, 0]));
            WriteLn(VMtxFiles[sk], 'A3  0      0     4      10823670    0    0');
            WriteLn(VMtxFiles[sk], Format('C%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s%12.7f%11.7f%s', [VCoords.Left, VCoords.Bottom, 'A', VCoords.Left, VCoords.Top, 'A', VCoords.Right, VCoords.Top, 'A', VCoords.Right, VCoords.Bottom, 'A'], FFormatSettings));
          end;
        end;

        Dec(AVolumeInfo.TileCount);
      end;
      Inc(VTileIndex);
    end;
  finally
    // Delphi XE2 cannot correcly compile 'for' loop in this block,
    // so manual loop unrolling is necessary.
    if AVolumeInfo.SubmapsPresent[skFine] then begin
      CloseFile(VMtxFiles[skFine]);
    end;
    if AVolumeInfo.SubmapsPresent[skCoarse] then begin
      CloseFile(VMtxFiles[skCoarse]);
    end;
  end;

  Result := True;
end;

function StartProcessAndWaitForCompletion(const ACommandLine, AStartPath, AStartError: string; AShowWindow, ACreationFlags: DWORD; CancellationEvent: THandle; AOutput: TStrings = nil): Boolean;
var
  VStartupInfo: TStartupInfo;
  VProcessInfo: TProcessInformation;
  VWaitObjects: array [0..1] of THandle;
  VSecurityAttributes: TSecurityAttributes;
  VStdOutPipeRead, VStdOutPipeWrite: THandle;
  VOutputReaderThread: TThread;
begin
  Result := False;

  VStdOutPipeRead := 0;
  VStdOutPipeWrite := 0;
  VOutputReaderThread := nil;


  FillChar(VStartupInfo, SizeOf(VStartupInfo), 0);
  VStartupInfo.cb := SizeOf(VStartupInfo);
  VStartupInfo.wShowWindow := AShowWindow;
  VStartupInfo.dwFlags := STARTF_USESHOWWINDOW;

  if Assigned(AOutput) then begin
    // Setup security attributes for the pipe
    VSecurityAttributes.nLength := SizeOf(VSecurityAttributes);
    VSecurityAttributes.bInheritHandle := True;
    VSecurityAttributes.lpSecurityDescriptor := nil;

    // Create a single pipe for both streams
    if not CreatePipe(VStdOutPipeRead, VStdOutPipeWrite, @VSecurityAttributes, 0) then begin
      ShowErrorMessageSync(Format(AStartError, [SysErrorMessage(GetLastError)]));
      Exit;
    end;

    VStartupInfo.dwFlags := VStartupInfo.dwFlags or STARTF_USESTDHANDLES;
    VStartupInfo.hStdOutput := VStdOutPipeWrite;
    VStartupInfo.hStdError := VStdOutPipeWrite; // Redirect both streams to the same pipe
  end;

  // Start the process
  FillChar(VProcessInfo, SizeOf(VProcessInfo), 0);
  if not CreateProcess(nil, PChar(ACommandLine), nil, nil, True, ACreationFlags, nil, PChar(AStartPath), VStartupInfo, VProcessInfo) then begin
    ShowErrorMessageSync(Format(AStartError, [SysErrorMessage(GetLastError)]));
    if Assigned(AOutput) then begin
      CloseHandle(VStdOutPipeRead);
      CloseHandle(VStdOutPipeWrite);
    end;
    Exit;
  end;

  if Assigned(AOutput) then begin
    // Close the write-end of the pipe in this process
    CloseHandle(VStdOutPipeWrite);

    // Asynchronous stream reading
    VOutputReaderThread := TThread.CreateAnonymousThread(
      procedure
      var
        VBuffer: array[0..4095] of AnsiChar;
        VBytesRead: DWORD;
        VOutputLine: AnsiString;
      begin
        while ReadFile(VStdOutPipeRead, VBuffer, SizeOf(VBuffer) - 1, VBytesRead, nil) and (VBytesRead > 0) do begin
          VBuffer[VBytesRead] := #0;
          SetString(VOutputLine, VBuffer, VBytesRead);
          AOutput.Add(string(VOutputLine));
          if TThread.Current.CheckTerminated then Break;
        end;
      end);
    VOutputReaderThread.FreeOnTerminate := False;
    VOutputReaderThread.Start;
  end;

  try
    // Wait for the process to complete or for cancellation
    VWaitObjects[0] := VProcessInfo.hProcess;
    VWaitObjects[1] := CancellationEvent;
    if WaitForMultipleObjects(2, @VWaitObjects[0], False, INFINITE) = WAIT_OBJECT_0 then begin
      Result := True
    end else begin
      TerminateProcess(VProcessInfo.hProcess, 0);
      if Assigned(VOutputReaderThread) then begin
        VOutputReaderThread.Terminate; // Signal the thread to terminate
      end;
    end;
  finally
    // Wait for the reader thread to finish and free resources
    if Assigned(VOutputReaderThread) then begin
      VOutputReaderThread.WaitFor;
      VOutputReaderThread.Free;
    end;

    // Close handles
    CloseHandle(VProcessInfo.hProcess);
    CloseHandle(VProcessInfo.hThread);
    if Assigned(AOutput) then begin
      CloseHandle(VStdOutPipeRead);
    end;
  end;
end;

function TExportTaskToIMG.CompileMaps(var AVolumeInfo: TVolumeInfo; AAddVolumeSuffix: Boolean): Boolean;
var
  sk: TSubmapKind;
  I: Integer;
  VCommandLine: string;
  VFileName: string;
  VLogFileStringList: TStringList;
  VErrorMessage: string;
begin
  Result := False;

  // Compiling MTX files with bld_gmap32.
  for sk := skFine to skCoarse do begin
    if AVolumeInfo.SubmapsPresent[sk] then begin
      if FTask.FMapCompilerLicensePath <> '' then begin
        VCommandLine := '/mpc "' + FTask.FMapCompilerLicensePath + '" ';
      end else begin
        VCommandLine := '';
      end;

      VCommandLine := '"' + FTask.FMapCompilerPath + '" ' + VCommandLine + '/nep ' + AVolumeInfo.SubmapMTXNames[sk] + ' .';
      WriteLog(VCommandLine);
      if StartProcessAndWaitForCompletion(VCommandLine, FTempFolder, FStrCannotStartCompiler, SW_HIDE, 0, FCancelEvent) then begin
        // Checking if the compilation succeeded.
        // Looking for errors in log file.
        VFileName := FTempFolder + AVolumeInfo.SubmapMTXNames[sk] + '.log';
        if FileExists(VFileName) then begin
          VLogFileStringList := TStringList.Create;
          try
            VLogFileStringList.LoadFromFile(VFileName);
            if Pos('Completion status = FATAL ERROR', VLogFileStringList[VLogFileStringList.Count - 1]) > 0 then begin
              VErrorMessage := '';
              // Compilation failed. Finding out the reason.
              for I := 0 to VLogFileStringList.Count - 1 do begin
                if Pos('ERROR!', VLogFileStringList[I]) > 0 then begin
                  VErrorMessage := VErrorMessage + StringReplace(VLogFileStringList[I], 'ERROR!', #13#10, []);
                end;
              end;

              ShowErrorMessageSync(Format(FStrCompileErrorFormat, [VErrorMessage]));
              Exit;
            end;
          finally
            VLogFileStringList.Free;
          end;
        end;

        // Checking if the map parts were made.
        for I := 1 to 3 do begin
          if (MapFormatSubfileExts[FTask.FIMGMapFormat, I] <> '') and not FileExists(FTempFolder + AVolumeInfo.SubmapMTXNames[sk] + MapFormatSubfileExts[FTask.FIMGMapFormat, I]) then begin
            ShowErrorMessageSync(FStrCompileErrorNoMessage);
            Exit;
          end;
        end;
      end else begin
        Exit;
      end;
    end;
  end;

  // Making IMG file from sub-files.
  VFileName := FTargetFileName;
  if AAddVolumeSuffix then begin
    VFileName := VFileName + '_Part' + IntToStr(AVolumeInfo.VolumeIndex + 1);
  end;
  VFileName := VFileName + FTargetFileExt;

  VCommandLine := '"' + FTask.FGMTPath + '" -j -v -o "' + VFileName + '" -m "' + FTask.FMapName;
  if AAddVolumeSuffix then begin
    VCommandLine := VCommandLine + ' Part ' + IntToStr(AVolumeInfo.VolumeIndex + 1);
  end;
  VCommandLine := VCommandLine + '"';

  for sk := skFine to skCoarse do begin
    for I := 1 to 3 do begin
      if AVolumeInfo.SubmapsPresent[sk] and (MapFormatSubfileExts[FTask.FIMGMapFormat, I] <> '') then begin
        VCommandLine := VCommandLine + ' ' + AVolumeInfo.SubmapMTXNames[sk] + MapFormatSubfileExts[FTask.FIMGMapFormat, I];
      end;
    end;
  end;

  VLogFileStringList := TStringList.Create;
  try
    WriteLog(VCommandLine);
    if not StartProcessAndWaitForCompletion(VCommandLine, FTempFolder, FStrCannotStartGMT, SW_SHOWNORMAL, DETACHED_PROCESS, FCancelEvent, VLogFileStringList) then begin
      Exit
    end;
  finally
    try
      VLogFileStringList.SaveToFile(FTempFolder + 'GMapTool.log');
    finally
      VLogFileStringList.Free;
    end;
  end;

  // Checking if the compilation succeeded.
  Result := FileExists(VFileName);

  if not Result then begin
    ShowErrorMessageSync(FStrIMGBuildError);
  end;
end;

procedure TExportTaskToIMG.ClearTempFolder;
var
  VFilesDeleted: Int64;
  VSearchRec: TSearchRec;
begin
  if not FTask.FKeepTempFiles then begin
    ProgressInfo.SetFirstLine(FStrPhase3);
    ProgressFormUpdateOnProgress(0, FTilesToProcess);

    // Delete any files from the folder.
    VFilesDeleted := 0;
    if FindFirst(FTempFolder + '*.*', faAnyFile, VSearchRec) = 0 then
    try
      repeat
        if VSearchRec.Attr and faDirectory = 0 then begin
          DeleteFile(FTempFolder + VSearchRec.Name);
          Inc(VFilesDeleted);
          if VFilesDeleted mod 256 = 0 then begin
            ProgressFormUpdateOnProgress(VFilesDeleted, FTilesToProcess);
          end;
        end;
      until FindNext(VSearchRec) <> 0;
    finally
      FindClose(VSearchRec);
    end;

    ProgressFormUpdateOnProgress(VFilesDeleted, FTilesToProcess);

    // Remove temp folder.
    try
      if not RemoveDir(FTempFolder) then begin
        RaiseLastOSError;
      end;
    except
      // It's not a big deal if we could not delete the temp folder.
      {$IFDEF DEBUG}
      on E: Exception do begin
        ShowWarningMessageSync(E.ClassName + ': ' + E.Message + #13#10 + FTempFolder);
      end;
      {$ENDIF}
    end;
  end;
end;

procedure ClearVolumeInfo(var AVolumeInfo: TVolumeInfo);
var
  sk: TSubmapKind;
begin
  // Volume Index is left intact by this procedure.
  AVolumeInfo.TileCount := 0;
  AVolumeInfo.JPEGFileInfos := nil;
  for sk:=skFine to skCoarse do begin
    AVolumeInfo.SubmapsPresent[sk] := False;
    AVolumeInfo.SubmapsTileCount[sk] := 0;
    AVolumeInfo.SubmapMTXNames[sk] := '';
  end;
end;

function SizeToMb(const ABytes: Int64): Int64; inline;
begin
  Result := ABytes div (1024 * 1024);
end;

procedure TExportTaskToIMG.ProcessRegion;
var
  I: Integer;
  VBitmapTile: IBitmap32Static;
  VZoom: Byte;
  VTile: TPoint;
  VTileIterator: ITileIterator;
  VSaver: IBitmapTileSaver;
  VProjectionSet: IProjectionSet;
  VTilesProcessed: Int64;
  VData: IBinaryData;
  VTileStorage: ITileStorage;
  VVersion: IMapVersionRequest;
  VTileInfo: ITileInfoWithData;
  VContentTypeInfoBitmap: IContentTypeInfoBitmap;
  VCrc32: Cardinal;
  VFileName: string;
  VFile: File;
  VAbsolutePath: string;
  VRelativePath: string;
  VFileCacheFlagIsValid: Boolean;
  VFileCache: Boolean;
  VUseFileFromCache: Boolean;
  VIndex: Integer;
  VTileHash: string;
  VRunningTotalTileSize: Int64;
  VCurrentVolumeInfo: TVolumeInfo;
  VTileHashTable: TStringList;
  VJpegTileInfo: PJpegFileInfo;
  VErrorMessage: string;
  VDiskFreeSpace: Int64;
  VEstimatedSize: Int64;
begin
  ProgressInfo.SetCaption(SAS_STR_ExportTiles);

  FTempFolder := FTask.FTempFilesPath + 'sasgis-tmp-' + IntToHex(GetCurrentThreadId, 8) + '\';

  // Create temp folder.
  if not ForceDirectories(FTempFolder) then begin
    try
      RaiseLastOSError;
    except
      on E: Exception do begin
        ShowErrorMessageSync(E.ClassName + ': ' + E.Message + #13#10 + FTempFolder);
        Exit;
      end;
    end;
  end;

  VDiskFreeSpace := GetDiskFree(ExtractFileDrive(FTempFolder)[1]);
  WriteLogFmt('Available disk space for temporary files: %d MB', [SizeToMb(VDiskFreeSpace)]);

  ResetEvent(FCancelEvent);
  CancelNotifier.AddListener(FCancelListener);
  try
    VTileHashTable := TStringList.Create;
    try
      // Preparing the internal structures used during the export.
      InitializeTaskInternalInfo;

      VSaver := FTask.FBitmapTileSaver;

      VCurrentVolumeInfo.VolumeIndex := 0;
      ClearVolumeInfo(VCurrentVolumeInfo);

      // Writing the tiles to temp folder.
      ProgressInfo.SetFirstLine(Format(FStrPhase1Format, [SAS_STR_AllSaves, FTilesToProcess, SAS_STR_Files]));
      VTilesProcessed := 0;
      ProgressFormUpdateOnProgress(VTilesProcessed, FTilesToProcess);
      VRunningTotalTileSize := 0;

      VEstimatedSize := FTilesToProcess * (15*1024); // 15k per tile
      VEstimatedSize := VEstimatedSize * 2;

      WriteLogFmt('Estimated temporary files size: %d MB', [SizeToMb(VEstimatedSize)]);
      WriteLogFmt('Number of tiles to process: %d', [FTilesToProcess]);

      if VEstimatedSize > VDiskFreeSpace then begin
        VErrorMessage := Format(FStrNoFreeSpaceErrorFormat, [ExtractFileDrive(FTempFolder), SizeToMb(VEstimatedSize), SizeToMb(VDiskFreeSpace)]);
        if ShowWarningMessageSync(VErrorMessage, MB_YESNO) <> ID_YES then begin
          Exit;
        end;
      end;

      for I := 0 to Length(FTask.FItems) - 1 do begin
        WriteLogFmt('Processing task %d', [I]);

        VTileStorage := FTask.FItems[I].FSourceTileStorage;
        VVersion := FTask.FItems[I].FSourceMapVersion;
        VZoom := FTask.FItems[I].FSourceScale;
        VProjectionSet := VTileStorage.ProjectionSet;
        VTileIterator := FTileIterators[I];

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
            Exit;
          end;

          if Supports(VTileStorage.GetTileInfoEx(VTile, VZoom, VVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
            VData := nil;
            VUseFileFromCache := False;

            if Assigned(FBitmapPostProcessing) or not SameTextA(VTileInfo.ContentType.GetContentType, 'image/jpg') then begin
              if Supports(VTileInfo.ContentType, IContentTypeInfoBitmap, VContentTypeInfoBitmap) then begin
                try
                  VBitmapTile := VContentTypeInfoBitmap.GetLoader.Load(VTileInfo.TileData);
                  if Assigned(FBitmapPostProcessing) then begin
                    VBitmapTile := FBitmapPostProcessing.Process(VBitmapTile);
                  end;
                  if Assigned(VBitmapTile) then begin
                    VData := VSaver.Save(VBitmapTile);
                  end;
                except
                  on E: Exception do begin
                    VErrorMessage := Format(FTileProcessErrorFmt, [VTile.X, VTile.Y, VZoom + 1, E.ClassName, E.Message]);
                    WriteLog(VErrorMessage);
                    if ShowErrorMessageSync(VErrorMessage, MB_YESNO) <> ID_YES then begin
                      Exit;
                    end;
                    VData := nil;
                  end;
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
                    if PathRelativePathTo(PChar(VRelativePath), PChar(FTempFolder), FILE_ATTRIBUTE_DIRECTORY, PChar(VAbsolutePath), FILE_ATTRIBUTE_NORMAL) then begin
                      VRelativePath := PChar(VRelativePath);
                      VUseFileFromCache := Length(AnsiString(VRelativePath)) <= MaxFilePathInMTX;
                    end;
                  end;
                end;
              end;
            end;

            if Assigned(VData) then begin
              // Save the current map volume when the next tile doesn't fit into the specified limit.
              // TODO: If a tile is references more than once, its size should only be counted for the first time (in each submap independently!).
              if VRunningTotalTileSize + VData.Size > FTask.FVolumeSize then begin
                // Write MTX files and compile IMG files from them.
                ProgressInfo.SetFirstLine(FStrPhase2);

                if not WriteMTXFiles(VCurrentVolumeInfo) or not CompileMaps(VCurrentVolumeInfo, True) then begin
                  // Exit if the user has cancelled the export during map compilation.
                  Exit;
                end;
                ClearVolumeInfo(VCurrentVolumeInfo);
                VTileHashTable.Clear;
                VRunningTotalTileSize := 0;
                Inc(VCurrentVolumeInfo.VolumeIndex);

                ProgressInfo.SetFirstLine(Format(FStrPhase1Format, [SAS_STR_AllSaves, FTilesToProcess, SAS_STR_Files]));
              end;

              VCrc32 := crc32(0, VData.Buffer, VData.Size);
              VTileHash := IntToHex(VCrc32, 8) + '_' + IntToHex(VData.Size, 8);
              VIndex := VTileHashTable.IndexOf(VTileHash);
              if VIndex <> -1 then begin
                VIndex := Integer(VTileHashTable.Objects[VIndex]) - 1;
              end;

              // The same tile already saved. Reusing the existing path.
              if VIndex <> -1 then begin
                VFileName := VCurrentVolumeInfo.JPEGFileInfos[VIndex].FilePath;
              end else begin
                if not VUseFileFromCache then begin
                  VFileName := FTempFolder + VTileHash + '.jpg';
                  if not FileExists(VFileName) then begin
                    AssignFile(VFile, VFileName);
                    try
                      Rewrite(VFile, 1);
                      BlockWrite(VFile, VData.Buffer^, VData.Size);
                    finally
                      CloseFile(VFile);
                    end;
                  end;

                  // Stripping temp folder path from the filename.
                  VFileName := VTileHash;
                end else begin
                  VFileName := VRelativePath;
                end;

                VTileHashTable.AddObject(VTileHash, TObject(VCurrentVolumeInfo.TileCount + 1));
              end;

              SetLength(VCurrentVolumeInfo.JPEGFileInfos, VCurrentVolumeInfo.TileCount + 1);
              VJpegTileInfo := @VCurrentVolumeInfo.JPEGFileInfos[VCurrentVolumeInfo.TileCount];
              VJpegTileInfo.Coords := VProjectionSet.Zooms[VZoom].TilePos2LonLatRect(VTile);
              VJpegTileInfo.FilePath := ChangeFileExt(VFileName, '');
              VJpegTileInfo.StartLevel := FTask.FItems[I].FDeviceZoomStart;
              VJpegTileInfo.EndLevel := FTask.FItems[I].FDeviceZoomEnd;
              VJpegTileInfo.FileSize := VData.Size;

              if FTask.FItems[I].FDeviceZoomStart <= 7 then begin
                VCurrentVolumeInfo.SubmapsPresent[skFine] := True;
                Inc(VCurrentVolumeInfo.SubmapsTileCount[skFine]);
              end;
              if FTask.FItems[I].FDeviceZoomEnd > 7 then begin
                VCurrentVolumeInfo.SubmapsPresent[skCoarse] := True;
                Inc(VCurrentVolumeInfo.SubmapsTileCount[skCoarse]);
              end;

              Inc(VRunningTotalTileSize, VData.Size);
              Inc(VCurrentVolumeInfo.TileCount);
            end;
          end;

          Inc(VTilesProcessed);
          ProgressFormUpdateOnProgress(VTilesProcessed, FTilesToProcess);
        end;
      end;
    finally
      FreeAndNil(VTileHashTable);
    end;

    // If we haven't saved anything, report the user of empty export.
    if (VCurrentVolumeInfo.VolumeIndex = 0) and (VRunningTotalTileSize = 0) then begin
      ShowErrorMessageSync(FStrNoTilesToExport);
      Exit;
    end else begin
      // Otherwise compile the last volume (in a multi-volume case), or the only volume (in a single-volume export).
      if WriteMTXFiles(VCurrentVolumeInfo) then begin
        if not CompileMaps(VCurrentVolumeInfo, VCurrentVolumeInfo.VolumeIndex > 0) then begin
          Exit;
        end;
      end;
    end;
  finally
    CancelNotifier.RemoveListener(FCancelListener);

    // Deleting the files from temp folder.
    ClearTempFolder;
  end;
end;

end.
