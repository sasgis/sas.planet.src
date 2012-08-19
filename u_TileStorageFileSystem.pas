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

unit u_TileStorageFileSystem;

interface

uses
  Windows,
  Classes,
  SysUtils,
  GR32,
  i_BinaryData,
  i_SimpleTileStorageConfig,
  i_CoordConverter,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_TileFileNameParsersList,
  i_TileFileNameGeneratorsList,
  i_ContentTypeManager,
  i_InternalPerformanceCounter,
  i_TileFileNameParser,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

{$IFDEF DEBUG}
  {$DEFINE WITH_PERF_COUNTER}
{$ENDIF}

type
  TTileStorageFileSystem = class(TTileStorageAbstract)
  private
    FLock: IReadWriteSync;
    FCacheConfig: TMapTypeCacheConfigAbstract;
    FMainContentType: IContentTypeInfoBasic;
    FFormatSettings: TFormatSettings;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FTileFileNameParser: ITileFileNameParser;
    {$IFDEF WITH_PERF_COUNTER}
    FPerfCounterList: IInternalPerformanceCounterList;
    FGetTileInfoCounter: IInternalPerformanceCounter;
    FLoadTileCounter: IInternalPerformanceCounter;
    FDeleteTileCounter: IInternalPerformanceCounter;
    FDeleteTNECounter: IInternalPerformanceCounter;
    FSaveTileCounter: IInternalPerformanceCounter;
    FSaveTNECounter: IInternalPerformanceCounter;
    {$ENDIF}
    procedure CreateDirIfNotExists(const APath: string);
    function GetTileInfoByPath(
      const APath: string;
      const AVersionInfo: IMapVersionInfo;
      AIsLoadIfExists: Boolean
    ): ITileInfoBasic;
  public
    constructor Create(
      const AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      const ATileNameGeneratorList: ITileFileNameGeneratorsList;
      const ATileNameParserList: ITileFileNameParsersList;
      const AContentTypeManager: IContentTypeManager;
      const APerfCounterList: IInternalPerformanceCounterList
    );
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;
    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;
    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;
    function DeleteTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    ); override;
    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ); override;

    function ScanTiles(
      const AIgnoreTNE: Boolean
    ): IEnumTileInfo; override;
  end;

implementation

uses
  WideStrings,
  t_CommonTypes,
  c_CacheTypeCodes,
  i_TileIterator,
  i_FileNameIterator,
  i_StorageState,
  u_TileRectInfoShort,
  u_BinaryDataByMemStream,
  u_MapVersionFactorySimpleString,
  u_TileStorageTypeAbilities,
  u_TileIteratorByRect,
  u_FileNameIteratorFolderWithSubfolders,
  u_FoldersIteratorRecursiveByLevels,
  u_FileNameIteratorInFolderByMaskList,
  u_TreeFolderRemover,
  u_Synchronizer,
  u_TileInfoBasic;

const
  CTneFileExt = '.tne';

{ TTileStorageFileSystem }

constructor TTileStorageFileSystem.Create(
  const AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  const ATileNameGeneratorList: ITileFileNameGeneratorsList;
  const ATileNameParserList: ITileFileNameParsersList;
  const AContentTypeManager: IContentTypeManager;
  const APerfCounterList: IInternalPerformanceCounterList
);
var
  VCacheType: Byte;
begin
  inherited Create(
    TTileStorageTypeAbilitiesFileFolder.Create,
    TMapVersionFactorySimpleString.Create,
    AConfig
  );
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.DateSeparator := '-';
  FFormatSettings.ShortDateFormat := 'yyyy-MM-dd';
  FFormatSettings.TimeSeparator := '-';
  FFormatSettings.LongTimeFormat := 'HH-mm-ss';
  FFormatSettings.ShortTimeFormat := 'HH-mm-ss';
  FFormatSettings.ListSeparator := ';';
  FFormatSettings.TwoDigitYearCenturyWindow := 50;
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FLock := MakeSyncRW_Big(Self, False);
  FCacheConfig := TMapTypeCacheConfigFileSystem.Create(AConfig, AGlobalCacheConfig, ATileNameGeneratorList);
  FMainContentType := AContentTypeManager.GetInfoByExt(Config.TileFileExt);

  VCacheType := AConfig.CacheTypeCode;
  if VCacheType = c_File_Cache_Id_DEFAULT then begin
    VCacheType := AGlobalCacheConfig.DefCache;
  end;
  FTileFileNameParser := ATileNameParserList.GetParser(VCacheType);

  {$IFDEF WITH_PERF_COUNTER}
  FPerfCounterList := APerfCounterList.CreateAndAddNewSubList('FileSystem');
  FGetTileInfoCounter := FPerfCounterList.CreateAndAddNewCounter('GetTileInfo');
  FLoadTileCounter := FPerfCounterList.CreateAndAddNewCounter('LoadTile');
  FDeleteTileCounter := FPerfCounterList.CreateAndAddNewCounter('DeleteTile');
  FDeleteTNECounter := FPerfCounterList.CreateAndAddNewCounter('DeleteTNE');
  FSaveTileCounter := FPerfCounterList.CreateAndAddNewCounter('SaveTile');
  FSaveTNECounter := FPerfCounterList.CreateAndAddNewCounter('SaveTNE');
  {$ENDIF}
end;

destructor TTileStorageFileSystem.Destroy;
begin
  FreeAndNil(FCacheConfig);
  FTileFileNameParser := nil;
  inherited;
end;

procedure TTileStorageFileSystem.CreateDirIfNotExists(const APath: string);
var
  i: integer;
  VPath: string;
begin
  i := LastDelimiter(PathDelim, APath);
  VPath := copy(APath, 1, i);
  if not (DirectoryExists(VPath)) then begin
    ForceDirectories(VPath);
  end;
end;

function TTileStorageFileSystem.DeleteTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
{$IFDEF WITH_PERF_COUNTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUNTER}
  VCounterContext := FDeleteTileCounter.StartOperation;
  try
  {$ENDIF}
    Result := false;
    if StorageStateStatic.DeleteAccess <> asDisabled then begin
      try
        VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
        FLock.BeginWrite;
        try
          Result := DeleteFile(VPath);
        finally
          FLock.EndWrite;
        end;
        DeleteTNE(AXY, AZoom, AVersionInfo);
      except
        Result := false;
      end;
      if Result then begin
        NotifyTileUpdate(AXY, AZoom, AVersionInfo);
      end;
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FDeleteTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

function TTileStorageFileSystem.DeleteTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
{$IFDEF WITH_PERF_COUNTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUNTER}
  VCounterContext := FDeleteTNECounter.StartOperation;
  try
  {$ENDIF}
    Result := False;
    if StorageStateStatic.DeleteAccess <> asDisabled then begin
      try
        VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
        VPath := ChangeFileExt(VPath, CTneFileExt);
        FLock.BeginWrite;
        try
          Result := DeleteFile(VPath);
        finally
          FLock.EndWrite;
        end;
      except
        Result := false;
      end;
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FDeleteTNECounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

function TTileStorageFileSystem.GetAllowDifferentContentTypes: Boolean;
begin
  Result := False;
end;

function TTileStorageFileSystem.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageFileSystem.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageFileSystem.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, AZoom);
end;

function _GetFileDateTime(var AInfo: WIN32_FILE_ATTRIBUTE_DATA): TDateTime;
var
  VSysTime: TSystemTime;
  VFileTimePtr: PFileTime;
begin
  Result := 0;
  VFileTimePtr := nil;

  // last modified time (if exists)
  if (AInfo.ftLastWriteTime.dwLowDateTime <> 0) and (AInfo.ftLastWriteTime.dwHighDateTime <> 0) then begin
    VFileTimePtr := @(AInfo.ftLastWriteTime);
  end;

  // created time (if exists and greater)
  if (AInfo.ftCreationTime.dwLowDateTime <> 0) and (AInfo.ftCreationTime.dwHighDateTime <> 0) then begin
    if (nil = VFileTimePtr) or (CompareFileTime(AInfo.ftCreationTime, VFileTimePtr^) > 0) then begin
      VFileTimePtr := @(AInfo.ftCreationTime);
    end;
  end;

  // convert max value
  if (nil <> VFileTimePtr) then begin
    if (FileTimeToSystemTime(VFileTimePtr^, VSysTime) <> FALSE) then begin
      try
        Result := SystemTimeToDateTime(VSysTime);
      except
      end;
    end;
  end;
end;

procedure UpdateTileInfoByFile(
  const AIsTneFile: Boolean;
  const AIsLoadData: Boolean;
  const AFileName: string;
  var ATileInfo: TTileInfo
);
var
  VInfo: WIN32_FILE_ATTRIBUTE_DATA;
  VMemStream: TMemoryStream;
begin
  if GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @VInfo) <> FALSE then begin
    if AIsTneFile then begin
      ATileInfo.FInfoType := titTneExists;
      ATileInfo.FLoadDate := _GetFileDateTime(VInfo);
      ATileInfo.FData := nil;
      ATileInfo.FSize := 0;
    end else begin
      ATileInfo.FInfoType := titExists;
      ATileInfo.FLoadDate := _GetFileDateTime(VInfo);
      if AIsLoadData then begin
        VMemStream := TMemoryStream.Create;
        try
          VMemStream.LoadFromFile(AFileName);
          ATileInfo.FData := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
          VMemStream := nil;
        finally
          VMemStream.Free;
        end;
        ATileInfo.FSize := ATileInfo.FData.Size;
      end else begin
        ATileInfo.FData := nil;
        ATileInfo.FSize := VInfo.nFileSizeLow;
      end;
    end;
  end else begin
    ATileInfo.FInfoType := titNotExists;
    ATileInfo.FLoadDate := 0;
    ATileInfo.FData := nil;
    ATileInfo.FSize := 0;
  end;
end;

function TTileStorageFileSystem.GetTileInfoByPath(
  const APath: string;
  const AVersionInfo: IMapVersionInfo;
  AIsLoadIfExists: Boolean
): ITileInfoBasic;
var
  VTileInfo: TTileInfo;
begin
  FLock.BeginRead;
  try
    UpdateTileInfoByFile(False, AIsLoadIfExists, APath, VTileInfo);
    if VTileInfo.FInfoType = titExists then begin
      // tile exists
      if AIsLoadIfExists then begin
        Result :=
          TTileInfoBasicExistsWithTile.Create(
            VTileInfo.FLoadDate,
            VTileInfo.FData,
            nil,
            FMainContentType
          );
      end else begin
        Result :=
          TTileInfoBasicExists.Create(
            VTileInfo.FLoadDate,
            VTileInfo.FSize,
            nil,
            FMainContentType
          );
      end;
    end else begin
      UpdateTileInfoByFile(True, AIsLoadIfExists, ChangeFileExt(APath, CTneFileExt), VTileInfo);
      if VTileInfo.FInfoType = titTneExists then begin
        // tne exists
        Result := TTileInfoBasicTNE.Create(VTileInfo.FLoadDate, nil);
      end else begin
        // neither tile nor tne
        Result := FTileNotExistsTileInfo;
      end;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TTileStorageFileSystem.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): ITileRectInfo;
var
  VTileInfo: TTileInfo;
  VFileName: string;
  VRect: TRect;
  VZoom: Byte;
  VCount: TPoint;
  VItems: PTileInfoShortInternalArray;
  VIndex: Integer;
  VTile: TPoint;
  VIterator: ITileIterator;
  VFolderName: string;
  VPrevFolderName: string;
  VPrevFolderExist: Boolean;
  VFolderExists: Boolean;
begin
  Result := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VRect := ARect;
    VZoom := AZoom;
    Config.CoordConverter.CheckTileRect(VRect, VZoom);
    VCount.X := VRect.Right - VRect.Left;
    VCount.Y := VRect.Bottom - VRect.Top;
    if (VCount.X > 0) and (VCount.Y > 0) then begin
      VItems := GetMemory(VCount.X * VCount.Y * SizeOf(TTileInfoShortInternal));
      try
        VPrevFolderName := '';
        VPrevFolderExist := False;
        FLock.BeginRead;
        try
          VIterator := TTileIteratorByRect.Create(VRect);
          while VIterator.Next(VTile) do begin
            VIndex := (VTile.Y - VRect.Top) * VCount.X + (VTile.X - VRect.Left);
            VFileName := FCacheConfig.GetTileFileName(VTile, VZoom);
            VFolderName := ExtractFilePath(VFileName);

            if VFolderName = VPrevFolderName then begin
              VFolderExists := VPrevFolderExist;
            end else begin
              VFolderExists := DirectoryExists(VFolderName);
              VPrevFolderName := VFolderName;
              VPrevFolderExist := VFolderExists;
            end;
            if VFolderExists then begin
              UpdateTileInfoByFile(False, False, VFileName, VTileInfo);
              if VTileInfo.FInfoType = titExists then begin
                // tile exists
                VItems[VIndex].FInfoType := titExists;
                VItems[VIndex].FLoadDate := VTileInfo.FLoadDate;
                VItems[VIndex].FSize := VTileInfo.FSize;
              end else begin
                UpdateTileInfoByFile(True, False, ChangeFileExt(VFileName, CTneFileExt), VTileInfo);
                if VTileInfo.FInfoType = titTneExists then begin
                  // tne exists
                  VItems[VIndex].FInfoType := titTneExists;
                  VItems[VIndex].FLoadDate := VTileInfo.FLoadDate;
                  VItems[VIndex].FSize := 0;
                end else begin
                  // neither tile nor tne
                  VItems[VIndex].FLoadDate := 0;
                  VItems[VIndex].FSize := 0;
                  VItems[VIndex].FInfoType := titNotExists;
                end;
              end;
            end else begin
              // neither tile nor tne
              VItems[VIndex].FLoadDate := 0;
              VItems[VIndex].FSize := 0;
              VItems[VIndex].FInfoType := titNotExists;
            end;
          end;
        finally
          FLock.EndRead;
        end;
        Result :=
          TTileRectInfoShort.CreateWithOwn(
            VRect,
            VZoom,
            nil,
            FMainContentType,
            VItems
          );
        VItems := nil;
      finally
        if VItems <> nil then begin
          FreeMemory(VItems);
        end;
      end;
    end;
  end;
end;

function TTileStorageFileSystem.GetTileInfo(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VPath: String;
{$IFDEF WITH_PERF_COUNTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  Result := nil;
  {$IFDEF WITH_PERF_COUNTER}
  VCounterContext := FGetTileInfoCounter.StartOperation;
  try
  {$ENDIF}
    if StorageStateStatic.ReadAccess <> asDisabled then begin
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      Result := GetTileInfoByPath(VPath, AVersionInfo, AMode = gtimWithData);
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FGetTileInfoCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

procedure TTileStorageFileSystem.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AData: IBinaryData
);
var
  VPath: String;
  VFileStream: TFileStream;
{$IFDEF WITH_PERF_COUNTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUNTER}
  VCounterContext := FSaveTileCounter.StartOperation;
  try
  {$ENDIF}
    if StorageStateStatic.WriteAccess <> asDisabled then begin
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      FLock.BeginWrite;
      try
        CreateDirIfNotExists(VPath);
        VFileStream := TFileStream.Create(VPath, fmCreate);
        try
          VFileStream.Size := AData.Size;
          VFileStream.Position := 0;
          VFileStream.WriteBuffer(AData.Buffer^, AData.Size);
        finally
          VFileStream.Free;
        end;
      finally
        FLock.EndWrite;
      end;
      NotifyTileUpdate(AXY, AZoom, AVersionInfo);
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FSaveTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

procedure TTileStorageFileSystem.SaveTNE(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
);
var
  VPath: String;
  VNow: TDateTime;
  VDateString: string;
  VFileStream: TFileStream;
{$IFDEF WITH_PERF_COUNTER}
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUNTER}
  VCounterContext := FSaveTNECounter.StartOperation;
  try
  {$ENDIF}
    if StorageStateStatic.WriteAccess <> asDisabled then begin
      VPath := FCacheConfig.GetTileFileName(AXY, AZoom);
      VPath := ChangeFileExt(VPath, CTneFileExt);
      FLock.BeginWrite;
      try
        if not FileExists(VPath) then begin
          CreateDirIfNotExists(VPath);
          VNow := Now;
          DateTimeToString(VDateString, 'yyyy-mm-dd-hh-nn-ss', VNow, FFormatSettings);
          VFileStream := TFileStream.Create(VPath, fmCreate);
          try
            VFileStream.WriteBuffer(VDateString[1], Length(VDateString) * SizeOf(VDateString[1]));
          finally
            VFileStream.Free;
          end;
        end;
      finally
        FLock.EndWrite;
      end;
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FSaveTNECounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

{ TEnumTileInfoByFileIterator }

type
  TEnumTileInfoByFileIterator = class(TInterfacedObject, IEnumTileInfo)
  private
    FFilesIterator: IFileNameIterator;
    FTileFileNameParser: ITileFileNameParser;
    FStorageState: IStorageStateChangeble;
    FMainContentType: IContentTypeInfoBasic;
    FLock: IReadWriteSync;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      ALock: IReadWriteSync;
      AFilesIterator: IFileNameIterator;
      ATileFileNameParser: ITileFileNameParser;
      AStorageState: IStorageStateChangeble;
      AMainContentType: IContentTypeInfoBasic
    );
  end;

constructor TEnumTileInfoByFileIterator.Create(
  ALock: IReadWriteSync;
  AFilesIterator: IFileNameIterator;
  ATileFileNameParser: ITileFileNameParser;
  AStorageState: IStorageStateChangeble;
  AMainContentType: IContentTypeInfoBasic);
begin
  inherited Create;
  FLock := ALock;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FStorageState := AStorageState;
  FMainContentType := AMainContentType;
end;

function TEnumTileInfoByFileIterator.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTileFileName: string;
  VTileFileNameW: WideString;
  VTileXY: TPoint;
  VTileZoom: Byte;
  VFullFileName: string;
begin
  Result := False;
  while FFilesIterator.Next(VTileFileNameW) do begin
    VTileFileName := VTileFileNameW;
    if FTileFileNameParser.GetTilePoint(VTileFileName, VTileXY, VTileZoom) then begin
      VFullFileName := FFilesIterator.GetRootFolderName + VTileFileNameW;
      if FStorageState.GetStatic.ReadAccess <> asDisabled then begin
        FLock.BeginRead;
        try
          UpdateTileInfoByFile(
            ExtractFileExt(VFullFileName) = CTneFileExt,
            True,
            VFullFileName,
            ATileInfo
          );
          ATileInfo.FTile := VTileXY;
          ATileInfo.FZoom := VTileZoom;
          ATileInfo.FVersionInfo := nil;
          ATileInfo.FContentType := FMainContentType;
          if ATileInfo.FInfoType <> titNotExists then begin
            Result := True;
            Break;
          end;
        finally
          FLock.EndRead;
        end;
      end;
    end;
  end;
end;

function TTileStorageFileSystem.ScanTiles(
  const AIgnoreTNE: Boolean): IEnumTileInfo;
const
  cMaxFolderDepth = 10;
var
  VProcessFileMasks: TWideStringList;
  VFilesIterator: IFileNameIterator;
  VFoldersIteratorFactory: IFileNameIteratorFactory;
  VFilesInFolderIteratorFactory: IFileNameIteratorFactory;
begin
  VProcessFileMasks := TWideStringList.Create;
  try
    VProcessFileMasks.Add('*' + FMainContentType.GetDefaultExt);
    if not AIgnoreTNE then begin
      VProcessFileMasks.Add('*.tne');
    end;

    VFoldersIteratorFactory :=
      TFoldersIteratorRecursiveByLevelsFactory.Create(cMaxFolderDepth);

    VFilesInFolderIteratorFactory :=
      TFileNameIteratorInFolderByMaskListFactory.Create(VProcessFileMasks, True);

    VFilesIterator := TFileNameIteratorFolderWithSubfolders.Create(
      FCacheConfig.BasePath,
      '',
      VFoldersIteratorFactory,
      VFilesInFolderIteratorFactory
    );
    Result :=
      TEnumTileInfoByFileIterator.Create(
        FLock,
        VFilesIterator,
        FTileFileNameParser,
        State,
        FMainContentType
      );
  finally
    VProcessFileMasks.Free;
  end;
end;

end.
