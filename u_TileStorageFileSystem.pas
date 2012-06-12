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
  Types,
  Classes,
  SysUtils,
  GR32,
  i_BinaryData,
  i_FillingMapColorer,
  i_OperationNotifier,
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
      const AVersionInfo: IMapVersionInfo
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
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;
    function GetTileInfo(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; override;
    function GetTileRectInfo(
      const ARect: TRect;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; override;

    function LoadTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      out ATileInfo: ITileInfoBasic
    ): IBinaryData; override;

    function DeleteTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;
    function DeleteTNE(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AData: IBinaryData
    ); override;
    procedure SaveTNE(
      const AXY: TPoint;
      const Azoom: byte;
      const AVersionInfo: IMapVersionInfo
    ); override;

    function LoadFillingMap(
      AOperationID: Integer;
      const ACancelNotifier: IOperationNotifier;
      btm: TCustomBitmap32;
      const AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const AColorer: IFillingMapColorer
    ): boolean; override;

    procedure Scan(
      const AOnTileStorageScan: TOnTileStorageScan;
      const AIgnoreTNE: Boolean;
      const ARemoveTileAfterProcess: Boolean
    ); override;
  end;

implementation

uses
  WideStrings,
  t_CommonTypes,
  t_GeoTypes,
  i_TileIterator,
  i_FileNameIterator,
  u_TileRectInfoShort,
  u_BinaryDataByMemStream,
  u_MapVersionFactorySimpleString,
  u_TileStorageTypeAbilities,
  u_TileIteratorByRect,
  u_FileNameIteratorFolderWithSubfolders,
  u_FoldersIteratorRecursiveByLevels,
  u_FileNameIteratorInFolderByMaskList,
  u_TreeFolderRemover,
  u_TileInfoBasic;

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
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FCacheConfig := TMapTypeCacheConfig.Create(AConfig, AGlobalCacheConfig, ATileNameGeneratorList);
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
  const Azoom: byte;
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
        VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
        FLock.BeginWrite;
        try
          Result := (DeleteFile(PChar(VPath)) <> FALSE);
        finally
          FLock.EndWrite;
        end;
        DeleteTNE(AXY, Azoom, AVersionInfo);
      except
        Result := false;
      end;
      if Result then begin
        NotifyTileUpdate(AXY, Azoom, AVersionInfo);
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
  const Azoom: byte;
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
        VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
        VPath := ChangeFileExt(VPath, '.tne');
        FLock.BeginWrite;
        try
          Result := (DeleteFile(PChar(VPath)) <> FALSE);
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
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, Azoom);
end;

function TTileStorageFileSystem.GetTileInfoByPath(
  const APath: string;
  const AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  VInfo: WIN32_FILE_ATTRIBUTE_DATA;

  function _GetAttributesEx(const AFileName: String): Boolean;
  begin
    Result := (GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @VInfo) <> FALSE);
  end;

  function _GetFileDateTime: TDateTime;
  var
    VSysTime: TSystemTime;
    VFileTimePtr: PFileTime;
  begin
    Result := 0;
    VFileTimePtr := nil;

    // last modified time (if exists)
    if (VInfo.ftLastWriteTime.dwLowDateTime <> 0) and (VInfo.ftLastWriteTime.dwHighDateTime <> 0) then begin
      VFileTimePtr := @(VInfo.ftLastWriteTime);
    end;

    // created time (if exists and greater)
    if (VInfo.ftCreationTime.dwLowDateTime <> 0) and (VInfo.ftCreationTime.dwHighDateTime <> 0) then begin
      if (nil = VFileTimePtr) or (CompareFileTime(VInfo.ftCreationTime, VFileTimePtr^) > 0) then begin
        VFileTimePtr := @(VInfo.ftCreationTime);
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

begin
  FLock.BeginRead;
  try
    if _GetAttributesEx(APath) then begin
      // tile exists
      Result := TTileInfoBasicExists.Create(
        _GetFileDateTime,
        VInfo.nFileSizeLow,
        nil,
        FMainContentType
      );
    end else if _GetAttributesEx(ChangeFileExt(APath, '.tne')) then begin
      // tne exists
      Result := TTileInfoBasicTNE.Create(_GetFileDateTime, nil);
    end else begin
      // neither tile nor tne
      Result := FTileNotExistsTileInfo;
    end;
  finally
    FLock.EndRead;
  end;
end;

function TTileStorageFileSystem.GetTileRectInfo(
  const ARect: TRect;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
): ITileRectInfo;
var
  VInfo: WIN32_FILE_ATTRIBUTE_DATA;

  function _GetAttributesEx(const AFileName: String): Boolean;
  begin
    Result := (GetFileAttributesEx(PChar(AFileName), GetFileExInfoStandard, @VInfo) <> FALSE);
  end;

  function _GetFileDateTime: TDateTime;
  var
    VSysTime: TSystemTime;
    VFileTimePtr: PFileTime;
  begin
    Result := 0;
    VFileTimePtr := nil;

    // last modified time (if exists)
    if (VInfo.ftLastWriteTime.dwLowDateTime <> 0) and (VInfo.ftLastWriteTime.dwHighDateTime <> 0) then begin
      VFileTimePtr := @(VInfo.ftLastWriteTime);
    end;

    // created time (if exists and greater)
    if (VInfo.ftCreationTime.dwLowDateTime <> 0) and (VInfo.ftCreationTime.dwHighDateTime <> 0) then begin
      if (nil = VFileTimePtr) or (CompareFileTime(VInfo.ftCreationTime, VFileTimePtr^) > 0) then begin
        VFileTimePtr := @(VInfo.ftCreationTime);
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

var
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
    VZoom := Azoom;
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
              if _GetAttributesEx(VFileName) then begin
                // tile exists
                VItems[VIndex].FLoadDate := _GetFileDateTime;
                VItems[VIndex].FSize := VInfo.nFileSizeLow;
                VItems[VIndex].FInfoType := titExists;
              end else if _GetAttributesEx(ChangeFileExt(VFileName, '.tne')) then begin
                // tne exists
                VItems[VIndex].FLoadDate := _GetFileDateTime;
                VItems[VIndex].FSize := 0;
                VItems[VIndex].FInfoType := titTneExists;
              end else begin
                // neither tile nor tne
                VItems[VIndex].FLoadDate := 0;
                VItems[VIndex].FSize := 0;
                VItems[VIndex].FInfoType := titNotExists;
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
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo
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
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      Result := GetTileInfoByPath(VPath, AVersionInfo);
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FGetTileInfoCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

function TTileStorageFileSystem.LoadFillingMap(
  AOperationID: Integer;
  const ACancelNotifier: IOperationNotifier;
  btm: TCustomBitmap32;
  const AXY: TPoint;
  Azoom, ASourceZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const AColorer: IFillingMapColorer
): boolean;
var
  VPixelsRect: TRect;
  VRelativeRect: TDoubleRect;
  VSourceTilesRect: TRect;
  VCurrTile: TPoint;
  VTileSize: TPoint;
  VSourceTilePixels: TRect;
  VSolidDrow: Boolean;
  VIterator: ITileIterator;
  VFileName: string;
  VFolderName: string;
  VTileColor: TColor32;
  VPrevFolderName: string;
  VPrevFolderExist: Boolean;
  VFolderExists: Boolean;
  VGeoConvert: ICoordConverter;
  VTileInfo: ITileInfoBasic;
  VTile: TPoint;
begin
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    Result := true;
    try
      VGeoConvert := Config.CoordConverter;
      VTile := AXY;
      VGeoConvert.CheckTilePosStrict(VTile, Azoom, True);
      VGeoConvert.CheckZoom(ASourceZoom);

      VPixelsRect := VGeoConvert.TilePos2PixelRect(VTile, Azoom);

      VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left, VPixelsRect.Bottom - VPixelsRect.Top);

      btm.Width := VTileSize.X;
      btm.Height := VTileSize.Y;
      btm.Clear(0);

      VRelativeRect := VGeoConvert.TilePos2RelativeRect(VTile, Azoom);
      VSourceTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
      VPrevFolderName := '';
      VPrevFolderExist := False;
      begin
        VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left)) or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left));
        VIterator := TTileIteratorByRect.Create(VSourceTilesRect);

        while VIterator.Next(VCurrTile) do begin
          if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
            break;
          end;

          VFileName := FCacheConfig.GetTileFileName(VCurrTile, ASourceZoom);
          VFolderName := ExtractFilePath(VFileName);

          if VFolderName = VPrevFolderName then begin
            VFolderExists := VPrevFolderExist;
          end else begin
            VFolderExists := DirectoryExists(VFolderName);
            VPrevFolderName := VFolderName;
            VPrevFolderExist := VFolderExists;
          end;

          if VFolderExists then begin
            VTileInfo := GetTileInfoByPath(VFileName, AVersionInfo);
          end else begin
            VTileInfo := FTileNotExistsTileInfo;
          end;

          VTileColor := AColorer.GetColor(VTileInfo);
          if VTileColor <> 0 then begin
            if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
              break;
            end;
            VRelativeRect := VGeoConvert.TilePos2RelativeRect(VCurrTile, ASourceZoom);
            VSourceTilePixels := VGeoConvert.RelativeRect2PixelRect(VRelativeRect, Azoom);
            if VSourceTilePixels.Left < VPixelsRect.Left then begin
              VSourceTilePixels.Left := VPixelsRect.Left;
            end;
            if VSourceTilePixels.Top < VPixelsRect.Top then begin
              VSourceTilePixels.Top := VPixelsRect.Top;
            end;
            if VSourceTilePixels.Right > VPixelsRect.Right then begin
              VSourceTilePixels.Right := VPixelsRect.Right;
            end;
            if VSourceTilePixels.Bottom > VPixelsRect.Bottom then begin
              VSourceTilePixels.Bottom := VPixelsRect.Bottom;
            end;
            VSourceTilePixels.Left := VSourceTilePixels.Left - VPixelsRect.Left;
            VSourceTilePixels.Top := VSourceTilePixels.Top - VPixelsRect.Top;
            VSourceTilePixels.Right := VSourceTilePixels.Right - VPixelsRect.Left;
            VSourceTilePixels.Bottom := VSourceTilePixels.Bottom - VPixelsRect.Top;
            if not VSolidDrow then begin
              Dec(VSourceTilePixels.Right);
              Dec(VSourceTilePixels.Bottom);
            end;
            if ((VSourceTilePixels.Right - VSourceTilePixels.Left) = 1) and
              ((VSourceTilePixels.Bottom - VSourceTilePixels.Top) = 1) then begin
              btm.Pixel[VSourceTilePixels.Left, VSourceTilePixels.Top] := VTileColor;
            end else begin
              btm.FillRectS(VSourceTilePixels.Left, VSourceTilePixels.Top, VSourceTilePixels.Right, VSourceTilePixels.Bottom, VTileColor);
            end;
          end;
        end;
      end;
      if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
        Result := false;
      end;
    except
      Result := false;
    end;
  end else begin
    Result := False;
  end;
end;

function TTileStorageFileSystem.LoadTile(
  const AXY: TPoint;
  const Azoom: byte;
  const AVersionInfo: IMapVersionInfo;
  out ATileInfo: ITileInfoBasic
): IBinaryData;
var
  VPath: String;
  VMemStream: TMemoryStream;
{$IFDEF WITH_PERF_COUNTER}
var
  VCounterContext: TInternalPerformanceCounterContext;
{$ENDIF}
begin
  {$IFDEF WITH_PERF_COUNTER}
  VCounterContext := FLoadTileCounter.StartOperation;
  try
  {$ENDIF}
    Result := nil;
    if StorageStateStatic.ReadAccess <> asDisabled then begin
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      ATileInfo := GetTileInfoByPath(VPath, AVersionInfo);
      if ATileInfo.GetIsExists then begin
        VMemStream := TMemoryStream.Create;
        try
          FLock.BeginRead;
          try
            VMemStream.LoadFromFile(VPath);
            Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
            // owned successfully
            VMemStream := nil;
          finally
            FLock.EndRead;
          end;
        finally
          VMemStream.Free;
        end;
      end;
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FLoadTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

procedure TTileStorageFileSystem.SaveTile(
  const AXY: TPoint;
  const Azoom: byte;
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
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
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
      NotifyTileUpdate(AXY, Azoom, AVersionInfo);
    end;
  {$IFDEF WITH_PERF_COUNTER}
  finally
    FSaveTileCounter.FinishOperation(VCounterContext);
  end;
  {$ENDIF}
end;

procedure TTileStorageFileSystem.SaveTNE(
  const AXY: TPoint;
  const Azoom: byte;
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
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      VPath := ChangeFileExt(VPath, '.tne');
      FLock.BeginWrite;
      try
        if not FileExists(VPath) then begin
          CreateDirIfNotExists(VPath);
          VNow := Now;
          DateTimeToString(VDateString, 'yyyy-mm-dd-hh-nn-ss', VNow, FFormatSettings);
          VFileStream := TFileStream.Create(VPath, fmCreate);
          try
            VFileStream.Write(VDateString[1], Length(VDateString) * SizeOf(VDateString[1]));
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

procedure TTileStorageFileSystem.Scan(
  const AOnTileStorageScan: TOnTileStorageScan;
  const AIgnoreTNE: Boolean;
  const ARemoveTileAfterProcess: Boolean
);
const
  cMaxFolderDepth = 100;
var
  VTileXY: TPoint;
  VTileZoom: Byte;
  VTileFileName: string;
  VTileFileNameW: WideString;
  VTileInfoBasic: ITileInfoBasic;
  VTileBinData: IBinaryData;
  VProcessFileMasks: TWideStringList;
  VFilesIterator: IFileNameIterator;
  VFoldersIteratorFactory: IFileNameIteratorFactory;
  VFilesInFolderIteratorFactory: IFileNameIteratorFactory;
  VAbort: Boolean;
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

    VAbort := False;

    while VFilesIterator.Next(VTileFileNameW) do begin
      VTileFileName := WideCharToString(PWideChar(VTileFileNameW));
      if FTileFileNameParser.GetTilePoint(VTileFileName, VTileXY, VTileZoom) then begin
        VTileBinData := Self.LoadTile(VTileXY, VTileZoom, nil, VTileInfoBasic);
        VAbort :=
          not AOnTileStorageScan(
            Self,
            VTileFileName,
            VTileXY,
            VTileZoom,
            VTileInfoBasic,
            VTileBinData
          );
        if (not VAbort and ARemoveTileAfterProcess) then begin
          if VTileInfoBasic.IsExists then begin
            Self.DeleteTile(VTileXY, VTileZoom, nil);
          end else if VTileInfoBasic.IsExistsTNE then begin
            Self.DeleteTNE(VTileXY, VTileZoom, nil);
          end;
        end;
      end else begin
        VAbort := True;
      end;
      if VAbort then begin
        Break;
      end;
    end;

    if (not VAbort and ARemoveTileAfterProcess) then begin
      FullRemoveDir(FCacheConfig.BasePath, True, False, True);
    end;

  finally
    VProcessFileMasks.Free;
  end;
end;

end.
