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

unit u_TileStorageFileSystem;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_BinaryData,
  i_CoordConverter,
  i_MapVersionInfo,
  i_MapVersionFactory,
  i_MapVersionRequest,
  i_ContentTypeInfo,
  i_NotifierOperation,
  i_NotifierTilePyramidUpdate,
  i_TileInfoBasic,
  i_TileInfoBasicMemCache,
  i_TileStorageAbilities,
  i_TileStorage,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  u_TileStorageAbstract;

type
  TTileStorageFileSystem = class(TTileStorageAbstract)
  private
    FTileInfoMemCache: ITileInfoBasicMemCache;
    FMainContentType: IContentTypeInfoBasic;
    FFileExt: string;
    FTileFileNameParser: ITileFileNameParser;
    FFileNameGenerator: ITileFileNameGenerator;

    FFsLock: IReadWriteSync;
    FTileNotExistsTileInfo: ITileInfoBasic;

    procedure CreateDirIfNotExists(const APath: string);
    function GetTileInfoByPath(
      const APath: string;
      const AVersionInfo: IMapVersionInfo;
      AIsLoadIfExists: Boolean
    ): ITileInfoBasic;
  protected
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
    function GetTileInfoEx(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;
    function GetTileRectInfo(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ARect: TRect;
      const AZoom: byte;
      const AVersionInfo: IMapVersionRequest
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;
    function SaveTile(
      const AXY: TPoint;
      const AZoom: byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData;
      const AIsOverwrite: Boolean
    ): Boolean; override;

    function ScanTiles(
      const AIgnoreTNE: Boolean;
      const AIgnoreMultiVersionTiles: Boolean
    ): IEnumTileInfo; override;
  public
    constructor Create(
      const AStorageTypeAbilities: ITileStorageTypeAbilities;
      const AStorageForceAbilities: ITileStorageAbilities;
      const AGeoConverter: ICoordConverter;
      const ATileNotifier: INotifierTilePyramidUpdateInternal;
      const AStoragePath: string;
      const AMainContentType: IContentTypeInfoBasic;
      const AMapVersionFactory: IMapVersionFactory;
      const ACacheTileInfo: ITileInfoBasicMemCache;
      const ATileNameGenerator: ITileFileNameGenerator;
      const ATileNameParser: ITileFileNameParser
    );

  end;

implementation

uses
  Types,
  WideStrings,
  t_CommonTypes,
  i_FileNameIterator,
  i_StorageState,
  u_TileRectInfoShort,
  u_BinaryDataByMemStream,
  u_TileIteratorByRect,
  u_FileNameIteratorFolderWithSubfolders,
  u_FoldersIteratorRecursiveByLevels,
  u_FileNameIteratorInFolderByMaskList,
  u_Synchronizer,
  u_TileInfoBasic,
  u_BaseInterfacedObject;

const
  CTneFileExt = '.tne';

{ TTileStorageFileSystem }

constructor TTileStorageFileSystem.Create(
  const AStorageTypeAbilities: ITileStorageTypeAbilities;
  const AStorageForceAbilities: ITileStorageAbilities;
  const AGeoConverter: ICoordConverter;
  const ATileNotifier: INotifierTilePyramidUpdateInternal;
  const AStoragePath: string;
  const AMainContentType: IContentTypeInfoBasic;
  const AMapVersionFactory: IMapVersionFactory;
  const ACacheTileInfo: ITileInfoBasicMemCache;
  const ATileNameGenerator: ITileFileNameGenerator;
  const ATileNameParser: ITileFileNameParser
);
begin
  Assert(AGeoConverter <> nil);
  Assert(AStoragePath <> '');
  Assert(AMainContentType <> nil);
  Assert(ATileNameGenerator <> nil);
  Assert(ATileNameParser <> nil);
  inherited Create(
    AStorageTypeAbilities,
    AStorageForceAbilities,
    AMapVersionFactory,
    AGeoConverter,
    ATileNotifier,
    AStoragePath
  );
  FTileInfoMemCache := ACacheTileInfo;
  FMainContentType := AMainContentType;
  FTileFileNameParser := ATileNameParser;
  FFileNameGenerator := ATileNameGenerator;

  FFileExt := FMainContentType.GetDefaultExt;
  FFsLock := GSync.SyncStdRecursive.Make(Self.ClassName);
  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
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
  VFileName: string;
  VTneName: string;
begin
  Result := false;
  if GetState.GetStatic.DeleteAccess <> asDisabled then begin
    try
      VPath :=
        StoragePath +
        FFileNameGenerator.GetTileFileName(AXY, AZoom);
      VFileName := VPath + FFileExt;
      VTneName := VPath + CTneFileExt;

      FFsLock.BeginWrite;
      try
        Result := DeleteFile(VFileName) or DeleteFile(VTneName);
      finally
        FFsLock.EndWrite;
      end;
    except
      Result := false;
    end;
    if Result then begin
      if Assigned(FTileInfoMemCache) then begin
        FTileInfoMemCache.Add(
          AXY,
          AZoom,
          nil,
          TTileInfoBasicNotExists.Create(0, nil)
        );
      end;
      NotifyTileUpdate(AXY, AZoom, nil);
    end;
  end;
end;

function TTileStorageFileSystem.GetTileFileName(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result :=
    StoragePath +
    FFileNameGenerator.GetTileFileName(AXY, AZoom) +
    FFileExt;
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
  FFsLock.BeginRead;
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
    FFsLock.EndRead;
  end;
end;

function TTileStorageFileSystem.GetTileInfoEx(const AXY: TPoint;
  const AZoom: byte; const AVersionInfo: IMapVersionRequest;
  const AMode: TGetTileInfoMode): ITileInfoBasic;
begin
  Result := GetTileInfo(AXY, AZoom, nil, AMode);
end;

function TTileStorageFileSystem.GetTileRectInfo(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ARect: TRect;
  const AZoom: byte;
  const AVersionInfo: IMapVersionRequest
): ITileRectInfo;
var
  VTileInfo: TTileInfo;
  VFileName: string;
  VRect: TRect;
  VZoom: Byte;
  VCount: TPoint;
  VItems: TArrayOfTileInfoShortInternal;
  VIndex: Integer;
  VTile: TPoint;
  VIterator: TTileIteratorByRectRecord;
  VFolderName: string;
  VPrevFolderName: string;
  VPrevFolderExist: Boolean;
  VFolderExists: Boolean;
begin
  Result := nil;
  if GetState.GetStatic.ReadAccess <> asDisabled then begin
    VRect := ARect;
    VZoom := AZoom;
    GeoConverter.ValidateTileRect(VRect, VZoom);
    VCount.X := VRect.Right - VRect.Left;
    VCount.Y := VRect.Bottom - VRect.Top;
    if (VCount.X > 0) and (VCount.Y > 0) and (VCount.X <= 2048) and (VCount.Y <= 2048) then begin
      SetLength(VItems, VCount.X * VCount.Y);
      VPrevFolderName := '';
      VPrevFolderExist := False;
      VIterator.Init(VRect);
      while VIterator.Next(VTile) do begin
        if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
          Result := nil;
          Exit;
        end;
        VIndex := TTileRectInfoShort.TileInRectToIndex(VTile, VRect);
        Assert(VIndex >=0);
        if VIndex >= 0 then begin
          VFileName := StoragePath + FFileNameGenerator.GetTileFileName(VTile, VZoom);
          VFolderName := ExtractFilePath(VFileName);

          if VFolderName = VPrevFolderName then begin
            VFolderExists := VPrevFolderExist;
          end else begin
            VFolderExists := DirectoryExists(VFolderName);
            VPrevFolderName := VFolderName;
            VPrevFolderExist := VFolderExists;
          end;
          if VFolderExists then begin
            UpdateTileInfoByFile(False, False, VFileName + FFileExt, VTileInfo);
            if VTileInfo.FInfoType = titExists then begin
              // tile exists
              VItems[VIndex].FInfoType := titExists;
              VItems[VIndex].FLoadDate := VTileInfo.FLoadDate;
              VItems[VIndex].FSize := VTileInfo.FSize;
            end else begin
              UpdateTileInfoByFile(True, False, VFileName + CTneFileExt, VTileInfo);
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
begin
  Result := nil;
  if Assigned(FTileInfoMemCache) then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom, nil, AMode, True);
    if Result <> nil then begin
      Exit;
    end;
  end;
  if GetState.GetStatic.ReadAccess <> asDisabled then begin
    VPath :=
      StoragePath +
      FFileNameGenerator.GetTileFileName(AXY, AZoom) +
      FFileExt;
    Result := GetTileInfoByPath(VPath, nil, AMode = gtimWithData);
    if Assigned(FTileInfoMemCache) then begin
      FTileInfoMemCache.Add(AXY, AZoom, nil, Result);
    end;
  end;
end;

function TTileStorageFileSystem.SaveTile(
  const AXY: TPoint;
  const AZoom: byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData;
  const AIsOverwrite: Boolean
): Boolean;
var
  VPath: String;
  VFileName: string;
  VTneName: string;
  VHandle: THandle;
  VFileStream: THandleStream;
  VTileInfo: ITileInfoBasic;
begin
  Result := False;
  if GetState.GetStatic.WriteAccess <> asDisabled then begin
    if Assigned(AContentType) then begin
      if not FMainContentType.CheckOtherForSaveCompatible(AContentType) then begin
        raise Exception.Create('Bad content type for this tile storage');
      end;
    end;
    VPath :=
      StoragePath +
      FFileNameGenerator.GetTileFileName(AXY, AZoom);
    VFileName := VPath + FFileExt;
    VTneName := VPath + CTneFileExt;
    FFsLock.BeginWrite;
    try
      if not AIsOverwrite then begin
        VTileInfo := GetTileInfo(AXY, AZoom, nil, gtimAsIs);
        if Assigned(VTileInfo) and (VTileInfo.IsExists or VTileInfo.IsExistsTNE) then begin
          Exit;
        end;
      end;
      if Assigned(AContentType) and Assigned(AData) then begin
        CreateDirIfNotExists(VFileName);
        VHandle := INVALID_HANDLE_VALUE;
        try
          VHandle :=
            CreateFile(
              PChar(VFileName),
              GENERIC_READ or GENERIC_WRITE,
              0,
              nil,
              CREATE_ALWAYS,
              FILE_ATTRIBUTE_NORMAL,
              0
            );
          if VHandle = INVALID_HANDLE_VALUE then begin
            RaiseLastOSError;
          end;

          {$WARN SYMBOL_PLATFORM OFF}
          FileSetDate(VHandle, DateTimeToFileDate(ALoadDate)); // (!) 'FileSetDate' is specific to a platform
          {$WARN SYMBOL_PLATFORM ON}
          VFileStream := THandleStream.Create(VHandle);
          try
            VFileStream.Size := AData.Size;
            VFileStream.Position := 0;
            VFileStream.WriteBuffer(AData.Buffer^, AData.Size);
          finally
            VFileStream.Free;
          end;
        finally
          if VHandle <> INVALID_HANDLE_VALUE then begin
            FileClose(VHandle);
          end;
        end;
        DeleteFile(VTneName);
        Result := True;
        if Assigned(FTileInfoMemCache) then begin
          VTileInfo :=
            TTileInfoBasicExistsWithTile.Create(
              ALoadDate,
              AData,
              nil,
              FMainContentType
            );
        end;
      end else begin
        if not FileExists(VTneName) then begin
          CreateDirIfNotExists(VTneName);
          VHandle := INVALID_HANDLE_VALUE;
          try
            VHandle :=
              CreateFile(
                PChar(VTneName),
                GENERIC_READ or GENERIC_WRITE,
                0,
                nil,
                CREATE_ALWAYS,
                FILE_ATTRIBUTE_NORMAL,
                0
              );
            if VHandle = INVALID_HANDLE_VALUE then begin
              RaiseLastOSError;
            end;
            {$WARN SYMBOL_PLATFORM OFF}
            FileSetDate(VHandle, DateTimeToFileDate(ALoadDate)); // (!) 'FileSetDate' is specific to a platform
            {$WARN SYMBOL_PLATFORM ON}
          finally
            if VHandle <> INVALID_HANDLE_VALUE then begin
              FileClose(VHandle);
            end;
          end;
          DeleteFile(VFileName);
          Result := True;
          if Assigned(FTileInfoMemCache) then begin
            VTileInfo := TTileInfoBasicTNE.Create(ALoadDate, nil);
          end;
        end;
      end;
    finally
      FFsLock.EndWrite;
    end;
    if Result then begin
      if Assigned(FTileInfoMemCache) then begin
        FTileInfoMemCache.Add(
          AXY,
          AZoom,
          nil,
          VTileInfo
        );
      end;
      NotifyTileUpdate(AXY, AZoom, nil);
    end;
  end;
end;

{ TEnumTileInfoByFileIterator }

type
  TEnumTileInfoByFileIterator = class(TBaseInterfacedObject, IEnumTileInfo)
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
      const AFilesIterator: IFileNameIterator;
      const ATileFileNameParser: ITileFileNameParser;
      const ALock: IReadWriteSync;
      const AStorageState: IStorageStateChangeble;
      const AMainContentType: IContentTypeInfoBasic
    );
  end;

constructor TEnumTileInfoByFileIterator.Create(
  const AFilesIterator: IFileNameIterator;
  const ATileFileNameParser: ITileFileNameParser;
  const ALock: IReadWriteSync;
  const AStorageState: IStorageStateChangeble;
  const AMainContentType: IContentTypeInfoBasic
);
begin
  inherited Create;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FLock := ALock;
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
  const AIgnoreTNE: Boolean;
  const AIgnoreMultiVersionTiles: Boolean
): IEnumTileInfo;
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
    VProcessFileMasks.Add('*' + FFileExt);
    if not AIgnoreTNE then begin
      VProcessFileMasks.Add('*' + CTneFileExt);
    end;

    VFoldersIteratorFactory :=
      TFoldersIteratorRecursiveByLevelsFactory.Create(cMaxFolderDepth);

    VFilesInFolderIteratorFactory :=
      TFileNameIteratorInFolderByMaskListFactory.Create(VProcessFileMasks, True);

    VFilesIterator := TFileNameIteratorFolderWithSubfolders.Create(
      StoragePath,
      '',
      VFoldersIteratorFactory,
      VFilesInFolderIteratorFactory
    );
    Result :=
      TEnumTileInfoByFileIterator.Create(
        VFilesIterator,
        FTileFileNameParser,
        FFsLock,
        GetState,
        FMainContentType
      );
  finally
    VProcessFileMasks.Free;
  end;
end;

end.
