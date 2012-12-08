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

unit u_TileStorageBerkeleyDB;

interface

uses
  Types,
  SysUtils,
  i_BinaryData,
  i_MapVersionInfo,
  i_ContentTypeInfo,
  i_TileInfoBasic,
  i_BasicMemCache,
  i_ContentTypeManager,
  i_CoordConverter,
  i_TileStorage,
  i_MapVersionConfig,
  i_NotifierTTLCheck,
  i_ListenerTTLCheck,
  i_TileFileNameGenerator,
  u_TileStorageBerkeleyDBHelper,
  u_TileInfoBasicMemCache,
  u_TileStorageAbstract;

type
  TTileStorageBerkeleyDB = class(TTileStorageAbstract, IBasicMemCache)
  private
    FHelper: TTileStorageBerkeleyDBHelper;
    FMainContentType: IContentTypeInfoBasic;
    FContentTypeManager: IContentTypeManager;
    FTileNotExistsTileInfo: ITileInfoBasic;
    FGCList: INotifierTTLCheck;
    FBDBTTLListener: IListenerTTLCheck;
    FMemCacheTTLListener: IListenerTTLCheck;
    FTileInfoMemCache: TTileInfoBasicMemCache;
    FUseMemCache: Boolean;
    FFileNameGenerator: ITileFileNameGenerator;

    procedure OnTTLSync(Sender: TObject);
  private
    { IBasicMemCache }
    procedure ClearMemCache;
    procedure IBasicMemCache.Clear = ClearMemCache;
  protected
    function GetIsFileCache: Boolean; override;
    function GetTileFileName(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AMode: TGetTileInfoMode
    ): ITileInfoBasic; override;

    function GetTileRectInfo(
      const ARect: TRect;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): ITileRectInfo; override;

    function DeleteTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime;
      const AContentType: IContentTypeInfoBasic;
      const AData: IBinaryData
    ); override;

    procedure SaveTNE(
      const AXY: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ALoadDate: TDateTime
    ); override;

    function ScanTiles(
      const AIgnoreTNE: Boolean
    ): IEnumTileInfo; override;
  public
    constructor Create(
      const AGeoConverter: ICoordConverter;
      const AStoragePath: string;
      const AGCList: INotifierTTLCheck;
      const AUseMemCache: Boolean;
      const AContentTypeManager: IContentTypeManager;
      const AMapVersionFactory: IMapVersionFactory;
      const AMainContentType: IContentTypeInfoBasic
    );
    destructor Destroy; override;
  end;

implementation

uses
  WideStrings,
  t_CommonTypes,
  i_TileIterator,
  i_FileNameIterator,
  i_TileFileNameParser,
  u_ListenerTTLCheck,
  u_TileRectInfoShort,
  u_TileFileNameBerkeleyDB,
  u_TileIteratorByRect,
  u_TileStorageTypeAbilities,
  u_FileNameIteratorFolderWithSubfolders,
  u_FoldersIteratorRecursiveByLevels,
  u_FileNameIteratorInFolderByMaskList,
  u_TileInfoBasic,
  u_BaseInterfacedObject;

{ TTileStorageBerkeleyDB }

procedure TTileStorageBerkeleyDB.ClearMemCache;
begin
  if Assigned(FTileInfoMemCache) then
    FTileInfoMemCache.Clear;
end;

constructor TTileStorageBerkeleyDB.Create(
  const AGeoConverter: ICoordConverter;
  const AStoragePath: string;
  const AGCList: INotifierTTLCheck;
  const AUseMemCache: Boolean;
  const AContentTypeManager: IContentTypeManager;
  const AMapVersionFactory: IMapVersionFactory;
  const AMainContentType: IContentTypeInfoBasic
);
const
  CBDBSync = 300000; // 5 min
  CBDBSyncCheckInterval = 60000; // 60 sec
begin
  inherited Create(
    TTileStorageTypeAbilitiesBerkeleyDB.Create,
    AMapVersionFactory,
    AGeoConverter,
    AStoragePath
  );
  FContentTypeManager := AContentTypeManager;
  FMainContentType := AMainContentType;

  FUseMemCache := AUseMemCache;

  FTileNotExistsTileInfo := TTileInfoBasicNotExists.Create(0, nil);
  FFileNameGenerator := TTileFileNameBerkeleyDB.Create as ITileFileNameGenerator;
  FHelper := TTileStorageBerkeleyDBHelper.Create(
    StoragePath,
    AGeoConverter.ProjectionEPSG
  );

  FBDBTTLListener := TListenerTTLCheck.Create(
    FHelper.Sync,
    CBDBSync,
    CBDBSyncCheckInterval
  );

  FMemCacheTTLListener := TListenerTTLCheck.Create(
    Self.OnTTLSync,
    CBDBSync,
    CBDBSyncCheckInterval
  );

  FGCList := AGCList;
  FGCList.Add(FBDBTTLListener);
  FGCList.Add(FMemCacheTTLListener);

  FTileInfoMemCache := TTileInfoBasicMemCache.Create(100, 30000);
end;

destructor TTileStorageBerkeleyDB.Destroy;
begin
  if Assigned(FGCList) then begin
    FGCList.Remove(FMemCacheTTLListener);
    FGCList.Remove(FBDBTTLListener);
    FGCList := nil;
  end;
  FBDBTTLListener := nil;
  FMemCacheTTLListener := nil;
  FTileInfoMemCache.Free;
  FreeAndNil(FHelper);
  FMainContentType := nil;
  FContentTypeManager := nil;
  FTileNotExistsTileInfo := nil;
  inherited;
end;

procedure TTileStorageBerkeleyDB.OnTTLSync(Sender: TObject);
begin
  FTileInfoMemCache.ClearByTTL;
end;

function TTileStorageBerkeleyDB.GetIsFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageBerkeleyDB.GetTileFileName(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): string;
begin
  Result :=
    StoragePath +
    FFileNameGenerator.GetTileFileName(AXY, AZoom) +
    '.sdb';
end;

function TTileStorageBerkeleyDB.GetTileInfo(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AMode: TGetTileInfoMode
): ITileInfoBasic;
var
  VPath: string;
  VResult: Boolean;
  VTileBinaryData: IBinaryData;
  VTileVersion: WideString;
  VTileContentType: WideString;
  VTileDate: TDateTime;
begin
  if FUseMemCache then begin
    Result := FTileInfoMemCache.Get(AXY, AZoom);
    if Result <> nil then begin
      Exit;
    end;
  end;
  Result := FTileNotExistsTileInfo;
  if GetState.GetStatic.ReadAccess <> asDisabled then begin

    VPath :=
      StoragePath +
      FFileNameGenerator.GetTileFileName(AXY, AZoom) +
      '.sdb';

    VResult := False;

    if FileExists(VPath) then begin

      VResult := FHelper.LoadTile(
        VPath,
        AXY,
        AZoom,
        AVersionInfo,
        VTileBinaryData,
        VTileVersion,
        VTileContentType,
        VTileDate
      );

      if VResult then begin
        if AMode = gtimWithoutData then begin
          Result := TTileInfoBasicExists.Create(
            VTileDate,
            VTileBinaryData.Size,
            MapVersionFactory.CreateByStoreString(VTileVersion),
            FContentTypeManager.GetInfo(VTileContentType)
          );
        end else begin
          Result := TTileInfoBasicExistsWithTile.Create(
            VTileDate,
            VTileBinaryData,
            MapVersionFactory.CreateByStoreString(VTileVersion),
            FContentTypeManager.GetInfo(VTileContentType)
          );
        end;
      end;
    end;

    if not VResult then begin
      VPath := ChangeFileExt(VPath, '.tne');
      if FileExists(VPath) then begin
        VResult := FHelper.IsTNEFound(
          VPath,
          AXY,
          AZoom,
          AVersionInfo,
          VTileDate
        );
        if VResult then begin
          Result := TTileInfoBasicTNE.Create(VTileDate, AVersionInfo);
        end;
      end;
    end;

    if not VResult then begin
      Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
    end;
  end;

  if FUseMemCache then begin
    FTileInfoMemCache.Add(AXY, AZoom, AVersionInfo, Result);
  end;
end;

function TTileStorageBerkeleyDB.GetTileRectInfo(
  const ARect: TRect;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): ITileRectInfo;

type
  TInfo = record
    Name: string;
    PrevName: string;
    Exists: Boolean;
    PrevExists: Boolean;
  end;

  procedure ClearInfo(var AInfo: TInfo);
  begin
    AInfo.Name := '';
    AInfo.PrevName := '';
    AInfo.Exists := False;
    AInfo.PrevExists := False;
  end;

var
  VRect: TRect;
  VZoom: Byte;
  VCount: TPoint;
  //VItems: PTileInfoInternalArray;
  VItems: TArrayOfTileInfoShortInternal;
  VIndex: Integer;
  VTile: TPoint;
  VIterator: ITileIterator;
  VFolderInfo: TInfo;
  VFileInfo: TInfo;
  VTneFileInfo: TInfo;
  VTileExists: Boolean;
  VTileBinaryData: IBinaryData;
  VTileVersion: WideString;
  VTileContentType: WideString;
  VTileDate: TDateTime;
begin
  Result := nil;
  if GetState.GetStatic.ReadAccess <> asDisabled then begin
    VRect := ARect;
    VZoom := AZoom;
    GeoConverter.CheckTileRect(VRect, VZoom);
    VCount.X := VRect.Right - VRect.Left;
    VCount.Y := VRect.Bottom - VRect.Top;
    if (VCount.X > 0) and (VCount.Y > 0) and (VCount.X <= 2048) and (VCount.Y <= 2048) then begin
      SetLength(VItems, VCount.X * VCount.Y);
      ClearInfo(VFolderInfo);
      ClearInfo(VFileInfo);
      ClearInfo(VTneFileInfo);
      VIterator := TTileIteratorByRect.Create(VRect);
      while VIterator.Next(VTile) do begin
        VIndex := TTileRectInfoShort.TileInRectToIndex(VTile, VRect);
        Assert(VIndex >=0);
        if VIndex >= 0 then begin
          VFileInfo.Name :=
            StoragePath +
            FFileNameGenerator.GetTileFileName(VTile, VZoom) +
            '.sdb';
          VTneFileInfo.Name := ChangeFileExt(VFileInfo.Name, '.tne');
          VFolderInfo.Name := ExtractFilePath(VFileInfo.Name);

          if VFolderInfo.Name = VFolderInfo.PrevName then begin
            VFolderInfo.Exists := VFolderInfo.PrevExists;
          end else begin
            VFolderInfo.Exists := DirectoryExists(VFolderInfo.Name);
            VFolderInfo.PrevName := VFolderInfo.Name;
            VFolderInfo.PrevExists := VFolderInfo.Exists;
          end;

          if VFileInfo.Name = VFileInfo.PrevName then begin
            VFileInfo.Exists := VFileInfo.PrevExists;
          end else begin
            VFileInfo.Exists := FileExists(VFileInfo.Name);
            VFileInfo.PrevName := VFileInfo.Name;
            VFileInfo.PrevExists := VFileInfo.Exists;
          end;

          if VTneFileInfo.Name = VTneFileInfo.PrevName then begin
            VTneFileInfo.Exists := VTneFileInfo.PrevExists;
          end else begin
            VTneFileInfo.Exists := FileExists(VTneFileInfo.Name);
            VTneFileInfo.PrevName := VTneFileInfo.Name;
            VTneFileInfo.PrevExists := VTneFileInfo.Exists;
          end;

          if (VFolderInfo.Exists and (VFileInfo.Exists or VTneFileInfo.Exists)) then begin
            VTileExists := FHelper.LoadTile(
              VFileInfo.Name,
              VTile,
              VZoom,
              AVersionInfo,
              VTileBinaryData,
              VTileVersion,
              VTileContentType,
              VTileDate
            );
            if VTileExists then begin
              // tile exists
              VItems[VIndex].FLoadDate := VTileDate;
              //VItems[VIndex].FVersionInfo := MapVersionFactory.CreateByStoreString(VTileVersion);
              //VItems[VIndex].FContentType := FContentTypeManager.GetInfo(VTileContentType);
              //VItems[VIndex].FData := VTileBinaryData;
              VItems[VIndex].FSize := VTileBinaryData.Size;
              VItems[VIndex].FInfoType := titExists;
            end else begin
              VTileExists := FHelper.IsTNEFound(
                VTneFileInfo.Name,
                VTile,
                VZoom,
                AVersionInfo,
                VTileDate
              );
              if VTileExists then begin
                // tne exists
                VItems[VIndex].FLoadDate := VTileDate;
                //VItems[VIndex].FVersionInfo := AVersionInfo;
                //VItems[VIndex].FContentType := nil;
                //VItems[VIndex].FData := nil;
                VItems[VIndex].FSize := 0;
                VItems[VIndex].FInfoType := titTneExists;
              end else begin
                // neither tile nor tne
                VItems[VIndex].FLoadDate := 0;
                //VItems[VIndex].FVersionInfo := nil;
                //VItems[VIndex].FContentType := nil;
                //VItems[VIndex].FData := nil;
                VItems[VIndex].FSize := 0;
                VItems[VIndex].FInfoType := titNotExists;
              end;
            end;
          end else begin
            // neither tile nor tne
            VItems[VIndex].FLoadDate := 0;
            //VItems[VIndex].FVersionInfo := nil;
            //VItems[VIndex].FContentType := nil;
            //VItems[VIndex].FData := nil;
            VItems[VIndex].FSize := 0;
            VItems[VIndex].FInfoType := titNotExists;
          end;
        end;
      end;
      //Result :=
      //  TTileRectInfo.CreateWithOwn(
      //    VRect,
      //    VZoom,
      //    VItems
      //  );
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

procedure TTileStorageBerkeleyDB.SaveTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime;
  const AContentType: IContentTypeInfoBasic;
  const AData: IBinaryData
);
var
  VPath: string;
  VResult: Boolean;
  VTileInfo: ITileInfoBasic;
  VContenetTypeStr: WideString;
begin
  if GetState.GetStatic.WriteAccess <> asDisabled then begin
    if not FMainContentType.CheckOtherForSaveCompatible(AContentType) then begin
      raise Exception.Create('Bad content type for this tile storage');
    end;
    VPath :=
      StoragePath +
      FFileNameGenerator.GetTileFileName(AXY, AZoom) +
      '.sdb';
    if FHelper.CreateDirIfNotExists(VPath) then begin
      VContenetTypeStr := AContentType.GetContentType;
      VResult := FHelper.SaveTile(
        VPath,
        AXY,
        AZoom,
        ALoadDate,
        AVersionInfo,
        PWideChar(VContenetTypeStr),
        AData
      );
      if VResult then begin
        VTileInfo :=
          TTileInfoBasicExistsWithTile.Create(
            ALoadDate,
            AData,
            AVersionInfo,
            FMainContentType
          );
        if FUseMemCache then begin
          FTileInfoMemCache.Add(
            AXY,
            AZoom,
            AVersionInfo,
            VTileInfo
          );
        end;
        NotifyTileUpdate(AXY, AZoom, AVersionInfo);
      end;
    end;
  end;
end;

procedure TTileStorageBerkeleyDB.SaveTNE(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ALoadDate: TDateTime
);
var
  VPath: String;
  VResult: Boolean;
  VContenetTypeStr: WideString;
begin
  if GetState.GetStatic.WriteAccess <> asDisabled then begin
    DeleteTile(AXY, AZoom, AVersionInfo); // del old tile if exists
    VPath :=
      StoragePath +
      FFileNameGenerator.GetTileFileName(AXY, AZoom) +
      '.tne';
    if FHelper.CreateDirIfNotExists(VPath) then begin
      VContenetTypeStr := FMainContentType.GetContentType;
      VResult := FHelper.SaveTile(
        VPath,
        AXY,
        AZoom,
        ALoadDate,
        AVersionInfo,
        PWideChar(VContenetTypeStr),
        nil
      );
      if VResult then begin
        if FUseMemCache then begin
          FTileInfoMemCache.Add(
            AXY,
            AZoom,
            AVersionInfo,
            TTileInfoBasicTNE.Create(ALoadDate, AVersionInfo)
          );
        end;
        NotifyTileUpdate(AXY, AZoom, AVersionInfo);
      end;
    end;
  end;
end;

function TTileStorageBerkeleyDB.DeleteTile(
  const AXY: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
begin
  Result := False;
  if GetState.GetStatic.DeleteAccess <> asDisabled then begin
    try
      VPath :=
        StoragePath +
        FFileNameGenerator.GetTileFileName(AXY, AZoom) +
        '.sdb';
      if FileExists(VPath) then begin
        Result := FHelper.DeleteTile(
          VPath,
          AXY,
          AZoom,
          AVersionInfo
        );
      end;
      if not Result then begin
        VPath :=
          StoragePath +
          FFileNameGenerator.GetTileFileName(AXY, AZoom) +
          '.tne';
        if FileExists(VPath) then begin
          Result := FHelper.DeleteTile(
            VPath,
            AXY,
            AZoom,
            AVersionInfo
          );
        end;
      end;
    except
      Result := False;
    end;
    if Result then begin
      if FUseMemCache then begin
        FTileInfoMemCache.Add(
          AXY,
          AZoom,
          AVersionInfo,
          TTileInfoBasicNotExists.Create(0, AVersionInfo)
        );
      end;
      NotifyTileUpdate(AXY, AZoom, AVersionInfo);
    end;
  end;
end;

{ TEnumTileInfoByBDB }

type
  TEnumTileInfoByBDB = class(TBaseInterfacedObject, IEnumTileInfo)
  private
    FFilesIterator: IFileNameIterator;
    FTileFileNameParser: ITileFileNameParser;
    FStorage: TTileStorageBerkeleyDB;

    FCurFileTilesArray: TPointArray;
    FCurFileIndex: Integer;
    FCurFileZoom: Byte;
  private
    function Next(var ATileInfo: TTileInfo): Boolean;
  public
    constructor Create(
      const AFilesIterator: IFileNameIterator;
      const ATileFileNameParser: ITileFileNameParser;
      AStorage: TTileStorageBerkeleyDB
    );
  end;

constructor TEnumTileInfoByBDB.Create(
  const AFilesIterator: IFileNameIterator;
  const ATileFileNameParser: ITileFileNameParser;
  AStorage: TTileStorageBerkeleyDB
);
begin
  inherited Create;
  FFilesIterator := AFilesIterator;
  FTileFileNameParser := ATileFileNameParser;
  FStorage := AStorage;
  FCurFileIndex := 0;
  SetLength(FCurFileTilesArray, 0);
end;

function TEnumTileInfoByBDB.Next(var ATileInfo: TTileInfo): Boolean;
var
  VTileFileName: string;
  VTileFileNameW: WideString;
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VData: IBinaryData;
  VTileXY: TPoint;
begin
  Result := False;
  while FCurFileIndex >= 0 do begin
    if FCurFileIndex < Length(FCurFileTilesArray) then begin
      ATileInfo.FZoom := FCurFileZoom;
      ATileInfo.FTile := FCurFileTilesArray[FCurFileIndex];
      Inc(FCurFileIndex);
      VTileInfo := FStorage.GetTileInfo(ATileInfo.FTile, FCurFileZoom, nil, gtimWithData);
      VData := nil;
      if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
        VData := VTileInfoWithData.TileData;
      end;
      ATileInfo.FLoadDate := VTileInfo.LoadDate;
      ATileInfo.FVersionInfo := VTileInfo.VersionInfo;
      ATileInfo.FContentType := VTileInfo.ContentType;
      ATileInfo.FData := VData;
      ATileInfo.FSize := VTileInfo.Size;
      if VTileInfo.IsExists then begin
        ATileInfo.FInfoType := titExists;
        Result := True;
        Break;
      end else if VTileInfo.IsExistsTNE then begin
        ATileInfo.FInfoType := titTneExists;
        Result := True;
        Break;
      end else begin
        ATileInfo.FInfoType := titNotExists;
        Continue;
      end;
    end;
    if FFilesIterator.Next(VTileFileNameW) then begin
      VTileFileName := VTileFileNameW;
      if FTileFileNameParser.GetTilePoint(VTileFileName, VTileXY, FCurFileZoom) and
        FStorage.FHelper.GetTileExistsArray(FStorage.StoragePath + VTileFileName, FCurFileZoom, nil, FCurFileTilesArray) then begin
        FCurFileIndex := 0;
      end else begin
        FCurFileIndex := Length(FCurFileTilesArray);
      end;
    end else begin
      FCurFileIndex := -1;
    end;
  end;
end;

function TTileStorageBerkeleyDB.ScanTiles(
  const AIgnoreTNE: Boolean): IEnumTileInfo;
const
  cMaxFolderDepth = 10;
var
  VProcessFileMasks: TWideStringList;
  VFileNameParser: ITileFileNameParser;
  VFilesIterator: IFileNameIterator;
  VFoldersIteratorFactory: IFileNameIteratorFactory;
  VFilesInFolderIteratorFactory: IFileNameIteratorFactory;
begin
  VProcessFileMasks := TWideStringList.Create;
  try
    VProcessFileMasks.Add('*.sdb');
    if not AIgnoreTNE then begin
      VProcessFileMasks.Add('*.tne');
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

    VFileNameParser := TTileFileNameBerkeleyDB.Create as ITileFileNameParser;
    Result :=
      TEnumTileInfoByBDB.Create(
        VFilesIterator,
        VFileNameParser,
        Self
      );
  finally
    VProcessFileMasks.Free;
  end;

end;

end.
