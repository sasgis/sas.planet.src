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

unit u_ThreadCacheConverter;

interface

uses
  Types,
  Classes,
  i_NotifierOperation,
  i_NotifierTTLCheck,
  i_ContentTypeManager,
  i_TileFileNameParsersList,
  i_TileFileNameGeneratorsList,
  i_InternalPerformanceCounter,
  i_TileInfoBasic,
  i_BinaryData,
  i_CacheConverterProgressInfo,
  u_GlobalCahceConfig,
  u_TileStorageAbstract,
  u_ThreadCacheManagerAbstract;

type
  TThreadCacheConverter = class(TThreadCacheManagerAbstract)
  private
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSourceTileStorage: TTileStorageAbstract;
    FDestTileStorage: TTileStorageAbstract;
    FSourceIgnoreTne: Boolean;
    FSourceRemoveTiles: Boolean;
    FDestOverwriteTiles: Boolean;
    FGCList: INotifierTTLCheck;
    FContentTypeManager: IContentTypeManager;
    FPerfCounterList: IInternalPerformanceCounterList;
    FProgressInfo: ICacheConverterProgressInfo;
    FGlobalCahceConfig: array of TGlobalCahceConfig;
    function CreateSimpleTileStorage(
      const ACacheIdent: string;
      const ARootPath: string;
      const ADefExtention: string;
      const AFormatID: Byte;
      const AIsReadOnly: Boolean;
      const AAllowDelete: Boolean;
      const AAllowAdd: Boolean;
      const AAllowReplace: Boolean
    ): TTileStorageAbstract;

    function OnSourceTileStorageScan(
      Sender: TObject;
      const ATileNameInCache: string;
      const ATileXY: TPoint;
      const ATileZoom: Byte;
      const ATileInfo: ITileInfoBasic;
      const ATileBinaryData: IBinaryData
    ): Boolean;

  protected
    procedure Process; override;
  public
    constructor Create(
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ASourcePath: string;
      const ADestPath: string;
      const ADefExtention: string;
      const ASourceCacheFormatID: Byte;
      const ADestCacheFormatID: Byte;
      const ASourceIgnoreTne: Boolean;
      const ASourceRemoveTiles: Boolean;
      const ADestOverwriteTiles: Boolean;
      const AGCList: INotifierTTLCheck;
      const AContentTypeManager: IContentTypeManager;
      const APerfCounterList: IInternalPerformanceCounterList;
      const AProgressInfo: ICacheConverterProgressInfo
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_PathConfig,
  i_CoordConverter,
  i_SimpleTileStorageConfig,
  u_SimpleTileStorageConfig,
  u_SimpleTileStorageConfigStatic,
  u_TileFileNameParsersSimpleList,
  u_TileFileNameGeneratorsSimpleList,
  u_CoordConverterMercatorOnSphere,
  u_PathConfig,  
  u_TileStorageFileSystem,
  u_TileStorageBerkeleyDB,
  u_TileStorageGE;

{ TThreadCacheConverter }

constructor TThreadCacheConverter.Create(
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ASourcePath: string;
  const ADestPath: string;
  const ADefExtention: string;
  const ASourceCacheFormatID: Byte;
  const ADestCacheFormatID: Byte;
  const ASourceIgnoreTne: Boolean;
  const ASourceRemoveTiles: Boolean;
  const ADestOverwriteTiles: Boolean;
  const AGCList: INotifierTTLCheck;
  const AContentTypeManager: IContentTypeManager;
  const APerfCounterList: IInternalPerformanceCounterList;
  const AProgressInfo: ICacheConverterProgressInfo
);
var
  VDefExtention: string;
  VDotPos: Integer;
begin
  FCancelNotifier := ACancelNotifier;
  FOperationID := AOperationID;
  FSourceIgnoreTne := ASourceIgnoreTne;
  FSourceRemoveTiles := ASourceRemoveTiles;
  FDestOverwriteTiles := ADestOverwriteTiles;
  FGCList := AGCList;
  FContentTypeManager := AContentTypeManager;
  FPerfCounterList := APerfCounterList;
  FProgressInfo := AProgressInfo;

  SetLength(FGlobalCahceConfig, 0);

  VDotPos := Pos('.', ADefExtention);
  if VDotPos > 0 then begin
    VDefExtention := Copy(ADefExtention, VDotPos, Length(ADefExtention) - VDotPos + 1);
  end else begin
    VDefExtention := '.' + ADefExtention;
  end;

  FSourceTileStorage :=
    CreateSimpleTileStorage(
      'SourcePath',
      ASourcePath,
      VDefExtention,
      ASourceCacheFormatID,
      (not FSourceRemoveTiles),
      FSourceRemoveTiles,
      False,
      False
    );

  ForceDirectories(ADestPath);

  FDestTileStorage :=
    CreateSimpleTileStorage(
      'DestPath',
      ADestPath,
      VDefExtention,
      ADestCacheFormatID,
      False,
      True,
      True,
      True
    );
  
  inherited Create(FCancelNotifier, FOperationID, Self.ClassName);
end;

destructor TThreadCacheConverter.Destroy;
var
  I: Integer;
begin
  FSourceTileStorage.Free;
  FDestTileStorage.Free;
  FCancelNotifier := nil;
  for I := 0 to Length(FGlobalCahceConfig) - 1 do begin
    FGlobalCahceConfig[I].Free;
  end;
  SetLength(FGlobalCahceConfig, 0);
  inherited Destroy;
end;

procedure TThreadCacheConverter.Process;
begin
  FSourceTileStorage.Scan(
    Self.OnSourceTileStorageScan,
    FSourceIgnoreTne,
    FSourceRemoveTiles
  );
  FProgressInfo.Finished := True;
end;

function TThreadCacheConverter.OnSourceTileStorageScan(
  Sender: TObject;
  const ATileNameInCache: string;
  const ATileXY: TPoint;
  const ATileZoom: Byte;
  const ATileInfo: ITileInfoBasic;
  const ATileBinaryData: IBinaryData
): Boolean;
var
  VTileInfo: ITileInfoBasic;
begin
  Result := False;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin

    if not FDestOverwriteTiles then begin
      VTileInfo :=
        FDestTileStorage.GetTileInfo(
          ATileXY,
          ATileZoom,
          ATileInfo.VersionInfo
        );
      if (VTileInfo.IsExists or (VTileInfo.IsExistsTNE and ATileInfo.IsExistsTNE)) then begin
        Result := True;
        FProgressInfo.TilesSkipped := FProgressInfo.TilesSkipped + 1;
        FProgressInfo.LastTileName := ATileNameInCache;
      end;
    end;

    if not Result then begin

      if ATileInfo.IsExists then begin
        FDestTileStorage.SaveTile(
          ATileXY,
          ATileZoom,
          ATileInfo.VersionInfo,
          ATileBinaryData
        );
        Result := True;
      end else if ATileInfo.IsExistsTNE then begin
        FDestTileStorage.SaveTNE(
          ATileXY,
          ATileZoom,
          ATileInfo.VersionInfo
        );
        Result := True;
      end;

      if Result then begin
        FProgressInfo.TilesProcessed := FProgressInfo.TilesProcessed + 1;
        if Assigned(ATileBinaryData) then begin
          FProgressInfo.TilesSize := FProgressInfo.TilesSize + ATileBinaryData.Size;
        end;
        FProgressInfo.LastTileName := ATileNameInCache;
      end;
    end;
  end;
end;

function TThreadCacheConverter.CreateSimpleTileStorage(
  const ACacheIdent: string;
  const ARootPath: string;
  const ADefExtention: string;
  const AFormatID: Byte;
  const AIsReadOnly: Boolean;
  const AAllowDelete: Boolean;
  const AAllowAdd: Boolean;
  const AAllowReplace: Boolean
): TTileStorageAbstract;
var
  I: Integer;
  VIsStoreFileCache: Boolean;
  VStorageConfig: ISimpleTileStorageConfig;
  VStorageConfigStatic: ISimpleTileStorageConfigStatic;
  VBaseCachePath: IPathConfig;
  VGlobalCacheConfig: TGlobalCahceConfig;
  VTileNameGeneratorList: ITileFileNameGeneratorsList;
  VTileNameParserList: ITileFileNameParsersList;
  VCoordConverterFake: ICoordConverter;
begin
  VIsStoreFileCache :=
    (AFormatID <> c_File_Cache_Id_BDB) and
    (AFormatID <> c_File_Cache_Id_GE) and
    (AFormatID <> c_File_Cache_Id_GC);

  VCoordConverterFake := TCoordConverterMercatorOnSphere.Create(6378137);

  VStorageConfigStatic :=
    TSimpleTileStorageConfigStatic.Create(
      VCoordConverterFake,
      AFormatID,
      '',
      ADefExtention,
      VIsStoreFileCache,
      AIsReadOnly,
      AAllowDelete,
      AAllowAdd,
      AAllowReplace
    );

  VStorageConfig := TSimpleTileStorageConfig.Create(VStorageConfigStatic);

  VBaseCachePath := TPathConfig.Create(ACacheIdent, ARootPath, nil);

  I := Length(FGlobalCahceConfig);
  SetLength(FGlobalCahceConfig, I + 1);
  FGlobalCahceConfig[I] := TGlobalCahceConfig.Create(VBaseCachePath);
  VGlobalCacheConfig := FGlobalCahceConfig[I];

  //emulate TGlobalCahceConfig.LoadConfig
  VGlobalCacheConfig.DefCache := AFormatID;
  VGlobalCacheConfig.NewCPath := '';
  VGlobalCacheConfig.OldCPath := '';
  VGlobalCacheConfig.ESCPath := '';
  VGlobalCacheConfig.GMTilesPath := '';
  VGlobalCacheConfig.GECachePath := '';
  VGlobalCacheConfig.GCCachePath := '';
  VGlobalCacheConfig.BDBCachePath := '';
  VGlobalCacheConfig.DBMSCachePath := '';

  VTileNameGeneratorList :=
    TTileFileNameGeneratorsSimpleList.Create(VGlobalCacheConfig);

  VTileNameParserList :=
    TTileFileNameParsersSimpleList.Create(VGlobalCacheConfig);

  if VStorageConfig.CacheTypeCode = c_File_Cache_Id_BDB then begin
    Result :=
      TTileStorageBerkeleyDB.Create(
        FGCList,
        VStorageConfig,
        VGlobalCacheConfig,
        FContentTypeManager,
        FPerfCounterList
      );
  end else if VStorageConfig.CacheTypeCode = c_File_Cache_Id_GE then begin
    Result :=
      TTileStorageGE.Create(
        VStorageConfig,
        VGlobalCacheConfig,
        FContentTypeManager
      );
  end else if VStorageConfig.CacheTypeCode = c_File_Cache_Id_GC then begin
    Result :=
      TTileStorageGC.Create(
        VStorageConfig,
        VGlobalCacheConfig,
        FContentTypeManager
      );
  end else begin
    Result :=
      TTileStorageFileSystem.Create(
        VStorageConfig,
        VGlobalCacheConfig,
        VTileNameGeneratorList,
        VTileNameParserList,
        FContentTypeManager,
        FPerfCounterList
      );
  end;
end;

end.
