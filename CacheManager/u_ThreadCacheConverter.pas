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
  Classes,
  i_NotifierOperation,
  i_NotifierTTLCheck,
  i_ContentTypeManager,
  i_TileFileNameParsersList,
  i_TileFileNameGeneratorsList,
  i_TileInfoBasic,
  i_TileStorage,
  i_CacheConverterProgressInfo,
  u_ThreadCacheManagerAbstract;

type
  TThreadCacheConverter = class(TThreadCacheManagerAbstract)
  private
    FOperationID: Integer;
    FCancelNotifier: INotifierOperation;
    FSourceTileStorage: ITileStorage;
    FDestTileStorage: ITileStorage;
    FSourceIgnoreTne: Boolean;
    FSourceRemoveTiles: Boolean;
    FDestOverwriteTiles: Boolean;
    FSourcePath: string;
    FGCList: INotifierTTLCheck;
    FContentTypeManager: IContentTypeManager;
    FFileNameGeneratorsList: ITileFileNameGeneratorsList;
    FFileNameParsersList: ITileFileNameParsersList;
    FProgressInfo: ICacheConverterProgressInfo;
    function CreateSimpleTileStorage(
      const ACacheIdent: string;
      const ARootPath: string;
      const ADefExtention: string;
      const AFormatID: Byte;
      const AIsReadOnly: Boolean;
      const AAllowDelete: Boolean;
      const AAllowAdd: Boolean;
      const AAllowReplace: Boolean
    ): ITileStorage;

    function OnSourceTileStorageScan(
      const ATileInfo: TTileInfo
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
      const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
      const AFileNameParsersList: ITileFileNameParsersList;
      const AProgressInfo: ICacheConverterProgressInfo
    );
  end;

implementation

uses
  SysUtils,
  c_CacheTypeCodes,
  i_TileFileNameGenerator,
  i_TileFileNameParser,
  i_CoordConverter,
  i_MapVersionConfig,
  i_ContentTypeInfo,
  u_CoordConverterMercatorOnSphere,
  u_MapVersionFactorySimpleString,
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
  const AFileNameGeneratorsList: ITileFileNameGeneratorsList;
  const AFileNameParsersList: ITileFileNameParsersList;
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
  FSourcePath := ASourcePath;
  FGCList := AGCList;
  FContentTypeManager := AContentTypeManager;
  FFileNameGeneratorsList := AFileNameGeneratorsList;
  FFileNameParsersList := AFileNameParsersList;
  FProgressInfo := AProgressInfo;

  VDotPos := Pos('.', ADefExtention);
  if VDotPos > 0 then begin
    VDefExtention := Copy(ADefExtention, VDotPos, Length(ADefExtention) - VDotPos + 1);
  end else begin
    VDefExtention := '.' + ADefExtention;
  end;

  FSourceTileStorage :=
    CreateSimpleTileStorage(
      'SourcePath',
      FSourcePath,
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

  inherited Create(FCancelNotifier, FOperationID, AnsiString(Self.ClassName));
end;

procedure TThreadCacheConverter.Process;
var
  VEnum: IEnumTileInfo;
  VTileInfo: TTileInfo;
begin
  VEnum := FSourceTileStorage.ScanTiles(FSourceIgnoreTne);
  while VEnum.Next(VTileInfo) do begin
    if FCancelNotifier.IsOperationCanceled(FOperationID) then begin
      Break;
    end;
    if not OnSourceTileStorageScan(VTileInfo) then begin
      Break;
    end;
    if FSourceRemoveTiles then begin
      if VTileInfo.FInfoType in [titExists, titTneExists] then begin
        FSourceTileStorage.DeleteTile(VTileInfo.FTile, VTileInfo.FZoom, VTileInfo.FVersionInfo);
      end;
    end;
  end;
  FProgressInfo.Finished := True;
end;

function TThreadCacheConverter.OnSourceTileStorageScan(
  const ATileInfo: TTileInfo
): Boolean;
var
  VTileInfo: ITileInfoBasic;
  VTileFullPath: string;
begin
  Result := False;
  if not FCancelNotifier.IsOperationCanceled(FOperationID) then begin

    if not FDestOverwriteTiles then begin
      VTileInfo :=
        FDestTileStorage.GetTileInfo(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          ATileInfo.FVersionInfo,
          gtimAsIs
        );
      if (VTileInfo.IsExists or (VTileInfo.IsExistsTNE and (ATileInfo.FInfoType = titTneExists))) then begin
        Result := True;
        FProgressInfo.TilesSkipped := FProgressInfo.TilesSkipped + 1;
      end;
    end;

    if not Result then begin

      if ATileInfo.FInfoType = titExists then begin
        FDestTileStorage.SaveTile(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          ATileInfo.FVersionInfo,
          ATileInfo.FLoadDate,
          ATileInfo.FData
        );
        Result := True;
      end else if ATileInfo.FInfoType = titTneExists then begin
        FDestTileStorage.SaveTNE(
          ATileInfo.FTile,
          ATileInfo.FZoom,
          ATileInfo.FVersionInfo,
          ATileInfo.FLoadDate
        );
        Result := True;
      end;

      if Result then begin
        FProgressInfo.TilesProcessed := FProgressInfo.TilesProcessed + 1;
        if Assigned(ATileInfo.FData) then begin
          FProgressInfo.TilesSize := FProgressInfo.TilesSize + ATileInfo.FData.Size;
        end;
      end;
    end;

    VTileFullPath :=
      FSourceTileStorage.GetTileFileName(
        ATileInfo.FTile,
        ATileInfo.FZoom,
        ATileInfo.FVersionInfo
      );

    FProgressInfo.LastTileName :=
      StringReplace(VTileFullPath, FSourcePath, '', [rfIgnoreCase]);
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
): ITileStorage;
var
  VCoordConverterFake: ICoordConverter;
  VMapVersionFactory: IMapVersionFactory;
  VContentType: IContentTypeInfoBasic;
  VFileNameGenerator: ITileFileNameGenerator;
  VFileNameParser: ITileFileNameParser;
begin
  VCoordConverterFake := TCoordConverterMercatorOnSphere.Create(6378137);
  VContentType := FContentTypeManager.GetInfoByExt(ADefExtention);
  if AFormatID = c_File_Cache_Id_BDB then begin
    VMapVersionFactory := TMapVersionFactorySimpleString.Create;
    Result :=
      TTileStorageBerkeleyDB.Create(
        VCoordConverterFake,
        ARootPath,
        FGCList,
        False,
        FContentTypeManager,
        VMapVersionFactory,
        VContentType
      );
  end else if AFormatID = c_File_Cache_Id_GE then begin
//    Result :=
//      TTileStorageGE.Create(
//        VStorageConfig,
//        VGlobalCacheConfig,
//        FContentTypeManager
//      );
  end else if AFormatID = c_File_Cache_Id_GC then begin
//    Result :=
//      TTileStorageGC.Create(
//        VStorageConfig,
//        VGlobalCacheConfig,
//        FContentTypeManager
//      );
  end else if AFormatID in [c_File_Cache_Id_GMV, c_File_Cache_Id_SAS, c_File_Cache_Id_ES, c_File_Cache_Id_GM, c_File_Cache_Id_GM_Aux, c_File_Cache_Id_GM_Bing] then begin
    VMapVersionFactory := TMapVersionFactorySimpleString.Create;
    VFileNameGenerator := FFileNameGeneratorsList.GetGenerator(AFormatID);
    VFileNameParser := FFileNameParsersList.GetParser(AFormatID);
    Result :=
      TTileStorageFileSystem.Create(
        VCoordConverterFake,
        ARootPath,
        ADefExtention,
        VContentType,
        VMapVersionFactory,
        VFileNameGenerator,
        VFileNameParser
      );
  end;
end;

end.
