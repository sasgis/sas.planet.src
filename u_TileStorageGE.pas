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

unit u_TileStorageGE;

interface

uses
  Types,
  Classes,
  i_SimpleTileStorageConfig,
  //i_TileObjCache,
  i_ContentTypeInfo,
  i_MapVersionInfo,
  i_TileInfoBasic,
  i_ContentTypeManager,
  u_MapTypeCacheConfig,
  u_GlobalCahceConfig,
  u_GEIndexFile,
  u_TileStorageAbstract;

type
  TTileStorageGE = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigGE;
    FIndex: TGEIndexFile;
    FMainContentType: IContentTypeInfoBasic;
    //FTileVersionsCacher: ITileObjCacheStrings;
  private
    function ListOfVersions_Make: TStrings;
    function ListOfVersions_NeedToCollect(const AXY: TPoint; const AZoom: Byte): Boolean;
    procedure ListOfVersions_SaveToCache(const AXY: TPoint; const AZoom: Byte;
                                         var AListOfVersions: TStrings);
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager//;
      //ATileVersionsCacher: ITileObjCacheStrings
    );
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): string; override;

    function GetTileInfo(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic; override;

    function LoadTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream;
      out ATileInfo: ITileInfoBasic
    ): Boolean; override;

    function DeleteTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;
    function DeleteTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ): Boolean; override;

    procedure SaveTile(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo;
      AStream: TStream
    ); override;
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); override;

    function GetListOfTileVersions(const AXY: TPoint; const Azoom: byte;
                                   const AAllowFromCache: Boolean;
                                   AListOfVersions: TStrings): Boolean; override;
  end;

implementation

uses
  SysUtils,
  t_CommonTypes,
  u_TileInfoBasic,
  u_TileStorageTypeAbilities,
  u_GECrypt;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager//;
  //ATileVersionsCacher: ITileObjCacheStrings
);
begin
  inherited Create(TTileStorageTypeAbilitiesGE.Create, AConfig);
  //FTileVersionsCacher := ATileVersionsCacher;
  FCacheConfig := TMapTypeCacheConfigGE.Create(AConfig, AGlobalCacheConfig);
  FIndex := TGEIndexFile.Create(StorageStateInternal, FCacheConfig);
  FMainContentType := AContentTypeManager.GetInfo('application/vnd.google-earth.tile-image');
end;

destructor TTileStorageGE.Destroy;
begin
  //FTileVersionsCacher:=nil;
  FreeAndNil(FIndex);
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGE.DeleteTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.DeleteTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetAllowDifferentContentTypes: Boolean;
begin
  Result := True;
end;

function TTileStorageGE.GetListOfTileVersions(const AXY: TPoint; const Azoom: byte;
                                              const AAllowFromCache: Boolean;
                                              AListOfVersions: TStrings): Boolean;
var
  VVersionInfo: IMapVersionInfo;
  VOffset: Integer;
  VSize: Integer;
begin
  Result:=FALSE;
  {if AAllowFromCache and Assigned(FTileVersionsCacher) then begin
    // allow from cache
    if FTileVersionsCacher.TryLoadTileFromCache(AInfo, AXY, Azoom, nil) then
      Inc(Result);
  end else begin}
    // from storage
    if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize, AListOfVersions) then
      Inc(Result);
  {end;}
end;

function TTileStorageGE.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageGE.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageGE.GetTileFileName(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): string;
begin
  Abort;
end;

function TTileStorageGE.GetTileInfo(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  VOffset: Integer;
  VSize: Integer;
  VVersionInfo: IMapVersionInfo;
  VListOfVersions: TStrings;
begin
  Result := nil;
  VListOfVersions := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then
  try
    VVersionInfo := AVersionInfo;

    // if need to collect versions
    if ListOfVersions_NeedToCollect(AXY, Azoom) then
      VListOfVersions:=ListOfVersions_Make;

    // do it
    if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize, VListOfVersions) then begin
      Result := TTileInfoBasicExists.Create(
        0,
        VSize,
        VVersionInfo,
        FMainContentType
      );
    end else begin
      Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
    end;

    // cache versions
    if (nil<>VListOfVersions) then
      ListOfVersions_SaveToCache(AXY, Azoom, VListOfVersions);
  finally
    FreeAndNil(VListOfVersions);
  end;
end;

function TTileStorageGE.ListOfVersions_Make: TStrings;
begin
  {if Assigned(FTileVersionsCacher) then begin
    Result := TStringList.Create;
    with TStringList(Result) do begin
      Sorted:=TRUE;
      Duplicates:=dupIgnore;
    end;
  end else}
    Result := nil;
end;

function TTileStorageGE.ListOfVersions_NeedToCollect(const AXY: TPoint; const AZoom: Byte): Boolean;
begin
  Result := FALSE; // not need to cache
  {if Assigned(FTileVersionsCacher) then begin
    // versioninfo always NIL
    if not FTileVersionsCacher.TryLoadTileFromCache(nil, AXY, AZoom, nil) then
      Inc(Result); // not in cache and need to cache
  end;}
end;

procedure TTileStorageGE.ListOfVersions_SaveToCache(const AXY: TPoint;
                                                    const AZoom: Byte;
                                                    var AListOfVersions: TStrings);
begin
  {if Assigned(FTileVersionsCacher) then
    FTileVersionsCacher.AddTileToCache(AListOfVersions, AXY, AZoom, nil);}
end;

function TTileStorageGE.LoadTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream;
  out ATileInfo: ITileInfoBasic
): Boolean;
var
  VFileName: string;
  VFileStream: TFileStream;
  VOffset: Integer;
  VSize: Integer;
  VMemStream: TMemoryStream;
  VTileStart: LongWord;
  VVersionInfo: IMapVersionInfo;
  VListOfVersions: TStrings;
begin
  Result := False;
  VListOfVersions := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then
  try
    VVersionInfo := AVersionInfo;

    // if need to collect versions
    if ListOfVersions_NeedToCollect(AXY, Azoom) then
      VListOfVersions:=ListOfVersions_Make;
    
    // do it
    if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize, VListOfVersions) then begin
      VFileName := FCacheConfig.GetDataFileName;
      if FileExists(VFileName) then begin
        VFileStream := TFileStream.Create(VFileName, fmOpenRead + fmShareDenyNone);
        try
          VFileStream.Position := VOffset + 36;
          VMemStream := TMemoryStream.Create;
          try
            VMemStream.CopyFrom(VFileStream, VSize);
            VMemStream.Position := 0;
            VMemStream.ReadBuffer(VTileStart, SizeOf(VTileStart));
            case VTileStart of
              CRYPTED_JPEG: begin
                GEcrypt(VMemStream.Memory, VMemStream.Size);
                Result := True;
              end;
              DECRYPTED_JPEG: begin
                Result := True;
              end;
              CRYPTED_DXT1: begin
                GEcrypt(VMemStream.Memory, VMemStream.Size);
                Result := True;
              end;
              DECRYPTED_DXT1: begin
                Result := True;
              end;
            end;
            if Result then begin
              VMemStream.SaveToStream(AStream);
            end;
            ATileInfo := TTileInfoBasicExists.Create(
              0,
              VSize,
              VVersionInfo,
              FMainContentType
            );
          finally
            VMemStream.Free;
          end;
        finally
          VFileStream.Free;
        end;
      end;
    end else begin
      ATileInfo := TTileInfoBasicNotExists.Create(0, VVersionInfo);
    end;

    // cache versions
    if (nil<>VListOfVersions) then
      ListOfVersions_SaveToCache(AXY, Azoom, VListOfVersions);
  finally
    FreeAndNil(VListOfVersions);
  end;
end;

procedure TTileStorageGE.SaveTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AStream: TStream
);
begin
  Abort;
end;

procedure TTileStorageGE.SaveTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
);
begin
  Abort;
end;

end.
