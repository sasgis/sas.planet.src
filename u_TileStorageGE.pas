{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      AContentTypeManager: IContentTypeManager
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
  end;

implementation

uses
  SysUtils,
  Variants,
  u_TileInfoBasic,
  u_GECrypt;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  AContentTypeManager: IContentTypeManager
);
begin
  inherited Create(AConfig);
  FCacheConfig := TMapTypeCacheConfigGE.Create(AConfig, AGlobalCacheConfig);
  FIndex := TGEIndexFile.Create(FCacheConfig);
  FMainContentType := AContentTypeManager.GetInfo('application/vnd.google-earth.tile-image');
end;

destructor TTileStorageGE.Destroy;
begin
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
begin
  VVersionInfo := AVersionInfo;
  if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize) then begin
    Result := TTileInfoBasicExists.Create(
      0,
      VSize,
      VVersionInfo,
      FMainContentType
    );
  end else begin
    Result := TTileInfoBasicNotExists.Create(0, AVersionInfo);
  end;
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
begin
  Result := False;
  VVersionInfo := AVersionInfo;
  if FIndex.FindTileInfo(AXY, Azoom, VVersionInfo, VOffset, VSize) then begin
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
