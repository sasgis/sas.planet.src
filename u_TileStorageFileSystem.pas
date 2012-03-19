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
  i_TileFileNameGeneratorsList,
  i_ContentTypeManager,
  u_GlobalCahceConfig,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageFileSystem = class(TTileStorageAbstract)
  private
    FLock: IReadWriteSync;
    FCacheConfig: TMapTypeCacheConfigAbstract;
    FMainContentType: IContentTypeInfoBasic;
    FFormatSettings: TFormatSettings;
    FTileNotExistsTileInfo: ITileInfoBasic;
    procedure CreateDirIfNotExists(APath: string);
    function GetTileInfoByPath(
      APath: string;
      AVersionInfo: IMapVersionInfo
    ): ITileInfoBasic;
  public
    constructor Create(
      AConfig: ISimpleTileStorageConfig;
      AGlobalCacheConfig: TGlobalCahceConfig;
      ATileNameGeneratorList: ITileFileNameGeneratorsList;
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
      out ATileInfo: ITileInfoBasic
    ): IBinaryData; override;

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
      AData: IBinaryData
    ); override;
    procedure SaveTNE(
      AXY: TPoint;
      Azoom: byte;
      AVersionInfo: IMapVersionInfo
    ); override;

    function LoadFillingMap(
      AOperationID: Integer;
      ACancelNotifier: IOperationNotifier;
      btm: TCustomBitmap32;
      AXY: TPoint;
      Azoom: byte;
      ASourceZoom: byte;
      AVersionInfo: IMapVersionInfo;
      AColorer: IFillingMapColorer
    ): boolean; override;
  end;

implementation

uses
  t_CommonTypes,
  t_GeoTypes,
  i_TileIterator,
  u_BinaryDataByMemStream,
  u_MapVersionFactorySimpleString,
  u_TileStorageTypeAbilities,
  u_TileIteratorByRect,
  u_TileInfoBasic;

{ TTileStorageFileSystem }

constructor TTileStorageFileSystem.Create(
  AConfig: ISimpleTileStorageConfig;
  AGlobalCacheConfig: TGlobalCahceConfig;
  ATileNameGeneratorList: ITileFileNameGeneratorsList;
  AContentTypeManager: IContentTypeManager
);
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
end;

destructor TTileStorageFileSystem.Destroy;
begin
  FreeAndNil(FCacheConfig);
  inherited;
end;

procedure TTileStorageFileSystem.CreateDirIfNotExists(APath: string);
var
  i: integer;
begin
  i := LastDelimiter(PathDelim, Apath);
  Apath := copy(Apath, 1, i);
  if not(DirectoryExists(Apath)) then begin
    ForceDirectories(Apath);
  end;
end;

function TTileStorageFileSystem.DeleteTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
begin
  Result := false;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      FLock.BeginWrite;
      try
        if FileExists(VPath) then begin
          result := DeleteFile(VPath);
        end;
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
end;

function TTileStorageFileSystem.DeleteTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): Boolean;
var
  VPath: string;
begin
  Result := False;
  if StorageStateStatic.DeleteAccess <> asDisabled then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      VPath := ChangeFileExt(VPath, '.tne');
      FLock.BeginWrite;
      try
        if FileExists(VPath) then begin
          result := DeleteFile(VPath);
        end;
      finally
        FLock.EndWrite;
      end;
    except
      Result := false;
    end;
  end;
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
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, Azoom);
end;

function TTileStorageFileSystem.GetTileInfoByPath(
  APath: string;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  InfoFile: TSearchRec;
  VSearchResult: Integer;
begin
  FLock.BeginRead;
  try
    VSearchResult := FindFirst(APath, faAnyFile, InfoFile);
    if VSearchResult <> 0 then begin
      APath := ChangeFileExt(APath, '.tne');
      VSearchResult := FindFirst(APath, faAnyFile, InfoFile);
      if VSearchResult <> 0 then begin
        Result := FTileNotExistsTileInfo;
      end else begin
        Result := TTileInfoBasicTNE.Create(FileDateToDateTime(InfoFile.Time), nil);
        FindClose(InfoFile);
      end;
    end else begin
      Result := TTileInfoBasicExists.Create(
        FileDateToDateTime(InfoFile.Time),
        InfoFile.Size,
        nil,
        FMainContentType
      );
      FindClose(InfoFile);
    end;
  finally
    FLock.EndRead;
  end;
end;

function TTileStorageFileSystem.GetTileInfo(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
): ITileInfoBasic;
var
  VPath: String;
begin
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    Result := GetTileInfoByPath(VPath, AVersionInfo);
  end;
end;

function TTileStorageFileSystem.LoadFillingMap(
  AOperationID: Integer;
  ACancelNotifier: IOperationNotifier;
  btm: TCustomBitmap32;
  AXY: TPoint;
  Azoom, ASourceZoom: byte;
  AVersionInfo: IMapVersionInfo;
  AColorer: IFillingMapColorer
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
begin
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    Result := true;
    try
      VGeoConvert := Config.CoordConverter;
      VGeoConvert.CheckTilePosStrict(AXY, Azoom, True);
      VGeoConvert.CheckZoom(ASourceZoom);

      VPixelsRect := VGeoConvert.TilePos2PixelRect(AXY, Azoom);

      VTileSize := Point(VPixelsRect.Right - VPixelsRect.Left, VPixelsRect.Bottom - VPixelsRect.Top);

      btm.Width := VTileSize.X;
      btm.Height := VTileSize.Y;
      btm.Clear(0);

      VRelativeRect := VGeoConvert.TilePos2RelativeRect(AXY, Azoom);
      VSourceTilesRect := VGeoConvert.RelativeRect2TileRect(VRelativeRect, ASourceZoom);
      VPrevFolderName := '';
      VPrevFolderExist := False;
      begin
        VSolidDrow := (VTileSize.X <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left))
          or (VTileSize.Y <= 2 * (VSourceTilesRect.Right - VSourceTilesRect.Left));
        VIterator := TTileIteratorByRect.Create(VSourceTilesRect);
        while VIterator.Next(VCurrTile) do begin
          if ACancelNotifier.IsOperationCanceled(AOperationID) then break;
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
            if ACancelNotifier.IsOperationCanceled(AOperationID) then break;
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
            if ((VSourceTilePixels.Right-VSourceTilePixels.Left)=1)and
               ((VSourceTilePixels.Bottom-VSourceTilePixels.Top)=1)then begin
              btm.Pixel[VSourceTilePixels.Left,VSourceTilePixels.Top]:=VTileColor;
            end else begin
              btm.FillRect(VSourceTilePixels.Left,VSourceTilePixels.Top,VSourceTilePixels.Right,VSourceTilePixels.Bottom, VTileColor);
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
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  out ATileInfo: ITileInfoBasic
): IBinaryData;
var
  VPath: String;
  VMemStream: TMemoryStream;
begin
  Result := nil;
  if StorageStateStatic.ReadAccess <> asDisabled then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    ATileInfo := GetTileInfoByPath(VPath, AVersionInfo);
    if ATileInfo.GetIsExists then begin
      FLock.BeginRead;
      try
        VMemStream := TMemoryStream.Create;
        try
          VMemStream.LoadFromFile(VPath);
          Result := TBinaryDataByMemStream.CreateWithOwn(VMemStream);
          VMemStream := nil;
        except
          VMemStream.Free;
        end;
      finally
        FLock.EndRead;
      end;
    end;
  end;
end;

procedure TTileStorageFileSystem.SaveTile(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo;
  AData: IBinaryData
);
var
  VPath: String;
  VFileStream: TFileStream;
begin
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
end;

procedure TTileStorageFileSystem.SaveTNE(
  AXY: TPoint;
  Azoom: byte;
  AVersionInfo: IMapVersionInfo
);
var
  VPath: String;
  VNow: TDateTime;
  VDateString: string;
  VFileStream: TFileStream;
begin
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
end;

end.
