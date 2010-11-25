unit u_TileStorageFileSystem;

interface

uses
  Windows,
  Types,
  Classes,
  GR32,
  i_IConfigDataProvider,
  i_ICoordConverter,
  i_ContentTypeInfo,
  i_ITileInfoBasic,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageFileSystem = class(TTileStorageAbstract)
  private
    FUseDel: boolean;
    FIsStoreReadOnly: Boolean;
    FUseSave: boolean;
    FTileFileExt: string;
    FCacheConfig: TMapTypeCacheConfigAbstract;
    FCoordConverter: ICoordConverter;
    FMainContentType: IContentTypeInfoBasic;
    procedure CreateDirIfNotExists(APath: string);
    function GetTileInfoByPath(APath: string; AVersion: Variant): ITileInfoBasic;
  public
    constructor Create(AConfig: IConfigDataProvider);
    destructor Destroy; override;

    function GetMainContentType: IContentTypeInfoBasic; override;
    function GetAllowDifferentContentTypes: Boolean; override;

    function GetIsStoreFileCache: Boolean; override;
    function GetUseDel: boolean; override;
    function GetUseSave: boolean; override;
    function GetIsStoreReadOnly: boolean; override;
    function GetTileFileExt: string; override;
    function GetCoordConverter: ICoordConverter; override;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string; override;
    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic; override;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean; override;

    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; override;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; override;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream); override;
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant); override;

    function LoadFillingMap(btm: TCustomBitmap32; AXY: TPoint; Azoom: byte; ASourceZoom: byte; AVersion: Variant; IsStop: PBoolean): boolean; override;
  end;

implementation

uses
  SysUtils,
  t_GeoTypes,
  i_ITileIterator,
  u_GlobalState,
  u_TileIteratorByRect,
  u_TileInfoBasic;

{ TTileStorageFileSystem }

constructor TTileStorageFileSystem.Create(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FUseDel:=VParams.ReadBool('Usedel',true);
  FIsStoreReadOnly:=VParams.ReadBool('ReadOnly', false);
  FUseSave:=VParams.ReadBool('Usesave',true);
  FTileFileExt:=LowerCase(VParams.ReadString('Ext','.jpg'));
  FCacheConfig := TMapTypeCacheConfig.Create(AConfig);
  FCoordConverter := GState.CoordConverterFactory.GetCoordConverterByConfig(VParams);
  FMainContentType := GState.ContentTypeManager.GetInfoByExt(FTileFileExt);
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

function TTileStorageFileSystem.DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
var
  VPath: string;
begin
  Result := false;
  if FUseDel then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      if FileExists(VPath) then begin
        result := DeleteFile(VPath);
      end;
      DeleteTNE(AXY, Azoom, AVersion);
    except
      Result := false;
    end;
  end else begin
    Exception.Create('Для этой карты запрещено удаление тайлов.');
  end;
end;

function TTileStorageFileSystem.DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
var
  VPath: string;
begin
  Result := False;
  if FUseDel then begin
    try
      VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
      VPath := ChangeFileExt(VPath, '.tne');
      if FileExists(VPath) then begin
        result := DeleteFile(VPath);
      end;
    except
      Result := false;
    end;
  end else begin
    Exception.Create('Для этой карты запрещено удаление тайлов.');
  end;
end;

destructor TTileStorageFileSystem.Destroy;
begin
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageFileSystem.GetAllowDifferentContentTypes: Boolean;
begin
  Result := False;
end;

function TTileStorageFileSystem.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageFileSystem.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TTileStorageFileSystem.GetIsStoreFileCache: Boolean;
begin
  Result := True;
end;

function TTileStorageFileSystem.GetIsStoreReadOnly: boolean;
begin
  Result := FIsStoreReadOnly;
end;

function TTileStorageFileSystem.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageFileSystem.GetTileFileExt: string;
begin
  Result := FTileFileExt;
end;

function TTileStorageFileSystem.GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, Azoom);
end;

function TTileStorageFileSystem.GetTileInfoByPath(APath: string; AVersion: Variant): ITileInfoBasic;
var
  InfoFile: TSearchRec;
  VSearchResult: Integer;
begin
  VSearchResult := FindFirst(APath, faAnyFile, InfoFile);
  if VSearchResult <> 0 then begin
    APath := ChangeFileExt(APath, '.tne');
    VSearchResult := FindFirst(APath, faAnyFile, InfoFile);
    if VSearchResult <> 0 then begin
      Result := TTileInfoBasicNotExists.Create(0, AVersion);
    end else begin
      Result := TTileInfoBasicTNE.Create(FileDateToDateTime(InfoFile.Time), AVersion);
      FindClose(InfoFile);
    end;
  end else begin
    Result := TTileInfoBasicExists.Create(
      FileDateToDateTime(InfoFile.Time),
      InfoFile.Size,
      AVersion,
      FMainContentType
    );
    FindClose(InfoFile);
  end;
end;

function TTileStorageFileSystem.GetTileInfo(AXY: TPoint; Azoom: byte;
  AVersion: Variant): ITileInfoBasic;
var
  VPath: String;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  Result := GetTileInfoByPath(VPath, AVersion);
end;

function TTileStorageFileSystem.GetUseDel: boolean;
begin
  Result := FUseDel;
end;

function TTileStorageFileSystem.GetUseSave: boolean;
begin
  Result := FUseSave;
end;

function TTileStorageFileSystem.LoadFillingMap(btm: TCustomBitmap32;
  AXY: TPoint; Azoom, ASourceZoom: byte; AVersion: Variant;
  IsStop: PBoolean): boolean;
var
  VPixelsRect: TRect;
  VRelativeRect: TDoubleRect;
  VSourceTilesRect: TRect;
  VCurrTile: TPoint;
  VTileSize: TPoint;
  VSourceTilePixels: TRect;
  VClMZ: TColor32;
  VClTne: TColor32;
  VSolidDrow: Boolean;
  VIterator: ITileIterator;
  VFileName: string;
  VFolderName: string;
  VTileColor: TColor32;
  VPrevFolderName: string;
  VPrevFolderExist: Boolean;
  VFolderExists: Boolean;
  VFileExists: Boolean;
  VGeoConvert: ICoordConverter;
begin
  Result := true;
  try
    VGeoConvert := GetCoordConverter;
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
      VClMZ := SetAlpha(Color32(GState.MapZapColor), GState.MapZapAlpha);
      VClTne := SetAlpha(Color32(GState.MapZapTneColor), GState.MapZapAlpha);
      VIterator := TTileIteratorByRect.Create(VSourceTilesRect);
      while VIterator.Next(VCurrTile) do begin
        if IsStop^ then break;
        VFileName := FCacheConfig.GetTileFileName(AXY, Azoom);
        VFolderName := ExtractFilePath(VFileName);
        if VFolderName = VPrevFolderName then begin
          VFolderExists := VPrevFolderExist;
        end else begin
          VFolderExists := DirectoryExists(VFolderName);
          VPrevFolderName := VFolderName;
          VPrevFolderExist := VFolderExists;
        end;
        if VFolderExists then begin
          VFileExists := FileExists(VFileName);
        end else begin
          VFileExists := False;
        end;

        if not VFileExists then begin
          if IsStop^ then break;
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
          if GState.MapZapShowTNE then begin
            if VFolderExists then begin
              VFileName := ChangeFileExt(VFileName, '.tne');
              if FileExists(VFileName) then begin
                VTileColor := VClTne;
              end else begin
                VTileColor := VClMZ;
              end;
            end else begin
              VTileColor := VClMZ;
            end;
          end else begin
            VTileColor := VClMZ;
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
    if IsStop^ then begin
      Result := false;
    end;
  except
    Result := false;
  end;
end;

function TTileStorageFileSystem.LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant;
  AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean;
var
  VPath: String;
  VMemStream: TMemoryStream;
  VTileInfo: ITileInfoBasic;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  VTileInfo := GetTileInfoByPath(VPath, AVersion);
  if VTileInfo.GetIsExists then begin
    if AStream is TMemoryStream then begin
      VMemStream := TMemoryStream(AStream);
      try
        VMemStream.LoadFromFile(VPath);
        Result := True;
      except
        Result := False;
      end;
    end else begin
      VMemStream := TMemoryStream.Create;
      try
        try
          VMemStream.LoadFromFile(VPath);
          VMemStream.SaveToStream(AStream);
          Result := True;
        except
          Result := False;
        end;
      finally
        VMemStream.Free;
      end;
    end;
  end else begin
    Result := False;
  end;
end;

procedure TTileStorageFileSystem.SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant;
  AStream: TStream);
var
  VPath: String;
  VMemStream: TMemoryStream;
begin
  if FUseSave then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    CreateDirIfNotExists(VPath);
    if AStream is TMemoryStream then begin
      VMemStream := TMemoryStream(AStream);
      VMemStream.SaveToFile(VPath);
    end else begin
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromStream(AStream);
        VMemStream.SaveToFile(VPath);
      finally
        VMemStream.Free;
      end;
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

procedure TTileStorageFileSystem.SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant);
var
  VPath: String;
  F:textfile;
begin
  if FUseSave then begin
    VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
    VPath := ChangeFileExt(VPath, '.tne');
    if not FileExists(VPath) then begin
      CreateDirIfNotExists(VPath);
      AssignFile(f,VPath);
      Rewrite(F);
      Writeln(f, DateTimeToStr(now));
      CloseFile(f);
    end;
  end else begin
    raise Exception.Create('Для этой карты запрещено добавление тайлов.');
  end;
end;

end.
