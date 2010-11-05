unit u_TileStorageFileSystem;

interface

uses
  Windows,
  Types,
  Classes,
  GR32,
  i_IConfigDataProvider,
  i_ICoordConverter,
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
    procedure CreateDirIfNotExists(APath: string);
    function GetTileInfoByPath(APath: string; AVersion: Variant): ITileInfoBasic;
  public
    constructor Create(ACoordConverter: ICoordConverter; AConfig: IConfigDataProvider);
    destructor Destroy; override;

    function GetIsStoreFileCache: Boolean; override;
    function GetUseDel: boolean; override;
    function GetUseSave: boolean; override;
    function GetIsStoreReadOnly: boolean; override;
    function GetTileFileExt: string; override;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string; override;
    function GetTileInfo(AXY: TPoint; Azoom: byte; AVersion: Variant): ITileInfoBasic; override;

    function LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean; override;

    function DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; override;
    function DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean; override;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant; AStream: TStream); override;
    procedure SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant); override;
  end;

implementation

uses
  SysUtils,
  u_TileInfoBasic;

{ TTileStorageFileSystem }

constructor TTileStorageFileSystem.Create(ACoordConverter: ICoordConverter; AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
begin
  inherited Create(ACoordConverter);

  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FUseDel:=VParams.ReadBool('Usedel',true);
  FIsStoreReadOnly:=VParams.ReadBool('ReadOnly', false);
  FUseSave:=VParams.ReadBool('Usesave',true);
  FTileFileExt:=LowerCase(VParams.ReadString('Ext','.jpg'));
  FCacheConfig := TMapTypeCacheConfig.Create(AConfig);
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

//function TTileStorageFileSystem.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
//var
//  VPath: String;
//begin
//  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
//  Result := Fileexists(VPath);
//end;
//
//function TTileStorageFileSystem.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
//var
//  VPath: String;
//begin
//  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
//  Result := Fileexists(ChangeFileExt(VPath, '.tne'));
//end;
//
function TTileStorageFileSystem.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageFileSystem.GetIsStoreFileCache: Boolean;
begin
  Result := True;
end;

function TTileStorageFileSystem.GetIsStoreReadOnly: boolean;
begin
  Result := FIsStoreReadOnly;
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
    Result := TTileInfoBasicExists.Create(FileDateToDateTime(InfoFile.Time), InfoFile.Size, AVersion);
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
      VMemStream.LoadFromFile(VPath);
      Result := True;
    end else begin
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromFile(VPath);
        VMemStream.SaveToStream(AStream);
        Result := True;
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
