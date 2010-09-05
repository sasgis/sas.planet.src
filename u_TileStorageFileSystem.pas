unit u_TileStorageFileSystem;

interface

uses
  Windows,
  Types,
  Classes,
  GR32,
  i_IConfigDataProvider,
  i_ICoordConverter,
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
  public
    constructor Create(ACoordConverter: ICoordConverter; AConfig: IConfigDataProvider);
    destructor Destroy; override;

    function GetIsStoreFileCache: Boolean; override;
    function GetUseDel: boolean; override;
    function GetUseSave: boolean; override;
    function GetIsStoreReadOnly: boolean; override;

    function ExistsTile(AXY: TPoint; Azoom: byte): Boolean; override;
    function ExistsTNE(AXY: TPoint; Azoom: byte): Boolean; override;

    function DeleteTile(AXY: TPoint; Azoom: byte): Boolean; override;
    function DeleteTNE(AXY: TPoint; Azoom: byte): Boolean; override;

    function GetTileFileName(AXY: TPoint; Azoom: byte): string; override;
    function GetTileFileExt: string; override;
    function GetCacheConfig: TMapTypeCacheConfigAbstract; override;

    function LoadTile(AXY: TPoint; Azoom: byte; AStream: TStream): Boolean; override;
    function TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime; override;
    function TileSize(AXY: TPoint; Azoom: byte): integer; override;

    procedure SaveTile(AXY: TPoint; Azoom: byte; AStream: TStream); override;
    procedure SaveTNE(AXY: TPoint; Azoom: byte); override;
  end;

implementation

uses
  SysUtils;

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

function TTileStorageFileSystem.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
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
      DeleteTNE(AXY, Azoom);
    except
      Result := false;
    end;
  end else begin
    Exception.Create('Для этой карты запрещено удаление тайлов.');
  end;
end;

function TTileStorageFileSystem.DeleteTNE(AXY: TPoint; Azoom: byte): Boolean;
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
  FCacheConfig := nil;
  inherited;
end;

function TTileStorageFileSystem.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: String;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  Result := Fileexists(VPath);
end;

function TTileStorageFileSystem.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
var
  VPath: String;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  Result := Fileexists(ChangeFileExt(VPath, '.tne'));
end;

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

function TTileStorageFileSystem.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Result := FCacheConfig.GetTileFileName(AXY, Azoom);
end;

function TTileStorageFileSystem.GetUseDel: boolean;
begin
  Result := FUseDel;
end;

function TTileStorageFileSystem.GetUseSave: boolean;
begin
  Result := FUseDel;
end;

function TTileStorageFileSystem.LoadTile(AXY: TPoint; Azoom: byte;
  AStream: TStream): Boolean;
var
  VPath: String;
  VMemStream: TMemoryStream;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  if FileExists(VPath) then begin
    if AStream is TMemoryStream then begin
      VMemStream := TMemoryStream(AStream);
      VMemStream.LoadFromFile(VPath);
      Result := True;
    end else begin
      VMemStream := TMemoryStream.Create;
      try
        VMemStream.LoadFromFile(VPath);
        VMemStream.SaveToStream(AStream);
      finally
        VMemStream.Free;
      end;
    end;
  end else begin
    Result := False;
  end;
end;

procedure TTileStorageFileSystem.SaveTile(AXY: TPoint; Azoom: byte;
  AStream: TStream);
var
  VPath: String;
  VMemStream: TMemoryStream;
  VFileExists: Boolean;
begin
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
end;

procedure TTileStorageFileSystem.SaveTNE(AXY: TPoint; Azoom: byte);
var
  VPath: String;
  F:textfile;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  VPath := ChangeFileExt(VPath, '.tne');
  if not FileExists(VPath) then begin
    CreateDirIfNotExists(VPath);
    AssignFile(f,VPath);
    Rewrite(F);
    Writeln(f, DateTimeToStr(now));
    CloseFile(f);
  end;
end;

function TTileStorageFileSystem.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
var
  VPath: String;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  Result := FileDateToDateTime(FileAge(VPath));
end;

function TTileStorageFileSystem.TileSize(AXY: TPoint; Azoom: byte): integer;
var
  InfoFile: TSearchRec;
  VPath: String;
begin
  VPath := FCacheConfig.GetTileFileName(AXY, Azoom);
  if FindFirst(VPath, faAnyFile, InfoFile) <> 0 then begin
    Result := -1;
  end else begin
    Result := InfoFile.Size;
  end;
  SysUtils.FindClose(InfoFile);
end;

end.
