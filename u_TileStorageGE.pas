unit u_TileStorageGE;

interface

uses
  Types,
  Classes,
  i_ICoordConverter,
  i_IConfigDataProvider,
  i_ContentTypeInfo,
  i_ITileInfoBasic,
  u_MapTypeCacheConfig,
  u_GEIndexFile,
  u_TileStorageAbstract;

type
  TTileStorageGE = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigGE;
    FCoordConverter: ICoordConverter;
    FIndex: TGEIndexFile;
    FMainContentType: IContentTypeInfoBasic;
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
  end;

implementation

uses
  SysUtils,
  Variants,
  c_CoordConverter,
  u_ContentTypeInfo,
  u_TileInfoBasic,
  u_GlobalState;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(AConfig: IConfigDataProvider);
begin
  FCacheConfig := TMapTypeCacheConfigGE.Create;
  FCoordConverter := GState.CoordConverterFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
  FIndex := TGEIndexFile.Create(FCacheConfig);
  FMainContentType := TContentTypeInfoBitmap.Create(
    'application/ge+image+decrypted',
    '.ge_tile',
    nil,
    nil
  );
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FIndex);
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGE.DeleteTile(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.DeleteTNE(AXY: TPoint; Azoom: byte; AVersion: Variant): Boolean;
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

function TTileStorageGE.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TTileStorageGE.GetIsStoreFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetIsStoreReadOnly: boolean;
begin
  Result := True;
end;

function TTileStorageGE.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageGE.GetTileFileExt: string;
begin
  Result := '.ge_tile';
end;

function TTileStorageGE.GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
begin
  Abort;
end;

function TTileStorageGE.GetTileInfo(AXY: TPoint; Azoom: byte;
  AVersion: Variant): ITileInfoBasic;
var
  VVersion: Word;
  VOffset: Integer;
  VSize: Integer;
begin
  try
    VVersion := AVersion;
  except
    VVersion := 0;
  end;
  if FIndex.FindTileInfo(AXY, Azoom, VVersion, VOffset, VSize) then begin
    Result := TTileInfoBasicExists.Create(
      0,
      VSize,
      VVersion,
      FMainContentType
    );
  end else begin
    Result := TTileInfoBasicNotExists.Create(0, VVersion);
  end;
end;

function TTileStorageGE.GetUseDel: boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetUseSave: boolean;
begin
  Result := False;
end;

function TTileStorageGE.LoadTile(AXY: TPoint; Azoom: byte; AVersion: Variant;
  AStream: TStream; out ATileInfo: ITileInfoBasic): Boolean;
var
  VFileName: string;
  VFileStream: TFileStream;
  VVersion: Word;
  VOffset: Integer;
  VSize: Integer;
begin
  Result := False;
  try
    VVersion := AVersion;
  except
    VVersion := 0;
  end;
  if FIndex.FindTileInfo(AXY, Azoom, VVersion, VOffset, VSize) then begin
    VFileName := FCacheConfig.GetDataFileName;
    if FileExists(VFileName) then begin
      VFileStream := TFileStream.Create(VFileName, fmOpenRead + fmShareDenyNone);
      try
        VFileStream.Position := VOffset + 36;
        AStream.CopyFrom(VFileStream, VSize);
        Result := True;
          ATileInfo := TTileInfoBasicExists.Create(
            0,
            VSize,
            VVersion,
            FMainContentType
          );
      finally
        VFileStream.Free;
      end;
    end;
  end else begin
    ATileInfo := TTileInfoBasicNotExists.Create(0, VVersion);
  end;
end;

procedure TTileStorageGE.SaveTile(AXY: TPoint; Azoom: byte; AVersion: Variant;
  AStream: TStream);
begin
  Abort;
end;

procedure TTileStorageGE.SaveTNE(AXY: TPoint; Azoom: byte; AVersion: Variant);
begin
  Abort;
end;

end.
