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
  u_TileStorageAbstract;

type
   TIndexRec = packed record
     Magic  : LongWord;  // число-идентификатор =  D5 BF 93 75
     Ver    : Word;      // версия тайла
     TileID : Byte;      // тип тайла
     Res1   : Byte;
     Zoom   : Byte;      // уровень зума
     Res2   : Byte;
     Layer  : Word;      // номер слоя (только для слоя, иначе = 0)
     NameLo : LongWord;  // первая часть имени
     NameHi : LongWord;  // вторая часть имени
     ServID : Word;      // номер сервера из списка в dbCache.dat
     Unk    : Word;
     Offset : LongWord;  // позиция тайла в кэше dbCache.dat
     Size   : LongWord;  // размер тайла
   end;

type
  TTileStorageGE = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigGE;
    FCoordConverter: ICoordConverter;

    indexfilename:string;
    FIndexInfo: array of TIndexRec;
    procedure GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
    function GEFindTileAdr(indexpath: string; APoint: TPoint; AZoom: Byte; var size:integer):integer;
    procedure UpdateIndexInfo;
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
  c_CoordConverter,
  u_GlobalState;

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(AConfig: IConfigDataProvider);
begin
  FCacheConfig := TMapTypeCacheConfigGE.Create;
  FCoordConverter := GState.CoordConverterFactory.GetCoordConverterByCode(CGELonLatProjectionEPSG, CTileSplitQuadrate256x256);
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FCacheConfig);
  FIndexInfo := nil;
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

procedure TTileStorageGE.GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
var
  VMask: Integer;
  i: byte;
  VValue: Byte;
begin
  ANameHi := 0;
  ANameLo := 0;
  if AZoom > 0 then begin
    VMask := 1 shl (AZoom - 1);
    for i := 1 to AZoom do begin
      if (APoint.X and VMask) > 0 then begin
        if (APoint.y and VMask) > 0 then begin
          VValue := 3;
        end else begin
          VValue := 0;
        end;
      end else begin
        if (APoint.y and VMask) > 0 then begin
          VValue := 2;
        end else begin
          VValue := 1;
        end;
      end;
      if i < 16 then begin
        ANameHi := ANameHi or (LongWord(VValue) shl (30 - i * 2));
      end else begin
        ANameLo := ANameLo or (LongWord(VValue) shl (30 - (i - 16) * 2));
      end;
      VMask := VMask shr 1;
    end;
  end;
end;

function TTileStorageGE.GEFindTileAdr(indexpath: string; APoint: TPoint; AZoom: Byte;
  var size: integer): integer;
var
  VNameLo, VNameHi: LongWord;
  i: Integer;
begin
  result:=0;
  size:=0;
  try
    UpdateIndexInfo;
    if Length(FIndexInfo) > 0  then begin
      GEXYZtoHexTileName(APoint, AZoom, VNameHi, VNameLo);
      for i := 0 to Length(FIndexInfo) - 1 do begin
        if FIndexInfo[i].TileID = 130 then begin
          if FIndexInfo[i].Zoom = AZoom then begin
            if (FIndexInfo[i].NameLo = VNameLo) and (FIndexInfo[i].NameHi = VNameHi) then begin
              Result := FIndexInfo[i].Offset;
              size := FIndexInfo[i].Size;
            end;
          end;
        end;
      end;
    end;
  except
    result := 0;
    size := 0;
  end;
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
  Result := nil;
end;

function TTileStorageGE.GetTileFileExt: string;
begin
  Result := 'ge_tile';
end;

function TTileStorageGE.GetTileFileName(AXY: TPoint; Azoom: byte; AVersion: Variant): string;
begin
  Abort;
end;

function TTileStorageGE.GetTileInfo(AXY: TPoint; Azoom: byte;
  AVersion: Variant): ITileInfoBasic;
begin
  Result := nil;
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
begin
  Result := False;
  Abort;
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

procedure TTileStorageGE.UpdateIndexInfo;
var
  indexpath: string;
  VFileStream: TFileStream;
  VCount: Cardinal;
begin
  indexpath := FCacheConfig.GetIndexFileName;
  if FileExists(indexpath) then begin
    if (indexfilename<>indexpath) then begin
      VFileStream := TFileStream.Create(indexpath, fmOpenRead);
      try
        VCount := VFileStream.Size div SizeOf(FIndexInfo[0]);
        SetLength(FIndexInfo, VCount );
        VFileStream.Read(FIndexInfo[0], VCount * SizeOf(FIndexInfo[0]));
      finally
        FreeAndNil(VFileStream);
      end;
      indexfilename:=indexpath;
    end;
  end else begin
    FIndexInfo := nil;
  end;
end;

end.
