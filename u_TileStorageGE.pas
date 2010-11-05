unit u_TileStorageGE;

interface

uses
  Types,
  Classes,
  i_ICoordConverter,
  i_ITileInfoBasic,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageGE = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigAbstract;

    indexfilename:string;
    indexfile:TMemoryStream;
    procedure GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
    function GEFindTileAdr(indexpath: string; APoint: TPoint; AZoom: Byte; var size:integer):integer;
  public
    constructor Create(ACoordConverter: ICoordConverter);
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
  SysUtils;

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

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(ACoordConverter: ICoordConverter);
begin
  inherited Create(ACoordConverter);
  FCacheConfig := TMapTypeCacheConfigGE.Create;
  indexfile:=TMemoryStream.Create;
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FCacheConfig);
  FreeAndNil(indexfile);
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
  VBlock: TIndexRec;
  VNameLo, VNameHi: LongWord;
begin
  result:=0;
  size:=0;
  try
    if FileExists(indexpath) then begin
      if (indexfilename<>indexpath) then begin
        indexfile.LoadFromFile(indexpath);
        indexfilename:=indexpath;
      end;
      GEXYZtoHexTileName(APoint, AZoom, VNameHi, VNameLo);
      indexfile.Position := 0;
      while indexfile.Read(VBlock, SizeOf(VBlock)) = SizeOf(VBlock) do begin
        if VBlock.TileID = 130 then begin
          if VBlock.Zoom = AZoom then begin
            if (VBlock.NameLo = VNameLo) and (VBlock.NameHi = VNameHi) then begin
              Result := VBlock.Offset;
              size := VBlock.Size;
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

function TTileStorageGE.GetCacheConfig: TMapTypeCacheConfigAbstract;
begin
  Result := FCacheConfig;
end;

function TTileStorageGE.GetIsStoreFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetIsStoreReadOnly: boolean;
begin
  Result := True;
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

end.
