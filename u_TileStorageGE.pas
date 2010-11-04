unit u_TileStorageGE;

interface

uses
  Types,
  Classes,
  i_ICoordConverter,
  u_MapTypeCacheConfig,
  u_TileStorageAbstract;

type
  TTileStorageGE = class(TTileStorageAbstract)
  private
    FCacheConfig: TMapTypeCacheConfigAbstract;

    indexfilename:string;
    indexfile:TMemoryStream;
    function GEXYZtoHexTileName(x,y:integer;z:byte):int64;
    function GEFindTileAdr(indexpath:string;x,y:integer;z:byte; var size:integer):integer;
  public
    constructor Create(ACoordConverter: ICoordConverter);
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

{ TTileStorageGEStuped }

constructor TTileStorageGE.Create(ACoordConverter: ICoordConverter);
begin
  inherited Create(ACoordConverter);
  FCacheConfig := TMapTypeCacheConfigGE.Create;
end;

function TTileStorageGE.DeleteTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.DeleteTNE(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

destructor TTileStorageGE.Destroy;
begin
  FreeAndNil(FCacheConfig);
  inherited;
end;

function TTileStorageGE.ExistsTile(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
  Abort;
end;

function TTileStorageGE.ExistsTNE(AXY: TPoint; Azoom: byte): Boolean;
begin
  Result := False;
end;

function TTileStorageGE.GEXYZtoHexTileName(x, y: integer; z: byte): int64;
var
  VMask: Integer;
  i: byte;
  VShift: byte;
  VValue: Byte;
begin
  Result := 0;
  VShift := 62;
  if z > 0 then begin
    VMask := 1 shl (z - 1);
    for i := 1 to z do begin
      if (x and VMask) > 0 then begin
        if (y and VMask) > 0 then begin
          VValue := 3;
        end else begin
          VValue := 0;
        end;
      end else begin
        if (y and VMask) > 0 then begin
          VValue := 2;
        end else begin
          VValue := 1;
        end;
      end;
      Result := Result or (int64(VValue) shl VShift);
      Dec(VShift, 2);
      VMask := VMask shr 1;
    end;
  end;
end;

function TTileStorageGE.GEFindTileAdr(indexpath: string; x, y: integer; z: byte;
  var size: integer): integer;
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
var iblock:array [0..31] of byte;
    name,FindName:int64;
    i:integer;
begin
 result:=0;
 size:=0;
 i:=0;
 if FileExists(indexpath) then
 try
   if (indexfilename<>indexpath)or(indexfile=nil) then begin
     indexfile:=TMemoryStream.Create;
     indexfile.LoadFromFile(indexpath);
     indexfilename:=indexpath;
   end;
   FindName:=GEXYZtoHexTileName(x,y,z);
   While ((FindName<>name))and(i<indexfile.Size) do begin
     if (Pbyte(longint(indexfile.Memory)+i+6)^=130)and
        (z=Pbyte(longint(indexfile.Memory)+i+8)^+1)and
        (Pbyte(longint(indexfile.Memory)+i+20)^=0) then begin
//       copymemory(@iblock,Pointer(longint(indexfile.Memory)+i),32);
       name:=
        (int64(iblock[12])shl 32)or
        (int64(iblock[13])shl 40)or
        (int64(iblock[14])shl 48)or
        (int64(iblock[15])shl 56)or
        (iblock[16])or
        (iblock[17]shl 8)or
        (iblock[18]shl 16)or
        (iblock[19]shl 24);
     end;
     inc(i,32);
   end;
   if FindName=name then begin
     result:=(iblock[24]or(iblock[25]shl 8)or(iblock[26]shl 16)or(iblock[27]shl 24));
     size:=(iblock[28]or(iblock[29]shl 8)or(iblock[30]shl 16)or(iblock[31]shl 24));
   end;
 except
  result:=0;
  size:=0;
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

function TTileStorageGE.GetTileFileName(AXY: TPoint; Azoom: byte): string;
begin
  Abort;
end;

function TTileStorageGE.GetUseDel: boolean;
begin
  Result := False;
end;

function TTileStorageGE.GetUseSave: boolean;
begin
  Result := False;
end;

function TTileStorageGE.LoadTile(AXY: TPoint; Azoom: byte;
  AStream: TStream): Boolean;
begin
  Result := False;
  Abort;
end;

procedure TTileStorageGE.SaveTile(AXY: TPoint; Azoom: byte;
  AStream: TStream);
begin
  Abort;
end;

procedure TTileStorageGE.SaveTNE(AXY: TPoint; Azoom: byte);
begin
  Abort;
end;

function TTileStorageGE.TileLoadDate(AXY: TPoint; Azoom: byte): TDateTime;
begin
  Result := 0;
end;

function TTileStorageGE.TileSize(AXY: TPoint; Azoom: byte): integer;
begin
  Result := 0;
end;

end.
