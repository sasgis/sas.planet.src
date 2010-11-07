unit u_GEIndexFile;

interface

uses
  Types,
  SysUtils,
  u_MapTypeCacheConfig;

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

  TGEIndexFile = class
  private
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FCacheConfig: TMapTypeCacheConfigGE;
    indexfilename: string;
    FIndexInfo: array of TIndexRec;
    procedure GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
    function GEFindTileAdr(indexpath: string; APoint: TPoint; AZoom: Byte; var size:integer):integer;
    procedure UpdateIndexInfo;
  protected
  public
    constructor Create(ACacheConfig: TMapTypeCacheConfigGE);
    destructor Destroy; override;
    function FindTileInfo(
      APoint: TPoint;
      AZoom: Byte;
      var AVersion: Word;
      out AOffset: Integer;
      out ASize: Integer
    ): Boolean;
  end;

implementation

{ TGEIndexFile }

constructor TGEIndexFile.Create(ACacheConfig: TMapTypeCacheConfigGE);
begin
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FCacheConfig := ACacheConfig;

end;

destructor TGEIndexFile.Destroy;
begin
  FreeAndNil(FSync);
  FIndexInfo := nil;
  inherited;
end;

function TGEIndexFile.FindTileInfo(APoint: TPoint; AZoom: Byte;
  var AVersion: Word; out AOffset, ASize: Integer): Boolean;
begin

end;

function TGEIndexFile.GEFindTileAdr(indexpath: string; APoint: TPoint;
  AZoom: Byte; var size: integer): integer;
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

procedure TGEIndexFile.GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte;
  out ANameHi, ANameLo: LongWord);
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

procedure TGEIndexFile.UpdateIndexInfo;
begin

end;

end.
