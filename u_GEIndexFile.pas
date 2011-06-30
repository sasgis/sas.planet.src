unit u_GEIndexFile;

interface

uses
  Types,
  SysUtils,
  i_JclNotify,
  i_MapVersionInfo,
  u_MapTypeCacheConfig;

type
  TIndexRec = packed record
    Magic  : LongWord;  // число-идентификатор =  D5 BF 93 75
    Ver    : Word;      // версия тайла
    TileID : Byte;      // тип тайла
    Res1   : Byte;      // ?
    Zoom   : Byte;      // уровень зума
    Res2   : Byte;      // ?
    Layer  : Word;      // номер слоя (только для слоя, иначе = 0)
    NameLo : LongWord;  // первая часть имени
    NameHi : LongWord;  // вторая часть имени
    ServID : Word;      // номер сервера из списка в dbCache.dat 
    Unk    : Word;      // ? наличие поля зависит от ОС (в Win - есть, в Linux - нет)
    Offset : LongWord;  // позиция тайла в кэше dbCache.dat
    Size   : LongWord;  // размер тайла
  end;

  TGEIndexFile = class
  private
    FSync: TMultiReadExclusiveWriteSynchronizer;
    FCacheConfig: TMapTypeCacheConfigGE;
    FIndexFileName: string;
    FIndexInfo: array of TIndexRec;
    FConfigChangeListener: IJclListener;
    FFileInited: Boolean;
    procedure GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
    procedure _UpdateIndexInfo;
    procedure OnConfigChange(Sender: TObject);
  protected
  public
    constructor Create(ACacheConfig: TMapTypeCacheConfigGE);
    destructor Destroy; override;
    function FindTileInfo(
      APoint: TPoint;
      AZoom: Byte;
      var AVersionInfo: IMapVersionInfo;
      out AOffset: Integer;
      out ASize: Integer
    ): Boolean;
  end;

implementation

uses
  Classes,
  u_NotifyEventListener,
  u_MapVersionInfo;

{ TGEIndexFile }

constructor TGEIndexFile.Create(ACacheConfig: TMapTypeCacheConfigGE);
begin
  FSync := TMultiReadExclusiveWriteSynchronizer.Create;
  FCacheConfig := ACacheConfig;
  FFileInited := False;
  FConfigChangeListener := TNotifyEventListener.Create(Self.OnConfigChange);
  FCacheConfig.ConfigChangeNotifier.Add(FConfigChangeListener);
end;

destructor TGEIndexFile.Destroy;
begin
  FCacheConfig.ConfigChangeNotifier.Remove(FConfigChangeListener);
  FConfigChangeListener := nil;
  FreeAndNil(FSync);
  FIndexInfo := nil;
  inherited;
end;

procedure TGEIndexFile.GEXYZtoHexTileName(APoint: TPoint; AZoom: Byte; out ANameHi, ANameLo: LongWord);
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
          VValue := 1;
        end else begin
          VValue := 2;
        end;
      end else begin
        if (APoint.y and VMask) > 0 then begin
          VValue := 0;
        end else begin
          VValue := 3;
        end;
      end;
      if i <= 16 then begin
        ANameLo := ANameLo or (LongWord(VValue) shl (32 - i * 2));
      end else begin
        ANameHi := ANameHi or (LongWord(VValue) shl (32 - (i - 16) * 2));
      end;
      VMask := VMask shr 1;
    end;
  end;
end;

function TGEIndexFile.FindTileInfo(
  APoint: TPoint;
  AZoom: Byte;
  var AVersionInfo: IMapVersionInfo;
  out AOffset, ASize: Integer
): Boolean;
var
  VNameLo: LongWord;
  VNameHi: LongWord;
  i: Integer;
  VProcessed: Boolean;
  VVersion: Word;
begin
  Result := False;
  AOffset := 0;
  ASize := 0;

  VVersion := 0;
  AVersionInfo := nil;
  VProcessed := False;
  while not VProcessed do begin
    if not FFileInited then begin
      FSync.BeginWrite;
      try
        if not FFileInited then begin
          _UpdateIndexInfo;
        end;
      finally
        FSync.EndWrite;
      end;
    end;
    FSync.BeginRead;
    try
      if FFileInited then begin
        if Length(FIndexInfo) > 0 then begin
          GEXYZtoHexTileName(APoint, AZoom, VNameHi, VNameLo);
          for i := Length(FIndexInfo) - 1 downto 0 do begin
            if FIndexInfo[i].Magic = $7593BFD5 then begin
              if FIndexInfo[i].TileID = 130 then begin
                if FIndexInfo[i].ServID = 0 then begin
                  if FIndexInfo[i].Zoom = AZoom then begin
                    if (FIndexInfo[i].NameLo = VNameLo) and (FIndexInfo[i].NameHi = VNameHi) then begin
                      AOffset := FIndexInfo[i].Offset;
                      ASize := FIndexInfo[i].Size;
                      VVersion := FIndexInfo[i].Ver;
                      AVersionInfo := TMapVersionInfo.Create(VVersion);
                      Result := True;
                      Break;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        VProcessed := True;
      end;
    finally
      FSync.EndRead;
    end;
  end;
end;

procedure TGEIndexFile.OnConfigChange(Sender: TObject);
begin
  FSync.BeginWrite;
  try
    FFileInited := False;
  finally
    FSync.EndWrite;
  end;
end;

procedure TGEIndexFile._UpdateIndexInfo;
var
  VFileName: string;
  VFileStream: TFileStream;
  VCount: Cardinal;
begin
  VFileName := FCacheConfig.GetIndexFileName;
  if VFileName <> FIndexFileName then begin
    FIndexInfo := nil;
    FIndexFileName := VFileName;
    if FileExists(VFileName) then begin
      VFileStream := TFileStream.Create(VFileName, fmOpenRead);
      try
        VCount := VFileStream.Size div SizeOf(FIndexInfo[0]);
        SetLength(FIndexInfo, VCount );
        VFileStream.ReadBuffer(FIndexInfo[0], VCount * SizeOf(FIndexInfo[0]));
      finally
        FreeAndNil(VFileStream);
      end;
    end;
  end;
  FFileInited := True;
end;

end.
