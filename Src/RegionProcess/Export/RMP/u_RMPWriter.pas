unit u_RMPWriter;

interface

uses
  Math,
  librmp;

type
  TRMPFileWriter = class(TObject)
  private
    FRMPFile: TRMPFile;
    FTLMFile: TTLMFile;
    FRMPLayerWriter: TRMPLayerWriter;
    FFileName: string;
    FFilesCount: Integer;
    FLayersCount: Integer;
    FLayerNameBase: AnsiString;
    FImgName, FProduct, FProvider, FComments: AnsiString;
    FPreallocLayersCount: Integer;
    FLeftLimit: Double;
    FTopLimit: Double;
    FRightLimit: Double;
    FBottomLimit: Double;
    function BuildRMPFileName: string;
    procedure StartNewLayer;
    procedure FinishLayer;
    procedure StartNewFile;
    procedure FinishFile;
  public
     // call this on zoom change or new row
    procedure ForceNewLayer(
      const ALeftLimit: Double = NaN;
      const ATopLimit: Double = NaN;
      const ARightLimit: Double = NaN;
      const ABottomLimit: Double = NaN
    );
    procedure AddTile(
      const AX, AY: Integer;
      const ALeft, ATop, ARight, ABottom: Double;
      const AData: PByte;
      const ASize: Integer
    );
    procedure AddEmptyTile(
      const AX, AY: Integer;
      const ALeft, ATop, ARight, ABottom: Double
    );
    constructor Create(
      const AFileName: string;
      const AImgName, AProduct, AProvider, AComments: AnsiString;
      const APreallocLayersCount: Integer
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

const
  cMaxRmpFileSize = $7D000000; // 2000 Mb

{$REGION 'EMPTY_TILE_CONTENT'}

const
  cBlackJpeg: array[0..1649] of Byte = (
	$FF, $D8, $FF, $E0, $00, $10, $4A, $46, $49, $46, $00, $01, $01, $01, $01, $2C,
	$01, $2C, $00, $00, $FF, $DB, $00, $43, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $DB, $00, $43, $01, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF,
	$FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $C0,
	$00, $11, $08, $01, $00, $01, $00, $03, $01, $22, $00, $02, $11, $01, $03, $11,
	$01, $FF, $C4, $00, $1F, $00, $00, $01, $05, $01, $01, $01, $01, $01, $01, $00,
	$00, $00, $00, $00, $00, $00, $00, $01, $02, $03, $04, $05, $06, $07, $08, $09,
	$0A, $0B, $FF, $C4, $00, $B5, $10, $00, $02, $01, $03, $03, $02, $04, $03, $05,
	$05, $04, $04, $00, $00, $01, $7D, $01, $02, $03, $00, $04, $11, $05, $12, $21,
	$31, $41, $06, $13, $51, $61, $07, $22, $71, $14, $32, $81, $91, $A1, $08, $23,
	$42, $B1, $C1, $15, $52, $D1, $F0, $24, $33, $62, $72, $82, $09, $0A, $16, $17,
	$18, $19, $1A, $25, $26, $27, $28, $29, $2A, $34, $35, $36, $37, $38, $39, $3A,
	$43, $44, $45, $46, $47, $48, $49, $4A, $53, $54, $55, $56, $57, $58, $59, $5A,
	$63, $64, $65, $66, $67, $68, $69, $6A, $73, $74, $75, $76, $77, $78, $79, $7A,
	$83, $84, $85, $86, $87, $88, $89, $8A, $92, $93, $94, $95, $96, $97, $98, $99,
	$9A, $A2, $A3, $A4, $A5, $A6, $A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6, $B7,
	$B8, $B9, $BA, $C2, $C3, $C4, $C5, $C6, $C7, $C8, $C9, $CA, $D2, $D3, $D4, $D5,
	$D6, $D7, $D8, $D9, $DA, $E1, $E2, $E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA, $F1,
	$F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9, $FA, $FF, $C4, $00, $1F, $01, $00, $03,
	$01, $01, $01, $01, $01, $01, $01, $01, $01, $00, $00, $00, $00, $00, $00, $01,
	$02, $03, $04, $05, $06, $07, $08, $09, $0A, $0B, $FF, $C4, $00, $B5, $11, $00,
	$02, $01, $02, $04, $04, $03, $04, $07, $05, $04, $04, $00, $01, $02, $77, $00,
	$01, $02, $03, $11, $04, $05, $21, $31, $06, $12, $41, $51, $07, $61, $71, $13,
	$22, $32, $81, $08, $14, $42, $91, $A1, $B1, $C1, $09, $23, $33, $52, $F0, $15,
	$62, $72, $D1, $0A, $16, $24, $34, $E1, $25, $F1, $17, $18, $19, $1A, $26, $27,
	$28, $29, $2A, $35, $36, $37, $38, $39, $3A, $43, $44, $45, $46, $47, $48, $49,
	$4A, $53, $54, $55, $56, $57, $58, $59, $5A, $63, $64, $65, $66, $67, $68, $69,
	$6A, $73, $74, $75, $76, $77, $78, $79, $7A, $82, $83, $84, $85, $86, $87, $88,
	$89, $8A, $92, $93, $94, $95, $96, $97, $98, $99, $9A, $A2, $A3, $A4, $A5, $A6,
	$A7, $A8, $A9, $AA, $B2, $B3, $B4, $B5, $B6, $B7, $B8, $B9, $BA, $C2, $C3, $C4,
	$C5, $C6, $C7, $C8, $C9, $CA, $D2, $D3, $D4, $D5, $D6, $D7, $D8, $D9, $DA, $E2,
	$E3, $E4, $E5, $E6, $E7, $E8, $E9, $EA, $F2, $F3, $F4, $F5, $F6, $F7, $F8, $F9,
	$FA, $FF, $DA, $00, $0C, $03, $01, $00, $02, $11, $03, $11, $00, $3F, $00, $8E,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02,
	$8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $02, $8A, $28, $A0, $0F,
	$FF, $D9
);

{$ENDREGION}

{ TRMPFileWriter }

constructor TRMPFileWriter.Create(
  const AFileName: string;
  const AImgName, AProduct, AProvider, AComments: AnsiString;
  const APreallocLayersCount: Integer
);
begin
  FFileName := AFileName;
  FImgName := AImgName;
  FProduct := AProduct;
  FProvider := AProvider;
  FComments := AComments;
  FPreallocLayersCount := APreallocLayersCount;

  FLeftLimit := NaN;
  FTopLimit := NaN;
  FRightLimit := NaN;
  FBottomLimit := NaN;

  FTLMFile := nil;
  FRMPLayerWriter := nil;
  FLayersCount := 0;
  FLayerNameBase := 'topo';

  FRMPFile := nil;
  FFilesCount := 0;

  StartNewFile;
end;

destructor TRMPFileWriter.Destroy;
begin
  FinishFile;
  inherited;
end;

function TRMPFileWriter.BuildRMPFileName: string;
begin
  if FFilesCount = 1 then begin
    Result := FFileName;
  end else begin
    Result := ChangeFileExt(FFileName, Format('_%d.rmp', [FFilesCount-1]))
  end;
end;

procedure TRMPFileWriter.StartNewFile;
var
  VFileName: string;
  I: TRMPICSFileType;
  VICSFile: TRMPICSFile;
  VDescriptionFile: TRMPDescriptionFile;
begin
  FinishFile;
  Inc(FFilesCount);

  VFileName := BuildRMPFileName;
  FRMPFile := TRMPFile.Create(VFileName, FPreallocLayersCount*2 + 4);

  for I in [icsBmp4bit, icsChunk] do begin
    VICSFile := TRMPICSFile.Create(I);
    try
      VICSFile.WriteEntry(FRMPFile.EntryWriter);
    finally
      VICSFile.Free;
    end;
  end;

  VDescriptionFile := TRMPDescriptionFile.Create(FImgName, FProduct, FProvider, FComments);
  try
    VDescriptionFile.WriteEntry(FRMPFile.EntryWriter);
  finally
    VDescriptionFile.Free;
  end;

  FLayersCount := 0;
  StartNewLayer;
end;

procedure TRMPFileWriter.StartNewLayer;
begin
  FinishLayer;
  Inc(FLayersCount);
  FRMPLayerWriter := TRMPLayerWriter.Create(FLayerNameBase, FLayersCount, FRMPFile.EntryWriter);
  FTLMFile := TTLMFile.Create(FLayerNameBase, FLayersCount);
  FTLMFile.SetBoundingLimit(FLeftLimit, FTopLimit, FRightLimit, FBottomLimit);
end;

procedure TRMPFileWriter.ForceNewLayer(
  const ALeftLimit: Double;
  const ATopLimit: Double;
  const ARightLimit: Double;
  const ABottomLimit: Double
);
begin
  FLeftLimit := ALeftLimit;
  FTopLimit := ATopLimit;
  FRightLimit := ARightLimit;
  FBottomLimit := ABottomLimit;

  if FTLMFile.TilesCount > 0 then begin
    StartNewLayer;
  end else begin
    FTLMFile.SetBoundingLimit(FLeftLimit, FTopLimit, FRightLimit, FBottomLimit);
  end;
end;

procedure TRMPFileWriter.AddEmptyTile(
  const AX, AY: Integer;
  const ALeft, ATop, ARight, ABottom: Double
);
begin
  AddTile(AX, AY, ALeft, ATop, ARight, ABottom, @cBlackJpeg[0], Length(cBlackJpeg));
end;

procedure TRMPFileWriter.AddTile(
  const AX, AY: Integer;
  const ALeft, ATop, ARight, ABottom: Double;
  const AData: PByte;
  const ASize: Integer
);
var
  VTileOffset: Integer;
begin
  if FRMPFile.Size >= cMaxRmpFileSize then begin
    StartNewFile;
  end else if FTLMFile.TilesCount >= FTLMFile.MaxTilesCount then begin
    StartNewLayer;
  end;
  FRMPLayerWriter.WriteTileData(AData^, ASize, VTileOffset);
  FTLMFile.AddTile(AX, AY, ALeft, ATop, ARight, ABottom, VTileOffset);
end;

procedure TRMPFileWriter.FinishLayer;
begin
  if Assigned(FRMPFile) then begin
    if Assigned(FRMPLayerWriter) then begin
      FreeAndNil(FRMPLayerWriter);
      if Assigned(FTLMFile) then begin
        try
          if FTLMFile.TilesCount > 0 then begin
            FTLMFile.WriteEntry(FRMPFile.EntryWriter);
          end else begin
            Dec(FLayersCount);
          end;
        finally
          FreeAndNil(FTLMFile);
        end;
      end;
    end else begin
      Assert(FTLMFile = nil);
    end;
  end else begin
    Assert(FRMPLayerWriter = nil);
    Assert(FTLMFile = nil);
  end;
end;

procedure TRMPFileWriter.FinishFile;
var
  VIni: TRMPIniFile;
begin
  if Assigned(FRMPFile) then begin
    try
      FinishLayer;

      VIni := TRMPIniFile.Create(FLayerNameBase, FLayersCount);
      try
        VIni.WriteEntry(FRMPFile.EntryWriter);
      finally
        VIni.Free;
      end;

      FRMPFile.Finish;
    finally
      FreeAndNil(FRMPFile);
    end;
  end;
end;

end.
