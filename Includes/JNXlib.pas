unit {$IFNDEF JNXLIB_DLL} JNXLib {$ELSE} uJNXLib {$ENDIF};
{ Unit:    JNXLib
  Author:  Alex Whiter
  Version: 1.11
  Date:    2014.03.10

  Description: This unit provides the necessary classes and routines to read
    and write JNX raster maps files.
    The JNX format description can be found here: http://whiter.brinkster.net/en/JNX.shtml


  Main classes:
    TJNXReader can be used to read an existing JNX map to get info and tiles from it.
      The tiles are returned in form of String, which can then be used to create
      an instance of TStringStream, or fed to some JPEG-processing library directly.

    TSimpleJNXWriter is meant for writing new JNX map by setting the following required fields:
      Levels - number of levels in the map,
      LevelScale[] - scales of the levels (indexed from 0 to Levels-1),
      TileCount[] - number of tiles on each level (indexed from 0 to Levels-1),
    and then adding the tiles by calling WriteTile method.

    TMultiVolumeJNXWriter can be used to create multi-volume JNX maps with
      more than 50000 tiles per level.
    It has the same set of fields and methods as TSimpleJNXWriter.

  Check the attached sample projects as examples of reading and writing the JNX files.

  Newer versions of this unit can be available from
    http://whiter.brinkster.net/en/JNX.shtml
}

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  SysUtils;

const
  MAX_LEVELS = 5;
  MAX_TILES  = 50000;
  MAX_FILE_SIZE = $100000000;

  JNX3_ZORDER = 30;
  META_BLOCK_VERSION = 9;
  JNX_EOF_SIGNATURE: AnsiString = 'BirdsEye';
  META_BLOCK_MINSIZE = 1024;

type
  TJNXPoint = packed record
    lat, lon: integer;
  end;

  TJNXRect = packed record
    case Boolean of
      False: (northern_lat, eastern_lon, southern_lat, western_lon: integer);
      True:  (NorthEast, SouthWest: TJNXPoint);
  end;

  TJNXHeader = packed record
    Version: integer;
    DeviceSN: integer;
    MapBounds: TJNXRect;
    Levels: integer;
    Expiration: integer;
    ProductID: integer;
    CRC: integer;
    SigVersion: integer;
    SigOffset: longword;
    ZOrder: integer;
  end;

  TJNXLevelInfo = packed record
    TileCount: integer;
    TilesOffset: longword;
    Scale: integer;
    Copyright: WideString;
  end;
  PJNXLevelInfo = ^TJNXLevelInfo;

  TJNXTileInfo = packed record
    TileBounds: TJNXRect;
    PicWidth, PicHeight: Word;
    PicSize: integer;
    PicOffset: longword;
  end;
  PJNXTileInfo = ^TJNXTileInfo;

  TJNXLevelMeta = record
    Name: WideString;
    Description: WideString;
    Copyright: WideString;
    Zoom: integer;
  end;

  TJNXMapMeta = record
    Version: integer;
    GUID: AnsiString;
    ProductName: WideString;
    MapName: WideString;
    LevelMetaCount: integer;
    LevelMetas: array of TJNXLevelMeta;
    RawTailData: AnsiString;
  end;
  PJNXMapMeta = ^TJNXMapMeta;


  TJNXMapFile = class
  protected
    FHeader: TJNXHeader;
    FLevels: array of TJNXLevelInfo;
    FTiles: array of array of TJNXTileInfo;
    FMeta: TJNXMapMeta;
    FFileOpened: Boolean;

    function OpenFile(const Path: String): Boolean; virtual;
    procedure CloseFile; virtual;

    function GetLevelInfo(l: integer): PJNXLevelInfo; virtual;
    function GetTileInfo(l, t: integer): PJNXTileInfo; virtual;
    function GetMetaInfo: PJNXMapMeta; virtual;

    procedure SetLevelCount(Value: integer); virtual;
    function GetLevelScale(l: integer): integer; virtual;
    procedure SetLevelScale(l: integer; const Value: integer); virtual;
    function GetLevelCopyright(l: integer): WideString; virtual;
    procedure SetLevelCopyright(l: integer; const Value: WideString); virtual;
    function GetLevelDescription(l: integer): WideString; virtual;
    procedure SetLevelDescription(l: integer; const Value: WideString); virtual;
    function GetLevelName(l: integer): WideString; virtual;
    procedure SetLevelName(l: integer; const Value: WideString); virtual;
    function GetLevelZoom(l: integer): integer; virtual;
    procedure SetLevelZoom(l: integer; const Value: integer); virtual;
    function GetTileCount(l: integer): integer; virtual;
    procedure SetTileCount(l: integer; const Value: integer); virtual;
  public
    constructor Create(const Path: String);
    destructor Destroy; override;

    // Low-level access to the map structures
    property Header: TJNXHeader read FHeader;
    property LevelInfo[l: integer]: PJNXLevelInfo read GetLevelInfo;
    property TileInfo[l, t: integer]: PJNXTileInfo read GetTileInfo;
    property MetaInfo: PJNXMapMeta read GetMetaInfo;

    // High-level access to the commonly used fields
    // Global map info
    property Version: integer read FHeader.Version write FHeader.Version;
    property ProductID: integer read FHeader.ProductId write FHeader.ProductId;
    property ZOrder: integer read FHeader.ZOrder write FHeader.ZOrder;
    property Levels: integer read FHeader.Levels write SetLevelCount;
    property ProductName: WideString read FMeta.ProductName write FMeta.ProductName;
    property MapName: WideString read FMeta.MapName write FMeta.MapName;

    // Level info
    property LevelScale[l: integer]: integer read GetLevelScale write SetLevelScale;
    property LevelName[l: integer]: WideString read GetLevelName write SetLevelName;
    property LevelDescription[l: integer]: WideString read GetLevelDescription write SetLevelDescription;
    property LevelCopyright[l: integer]: WideString read GetLevelCopyright write SetLevelCopyright;
    property LevelZoom[l: integer]: integer read GetLevelZoom write SetLevelZoom;
    property TileCount[l: integer]: integer read GetTileCount write SetTileCount;
  end;

  EJNXException = class(Exception);


  TJNXReader = class(TJNXMapFile)
  protected
    FFile: file;

    function GetJPEGStream(l, t: integer): AnsiString;

    function OpenFile(const Path: String): Boolean; override;
    procedure CloseFile; override;

    procedure ReadFileHeaders; virtual;

    procedure ReadMainHeader;
    procedure ReadLevelsInfo;
    procedure ReadMeta;
    procedure ReadTilesInfo;
  public
    property JPEGStreams[l, t: integer]: AnsiString read GetJPEGStream;
  end;

  TBaseJNXWriter = class(TJNXMapFile)
  protected
    function OpenFile(const Path: String): Boolean; override;

    procedure InitHeader;
  public
    procedure WriteTile(Level, PicWidth, PicHeight: integer; const Bounds: TJNXRect; const JpegString: AnsiString; AdjustMapBounds: boolean = True); virtual; abstract;
    function CanAcceptTile(LevelIndex, TileLength: integer): boolean; virtual;
  end;

  TSimpleJNXWriter = class(TBaseJNXWriter)
  protected
    FPath: String;
    FFile: file;
    FFileOpened: boolean;

    FFileHeadersAllocated: boolean;
    FFirstTileInfoOffset: longword;
    FNextPictureOffset: longword;
    FFirstTile: boolean;

    function OpenFile(const Path: String): Boolean; override;
    procedure CloseFile; override;

    procedure SetLevelCount(Value: integer); override;
    procedure SetTileCount(l: integer; const Value: integer); override;

    procedure WriteFileHeaders(AllocateOnly: boolean);
    procedure WriteEOFSignature;

    procedure WriteMainHeader;
    procedure WriteLevelsInfo;
    procedure WriteMeta;
    procedure WriteTilesInfo;

    procedure ValidateZoomValues;
  public
    procedure WriteTile(Level, PicWidth, PicHeight: integer; const Bounds: TJNXRect; const JpegString: AnsiString; AdjustMapBounds: boolean = True); override;
    function CanAcceptTile(LevelIndex, TileLength: integer): boolean; override;
  end;

  TMultiVolumeJNXWriter = class(TBaseJNXWriter)
  protected
    FFile: file;

    FBasePath: String;
    FTotalTileCount: array [0..MAX_LEVELS-1] of integer;
    FVolumes: array of TBaseJNXWriter;
    FVolumesAllocated: boolean;
    FCurrentVolumeIndex: array [0..MAX_LEVELS-1] of integer;
    FFirstVolumePath: String;

    function OpenFile(const Path: String): Boolean; override;

    function GetLevelInfo(l: integer): PJNXLevelInfo; override;
    function GetTileInfo(l, t: integer): PJNXTileInfo; override;
    function GetMetaInfo: PJNXMapMeta; override;

    function GetTileCount(l: integer): integer; override;
    procedure SetTileCount(l: integer; const Value: integer); override;

    procedure AllocateVolumes;
    procedure AllocateSingleVolume(Index: integer);
    function GetVolumeName(Index: integer): String;
  public
    destructor Destroy; override;

    procedure WriteTile(Level, PicWidth, PicHeight: integer; const Bounds: TJNXRect; const JpegString: AnsiString; AdjustMapBounds: boolean = True); override;
  end;


function ReadUTFString(var f: File): WideString;
procedure WriteUTFString(var f: File; const s: WideString);

function JNXCoordToWGS84(c: integer): double;
function WGS84CoordToJNX(c: double): integer;

function CreateGUID: AnsiString;

function JNXRect(northern_lat, eastern_lon, southern_lat, western_lon: integer): TJNXRect;

function DigitalGlobeZoomToScale(z: integer): integer; {$IFDEF JNXLIB_DLL} stdcall; {$ENDIF}
function MetersPerPixelToScale(d: double): integer;

implementation

{$IFDEF CONDITIONALEXPRESSIONS}
  {$DEFINE NO_UTF8_UNIT}
{$ENDIF}

uses
  Windows, Math {$IFNDEF NO_UTF8_UNIT}, UTF8 {$ENDIF};

function ReadUTFString(var f: File): WideString;
var
  UTF: AnsiString;
  c: AnsiChar;
begin
  UTF := '';
  repeat
    BlockRead(f, c, SizeOf(c));
    if c <> #0 then
      UTF := UTF + c;
  until c = #0;

  Result := UTF8Decode(UTF);
end;

procedure WriteUTFString(var f: File; const s: WideString);
var
  UTF: AnsiString;
begin
  UTF := UTF8Encode(s) + #0;
  BlockWrite(f, UTF[1], Length(UTF));
end;

function JNXCoordToWGS84(c: integer): double;
begin
  Result := c / $7fffffff * 180;
end;

function WGS84CoordToJNX(c: double): integer;
begin
  Result := round(c * $7fffffff / 180);
end;

procedure Seek64(var f: File; Pos: longword);
var
  HiDWord: DWORD;
begin
  HiDWord := 0;
  SetFilePointer(TFileRec(f).Handle, Pos, @HiDWord, FILE_BEGIN);
end;

function CreateGUID: AnsiString;
  function MakeRandSeq(Len: integer): AnsiString;
  var
    i: integer;
  begin
    Result := '';
    for i:=1 to Len do
      Result := Result + IntToHex(random(256), 2);
  end;
begin
  Result :=
    MakeRandSeq(4) + '-' +
    MakeRandSeq(2) + '-' +
    MakeRandSeq(2) + '-' +
    MakeRandSeq(2) + '-' +
    MakeRandSeq(6);
end;

function GetMaxBounds(const b1, b2: TJNXRect): TJNXRect;
begin
  with Result do
  begin
    northern_lat := Max(b1.northern_lat, b2.northern_lat);
    eastern_lon  := Max(b1.eastern_lon,  b2.eastern_lon );
    southern_lat := Min(b1.southern_lat, b2.southern_lat);
    western_lon  := Min(b1.western_lon,  b2.western_lon );
  end;
end;

function JNXRect(northern_lat, eastern_lon, southern_lat, western_lon: integer): TJNXRect;
begin
  Result.northern_lat := Max(northern_lat, southern_lat);
  Result.eastern_lon  := Max(eastern_lon,  western_lon );
  Result.southern_lat := Min(northern_lat, southern_lat);
  Result.western_lon  := Min(eastern_lon,  western_lon );
end;

function DigitalGlobeZoomToScale(z: integer): integer;
const
  ZoomToScale: array [0..23] of integer = (
    2446184, 2446184, 2446184, 2446184, 2446184, 2446184, 2446184, 1223072,
     611526,  305758,  152877,   76437,   38218,   19109,    9554,    4777,
       2388,    1194,     597,     298,     149,      75,      37,      19
  );
begin
  Result := ZoomToScale[z];
end;

function MetersPerPixelToScale(d: double): integer;
var
  z: integer;
begin
  z := 11 - round(ln(d * 1000 / 76437) / ln(2));
  Result := DigitalGlobeZoomToScale(z);
end;

{ TJNXMapFile }


constructor TJNXMapFile.Create(const Path: String);
begin
  inherited Create;

  FFileOpened := OpenFile(Path);
end;

destructor TJNXMapFile.Destroy;
begin
  if FFileOpened then
    CloseFile;

  inherited;
end;

function TJNXMapFile.GetLevelInfo(l: integer): PJNXLevelInfo;
begin
  Result := @FLevels[l];
end;

function TJNXMapFile.GetTileInfo(l, t: integer): PJNXTileInfo;
begin
  Result := @FTiles[l, t];
end;

function TJNXMapFile.GetLevelCopyright(l: integer): WideString;
begin
  Result := FMeta.LevelMetas[l].Copyright;
end;

function TJNXMapFile.GetLevelDescription(l: integer): WideString;
begin
  Result := FMeta.LevelMetas[l].Description;
end;

function TJNXMapFile.GetLevelName(l: integer): WideString;
begin
  Result := FMeta.LevelMetas[l].Name;
end;

function TJNXMapFile.GetLevelScale(l: integer): integer;
begin
  Result := FLevels[l].Scale;
end;

function TJNXMapFile.GetLevelZoom(l: integer): integer;
begin
  Result := FMeta.LevelMetas[l].Zoom;
end;

function TJNXMapFile.GetTileCount(l: integer): integer;
begin
  Result := Length(FTiles[l]);
end;

procedure TJNXMapFile.SetLevelCopyright(l: integer;
  const Value: WideString);
begin
  FMeta.LevelMetas[l].Copyright := Value;
  FLevels[l].Copyright := Value;
end;

procedure TJNXMapFile.SetLevelCount(Value: integer);
begin
  if Value < 0 then
    raise EJNXException.Create('Level count cannot be less than zero')
  else
  if Value > MAX_LEVELS then
    raise EJNXException.Create('Level count cannot exceed ' + IntToStr(MAX_LEVELS));

  FHeader.Levels := Value;
  SetLength(FLevels, Value);
  SetLength(FTiles, Value);

  FMeta.LevelMetaCount := Value;
  SetLength(FMeta.LevelMetas, Value);
end;

procedure TJNXMapFile.SetLevelDescription(l: integer;
  const Value: WideString);
begin
  FMeta.LevelMetas[l].Description := Value;
end;

procedure TJNXMapFile.SetLevelName(l: integer; const Value: WideString);
begin
  FMeta.LevelMetas[l].Name := Value;
end;

procedure TJNXMapFile.SetLevelScale(l: integer; const Value: integer);
begin
  FLevels[l].Scale := Value;
end;

procedure TJNXMapFile.SetLevelZoom(l: integer; const Value: integer);
begin
  if Value <> 0 then
    FMeta.LevelMetas[l].Zoom := Value
  else
    FMeta.LevelMetas[l].Zoom := l + 1;
end;

procedure TJNXMapFile.SetTileCount(l: integer; const Value: integer);
begin
  if Value > MAX_TILES then
    Raise EJNXException.Create('Too many tiles on level ' + IntToStr(l));

  SetLength(FTiles[l], Value);
end;

function TJNXMapFile.GetMetaInfo: PJNXMapMeta;
begin
  Result := @FMeta;
end;

procedure TJNXMapFile.CloseFile;
begin
  // do nothing
end;

function TJNXMapFile.OpenFile(const Path: String): Boolean;
begin
  // do nothing
  Result := False;
end;

{ TJNXReader }

function TJNXReader.GetJPEGStream(l, t: integer): AnsiString;
begin
  Seek64(FFile, TileInfo[l, t].PicOffset);
  SetLength(Result, TileInfo[l, t].PicSize + 2);

  Result[1] := #$FF;
  Result[2] := #$D8;
  BlockRead(FFile, Result[3], TileInfo[l, t].PicSize);
end;

function TJNXReader.OpenFile(const Path: String): Boolean;
begin
  AssignFile(FFile, Path);
  Reset(FFile, 1);

  ReadFileHeaders;

  Result := True;
end;

procedure TJNXReader.ReadFileHeaders;
begin
  ReadMainHeader;
  ReadLevelsInfo;
  ReadMeta;
  ReadTilesInfo;
end;

procedure TJNXReader.ReadMainHeader;
var
  Size: integer;
begin
  BlockRead(FFile, FHeader.Version, sizeof(FHeader.Version));
  case FHeader.Version of
    3:
      begin
        Size := SizeOf(FHeader) - SizeOf(FHeader.ZOrder);
        FHeader.ZOrder := JNX3_ZORDER;
      end;
    4: Size := SizeOf(FHeader);
  else
    Raise EJNXException.Create('Unsupprted JNX version: ' + IntToStr(FHeader.Version));
  end;
  dec(Size, SizeOf(FHeader.Version));
  BlockRead(FFile, FHeader.DeviceSN, Size);
end;

procedure TJNXReader.ReadLevelsInfo;
var
  l: integer;
  Dummy: integer;
begin
  SetLength(FLevels, Header.Levels);
  for l:=0 to Header.Levels - 1 do
  begin
    BlockRead(FFile, FLevels[l], SizeOf(FLevels[l]) - SizeOf(FLevels[l].Copyright));
    if Header.Version = 4 then
    begin
      BlockRead(FFile, Dummy, SizeOf(Dummy));
      FLevels[l].Copyright := ReadUTFString(FFile);
    end
    else
      FLevels[l].Copyright := '';
  end;
end;

procedure TJNXReader.ReadMeta;
var
  Dummy1: array [1..3] of byte;
  i: integer;
  Dummy2: String;
  Dummy3: Word;
begin
  with FMeta do
  begin
    {2012.09.07 Whiter: In some of the maps, the signature can reside right after the level info block.
      In this case, we should skip it.}
    if Header.SigOffset = LongWord(FilePos(FFile)) then
      Seek64(FFile, Header.SigOffset + $314);

    BlockRead(FFile, Version, SizeOf(Version));
    GUID := AnsiString(ReadUTFString(FFile));
    ProductName := ReadUTFString(FFile);
    if Version = 3 then
    begin
      Dummy2 := ReadUTFString(FFile);
      BlockRead(FFile, Dummy3, SizeOf(Dummy3));      
    end
    else
      BlockRead(FFile, Dummy1, SizeOf(Dummy1));
    MapName := ReadUTFString(FFile);
    BlockRead(FFile, LevelMetaCount, SizeOf(LevelMetaCount));

    SetLength(LevelMetas, LevelMetaCount);
    for i:=0 to LevelMetaCount - 1 do
    begin
      with LevelMetas[i] do
      begin
        Name := ReadUTFString(FFile);
        Description := ReadUTFString(FFile);
        Copyright := ReadUTFString(FFile);
        BlockRead(FFile, Zoom, SizeOf(Zoom));
      end;

      if (Header.Version = 3) and (i < Header.Levels) then
        FLevels[i].Copyright := LevelMetas[i].Copyright;
    end;

    SetLength(RawTailData, LevelInfo[0].TilesOffset - longword(FilePos(FFile)));
    BlockRead(FFile, RawTailData[1], Length(RawTailData));
  end;
end;

procedure TJNXReader.ReadTilesInfo;
var
  l: integer;
begin
  SetLength(FTiles, Header.Levels);
  for l:=0 to Header.Levels - 1 do
  begin
    SetLength(FTiles[l], LevelInfo[l].TileCount);
    BlockRead(FFile, FTiles[l, 0], SizeOf(FTiles[l, 0]) * LevelInfo[l].TileCount);
  end;
end;

procedure TJNXReader.CloseFile;
begin
  System.CloseFile(FFile);
end;

{ TBaseJNXWriter }

function TBaseJNXWriter.CanAcceptTile(LevelIndex,
  TileLength: integer): boolean;
begin
  Result := True;
end;

procedure TBaseJNXWriter.InitHeader;
begin
  FillChar(FHeader, SizeOf(FHeader), 0);

  with FHeader do
  begin
    Version := 3;
    ZOrder := JNX3_ZORDER;
  end;

  FMeta.Version := META_BLOCK_VERSION;
  FMeta.GUID := CreateGUID;
  SetLength(FMeta.RawTailData, META_BLOCK_MINSIZE);
  FillChar(FMeta.RawTailData[1], META_BLOCK_MINSIZE, 0);
end;

function TBaseJNXWriter.OpenFile(const Path: String): Boolean;
begin
  InitHeader;

  Result := True;
end;


{ TSimpleJNXWriter }

procedure TSimpleJNXWriter.CloseFile;
begin
  if FFileOpened then
  begin
    WriteFileHeaders(False);
    WriteEOFSignature;

    System.CloseFile(FFile);
  end;
end;

function TSimpleJNXWriter.OpenFile(const Path: String): Boolean;
begin
  Result := inherited OpenFile(Path);

  FPath := Path;
end;

procedure TSimpleJNXWriter.SetLevelCount(Value: integer);
var
  i: integer;
begin
  if FFileHeadersAllocated then
    raise EJNXException.Create('Cannot change the count of levels after WriteTile');

  inherited;

  for i:=0 to Levels - 1 do
    FMeta.LevelMetas[i].Zoom := i + 1;
end;

procedure TSimpleJNXWriter.SetTileCount(l: integer; const Value: integer);
begin
  if FFileHeadersAllocated then
    raise EJNXException.Create('Cannot change the count of tiles after WriteTile');

  inherited;
end;

procedure TSimpleJNXWriter.WriteEOFSignature;
begin
  Seek64(FFile, FNextPictureOffset);
  BlockWrite(FFile, JNX_EOF_SIGNATURE[1], Length(JNX_EOF_SIGNATURE));
end;

procedure TSimpleJNXWriter.WriteFileHeaders(AllocateOnly: boolean);
begin
  if AllocateOnly and FFileHeadersAllocated then
    exit;

  if not FFileOpened then
  begin
    AssignFile(FFile, FPath);
    Rewrite(FFile, 1);

    FFirstTile := True;
    FFileOpened := True;
  end;

  Seek64(FFile, 0);

  WriteMainHeader;
  WriteLevelsInfo;
  WriteMeta;
  WriteTilesInfo;

  if not FFileHeadersAllocated then
    FNextPictureOffset := FilePos(FFile);

  FFileHeadersAllocated := True;
end;

procedure TSimpleJNXWriter.WriteMainHeader;
var
  Size: integer;
begin
  case FHeader.Version of
    3: Size := SizeOf(FHeader) - SizeOf(FHeader.ZOrder);
    4: Size := SizeOf(FHeader);
  else
    Raise EJNXException.Create('Unsupprted JNX version: ' + IntToStr(FHeader.Version));
  end;

  BlockWrite(FFile, FHeader, Size);
end;

procedure TSimpleJNXWriter.WriteLevelsInfo;
var
  l: integer;
  Dummy: integer;
  Offset: integer;
  Count: integer;
begin
  Offset := FFirstTileInfoOffset;

  for l:=0 to FHeader.Levels - 1 do
  begin
    FLevels[l].TilesOffset := Offset;

    BlockWrite(FFile, FLevels[l], SizeOf(FLevels[l]) - SizeOf(FLevels[l].Copyright));
    if FHeader.Version = 4 then
    begin
      Dummy := 2;
      BlockWrite(FFile, Dummy, SizeOf(Dummy));
      WriteUTFString(FFile, FLevels[l].Copyright);
    end;

    if FFileHeadersAllocated then
      Count := FLevels[l].TileCount
    else
      Count := TileCount[l];

    inc(Offset, Count * SizeOf(TJNXTileInfo));      
  end;
end;

procedure TSimpleJNXWriter.WriteMeta;
var
  Dummy: array [1..3] of byte;
  i: integer;
begin
  with FMeta do
  begin
    BlockWrite(FFile, Version, SizeOf(Version));
    WriteUTFString(FFile, GUID);
    WriteUTFString(FFile, ProductName);

    {2014.03.10 Whiter: Dummy[2] is not so dummy anymore. It seems to hold the ProductID,
      and unless it is set to a correct value, BaseCamp activates the map as ProductID
      was set to 0.}
    Dummy[1] := 0;
    Dummy[2] := FHeader.ProductID;
    Dummy[3] := 0;
    BlockWrite(FFile, Dummy, SizeOf(Dummy));

    WriteUTFString(FFile, MapName);
    BlockWrite(FFile, LevelMetaCount, SizeOf(LevelMetaCount));

    {2014.03.10 Whiter: The level zooms should be unique for the map
      to be accepted by BaseCamp.}
    ValidateZoomValues;

    for i:=0 to LevelMetaCount - 1 do
    begin
      with LevelMetas[i] do
      begin
        WriteUTFString(FFile, Name);
        WriteUTFString(FFile, Description);
        WriteUTFString(FFile, Copyright);
        BlockWrite(FFile, Zoom, SizeOf(Zoom));
      end;
    end;

    BlockWrite(FFile, RawTailData[1], Length(RawTailData));
  end;

  if not FFileHeadersAllocated then
    FFirstTileInfoOffset := FilePos(FFile);
end;

procedure TSimpleJNXWriter.WriteTilesInfo;
var
  l: integer;
  Count: integer;
begin
  for l:=0 to FHeader.Levels - 1 do
  begin
    if FFileHeadersAllocated then
      Count := FLevels[l].TileCount
    else
      Count := TileCount[l];

    BlockWrite(FFile, FTiles[l, 0], SizeOf(FTiles[l, 0]) * Count);
  end;
end;

procedure TSimpleJNXWriter.WriteTile(Level, PicWidth, PicHeight: integer;
  const Bounds: TJNXRect; const JpegString: AnsiString; AdjustMapBounds: boolean = True);
var
  Len: integer;
  TileIndex: integer;
begin
  WriteFileHeaders(True);

  {2013.06.25 Whiter: Silently skipping empty tiles.}
  if Length(JpegString) < 3 then
    exit;

  if not CanAcceptTile(Level, Length(JpegString)) then
    Raise EJNXException.Create('JNX file is too large to accept more tiles.');

  TileIndex := FLevels[Level].TileCount;

  FTiles[Level, TileIndex].TileBounds := Bounds;
  if AdjustMapBounds then
  begin
    if FFirstTile then
    begin
      FHeader.MapBounds := Bounds;
      FFirstTile := False;
    end
    else
      FHeader.MapBounds := GetMaxBounds(FHeader.MapBounds, Bounds);
  end;
  FTiles[Level, TileIndex].PicWidth := PicWidth;
  FTiles[Level, TileIndex].PicHeight := PicHeight;
  Len := Length(JpegString) - 2;
  FTiles[Level, TileIndex].PicSize := Len;
  FTiles[Level, TileIndex].PicOffset := FNextPictureOffset;
  Seek64(FFile, FNextPictureOffset);

  BlockWrite(FFile, JpegString[3], Len);
  inc(FNextPictureOffset, Len);
  inc(FLevels[Level].TileCount);
end;

function TSimpleJNXWriter.CanAcceptTile(LevelIndex, TileLength: integer): boolean;
begin
  Result :=
    (LevelIndex < Levels) and (LevelInfo[LevelIndex].TileCount < TileCount[LevelIndex]) and
    (int64(FNextPictureOffset) + int64(TileLength - 2 + Length(JNX_EOF_SIGNATURE)) < MAX_FILE_SIZE);
end;

procedure TSimpleJNXWriter.ValidateZoomValues;
var
  i, j: integer;
  NeedToFix: boolean;
begin
  NeedToFix := False;

  for i:=0 to FMeta.LevelMetaCount - 2 do
    for j:=i + 1 to FMeta.LevelMetaCount - 1 do
      if FMeta.LevelMetas[i].Zoom = FMeta.LevelMetas[j].Zoom then
      begin
        NeedToFix := True;
        Break;
      end;

  if NeedToFix then
    for i:=0 to FMeta.LevelMetaCount - 1 do
      FMeta.LevelMetas[i].Zoom := i + 1; 
end;

{ TMultiVolumeJNXWriter }

procedure TMultiVolumeJNXWriter.AllocateSingleVolume(Index: integer);
var
  VolFileName: String;
  l: integer;
begin
  if Length(FVolumes) > Index then
    exit;

  SetLength(FVolumes, Index + 1);

  if Index > 0 then
    VolFileName := GetVolumeName(Index)
  else
    VolFileName := FFirstVolumePath;

  FVolumes[Index] := TSimpleJNXWriter.Create(VolFileName);
  FVolumes[Index].Version     := Version;
  FVolumes[Index].ProductID   := ProductID;
  FVolumes[Index].ZOrder      := ZOrder;
  FVolumes[Index].ProductName := ProductName;
  FVolumes[Index].MapName     := MapName;
  FVolumes[Index].Levels      := Levels;

  for l:=0 to Levels - 1 do
  begin
    FVolumes[Index].LevelScale[l]       := LevelScale[l];
    FVolumes[Index].LevelName[l]        := LevelName[l];
    FVolumes[Index].LevelDescription[l] := LevelDescription[l];
    FVolumes[Index].LevelCopyright[l]   := LevelCopyright[l];
    FVolumes[Index].LevelZoom[l]        := LevelZoom[l];
    FVolumes[Index].TileCount[l]        := Min(TileCount[l], MAX_TILES);
  end;
end;

procedure TMultiVolumeJNXWriter.AllocateVolumes;
var
  l: integer;
begin
  if FVolumesAllocated then
    exit;

  FVolumesAllocated := True;

  AllocateSingleVolume(0);
  for l:=0 to Levels - 1 do
    FCurrentVolumeIndex[l] := 0;
end;

destructor TMultiVolumeJNXWriter.Destroy;
var
  i: integer;
  NewName: String;
begin
  for i:=0 to High(FVolumes) do
    FreeAndNil(FVolumes[i]);

  if Length(FVolumes) > 1 then
  begin
    NewName := GetVolumeName(0);
    if FileExists(NewName) then
      SysUtils.DeleteFile(NewName);
    RenameFile(FFirstVolumePath, NewName);
  end;

  inherited;
end;

function TMultiVolumeJNXWriter.GetLevelInfo(l: integer): PJNXLevelInfo;
begin
  Result := Nil;
end;

function TMultiVolumeJNXWriter.GetMetaInfo: PJNXMapMeta;
begin
  Result := Nil;
end;

function TMultiVolumeJNXWriter.GetTileCount(l: integer): integer;
begin
  Result := FTotalTileCount[l];
end;

function TMultiVolumeJNXWriter.GetTileInfo(l, t: integer): PJNXTileInfo;
begin
  Result := Nil;
end;

function TMultiVolumeJNXWriter.GetVolumeName(Index: integer): String;
begin
  Result := FBasePath + '_Part' + Format('%.2d', [Index + 1]) + '.jnx';
end;

function TMultiVolumeJNXWriter.OpenFile(const Path: String): Boolean;
begin
  Result := inherited OpenFile(Path);

  FBasePath := ChangeFileExt(Path, '');
  FFirstVolumePath := FBasePath + '.jnx';
end;

procedure TMultiVolumeJNXWriter.SetTileCount(l: integer;
  const Value: integer);
begin
  FTotalTileCount[l] := Value;
end;

procedure TMultiVolumeJNXWriter.WriteTile(Level, PicWidth,
  PicHeight: integer; const Bounds: TJNXRect; const JpegString: AnsiString;
  AdjustMapBounds: boolean);
var
  VolumeIndex: integer;
  NextVolumeFound: boolean;
begin
  AllocateVolumes;

  repeat
    VolumeIndex := FCurrentVolumeIndex[Level];
    NextVolumeFound := FVolumes[VolumeIndex].CanAcceptTile(Level, Length(JpegString));

    if not NextVolumeFound then
    begin
      inc(VolumeIndex);
      inc(FCurrentVolumeIndex[Level]);
      if VolumeIndex = Length(FVolumes) then
        AllocateSingleVolume(VolumeIndex);
    end;
  until NextVolumeFound;

  FVolumes[VolumeIndex].WriteTile(Level, PicWidth, PicHeight, Bounds, JpegString, AdjustMapBounds);
end;

initialization
  Randomize;
end.

