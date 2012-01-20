unit JNXLib;
{ Unit:    JNXLib
  Author:  Alex Whiter
  Version: 1.2
  Date:    2012.01.20

  Description: This unit provides the necessary classes and routines to read
    and write JNX raster maps files.
    The JNX format description can be found here: http://whiter.brinkster.net/en/JNX.shtml


  Main classes:
    TJNXReader can be used to read an existing JNX map to get info and tiles from it.
      The tiles are returned in form of String, which can then be used to create
      an instance of TStringStream, or fed to some JPEG-processing library directly.

    TJNXWriter is meant for writing new JNX map by setting the following required fields:
      Levels - number of levels in the map,
      LevelScale[] - scales of the levels (indexed from 0 to Levels-1),
      TileCount[] - number of tiles on each level (indexed from 0 to Levels-1),
    and then adding the tiles by calling WriteTile method.

  Check the attached sample projects as examples of reading and writing the JNX files.

  The unit was checked in Delphi 5 and 7, and FreePascal 2.2.2.

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
  JNX3_ZORDER = 30;
  META_BLOCK_VERSION = 9;
  JNX_EOF_SIGNATURE = 'BirdsEye';
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
    GUID: String;
    ProductName: WideString;
    MapName: WideString;
    LevelMetaCount: integer;
    LevelMetas: array of TJNXLevelMeta;
    RawTailData: String;
  end;
  PJNXMapMeta = ^TJNXMapMeta;


  TJNXMapFile = class
  protected
    FFile: file;

    FHeader: TJNXHeader;
    FLevels: array of TJNXLevelInfo;
    FTiles: array of array of TJNXTileInfo;
    FMeta: TJNXMapMeta;

    function GetLevelInfo(l: integer): PJNXLevelInfo; virtual;
    function GetTileInfo(l, t: integer): PJNXTileInfo; virtual;

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

    procedure OpenFile(const Path: String); virtual; abstract;
    procedure CloseFile; virtual;
  public
    constructor Create(const Path: String);
    destructor Destroy; override;

    // Low-level access to the map structures
    property Header: TJNXHeader read FHeader;
    property LevelInfo[l: integer]: PJNXLevelInfo read GetLevelInfo;
    property TileInfo[l, t: integer]: PJNXTileInfo read GetTileInfo;
    property MetaInfo: TJNXMapMeta read FMeta;

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
    function GetJPEGStream(l, t: integer): String;

    procedure OpenFile(const Path: String); override;
    procedure ReadFileHeaders; virtual;

    procedure ReadMainHeader;
    procedure ReadLevelsInfo;
    procedure ReadMeta;
    procedure ReadTilesInfo;
  public
    property JPEGStreams[l, t: integer]: String read GetJPEGStream;
  end;

  TJNXWriter = class(TJNXMapFile)
  protected
    FFileHeadersAllocated: boolean;
    FFirstTileInfoOffset: longword;
    FNextPictureOffset: longword;
    FFirstTile: boolean;

    procedure OpenFile(const Path: String); override;
    procedure CloseFile; override;

    procedure SetLevelCount(Value: integer); override;
    procedure SetTileCount(l: integer; const Value: integer); override;        

    procedure InitHeader;
    procedure WriteFileHeaders(AllocateOnly: boolean);
    procedure WriteEOFSignature;

    procedure WriteMainHeader;
    procedure WriteLevelsInfo;
    procedure WriteMeta;
    procedure WriteTilesInfo;
  public
    // Map info
    property Version: integer read FHeader.Version write FHeader.Version;
    property ProductID: integer read FHeader.ProductId write FHeader.ProductId;
    property ZOrder: integer read FHeader.ZOrder write FHeader.ZOrder;
    property Levels: integer read FHeader.Levels write SetLevelCount;
    property ProductName: WideString read FMeta.ProductName write FMeta.ProductName;
    property MapName: WideString read FMeta.MapName write FMeta.MapName;

    // Levels
    property LevelScale[l: integer]: integer read GetLevelScale write SetLevelScale;
    property LevelName[l: integer]: WideString read GetLevelName write SetLevelName;
    property LevelDescription[l: integer]: WideString read GetLevelDescription write SetLevelDescription;
    property LevelCopyright[l: integer]: WideString read GetLevelCopyright write SetLevelCopyright;
    property LevelZoom[l: integer]: integer read GetLevelZoom write SetLevelZoom;
    property TileCount[l: integer]: integer read GetTileCount write SetTileCount;

    procedure WriteTile(Level, TileIndex, PicWidth, PicHeight: integer; const Bounds: TJNXRect; const JpegString: String; AdjustMapBounds: boolean = True);
  end;

function ReadUTFString(var f: File): WideString;
procedure WriteUTFString(var f: File; const s: WideString);

function JNXCoordToWGS84(c: integer): double;
function WGS84CoordToJNX(c: double): integer;

function CreateGUID: String;

function JNXRect(northern_lat, eastern_lon, southern_lat, western_lon: integer): TJNXRect;

function DigitalGlobeZoomToScale(z: integer): integer;
function MetersPerPixelToScale(d: double): integer;

implementation

uses
  Windows, ActiveX, Math;

function ReadUTFString(var f: File): WideString;
var
  UTF: String;
  c: char;
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
  UTF: String;
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

function CreateGUID: String;
var
  ID: TGUID;
begin
  if CoCreateGuid(ID) = S_OK then
  begin
    Result := GUIDToString(ID);
    delete(Result, 1, 1);
    delete(Result, Length(Result), 1);
  end
  else
    Result := '';
end;

function GetMaxBounds(const b1, b2: TJNXRect): TJNXRect;
begin
  with Result do
  begin
    northern_lat := Max(b1.northern_lat, b2.southern_lat);
    eastern_lon  := Max(b1.eastern_lon,  b2.western_lon );
    southern_lat := Min(b1.northern_lat, b2.southern_lat);
    western_lon  := Min(b1.eastern_lon,  b2.western_lon );
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
  ZoomToScale: array [0..21] of integer = (
    2446184, 2446184, 2446184, 2446184, 2446184, 2446184, 2446184, 1223072,
     611526,  305758,  152877,   76437,   38218,   19109,    9554,    4777,
       2388,    1194,     597,     298,     149,      75
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

procedure TJNXMapFile.CloseFile;
begin
  Close(FFile);
end;

constructor TJNXMapFile.Create(const Path: String);
begin
  inherited Create;

  OpenFile(Path);
end;

destructor TJNXMapFile.Destroy;
begin
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
  FMeta.LevelMetas[l].Zoom := Value;
end;

procedure TJNXMapFile.SetTileCount(l: integer; const Value: integer);
begin
  if Value >= MAX_TILES then
    Raise EJNXException.Create('Too many tiles on level ' + IntToStr(l));

  SetLength(FTiles[l], Value);
end;

{ TJNXReader }

function TJNXReader.GetJPEGStream(l, t: integer): String;
begin
  Seek64(FFile, TileInfo[l, t].PicOffset);
  SetLength(Result, TileInfo[l, t].PicSize + 2);

  Result[1] := #$FF;
  Result[2] := #$D8;
  BlockRead(FFile, Result[3], TileInfo[l, t].PicSize);
end;

procedure TJNXReader.OpenFile(const Path: String);
begin
  AssignFile(FFile, Path);
  Reset(FFile, 1);

  ReadFileHeaders;
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
begin
  with FMeta do
  begin
    BlockRead(FFile, Version, SizeOf(Version));
    GUID := String(ReadUTFString(FFile));
    ProductName := ReadUTFString(FFile);
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

{ TJNXWriter }

procedure TJNXWriter.CloseFile;
begin
  WriteFileHeaders(False);
  WriteEOFSignature;

  inherited;
end;

procedure TJNXWriter.InitHeader;
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

procedure TJNXWriter.OpenFile(const Path: String);
begin
  AssignFile(FFile, Path);
  Rewrite(FFile, 1);

  InitHeader;

  FFirstTile := True;
end;

procedure TJNXWriter.SetLevelCount(Value: integer);
begin
  if FFileHeadersAllocated then
    raise EJNXException.Create('Cannot change the count of levels after WriteTile');

  inherited;
end;

procedure TJNXWriter.SetTileCount(l: integer; const Value: integer);
begin
  if FFileHeadersAllocated then
    raise EJNXException.Create('Cannot change the count of tiles after WriteTile');

  inherited;
end;

procedure TJNXWriter.WriteEOFSignature;
begin
  Seek64(FFile, FNextPictureOffset);
  BlockWrite(FFile, JNX_EOF_SIGNATURE[1], Length(JNX_EOF_SIGNATURE));
end;

procedure TJNXWriter.WriteFileHeaders(AllocateOnly: boolean);
begin
  if AllocateOnly and FFileHeadersAllocated then
    exit;

  Seek64(FFile, 0);  

  WriteMainHeader;
  WriteLevelsInfo;
  WriteMeta;
  WriteTilesInfo;

  if AllocateOnly then
    FNextPictureOffset := FilePos(FFile);
    
  FFileHeadersAllocated := True;
end;

procedure TJNXWriter.WriteMainHeader;
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

procedure TJNXWriter.WriteLevelsInfo;
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

procedure TJNXWriter.WriteMeta;
var
  Dummy: array [1..3] of byte;
  i: integer;
begin
  with FMeta do
  begin
    BlockWrite(FFile, Version, SizeOf(Version));
    WriteUTFString(FFile, GUID);
    WriteUTFString(FFile, ProductName);

    Dummy[1] := 0;
    Dummy[2] := 0;
    Dummy[3] := 0;
    BlockWrite(FFile, Dummy, SizeOf(Dummy));

    WriteUTFString(FFile, MapName);
    BlockWrite(FFile, LevelMetaCount, SizeOf(LevelMetaCount));

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

procedure TJNXWriter.WriteTilesInfo;
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

procedure TJNXWriter.WriteTile(Level, TileIndex, PicWidth, PicHeight: integer;
  const Bounds: TJNXRect; const JpegString: String; AdjustMapBounds: boolean = True);
var
  Len: integer;
begin
  WriteFileHeaders(True);

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

end.

