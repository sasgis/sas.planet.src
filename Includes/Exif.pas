unit Exif;

interface

uses
  Classes;

type
  TExifSimple = class(TObject)
  private
    FStream: TMemoryStream;
    function GetStream(): TMemoryStream;
  public
    constructor Create(ALatitude: Double; ALongitude: Double);
    destructor Destroy; override;
    property Stream: TMemoryStream read GetStream;
  end;

implementation

const
  CExifDump: array[0..165] of byte = (
	$45, $78, $69, $66, $00, $00, $4D, $4D, $00, $2A, $00, $00, $00, $08, $00, $01,
	$88, $25, $00, $04, $00, $00, $00, $01, $00, $00, $00, $1A, $00, $00, $00, $00,
	$00, $06, $00, $00, $00, $01, $00, $00, $00, $04, $02, $02, $00, $00, $00, $01,
	$00, $02, $00, $00, $00, $02, $4E, $00, $00, $00, $00, $02, $00, $05, $00, $00,
	$00, $03, $00, $00, $00, $68, $00, $03, $00, $02, $00, $00, $00, $02, $57, $00,
	$00, $00, $00, $04, $00, $05, $00, $00, $00, $03, $00, $00, $00, $80, $00, $12,
	$00, $02, $00, $00, $00, $07, $00, $00, $00, $98, $00, $00, $00, $00, $00, $00,
	$00, $22, $00, $00, $00, $01, $00, $00, $00, $25, $00, $00, $00, $01, $00, $00,
	$53, $F9, $00, $00, $01, $69, $00, $00, $00, $6D, $00, $00, $00, $01, $00, $00,
	$00, $36, $00, $00, $00, $01, $00, $00, $AF, $B5, $00, $00, $06, $C1, $57, $47,
	$53, $2D, $38, $34, $00, $00 );

  CGpsLatitudeRefOffset = $36;
  CGpsLongitudeRefOffset = $4E;
  CGpsLatitudeValueOffset = $6E;
  CGpsLongitudeValueOffset = $86;

function Swap(AValue: Cardinal): Cardinal; assembler;
asm
  bswap eax;
end;

procedure CoordToDegrMinSec(ACoord: Double; out ADegr, AMin, ASec, ASecDiv: Cardinal);
var
  VDegr: Double;
  VInt: Integer;
  VValue: Integer;
begin
  VDegr := Abs(ACoord);
  VValue := Trunc(VDegr * 60 * 60 * 100 + 0.005);
  VInt := Trunc(VValue / (60 * 60 * 100));
  VValue := VValue - VInt * (60 * 60 * 100);
  ADegr := VInt;
  VInt := Trunc(VValue / (60 * 100));
  VValue := VValue - VInt * (60 * 100);
  AMin := VInt;
  ASec := VValue;
  ASecDiv := 100;
end;

{ TExifSimple }

constructor TExifSimple.Create(ALatitude: Double; ALongitude: Double);
var
  VDegr, VMin, VDegrMinDiv, VSec, VSecDiv: Cardinal;
  VCoordRef: array [0..3] of AnsiChar;
begin
  inherited Create;
  FStream := TMemoryStream.Create;
  FStream.WriteBuffer(CExifDump[0], Length(CExifDump));

  if ALatitude > 0 then begin
    VCoordRef := 'N'#0#0#0;
  end else begin
    VCoordRef := 'S'#0#0#0;
  end;
  FStream.Position := CGpsLatitudeRefOffset;
  FStream.WriteBuffer(VCoordRef[0], Length(VCoordRef));

  if ALongitude > 0 then begin
    VCoordRef := 'E'#0#0#0;
  end else begin
    VCoordRef := 'W'#0#0#0;
  end;
  FStream.Position := CGpsLongitudeRefOffset;
  FStream.WriteBuffer(VCoordRef[0], Length(VCoordRef));

  CoordToDegrMinSec(ALatitude, VDegr, VMin, VSec, VSecDiv);

  VDegr := Swap(VDegr);
  VMin  := Swap(VMin);
  VDegrMinDiv := Swap(1);
  VSec := Swap(VSec);
  VSecDiv := Swap(VSecDiv);

  FStream.Position := CGpsLatitudeValueOffset;
  FStream.WriteBuffer(VDegr, 4);
  FStream.WriteBuffer(VDegrMinDiv, 4);
  FStream.WriteBuffer(VMin, 4);
  FStream.WriteBuffer(VDegrMinDiv, 4);
  FStream.WriteBuffer(VSec, 4);
  FStream.WriteBuffer(VSecDiv, 4);

  CoordToDegrMinSec(ALongitude, VDegr, VMin, VSec, VSecDiv);

  VDegr := Swap(VDegr);
  VMin  := Swap(VMin);
  VDegrMinDiv := Swap(1);
  VSec := Swap(VSec);
  VSecDiv := Swap(VSecDiv);

  FStream.Position := CGpsLongitudeValueOffset;
  FStream.WriteBuffer(VDegr, 4);
  FStream.WriteBuffer(VDegrMinDiv, 4);
  FStream.WriteBuffer(VMin, 4);
  FStream.WriteBuffer(VDegrMinDiv, 4);
  FStream.WriteBuffer(VSec, 4);
  FStream.WriteBuffer(VSecDiv, 4);

  FStream.Position := 0;
end;

destructor TExifSimple.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TExifSimple.GetStream(): TMemoryStream;
begin
  FStream.Position := 0;
  Result := FStream;
end;

end.
