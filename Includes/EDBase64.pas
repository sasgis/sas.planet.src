unit EDBase64;
 
interface

function Base64Encode(const Data: AnsiString): AnsiString; overload;
function Base64UrlEncode(const Data: AnsiString): AnsiString; overload;

function Base64Decode(const Data: AnsiString): AnsiString; overload;

function Base64Encode(const Data: PAnsiChar; const DataSize: Integer): AnsiString; overload;
function Base64UrlEncode(const Data: PAnsiChar; const DataSize: Integer): AnsiString; overload;
 
function Base64Decode(const Data: PAnsiChar; const DataSize: Integer): AnsiString; overload;
 
implementation

uses
  SysUtils;

type
  EBase64Exception = class(Exception);
 
const
  Base64Table    : array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
  Base64URLTable : array[0..63] of AnsiChar = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_';
  b64FillChar = '=';
  b64Mask1    = $FC000000;
  b64Mask2    = $03F00000;
  b64Mask3    = $000FC000;
  b64Mask4    = $00003F00;
 
type
  PBytes = ^TBytes;
  TBytes = packed array[0..0] of byte;
 
  b64IntChar = packed record case Integer of
    0: (l : Integer);
    1: (c : array[0..3] of AnsiChar);
  end;
 
const
  _FI1 : packed array['A'..'Z'] of byte = (0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25);
  _FI2 : packed array['a'..'z'] of byte = (26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51);
  _FI3 : packed array['0'..'9'] of byte = (52,53,54,55,56,57,58,59,60,61);
 
function FastIndexOf(const C: AnsiChar): Integer;
begin
  case byte(C) of
    $2B,$2D {+-}        : result := 62;
    $2F,$5F {/_}        : result := 63;
    $30..$39 {'0'..'9'} : result := _FI3[C];
    $41..$5A {'A'..'Z'} : result := _FI1[C];
    $61..$7A {'a'..'z'} : result := _FI2[C];
  else
    raise EBase64Exception.Create('Function FastIndexOf is out of range!');
  end;
end;

function Base64Encode(const Data: PAnsiChar; const DataSize: Integer): AnsiString;
var
  trail, sz, i, k : Integer;
  b64 : b64IntChar;
  pR  : PAnsiChar;
begin
  sz    := (DataSize div 3) shl 2;
  trail := DataSize mod 3;
  if trail <> 0 then
    inc(sz, 4);
  SetLength(result, sz);
  pR := PAnsiChar(result);
 
  i := 0;
  k := 0;
  while (i < (DataSize-trail)) do
  begin
    b64.c[3] := Data[i];
    b64.c[2] := Data[i+1];
    b64.c[1] := Data[i+2];
    inc(i, 3);
 
    pR[k]   := Base64Table[(b64.l and b64Mask1) shr 26];
    pR[k+1] := Base64Table[(b64.l and b64Mask2) shr 20];
    pR[k+2] := Base64Table[(b64.l and b64Mask3) shr 14];
    pR[k+3] := Base64Table[(b64.l and b64Mask4) shr 8];
    inc(k,4);
  end;
 
  b64.l := 0;
 
  case trail of
    1 : begin
      b64.c[3] := Data[i];
 
      pR[k]   := Base64Table[(b64.l and b64Mask1) shr 26];
      pR[k+1] := Base64Table[(b64.l and b64Mask2) shr 20];
      pR[k+2] := b64FillChar;
      pR[k+3] := b64FillChar;
    end;
    2 : begin
      b64.c[3] := Data[i];
      b64.c[2] := Data[i+1];
 
      pR[k]   := Base64Table[(b64.l and b64Mask1) shr 26];
      pR[k+1] := Base64Table[(b64.l and b64Mask2) shr 20];
      pR[k+2] := Base64Table[(b64.l and b64Mask3) shr 14];
      pR[k+3] := b64FillChar;
    end;
  end;
end;
 
function Base64UrlEncode(const Data: PAnsiChar; const DataSize: Integer): AnsiString;
var
  trail, sz, i, k : Integer;
  b64 : b64IntChar;
  pR  : PAnsiChar;
begin
  sz    := (DataSize div 3) shl 2;
  trail := DataSize mod 3;
  if trail <> 0 then
    inc(sz, 4);
  SetLength(result, sz);
  pR := PAnsiChar(result);
 
  i := 0;
  k := 0;
  while (i < (DataSize-trail)) do
  begin
    b64.c[3] := Data[i];
    b64.c[2] := Data[i+1];
    b64.c[1] := Data[i+2];
    inc(i, 3);
 
    pR[k]   := Base64URLTable[(b64.l and b64Mask1) shr 26];
    pR[k+1] := Base64URLTable[(b64.l and b64Mask2) shr 20];
    pR[k+2] := Base64URLTable[(b64.l and b64Mask3) shr 14];
    pR[k+3] := Base64URLTable[(b64.l and b64Mask4) shr 8];
    inc(k,4);
  end;
 
  b64.l := 0;
 
  case trail of
    1 : begin
      b64.c[3] := Data[i];
 
      pR[k]   := Base64URLTable[(b64.l and b64Mask1) shr 26];
      pR[k+1] := Base64URLTable[(b64.l and b64Mask2) shr 20];
      pR[k+2] := b64FillChar;
      pR[k+3] := b64FillChar;
    end;
    2 : begin
      b64.c[3] := Data[i];
      b64.c[2] := Data[i+1];
 
      pR[k]   := Base64URLTable[(b64.l and b64Mask1) shr 26];
      pR[k+1] := Base64URLTable[(b64.l and b64Mask2) shr 20];
      pR[k+2] := Base64URLTable[(b64.l and b64Mask3) shr 14];
      pR[k+3] := b64FillChar;
    end;
  end;
end;
 
function Base64Decode(const Data: PAnsiChar; const DataSize: Integer): AnsiString;
var
  trail, szin, szout, i, k : Integer;
  b64 : b64IntChar;
  pR  : PAnsiChar;
begin
  if Data[DataSize-1] = b64FillChar then
  begin
    if Data[DataSize-2] = b64FillChar then
      trail := 2
    else
      trail := 1;
  end
  else
    trail := 0;
 
  if trail = 0 then
    szin := DataSize
  else
    szin := DataSize-4;
 
  szout := (szin shr 2) * 3;
  if Trail <> 0 then
    if Trail = 1 then
      inc(szout, 2)
    else
      inc(szout, 1);
 
  SetLength(result, szout);
  pR := PAnsiChar(result);
 
  i := 0;
  k := 0;
  while i < szin do
  begin
    b64.l := 0;
    b64.l := (FastIndexOf(Data[i]) shl 26) +
             (FastIndexOf(Data[i+1]) shl 20) +
             (FastIndexOf(Data[i+2]) shl 14) +
             (FastIndexOf(Data[i+3]) shl 8);
    inc(i, 4);
 
    pR[k]   := b64.c[3];
    pR[k+1] := b64.c[2];
    pR[K+2] := b64.c[1];
    inc(k, 3);
  end;
 
  b64.l := 0;
 
  case trail of
    1 : begin
      b64.l := (FastIndexOf(Data[i]) shl 26) +
               (FastIndexOf(Data[i+1]) shl 20) +
               (FastIndexOf(Data[i+2]) shl 14);
      pR[k]   := b64.c[3];
      pR[K+1] := b64.c[2];
    end;
    2 : begin
      b64.l := (FastIndexOf(Data[i]) shl 26) +
               (FastIndexOf(Data[i+1]) shl 20);
      pR[k]   := b64.c[3];
    end;
  end;
end;

function Base64Encode(const Data: AnsiString): AnsiString; overload;
begin
  Result := Base64Encode(PAnsiChar(Data), Length(Data));
end;

function Base64UrlEncode(const Data: AnsiString): AnsiString; overload;
begin
  Result := Base64UrlEncode(PAnsiChar(Data), Length(Data));
end;

function Base64Decode(const Data: AnsiString): AnsiString; overload;
begin
  Result := Base64Decode(PAnsiChar(Data), Length(Data));
end;
 
end.