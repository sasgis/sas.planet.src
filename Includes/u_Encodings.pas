unit u_Encodings;

interface

uses
  SysUtils,
  Classes;

function FileToText(const FileName: String; AEncoding: TEncoding = nil): String;
function FileToString(const FileName: string): RawByteString;

function StreamToText(const AStream: TStream; AEncoding: TEncoding = nil): String;
function StreamToString(const AStream: TStream): RawByteString;

procedure TextToFile(const FileName: string; const Contents: String; AEncoding: TEncoding = nil; Append: Boolean = False);
procedure TextToStream(const AStream: TStream; const Contents: String; AEncoding: TEncoding = nil; Append: Boolean = False);

function TextToString(const AString: String; AEncoding: TEncoding = nil): RawByteString;
function StringToText(const AString: RawByteString; AEncoding: TEncoding): String;

procedure StringToFile(const FileName: string; const Contents: RawByteString; Append: Boolean = False);
procedure StringToStream(const AStream: TStream; const Contents: RawByteString; Append: Boolean = False);

implementation

function FileToString(const FileName: string): RawByteString;
var
  FS: TFileStream;
begin
  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := StreamToString(FS);
  finally
    FreeAndNil(fs);
  end;
end;

procedure StringToFile(const FileName: string; const Contents: RawByteString; Append: Boolean);
var
  FS: TFileStream;
begin
  if Append and FileExists(FileName) then
    FS := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyWrite)
  else
    FS := TFileStream.Create(FileName, fmCreate);
  try
    StringToStream(FS, Contents, Append);
  finally
    FreeAndNil(FS);
  end;
end;

function StreamToString(const AStream: TStream): RawByteString;
var
  Len: Integer;
begin
  Len := AStream.Size;
  SetLength(Result, Len);
  if Len > 0 then
    AStream.ReadBuffer(Result[1], Len);
end;

procedure StringToStream(const AStream: TStream; const Contents: RawByteString; Append: Boolean = False);
var
  Len: Integer;
begin
  if Append then
    AStream.Seek(0, Ord(soEnd));  // faster than .Position := .Size
  Len := Length(Contents);
  if Len > 0 then
    AStream.WriteBuffer(Contents[1], Len);

  if not Append then
    AStream.Size := Len;
end;

function GetStreamEncoding(const AStream: TStream): TEncoding;
const
  MaxBufferSize = 4096;
var
  Buffer: TBytes;
  Size: Integer;
  Position: Int64;
begin
  if AStream.Size > MaxBufferSize then
    Size := MaxBufferSize
  else
    Size := AStream.Size;
  SetLength(Buffer, Size);
  Position := AStream.Position;
  AStream.Seek(0, soBeginning);
  AStream.ReadBuffer(Buffer[0], Size);
  AStream.Seek(Position, soBeginning);

  Result := nil;
  TEncoding.GetBufferEncoding(Buffer, Result);
end;

function GetFileEncoding(const AFileName: String): TEncoding;
const
  MaxBufferSize = 4096;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := GetStreamEncoding(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

function StringToBytes(const AString: RawByteString): TBytes;
var
  L: Integer;
begin
  L := Length(AString);
  SetLength(Result, L);
  if L > 0 then
    Move(AString[1], Result[0], L);
end;

function BytesToString(const ABytes: TBytes): RawByteString;
var
  L: Integer;
begin
  L := Length(ABytes);
  SetLength(Result, L);
  if L> 0 then
    Move(ABytes[0], Result[1], L);
end;

function StringToText(const AString: RawByteString; AEncoding: TEncoding): String;
var
  Buffer: TBytes;
  Size: Integer;
begin
  Buffer := StringToBytes(AString);

  Size := TEncoding.GetBufferEncoding(Buffer, AEncoding);

  Result := AEncoding.GetString(Buffer, Size, Length(Buffer) - Size);
end;

function TextToString(const AString: String; AEncoding: TEncoding): RawByteString;
var
  Buffer, Preamble: TBytes;
begin
  if AEncoding = nil then
    AEncoding := TEncoding.Default;
  Buffer := AEncoding.GetBytes(AString);
  Preamble := AEncoding.GetPreamble;
  Result := BytesToString(Preamble) + BytesToString(Buffer);
end;

function FileToText(const FileName: String; AEncoding: TEncoding): String;
begin
  Result := StringToText(FileToString(FileName), AEncoding);
end;

procedure TextToFile(const FileName: string; const Contents: String; AEncoding: TEncoding; Append: Boolean);
var
  Content: String;
  Encoding: TEncoding;
begin
  if Append and FileExists(FileName) then
  begin
    Content := FileToText(FileName) + Contents;
    Encoding := GetFileEncoding(FileName);
  end
  else
  begin
    Content := Contents;
    Encoding := AEncoding;
  end;

  StringToFile(FileName, TextToString(Content, Encoding));
end;

function StreamToText(const AStream: TStream; AEncoding: TEncoding = nil): String;
begin
  Result := StringToText(StreamToString(AStream), AEncoding);
end;

procedure TextToStream(const AStream: TStream; const Contents: String; AEncoding: TEncoding = nil; Append: Boolean = False);
var
  Content: String;
  Encoding: TEncoding;
begin
  if Append and (AStream.Size > 0) then
  begin
    Content := StreamToText(AStream) + Contents;
    Encoding := GetStreamEncoding(AStream);
  end
  else
  begin
    Content := Contents;
    Encoding := AEncoding;
  end;

  StringToStream(AStream, TextToString(Content, Encoding));
end;

end.
