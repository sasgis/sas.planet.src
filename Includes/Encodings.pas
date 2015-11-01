unit Encodings;

interface

uses
  Windows,
  SysUtils,
  {$IFNDef UNICODE}
  Compatibility,
  {$ENDIF}
  Classes;

function StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): string; // StringReplace are slow in all Delphi versions

function FileToString(const FileName: string): RawByteString; // AnsiString in JCL is not good enough
procedure StringToFile(const FileName: string; const Contents: RawByteString; Append: Boolean = False);
function StreamToString(const AStream: TStream): RawByteString; // AnsiString in JCL is not good enough
procedure StringToStream(const AStream: TStream; const Contents: RawByteString; Append: Boolean = False);


procedure FreeMemAndNil(var P; const Size: Integer = 0);

function GetFileEncoding(const AFileName: String): TEncoding;
function GetStreamEncoding(const AStream: TStream): TEncoding;
function StringToBytes(const AString: RawByteString): TBytes;
function BytesToString(const ABytes: TBytes): RawByteString;
function StringToText(const AString: RawByteString; AEncoding: TEncoding = nil): String;
function TextToString(const AString: String; AEncoding: TEncoding = nil): RawByteString;
function FileToText(const FileName: String; AEncoding: TEncoding = nil): String;
procedure TextToFile(const FileName: string; const Contents: String; AEncoding: TEncoding = nil; Append: Boolean = False);
function StreamToText(const AStream: TStream; AEncoding: TEncoding = nil): String;
procedure TextToStream(const AStream: TStream; const Contents: String; AEncoding: TEncoding = nil; Append: Boolean = False);
procedure LoadStringsFromFile(const AStrings: TStrings; const AFileName: String; const AEncoding: TEncoding = nil);
procedure SaveStringsToFile(const AStrings: TStrings; const AFileName: String; const AEncoding: TEncoding = nil);
procedure LoadStringsFromStream(const AStrings: TStrings; const AStream: TStream; const AEncoding: TEncoding = nil);
procedure SaveStringsToStream(const AStrings: TStrings; const AStream: TStream; const AEncoding: TEncoding = nil);

function UTF8EncodeToString(const AStr: String): String;

implementation

{$IFDEF UNICODE}
uses
  AnsiStrings;
{$ENDIF}

function StringReplace(const S, OldPattern, NewPattern: String; Flags: TReplaceFlags): string;
var
  OldPatternLen, TextLen, ResultLen, ResultActualSize, NewPatternLen: Integer;
  CompareText: function(const S1, S2: PChar; const ALen: Integer): Boolean;

  function CompareMemCS(const S1, S2: PChar; const ALen: Integer): Boolean;
  begin
    Result := CompareMem(S1, S2, ALen * SizeOf(Char));
  end;

  function CompareMemNC(const S1, S2: PChar; const ALen: Integer): Boolean;
  begin
    Result := (AnsiStrLIComp(S1, S2, ALen) = 0);
  end;

  procedure AddReplaceText(const Index: Integer);
  begin
    if NewPatternLen > 0 then
    begin
      if ResultLen + NewPatternLen > ResultActualSize then
      begin
        ResultActualSize := ResultActualSize * 2 + NewPatternLen;
        SetLength(Result, ResultActualSize);
      end;

      MoveChars(NewPattern[1], Result[ResultLen + 1], NewPatternLen);

      ResultLen := ResultLen + NewPatternLen;
    end;
  end;

  procedure AddOriginalText(const LastIndex, Index: Integer);
  var
    Sz, Dest: Integer;

  begin
    Sz := Index - LastIndex;
    if Sz <= 0 then
      Exit;

    Dest := ResultLen;
    Inc(Dest);
    ResultLen := ResultLen + Sz;

    if ResultLen > ResultActualSize then
    begin
      ResultActualSize := ResultActualSize * 2;
      SetLength(Result, ResultActualSize);
    end;

    MoveChars(S[LastIndex], Result[Dest], Sz);
  end;

var
  Index, J, LastIndex: Integer;
  UpperFirstChar, LowerFirstChar: Char;
  Tmp: String;
begin
  if (S = '') or (OldPattern = '') then // Do Not Localize
  begin
    Result := S;
    Exit;
  end;

  OldPatternLen := Length(OldPattern);
  NewPatternLen := Length(NewPattern);
  TextLen := Length(S);
  ResultLen := 0;
  ResultActualSize := TextLen;
  SetLength(Result, ResultActualSize);

  if rfIgnoreCase in Flags then
  begin
    Tmp := OldPattern[1];
    UpperFirstChar := AnsiUpperCase(Tmp)[1];
    LowerFirstChar := AnsiLowerCase(Tmp)[1];
    CompareText := @CompareMemNC;
  end
  else
  begin
    UpperFirstChar := OldPattern[1];
    LowerFirstChar := OldPattern[1];
    CompareText := @CompareMemCS;
  end;

  Index := 1;
  J := OldPatternLen;
  LastIndex := 1;
  while Index <= TextLen do
  begin
    if J > TextLen then
      Break;

    if ((S[Index] = LowerFirstChar) or
        (S[Index] = UpperFirstChar)) and
       ((OldPatternLen = 1) or
        CompareText(@(PChar(Pointer(S))[Index]), @(PChar(Pointer(OldPattern))[1]), OldPatternLen - 1)) then
    begin
      AddOriginalText(LastIndex, Index);
      AddReplaceText(Index);

      Index := Index + OldPatternLen;
      LastIndex := Index;
      J := J + OldPatternLen;

      if not (rfReplaceAll in Flags) then
        Break;
    end
    else
    begin
      Inc(Index);
      Inc(J);
    end;
  end;
  AddOriginalText(LastIndex, TextLen + 1);

  if Length(Result) <> ResultLen then
    SetLength(Result, ResultLen);
end;

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

procedure FreeMemAndNil(var P; const Size: Integer);
var
  Data: Pointer;
begin
  Data := Pointer(P);
  Pointer(P) := nil;
  if Size <> 0 then
    FreeMem(Data, Size)
  else
    FreeMem(Data);
end;

// __ UTF8 _____________________________________________________________________________________________________________

function UTF8EncodeToString(const AStr: String): String;
var
  S: RawByteString;
begin
  S := UTF8Encode(AStr);
  {$IFDEF UNICODE}
  SetCodePage(S, 0, False);
  {$ENDIF}
  Result := String(S);
end;

// __ Files ____________________________________________________________________________________________________________

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

procedure LoadStringsFromFile(const AStrings: TStrings; const AFileName: String; const AEncoding: TEncoding);
begin
  {$IFDEF UNICODE}
  AStrings.LoadFromFile(AFileName, AEncoding);
  {$ELSE}
  AStrings.Text := FileToText(AFileName, AEncoding);
  {$ENDIF}
end;

procedure SaveStringsToFile(const AStrings: TStrings; const AFileName: String; const AEncoding: TEncoding);
var
  Encoding: TEncoding;
begin
  if (AEncoding = nil) and FileExists(AFileName) then
    Encoding := GetFileEncoding(AFileName)
  else
    Encoding := AEncoding;

  {$IFDEF UNICODE}
  AStrings.SaveToFile(AFileName, Encoding);
  {$ELSE}
  TextToFile(AFileName, AStrings.Text, Encoding);
  {$ENDIF}
end;

procedure LoadStringsFromStream(const AStrings: TStrings; const AStream: TStream; const AEncoding: TEncoding);
begin
  {$IFDEF UNICODE}
  AStrings.LoadFromStream(AStream, AEncoding);
  {$ELSE}
  AStrings.Text := StreamToText(AStream, AEncoding);
  {$ENDIF}
end;

procedure SaveStringsToStream(const AStrings: TStrings; const AStream: TStream; const AEncoding: TEncoding);
var
  Encoding: TEncoding;
begin
  if (AEncoding = nil) and (AStream.Size > 0)  then
    Encoding := GetStreamEncoding(AStream)
  else
    Encoding := AEncoding;

  {$IFDEF UNICODE}
  AStrings.SaveToStream(AStream, Encoding);
  {$ELSE}
  TextToStream(AStream, AStrings.Text, Encoding);
  {$ENDIF}
end;

end.
