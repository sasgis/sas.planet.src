unit CompatibilityStringList;

{$I jedi.inc}

interface

{$IFNDEF Compiler12_Up} // Delphi11_Down
uses
  SysUtils,
  Classes,
  Compatibility;
{$ENDIF}

{$IFNDEF Compiler12_Up} // Delphi11_Down
type
  TStringList = class(Classes.TStringList)
  private
    FEncoding: TEncoding;
    FDefaultEncoding: TEncoding;
    FWriteBOM: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); overload; override;
    procedure LoadFromFile(const FileName: string; Encoding: TEncoding); reintroduce; overload; virtual;
    procedure LoadFromStream(Stream: TStream); overload; override;
    procedure LoadFromStream(Stream: TStream; Encoding: TEncoding); reintroduce; overload; virtual;
    procedure SaveToFile(const FileName: string); overload; override;
    procedure SaveToFile(const FileName: string; Encoding: TEncoding); reintroduce; overload; virtual;
    procedure SaveToStream(Stream: TStream); overload; override;
    procedure SaveToStream(Stream: TStream; Encoding: TEncoding); reintroduce; overload; virtual;

    property DefaultEncoding: TEncoding read FDefaultEncoding write FDefaultEncoding;
    property Encoding: TEncoding read FEncoding;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
  end;
{$ENDIF}

implementation

{ TStringList }

{$IFNDEF Compiler12_Up} // Delphi11_Down
constructor TStringList.Create;
begin
  inherited Create;
  FDefaultEncoding := TEncoding.Default;
  FEncoding := nil;
  FWriteBOM := True;
end;

destructor TStringList.Destroy;
begin
  if (FEncoding <> nil) and not TEncoding.IsStandardEncoding(FEncoding) then
    FreeAndNil(FEncoding);
  if not TEncoding.IsStandardEncoding(FDefaultEncoding) then
    FreeAndNil(FDefaultEncoding);
  inherited Destroy;
end;

procedure TStringList.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TStringList.LoadFromFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStringList.LoadFromStream(Stream: TStream);
begin
  inherited;
  LoadFromStream(Stream, nil);
end;

procedure TStringList.LoadFromStream(Stream: TStream; Encoding: TEncoding);
var
  Size: Integer;
  Buffer: TBytes;
begin
  BeginUpdate;
  try
    if not Assigned(Encoding) then
      Encoding := DefaultEncoding;
    Size := Stream.Size - Stream.Position;
    SetLength(Buffer, Size);
    Stream.Read(Buffer[0], Size);
    Size := TEncoding.GetBufferEncoding(Buffer, Encoding, FDefaultEncoding);
    FEncoding := Encoding; // Keep Encoding in case the stream is saved
    SetTextStr(Encoding.GetString(Buffer, Size, Length(Buffer) - Size));
  finally
    EndUpdate;
  end;
end;

procedure TStringList.SaveToFile(const FileName: string);
begin
  SaveToFile(FileName, FEncoding);
end;

procedure TStringList.SaveToFile(const FileName: string; Encoding: TEncoding);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream, Encoding);
  finally
    Stream.Free;
  end;
end;

procedure TStringList.SaveToStream(Stream: TStream);
begin
  SaveToStream(Stream, FEncoding);
end;

procedure TStringList.SaveToStream(Stream: TStream; Encoding: TEncoding);
var
  Buffer, Preamble: TBytes;
  TextStr: string;
begin
  if Encoding = nil then
    Encoding := FEncoding;
  if Encoding = nil then
    Encoding := FDefaultEncoding;
  TextStr := GetTextStr;
  Buffer := Encoding.GetBytes(TextStr);
  if FWriteBOM then
  begin
    Preamble := Encoding.GetPreamble;
    if Length(Preamble) > 0 then
      Stream.WriteBuffer(Preamble[0], Length(Preamble));
  end;
  Stream.WriteBuffer(Buffer[0], Length(Buffer));
end;
{$ENDIF}

end.
