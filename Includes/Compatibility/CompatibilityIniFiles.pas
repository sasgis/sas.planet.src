unit CompatibilityIniFiles;

{$I jedi.inc}

interface

uses
  IniFiles,
  Compatibility;

{$IFNDEF Compiler12_Up} // Delphi11_Down
type
  TMemIniFile = class(IniFiles.TMemIniFile)
  private
    FEncoding: TEncoding;
    procedure LoadValues;
  public
    constructor Create(const FileName: string); overload;
    constructor Create(const FileName: string; Encoding: TEncoding); overload;
    procedure UpdateFile; override;
    property Encoding: TEncoding read FEncoding write FEncoding;
  end;
{$ENDIF}

implementation

uses
  Classes,
  SysUtils,
  Encodings;

{ TMemIniFile }

{$IFNDEF Compiler12_Up} // Delphi11_Down
constructor TMemIniFile.Create(const FileName: string);
begin
  Create(FileName, nil);
end;

constructor TMemIniFile.Create(const FileName: string; Encoding: TEncoding);
begin
  inherited Create('');
  FEncoding := Encoding;
  Rename(FileName, False);
  LoadValues;
end;

procedure TMemIniFile.LoadValues;
var
  Size: Integer;
  Buffer: TBytes;
  List: TStringList;
  Stream: TFileStream;
begin
  if (FileName <> '') and FileExists(FileName) then
  begin
    Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
     // Load file into buffer and detect encoding
      Size := Stream.Size - Stream.Position;
      SetLength(Buffer, Size);
      Stream.Read(Buffer[0], Size);
      Size := TEncoding.GetBufferEncoding(Buffer, FEncoding);

      // Load strings from buffer
      List := TStringList.Create;
      try
        List.Text := FEncoding.GetString(Buffer, Size, Length(Buffer) - Size);
        SetStrings(List);
      finally
        List.Free;
      end;
    finally
      Stream.Free;
    end;
  end
  else
    Clear;
end;

procedure TMemIniFile.UpdateFile;
var
  List: TStringList;
begin
  List := TStringList.Create;
  try
    GetStrings(List);
    SaveStringsToFile(List, FileName, FEncoding);
  finally
    List.Free;
  end;
end;
{$ENDIF}

end.
