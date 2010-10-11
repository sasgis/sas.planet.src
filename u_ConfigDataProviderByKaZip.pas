unit u_ConfigDataProviderByKaZip;

interface

uses
  Classes,
  KAZip,
  i_IConfigDataProvider;

type
  TConfigDataProviderByKaZip = class(TInterfacedObject, IConfigDataProvider)
  private
    FSourceFileName: string;
    FUnZip: TKAZip;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider; virtual;
    function ReadBinaryStream(const AIdent: string; AValue: TStream): Integer; virtual;
    function ReadString(const AIdent: string; const ADefault: string): string; virtual;
    function ReadInteger(const AIdent: string; const ADefault: Longint): Longint; virtual;
    function ReadBool(const AIdent: string; const ADefault: Boolean): Boolean; virtual;
    function ReadDate(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadDateTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;
    function ReadFloat(const AIdent: string; const ADefault: Double): Double; virtual;
    function ReadTime(const AIdent: string; const ADefault: TDateTime): TDateTime; virtual;

    procedure ReadSubItemsList(AList: TStrings); virtual;
    procedure ReadValuesList(AList: TStrings); virtual;
  public
    constructor Create(AFileName: string);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_ConfigDataProviderByIniFile;

{ TConfigDataProviderByKaZip }

constructor TConfigDataProviderByKaZip.Create(AFileName: string);
begin
  FSourceFileName := AFileName;
  if AFileName = '' then begin
    raise Exception.Create('Пустое имя файла с настройками карты');
  end;
  if not FileExists(AFileName) then begin
    raise Exception.CreateFmt('Файл %0:s не найден', [AFileName]);
  end;
  FUnZip := TKAZip.Create(nil);
  FUnZip.Open(AFileName);
end;

destructor TConfigDataProviderByKaZip.Destroy;
begin
  FreeAndNil(FUnZip);
  inherited;
end;

function TConfigDataProviderByKaZip.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VExt: string;
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TMemoryStream;
  VIndex: Integer;
begin
  Result := nil;
  VExt := UpperCase(ExtractFileExt(AIdent));
  if (VExt = '.INI') or (VExt = '.TXT') then begin
    VIndex := FUnZip.Entries.IndexOf(AIdent);
    if VIndex >= 0 then begin
      VIniFile := TMemIniFile.Create('');
      VIniStream := TMemoryStream.Create;
      try
        FUnZip.Entries.Items[VIndex].ExtractToStream(VIniStream);
        VIniStream.Position := 0;
        VIniStrings := TStringList.Create;
        try
          VIniStrings.LoadFromStream(VIniStream);
          VIniFile.SetStrings(VIniStrings);
          Result := TConfigDataProviderByIniFile.Create(VIniFile);
        finally
          VIniStrings.Free;
        end;
      finally
        VIniStream.Free;
      end;
    end;
  end;
end;

function TConfigDataProviderByKaZip.ReadBinaryStream(const AIdent: string;
  AValue: TStream): Integer;
var
  VIndex: Integer;
begin
  Result := 0;
  VIndex := FUnZip.Entries.IndexOf(AIdent);
  if VIndex >= 0 then begin
    FUnZip.Entries.Items[VIndex].ExtractToStream(AValue);
    Result := AValue.Size;
  end;
end;

function TConfigDataProviderByKaZip.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByKaZip.ReadString(const AIdent,
  ADefault: string): string;
var
  VExt: string;
  VStream: TMemoryStream;
  VIndex: Integer;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFileName;
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.TXT') then begin
      VIndex := FUnZip.Entries.IndexOf(AIdent);
      if VIndex >= 0 then begin
        VStream := TMemoryStream.Create;
        try
          FUnZip.Entries.Items[VIndex].ExtractToStream(VStream);
          SetLength(Result, VStream.Size);
          VStream.Position := 0;
          VStream.Read(Result[1], VStream.Size);
        finally
          VStream.Free;
        end;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

procedure TConfigDataProviderByKaZip.ReadSubItemsList(AList: TStrings);
var
  i: Integer;
  VExt: string;
  VFileName: string;
begin
  AList.Clear;
  for i := 0 to FUnZip.Entries.Count - 1 do begin
    VFileName := FUnZip.Entries.Items[i].FileName;
    VExt := UpperCase(ExtractFileExt(VFileName));
    if (VExt = '.INI') or (VExt = '.TXT') then begin
      AList.Add(VFileName);
    end;
  end;
end;

function TConfigDataProviderByKaZip.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderByKaZip.ReadValuesList(AList: TStrings);
var
  i: Integer;
  VExt: string;
  VFileName: string;
begin
  AList.Clear;
  for i := 0 to FUnZip.Entries.Count - 1 do begin
    VFileName := FUnZip.Entries.Items[i].FileName;
    VExt := UpperCase(ExtractFileExt(VFileName));
    if (VExt <> '.INI') then begin
      AList.Add(VFileName);
    end;
  end;
end;

end.
