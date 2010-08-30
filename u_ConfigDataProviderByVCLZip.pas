unit u_ConfigDataProviderByVCLZip;

interface

uses
  Classes,
  VCLZip,
  i_IConfigDataProvider;

type
  TConfigDataProviderByVCLZip = class(TInterfacedObject, IConfigDataProvider)
  private
    FSourceFileName: string;
    FUnZip: TVCLZip;
  protected
    function GetSubItem(const AIdent: string): IConfigDataProvider; virtual;
    function ReadBinaryStream(const AIdent: string; AValue: TStream): Integer; virtual;
    function ReadString(const AIdent: string; const ADefault: string): string; virtual;
    function ReadInteger(const AIdent: string; ADefault: Longint): Longint; virtual;
    function ReadBool(const AIdent: string; ADefault: Boolean): Boolean; virtual;
    function ReadDate(const AIdent: string; ADefault: TDateTime): TDateTime; virtual;
    function ReadDateTime(const AIdent: string; ADefault: TDateTime): TDateTime; virtual;
    function ReadFloat(const AIdent: string; ADefault: Double): Double; virtual;
    function ReadTime(const AIdent: string; ADefault: TDateTime): TDateTime; virtual;

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

{ TConfigDataProviderByVCLZip }

constructor TConfigDataProviderByVCLZip.Create(AFileName: string);
begin
  FSourceFileName := AFileName;
  if AFileName = '' then begin
    raise Exception.Create('Пустое имя файла с настройками карты');
  end;
  if not FileExists(AFileName) then begin
    raise Exception.Create('Файл ' + AFileName + ' не найден');
  end;
  FUnZip:=TVCLZip.Create(nil);
  FUnZip.ZipName:=AFileName;
  FUnZip.UnZip;
end;

destructor TConfigDataProviderByVCLZip.Destroy;
begin
  FreeAndNil(FUnZip);
  inherited;
end;

function TConfigDataProviderByVCLZip.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VExt: string;
  VIniFile: TMemIniFile;
  VIniStrings: TStringList;
  VIniStream: TMemoryStream;
begin
  Result := nil;
  VExt := UpperCase(ExtractFileExt(AIdent));
  if (VExt = '.INI') or (VExt = '.TXT') then begin
    VIniFile := TMemIniFile.Create('');
    VIniStream := TMemoryStream.Create;
    try
      FUnZip.UnZipToStream(VIniStream, AIdent);
      VIniStream.Position:=0;
      VIniStrings := TStringList.Create;
      try
        VIniStrings.LoadFromStream(VIniStream);
        VIniFile.SetStrings(VIniStrings);
      finally
        VIniStrings.Free;
      end;
    finally
      VIniStream.Free;
    end;
    Result := TConfigDataProviderByIniFile.Create(VIniFile);
  end;
end;

function TConfigDataProviderByVCLZip.ReadBinaryStream(const AIdent: string;
  AValue: TStream): Integer;
begin
  Result := FUnZip.UnZipToStream(AValue, AIdent);
end;

function TConfigDataProviderByVCLZip.ReadBool(const AIdent: string;
  ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByVCLZip.ReadDate(const AIdent: string;
  ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByVCLZip.ReadDateTime(const AIdent: string;
  ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByVCLZip.ReadFloat(const AIdent: string;
  ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByVCLZip.ReadInteger(const AIdent: string;
  ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByVCLZip.ReadString(const AIdent,
  ADefault: string): string;
var
  VExt: string;
  VStream: TMemoryStream;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFileName;
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.TXT') then begin
      VStream := TMemoryStream.Create;
      try
        FUnZip.UnZipToStream(VStream, AIdent);
        SetLength(Result, VStream.Size);
        VStream.Position := 0;
        VStream.Read(Result[1], VStream.Size);
      finally
        VStream.Free;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

procedure TConfigDataProviderByVCLZip.ReadSubItemsList(AList: TStrings);
var
  VExt: string;
begin
  AList.Clear;
  with TStringsEnumerator.Create(FUnZip.FilesList) do try
    while MoveNext do begin
      VExt := UpperCase(ExtractFileExt(Current));
      if (VExt = '.INI') or (VExt = '.TXT') then begin
        AList.Add(Current);
      end;
    end;
  finally
    Free;
  end;
end;

function TConfigDataProviderByVCLZip.ReadTime(const AIdent: string;
  ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderByVCLZip.ReadValuesList(AList: TStrings);
var
  VExt: string;
begin
  AList.Clear;
  with TStringsEnumerator.Create(FUnZip.FilesList) do try
    while MoveNext do begin
      VExt := UpperCase(ExtractFileExt(Current));
      if (VExt <> '.INI') then begin
        AList.Add(Current);
      end;
    end;
  finally
    Free;
  end;
end;

end.
