unit u_ConfigDataProviderByFolder;

interface

uses
  Classes,
  i_ConfigDataProvider;

type
  TConfigDataProviderByFolder = class(TInterfacedObject, IConfigDataProvider)
  private
    FSourceFolderName: string;
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
    constructor Create(AFolderName: string);
  end;

implementation

uses
  SysUtils,
  IniFiles,
  u_ConfigDataProviderByIniFile;

{ TConfigDataProviderByFolder }

constructor TConfigDataProviderByFolder.Create(AFolderName: string);
begin
  FSourceFolderName := AFolderName;
end;

function TConfigDataProviderByFolder.GetSubItem(
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
    try
      VIniStream := TMemoryStream.Create;
      try
        VIniStream.LoadFromFile(IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent);
        VIniStream.Position := 0;
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
    except
      VIniFile.Free;
      raise;
    end;
    Result := TConfigDataProviderByIniFile.Create(VIniFile);
  end;
end;

function TConfigDataProviderByFolder.ReadBinaryStream(const AIdent: string;
  AValue: TStream): Integer;
var
  VStream: TMemoryStream;
  VFileName: string;
begin
  VFileName := IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent;
  if FileExists(VFileName) then begin
    VStream := TMemoryStream.Create;
    try
      VStream.LoadFromFile(VFileName);
      VStream.Position := 0;
      VStream.SaveToStream(AValue);
      Result := VStream.Size;
    finally
      VStream.Free;
    end;
  end else begin
    Result := 0;
  end;
end;

function TConfigDataProviderByFolder.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByFolder.ReadString(const AIdent,
  ADefault: string): string;
var
  VExt: string;
  VStream: TMemoryStream;
  VFileName: string;
begin
  Result := '';
  if AIdent = '::FileName' then begin
    Result := FSourceFolderName;
  end else begin
    VExt := UpperCase(ExtractFileExt(AIdent));
    if (VExt = '.INI') or (VExt = '.HTML') or (VExt = '.TXT') then begin
      VFileName := IncludeTrailingPathDelimiter(FSourceFolderName) + AIdent;
      if FileExists(VFileName) then begin
        VStream := TMemoryStream.Create;
        try
          VStream.LoadFromFile(VFileName);
          SetLength(Result, VStream.Size);
          VStream.Position := 0;
          VStream.Read(Result[1], VStream.Size);
        finally
          VStream.Free;
        end;
      end else begin
        Result := ADefault;
      end;
    end else begin
      Result := ADefault;
    end;
  end;
end;

procedure TConfigDataProviderByFolder.ReadSubItemsList(AList: TStrings);
var
  VExt: string;
  VFolder: string;
  SearchRec: TSearchRec;
begin
  AList.Clear;
  VFolder := IncludeTrailingPathDelimiter(FSourceFolderName);
  if FindFirst(VFolder + '*', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then begin
        continue;
      end;
      VExt := UpperCase(ExtractFileExt(SearchRec.Name));
      if (VExt = '.INI') or (VExt = '.TXT') then begin
        AList.Add(SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
  end;
end;

function TConfigDataProviderByFolder.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderByFolder.ReadValuesList(AList: TStrings);
var
  VExt: string;
  VFolder: string;
  SearchRec: TSearchRec;
begin
  AList.Clear;
  VFolder := IncludeTrailingPathDelimiter(FSourceFolderName);
  if FindFirst(VFolder + '*', faAnyFile, SearchRec) = 0 then begin
    repeat
      if (SearchRec.Attr and faDirectory) = faDirectory then begin
        continue;
      end;
      VExt := UpperCase(ExtractFileExt(SearchRec.Name));
      if (VExt <> '.INI') then begin
        AList.Add(SearchRec.Name);
      end;
    until FindNext(SearchRec) <> 0;
  end;
end;

end.
