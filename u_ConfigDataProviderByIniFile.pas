unit u_ConfigDataProviderByIniFile;

interface

uses
  Classes,
  IniFiles,
  i_ConfigDataProvider;

type
  TConfigDataProviderByIniFile = class(TInterfacedObject, IConfigDataProvider)
  protected
    FIniFile: TCustomIniFile;
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
    constructor Create(AIniFile: TCustomIniFile);
    destructor Destroy; override;
  end;


implementation

uses
  SysUtils,
  u_ConfigDataProviderByIniFileSection;

{ TConfigDataProviderByIniFile }

constructor TConfigDataProviderByIniFile.Create(AIniFile: TCustomIniFile);
begin
  FIniFile := AIniFile;
end;

destructor TConfigDataProviderByIniFile.Destroy;
begin
  FreeAndNil(FIniFile);
  inherited;
end;

function TConfigDataProviderByIniFile.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  Result := nil;
  if FIniFile.SectionExists(AIdent) then begin
    Result := TConfigDataProviderByIniFileSection.Create(FIniFile, AIdent, Self);
  end;
end;

function TConfigDataProviderByIniFile.ReadBinaryStream(const AIdent: string;
  AValue: TStream): Integer;
begin
  Result := 0;
end;

function TConfigDataProviderByIniFile.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderByIniFile.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderByIniFile.ReadSubItemsList(AList: TStrings);
begin
  FIniFile.ReadSections(AList);
end;

function TConfigDataProviderByIniFile.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderByIniFile.ReadValuesList(AList: TStrings);
begin
  AList.Clear;
end;

end.
