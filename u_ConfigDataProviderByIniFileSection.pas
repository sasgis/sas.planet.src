unit u_ConfigDataProviderByIniFileSection;

interface

uses
  Classes,
  IniFiles,
  i_IConfigDataProvider;

type
  TConfigDataProviderByIniFileSection = class(TInterfacedObject, IConfigDataProvider)
  private
    FIniFile: TCustomIniFile;
    FSection: string;
    FParent: IConfigDataProvider;
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
    constructor Create(AIniFile: TCustomIniFile; ASection: string; AParent: IConfigDataProvider);
    destructor Destroy; override;
  end;


implementation

{ TConfigDataProviderByIniFileSection }

constructor TConfigDataProviderByIniFileSection.Create(AIniFile: TCustomIniFile;
  ASection: string; AParent: IConfigDataProvider);
begin
  FIniFile := AIniFile;
  FSection := ASection;
  FParent := AParent;
end;

destructor TConfigDataProviderByIniFileSection.Destroy;
begin
  FIniFile := nil;
  FSection := '';
  FParent := nil;
  inherited;
end;

function TConfigDataProviderByIniFileSection.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  Result := nil;
end;

function TConfigDataProviderByIniFileSection.ReadBinaryStream(
  const AIdent: string; AValue: TStream): Integer;
begin
  Result := FIniFile.ReadBinaryStream(FSection, AIdent, AValue);
end;

function TConfigDataProviderByIniFileSection.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := FIniFile.ReadBool(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDate(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadDateTime(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := FIniFile.ReadFloat(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := FIniFile.ReadInteger(FSection, AIdent, ADefault);
end;

function TConfigDataProviderByIniFileSection.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := FIniFile.ReadString(FSection, AIdent, ADefault);
end;

procedure TConfigDataProviderByIniFileSection.ReadSubItemsList(AList: TStrings);
begin
  AList.Clear;
end;

function TConfigDataProviderByIniFileSection.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := FIniFile.ReadTime(FSection, AIdent, ADefault);
end;

procedure TConfigDataProviderByIniFileSection.ReadValuesList(AList: TStrings);
begin
  FIniFile.ReadSection(FSection, AList);
end;

end.
