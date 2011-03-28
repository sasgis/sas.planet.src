unit u_ConfigDataProviderByIniFileSection;

interface

uses
  Classes,
  SysUtils,
  IniFiles,
  i_ConfigDataProvider;

type
  TConfigDataProviderByIniFileSection = class(TInterfacedObject, IConfigDataProvider)
  protected
    FIniFile: TCustomIniFile;
    FSection: string;
    FParent: IConfigDataProvider;
    FFormatSettings: TFormatSettings;
    function GetSubItemSectionName(const AIdent: string): string;
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
  FFormatSettings.DecimalSeparator := '.';
  FFormatSettings.DateSeparator := '.';
  FFormatSettings.ShortDateFormat := 'dd.MM.yyyy';
  FFormatSettings.TimeSeparator := ':';
  FFormatSettings.LongTimeFormat := 'HH:mm:ss';
  FFormatSettings.ShortTimeFormat := 'HH:mm:ss';
  FFormatSettings.ListSeparator := ';';
  FFormatSettings.TwoDigitYearCenturyWindow := 50;
end;

destructor TConfigDataProviderByIniFileSection.Destroy;
begin
  FIniFile := nil;
  FSection := '';
  FParent := nil;
  inherited;
end;

function TConfigDataProviderByIniFileSection.GetSubItemSectionName(
  const AIdent: string): string;
begin
  Result := FSection + '_' + AIdent;
end;

function TConfigDataProviderByIniFileSection.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VSectionName: string;
begin
  Result := nil;
  VSectionName := GetSubItemSectionName(AIdent);
  if FIniFile.SectionExists(VSectionName) then begin
    Result:= TConfigDataProviderByIniFileSection.Create(FIniFile, VSectionName, Self);
  end;
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
var
  DateStr: string;
begin
  Result := ADefault;
  DateStr := FIniFile.ReadString(FSection, AIdent, '');
  if DateStr <> '' then
  try
    Result := StrToDate(DateStr, FFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  DateStr: string;
begin
  DateStr := FIniFile.ReadString(FSection, AIdent, '');
  Result := ADefault;
  if DateStr <> '' then
  try
    Result := StrToDateTime(DateStr, FFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

function TConfigDataProviderByIniFileSection.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
var
  FloatStr: string;
begin
  FloatStr := FIniFile.ReadString(FSection, AIdent, '');
  Result := ADefault;
  if FloatStr <> '' then
  try
    Result := StrToFloat(FloatStr, FFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
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
var
  TimeStr: string;
begin
  TimeStr := FIniFile.ReadString(FSection, AIdent, '');
  Result := ADefault;
  if TimeStr <> '' then
  try
    Result := StrToTime(TimeStr, FFormatSettings);
  except
    on EConvertError do
      // Ignore EConvertError exceptions
    else
      raise;
  end;
end;

procedure TConfigDataProviderByIniFileSection.ReadValuesList(AList: TStrings);
begin
  FIniFile.ReadSection(FSection, AList);
end;

end.
