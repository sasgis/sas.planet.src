unit u_ConfigDataWriteProviderByIniFile;

interface

uses
  Classes,
  IniFiles,
  i_IConfigDataWriteProvider,
  u_ConfigDataProviderByIniFile;

type
  TConfigDataWriteProviderByIniFile = class(TConfigDataProviderByIniFile, IConfigDataWriteProvider)
  protected
    function GetOrCreateSubItem(const AIdent: string): IConfigDataWriteProvider;
    procedure DeleteSubItem(const AIdent: string);
    procedure DeleteValue(const AIdent: string);
    procedure WriteBinaryStream(const AIdent: string; AValue: TStream);
    procedure WriteString(const AIdent: string; const AValue: string);
    procedure WriteInteger(const AIdent: string; const AValue: Longint);
    procedure WriteBool(const AIdent: string; const AValue: Boolean);
    procedure WriteDate(const AIdent: string; const AValue: TDateTime);
    procedure WriteDateTime(const AIdent: string; const AValue: TDateTime);
    procedure WriteFloat(const AIdent: string; const AValue: Double);
    procedure WriteTime(const AIdent: string; const AValue: TDateTime);
  public
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ConfigDataWriteProviderByIniFileSection;

{ TConfigDataWriteProviderByIniFile }

procedure TConfigDataWriteProviderByIniFile.DeleteSubItem(const AIdent: string);
begin
  FIniFile.EraseSection(AIdent);
end;

procedure TConfigDataWriteProviderByIniFile.DeleteValue(const AIdent: string);
begin
  raise Exception.Create('Not expected');
end;

destructor TConfigDataWriteProviderByIniFile.Destroy;
begin
  try
    FIniFile.UpdateFile;
  except
  end;
  inherited;
end;

function TConfigDataWriteProviderByIniFile.GetOrCreateSubItem(
  const AIdent: string): IConfigDataWriteProvider;
begin
  Result := TConfigDataWriteProviderByIniFileSection.Create(FIniFile, AIdent, Self);
end;

procedure TConfigDataWriteProviderByIniFile.WriteBinaryStream(
  const AIdent: string; AValue: TStream);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteBool(const AIdent: string;
  const AValue: Boolean);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteDate(const AIdent: string;
  const AValue: TDateTime);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteDateTime(const AIdent: string;
  const AValue: TDateTime);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteFloat(const AIdent: string;
  const AValue: Double);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteInteger(const AIdent: string;
  const AValue: Integer);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteString(const AIdent,
  AValue: string);
begin
  raise Exception.Create('Not expected');
end;

procedure TConfigDataWriteProviderByIniFile.WriteTime(const AIdent: string;
  const AValue: TDateTime);
begin
  raise Exception.Create('Not expected');
end;

end.
