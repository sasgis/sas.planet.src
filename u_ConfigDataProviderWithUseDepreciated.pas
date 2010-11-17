unit u_ConfigDataProviderWithUseDepreciated;

interface

uses
  Classes,
  i_IConfigDataProvider;

type
  TConfigDataProviderWithUseDepreciated = class(TInterfacedObject, IConfigDataProvider)
  private
    FSource: IConfigDataProvider;
    FIdentRenamesList: TStringList;
    function GetDepreciatedName(AIdent: string): string;
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
    constructor Create(
      ASource: IConfigDataProvider;
      AIdentRenamesList: TStringList
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

{ TConfigDataProviderWithRenamesList }

constructor TConfigDataProviderWithUseDepreciated.Create(
  ASource: IConfigDataProvider; AIdentRenamesList: TStringList);
begin
  FSource := ASource;
  FIdentRenamesList := TStringList.Create;
  FIdentRenamesList.Assign(AIdentRenamesList);
end;

destructor TConfigDataProviderWithUseDepreciated.Destroy;
begin
  FreeAndNil(FIdentRenamesList);
  FSource := nil;
  inherited;
end;

function TConfigDataProviderWithUseDepreciated.GetDepreciatedName(
  AIdent: string): string;
begin
  Result := FIdentRenamesList.Values[AIdent];
end;

function TConfigDataProviderWithUseDepreciated.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  Result := FSource.GetSubItem(AIdent);
end;

function TConfigDataProviderWithUseDepreciated.ReadBinaryStream(
  const AIdent: string; AValue: TStream): Integer;
var
  VIdent: string;
begin
  Result := FSource.ReadBinaryStream(AIdent, AValue);
  if Result <= 0 then begin
    VIdent := GetDepreciatedName(AIdent);
    if VIdent <> '' then begin
      Result := FSource.ReadBinaryStream(VIdent, AValue);
    end;
  end;
end;

function TConfigDataProviderWithUseDepreciated.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadBool(VIdent, Result);
  end;
  Result := FSource.ReadBool(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadDate(VIdent, Result);
  end;
  Result := FSource.ReadDate(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadDateTime(VIdent, Result);
  end;
  Result := FSource.ReadDateTime(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadFloat(VIdent, Result);
  end;
  Result := FSource.ReadFloat(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadInteger(VIdent, Result);
  end;
  Result := FSource.ReadInteger(AIdent, Result);
end;

function TConfigDataProviderWithUseDepreciated.ReadString(const AIdent,
  ADefault: string): string;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadString(VIdent, Result);
  end;
  Result := FSource.ReadString(AIdent, Result);
end;

procedure TConfigDataProviderWithUseDepreciated.ReadSubItemsList(AList: TStrings);
begin
  FSource.ReadSubItemsList(AList);
end;

function TConfigDataProviderWithUseDepreciated.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
begin
  Result := ADefault;
  VIdent := GetDepreciatedName(AIdent);
  if VIdent <> '' then begin
    Result := FSource.ReadTime(VIdent, Result);
  end;
  Result := FSource.ReadTime(AIdent, Result);
end;

procedure TConfigDataProviderWithUseDepreciated.ReadValuesList(AList: TStrings);
begin
  FSource.ReadValuesList(AList);
end;

end.
