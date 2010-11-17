unit u_ConfigDataProviderWithReplacedSubItem;

interface

uses
  Classes,
  i_IConfigDataProvider;

type
  TConfigDataProviderWithReplacedSubItem = class(TInterfacedObject, IConfigDataProvider)
  private
    FSource: IConfigDataProvider;
    FSubItemName: string;
    FSubItem: IConfigDataProvider;
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
      ASubItemName: string;
      ASubItem: IConfigDataProvider
    );
    destructor Destroy; override;
  end;

implementation

{ TConfigDataProviderWithReplacedSubItem }

constructor TConfigDataProviderWithReplacedSubItem.Create(
  ASource: IConfigDataProvider;
  ASubItemName: string;
  ASubItem: IConfigDataProvider
);
begin
  FSource := ASource;
  FSubItemName := ASubItemName;
  FSubItem := ASubItem;
end;

destructor TConfigDataProviderWithReplacedSubItem.Destroy;
begin
  FSubItem := nil;
  FSource := nil;
  inherited;
end;

function TConfigDataProviderWithReplacedSubItem.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  if AIdent = FSubItemName then begin
    Result := FSubItem;
  end else begin
    Result := FSource.GetSubItem(AIdent);
  end;
end;

function TConfigDataProviderWithReplacedSubItem.ReadBinaryStream(
  const AIdent: string; AValue: TStream): Integer;
begin
  Result := FSource.ReadBinaryStream(AIdent, AValue);
end;

function TConfigDataProviderWithReplacedSubItem.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := FSource.ReadBool(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := FSource.ReadDate(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadDateTime(
  const AIdent: string; const ADefault: TDateTime): TDateTime;
begin
  Result := FSource.ReadDateTime(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := FSource.ReadFloat(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadInteger(
  const AIdent: string; const ADefault: Integer): Longint;
begin
  Result := FSource.ReadInteger(AIdent, ADefault);
end;

function TConfigDataProviderWithReplacedSubItem.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := FSource.ReadString(AIdent, ADefault);
end;

procedure TConfigDataProviderWithReplacedSubItem.ReadSubItemsList(
  AList: TStrings);
var
  VIndex: Integer;
begin
  FSource.ReadSubItemsList(AList);
  VIndex := AList.IndexOf(FSubItemName);
  if VIndex < 0 then begin
    if FSubItem <> nil then begin
      AList.Add(FSubItemName);
    end;
  end else begin
    if FSubItem = nil then begin
      AList.Delete(VIndex);
    end;
  end;
end;

function TConfigDataProviderWithReplacedSubItem.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := FSource.ReadTime(AIdent, ADefault);
end;

procedure TConfigDataProviderWithReplacedSubItem.ReadValuesList(
  AList: TStrings);
begin
  FSource.ReadValuesList(AList);
end;

end.
