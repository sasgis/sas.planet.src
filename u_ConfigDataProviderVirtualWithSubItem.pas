unit u_ConfigDataProviderVirtualWithSubItem;

interface

uses
  Classes,
  i_IConfigDataProvider;

type
  TConfigDataProviderVirtualWithSubItem = class(TInterfacedObject, IConfigDataProvider)
  private
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
      ASubItemName: string;
      ASubItem: IConfigDataProvider
    );
    destructor Destroy; override;
  end;

implementation

{ TConfigDataProviderVirtualWithSubItem }

constructor TConfigDataProviderVirtualWithSubItem.Create(ASubItemName: string;
  ASubItem: IConfigDataProvider);
begin
  FSubItemName := ASubItemName;
  FSubItem := ASubItem;
end;

destructor TConfigDataProviderVirtualWithSubItem.Destroy;
begin
  FSubItem := nil;
  inherited;
end;

function TConfigDataProviderVirtualWithSubItem.GetSubItem(
  const AIdent: string): IConfigDataProvider;
begin
  if AIdent = FSubItemName then begin
    Result := FSubItem;
  end else begin
    Result := nil;
  end;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBinaryStream(
  const AIdent: string; AValue: TStream): Integer;
begin
  Result := 0;
end;

function TConfigDataProviderVirtualWithSubItem.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
begin
  Result := ADefault;
end;

function TConfigDataProviderVirtualWithSubItem.ReadString(const AIdent,
  ADefault: string): string;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderVirtualWithSubItem.ReadSubItemsList(AList: TStrings);
begin
  AList.Clear;
  AList.Add(FSubItemName);
end;

function TConfigDataProviderVirtualWithSubItem.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
begin
  Result := ADefault;
end;

procedure TConfigDataProviderVirtualWithSubItem.ReadValuesList(AList: TStrings);
begin
  AList.Clear;
end;

end.
