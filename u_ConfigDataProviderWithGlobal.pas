unit u_ConfigDataProviderWithGlobal;

interface

uses
  Classes,
  i_ConfigDataProvider;

type
  TConfigDataProviderWithGlobal = class(TInterfacedObject, IConfigDataProvider)
  private
    FProviderMain: IConfigDataProvider;
    FProviderGlobalPrefix: string;
    FProviderGlobalPrefixLen: Integer;
    FProviderGlobal: IConfigDataProvider;
  protected
    property ProviderGlobalPrefix: string read FProviderGlobalPrefix;
    property ProviderGlobal: IConfigDataProvider read FProviderGlobal;
    function PrepareIdent(const AIdent: string; out AUseMain: Boolean): string;
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
      AProviderMain: IConfigDataProvider;
      AProviderGlobalPrefix: string;
      AProviderGlobal: IConfigDataProvider
    );
    destructor Destroy; override;
  public

  end;

implementation

uses
  StrUtils,
  SysUtils;

{ TConfigDataProviderWithGlobal }

constructor TConfigDataProviderWithGlobal.Create(
  AProviderMain: IConfigDataProvider;
  AProviderGlobalPrefix: string;
  AProviderGlobal: IConfigDataProvider
);
begin
  FProviderMain := AProviderMain;
  FProviderGlobalPrefix := AProviderGlobalPrefix;
  FProviderGlobalPrefixLen := Length(AProviderGlobalPrefix);
  FProviderGlobal := AProviderGlobal;
end;

destructor TConfigDataProviderWithGlobal.Destroy;
begin
  FProviderMain := nil;
  FProviderGlobal := nil;
  inherited;
end;

function TConfigDataProviderWithGlobal.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VIdent: string;
  VUseMain: Boolean;
  VSubItemMain: IConfigDataProvider;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      VSubItemMain := FProviderMain.GetSubItem(VIdent);
      Result := TConfigDataProviderWithGlobal.Create(VSubItemMain, FProviderGlobalPrefix, FProviderGlobal);
    end else begin
      Result := Self;
    end;
  end else begin
    Result := FProviderGlobal.GetSubItem(VIdent);
  end;
end;

function TConfigDataProviderWithGlobal.PrepareIdent(
  const AIdent: string;
  out AUseMain: Boolean): string;
begin
  if LeftStr(AIdent, FProviderGlobalPrefixLen) = FProviderGlobalPrefix then begin
    Result := MidStr(AIdent, FProviderGlobalPrefixLen, Length(AIdent));
    AUseMain := False;
  end else begin
    Result := AIdent;
    AUseMain := True;
  end;
end;

function TConfigDataProviderWithGlobal.ReadBinaryStream(const AIdent: string;
  AValue: TStream): Integer;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := 0;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadBinaryStream(VIdent, AValue);
    end;
  end else begin
    Result := FProviderGlobal.ReadBinaryStream(VIdent, AValue);
  end;
end;

function TConfigDataProviderWithGlobal.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadBool(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadBool(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadDate(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadDate(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadDateTime(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadDateTime(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadFloat(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadFloat(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadInteger(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadInteger(VIdent, Result);
  end;
end;

function TConfigDataProviderWithGlobal.ReadString(const AIdent,
  ADefault: string): string;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadString(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadString(VIdent, Result);
  end;
end;

procedure TConfigDataProviderWithGlobal.ReadSubItemsList(AList: TStrings);
begin
  if FProviderMain <> nil then begin
    FProviderMain.ReadSubItemsList(AList);
  end else begin
    AList.Clear;
  end;
end;

function TConfigDataProviderWithGlobal.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseMain);
  Result := ADefault;
  if VUseMain then begin
    if (FProviderMain <> nil) then begin
      Result := FProviderMain.ReadTime(VIdent, Result);
    end;
  end else begin
    Result := FProviderGlobal.ReadTime(VIdent, Result);
  end;
end;

procedure TConfigDataProviderWithGlobal.ReadValuesList(AList: TStrings);
begin
  if (FProviderMain <> nil) then begin
    FProviderMain.ReadValuesList(AList);
  end else begin
    AList.Clear;
  end;
end;

end.
