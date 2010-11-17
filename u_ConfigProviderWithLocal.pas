unit u_ConfigProviderWithLocal;

interface

uses
  Classes,
  i_IConfigDataProvider;

type
  TConfigProviderWithLocal = class(TInterfacedObject, IConfigDataProvider)
  private
    FProviderMain: IConfigDataProvider;
    FProviderLocal: IConfigDataProvider;
    function PrepareIdent(const AIdent: string; var AUseMain, AUseLocal: Boolean): string;
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

  end;

implementation

uses
  StrUtils,
  SysUtils;

{ TConfigProviderWithLocal }

function TConfigProviderWithLocal.GetSubItem(
  const AIdent: string): IConfigDataProvider;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
  VSubItemMain: IConfigDataProvider;
  VSubItemLocal: IConfigDataProvider;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);
  if VUseMain then begin
    if FProviderMain <> nil then begin
      VSubItemMain := FProviderMain.GetSubItem(VIdent);
    end;
  end;
  if VUseLocal then begin
    if FProviderLocal <> nil then begin
      VSubItemLocal := FProviderLocal.GetSubItem(VIdent);
    end;
  end;
  if VUseLocal and VUseMain then begin
    if (VSubItemMain = nil) and (VSubItemLocal = nil) then begin
      Result := nil;
    end else begin
      Result := TConfigProviderWithLocal.Create;
    end;
  end else if VUseLocal then begin
    Result := VSubItemLocal;
  end else if VUseMain then begin
    Result := VSubItemMain;
  end else begin
    Result := nil;
  end;
end;

function TConfigProviderWithLocal.PrepareIdent(
  const AIdent: string;
  var AUseMain,
  AUseLocal: Boolean): string;
var
  VPrefix: string;
  VSeparatorPos: Integer;
  VIdentLength: Integer;
begin
  VSeparatorPos := Pos(':', AIdent);
  if VSeparatorPos > 0 then begin
    VIdentLength := Length(AIdent);
    VPrefix := UpperCase(LeftStr(AIdent, VSeparatorPos));
    Result := MidStr(AIdent, VSeparatorPos + 1, VIdentLength - VSeparatorPos - 1);
  end else begin
    Result := AIdent;
  end;
  AUseMain := True;
  AUseLocal := True;
  if VPrefix = 'LOCAL' then begin
    AUseMain := False;
  end else if VPrefix = 'MAIN' then begin
    AUseLocal := False;
  end;
end;

function TConfigProviderWithLocal.ReadBinaryStream(const AIdent: string;
  AValue: TStream): Integer;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);
  Result := 0;
  if VUseLocal and (FProviderLocal <> nil) then begin
    Result := FProviderLocal.ReadBinaryStream(VIdent, AValue);
  end;
  if (Result = 0) and VUseMain and (FProviderMain <> nil) then begin
    Result := FProviderMain.ReadBinaryStream(VIdent, AValue);
  end;
end;

function TConfigProviderWithLocal.ReadBool(const AIdent: string;
  const ADefault: Boolean): Boolean;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);
  Result := ADefault;
  if VUseMain and (FProviderMain <> nil) then begin
    Result := FProviderMain.ReadBool(VIdent, Result);
  end;
  if VUseLocal and (FProviderLocal <> nil) then begin
    Result := FProviderLocal.ReadBool(VIdent, Result);
  end;
end;

function TConfigProviderWithLocal.ReadDate(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

function TConfigProviderWithLocal.ReadDateTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

function TConfigProviderWithLocal.ReadFloat(const AIdent: string;
  const ADefault: Double): Double;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

function TConfigProviderWithLocal.ReadInteger(const AIdent: string;
  const ADefault: Integer): Longint;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

function TConfigProviderWithLocal.ReadString(const AIdent,
  ADefault: string): string;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

procedure TConfigProviderWithLocal.ReadSubItemsList(AList: TStrings);
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

function TConfigProviderWithLocal.ReadTime(const AIdent: string;
  const ADefault: TDateTime): TDateTime;
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

procedure TConfigProviderWithLocal.ReadValuesList(AList: TStrings);
var
  VIdent: string;
  VUseLocal: Boolean;
  VUseMain: Boolean;
begin
  VIdent := PrepareIdent(AIdent, VUseLocal, VUseLocal);

end;

end.
