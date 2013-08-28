unit u_AppearanceOfMarkLine;

interface

uses
  GR32,
  t_Hash,
  i_Appearance,
  i_AppearanceOfVectorItem,
  u_BaseInterfacedObject;

type
  TAppearanceOfMarkLine = class(TBaseInterfacedObject, IAppearance, IAppearanceLine)
  private
    FHash: THashValue;
    FLineColor: TColor32;
    FLineWidth: Integer;
  private { IAppearance }
    function GetHash: THashValue;
    function IsEqual(const AValue: IAppearance): Boolean;
  private { IAppearanceLine }
    function GetLineColor: TColor32;
    function GetLineWidth: Integer;
  public
    constructor Create(
      const AHash: THashValue;
      const ALineColor: TColor32;
      const ALineWidth: Integer
    );
  end;

implementation

{ TAppearanceOfMarkLine }

constructor TAppearanceOfMarkLine.Create(
  const AHash: THashValue;
  const ALineColor: TColor32;
  const ALineWidth: Integer
);
begin
  inherited Create;
  FHash := AHash;
  FLineColor := ALineColor;
  FLineWidth := ALineWidth;
end;

function TAppearanceOfMarkLine.GetHash: THashValue;
begin
  Result := FHash;
end;

function TAppearanceOfMarkLine.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TAppearanceOfMarkLine.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

function TAppearanceOfMarkLine.IsEqual(const AValue: IAppearance): Boolean;
begin
  if not Assigned(AValue) then begin
    Result := False;
  end else begin
    Result := FHash = AValue.Hash;
  end;
end;

end.
