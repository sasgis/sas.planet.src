unit u_AppearanceOfMarkPolygon;

interface

uses
  GR32,
  t_Hash,
  i_Appearance,
  i_AppearanceOfVectorItem,
  u_BaseInterfacedObject;

type
  TAppearanceOfMarkPolygon = class(TBaseInterfacedObject, IAppearance, IAppearancePolygonBorder, IAppearancePolygonFill)
  private
    FHashCommon: THashValue;
    FHashBorder: THashValue;
    FHashFill: THashValue;
    FLineColor: TColor32;
    FLineWidth: Integer;
    FFillColor: TColor32;
  private
    function GetHashCommon: THashValue;
    function GetHashBorder: THashValue;
    function GetHashFill: THashValue;
  private { IAppearance }
    function IAppearance.GetHash = GetHashCommon;
    function IsEqual(const AValue: IAppearance): Boolean;
  private { IAppearancePolygonBorder }
    function IAppearancePolygonBorder.GetHash = GetHashBorder;
    function GetLineColor: TColor32;
    function GetLineWidth: Integer;
  private { IAppearancePolygonFill }
    function IAppearancePolygonFill.GetHash = GetHashFill;
    function GetFillColor: TColor32;
  public
    constructor Create(
      const AHashCommon: THashValue;
      const AHashBorder: THashValue;
      const AHashFill: THashValue;
      const ALineColor: TColor32;
      const ALineWidth: Integer;
      const AFillColor: TColor32
    );
  end;

implementation

{ TAppearanceOfMarkPolygon }

constructor TAppearanceOfMarkPolygon.Create(
  const AHashCommon, AHashBorder, AHashFill: THashValue;
  const ALineColor: TColor32;
  const ALineWidth: Integer;
  const AFillColor: TColor32
);
begin
  inherited Create;
  FHashCommon := AHashCommon;
  FHashBorder := AHashBorder;
  FHashFill := AHashFill;
  FLineColor := ALineColor;
  FLineWidth := ALineWidth;
  FFillColor := AFillColor;
end;

function TAppearanceOfMarkPolygon.GetFillColor: TColor32;
begin
  Result := FFillColor;
end;

function TAppearanceOfMarkPolygon.GetHashBorder: THashValue;
begin
  Result := FHashBorder;
end;

function TAppearanceOfMarkPolygon.GetHashCommon: THashValue;
begin
  Result := FHashCommon;
end;

function TAppearanceOfMarkPolygon.GetHashFill: THashValue;
begin
  Result := FHashFill;
end;

function TAppearanceOfMarkPolygon.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TAppearanceOfMarkPolygon.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

function TAppearanceOfMarkPolygon.IsEqual(const AValue: IAppearance): Boolean;
begin
  if not Assigned(AValue) then begin
    Result := False;
  end else begin
    Result := FHashCommon = AValue.Hash;
  end;
end;

end.
