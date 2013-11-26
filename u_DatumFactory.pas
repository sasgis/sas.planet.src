unit u_DatumFactory;

interface

uses
  i_Datum,
  i_HashFunction,
  i_CoordConverter,
  i_ConfigDataProvider,
  i_CoordConverterFactory,
  u_BaseInterfacedObject;

type
  TDatumFactory = class(TBaseInterfacedObject, IDatumFactory)
  private
    FHashFunction: IHashFunction;
    FDatumGoogle: IDatum;
    FDatumYandex: IDatum;
    FDatum53004: IDatum;
  private
    function GetByCode(ADatumEPSG: Integer): IDatum;
    function GetByRadius(const ARadiusA, ARadiusB: Double): IDatum;
  public
    constructor Create(
      const AHashFunction: IHashFunction
    );
  end;

implementation

uses
  t_Hash,
  c_CoordConverter,
  u_Datum;

{ TDatumFactory }

constructor TDatumFactory.Create(
  const AHashFunction: IHashFunction
);
var
  VRadiusA: Double;
  VRadiusB: Double;
  VHash: THashValue;
begin
  inherited Create;
  FHashFunction := AHashFunction;

  VRadiusA := 6378137;
  VRadiusB := VRadiusA;
  VHash := FHashFunction.CalcHashByDouble(VRadiusA);
  FHashFunction.UpdateHashByDouble(VHash, VRadiusB);
  FDatumGoogle := TDatum.Create(VHash, CGoogleDatumEPSG, VRadiusA, VRadiusA);

  VRadiusA := 6378137;
  VRadiusB := 6356752;
  VHash := FHashFunction.CalcHashByDouble(VRadiusA);
  FHashFunction.UpdateHashByDouble(VHash, VRadiusB);
  FDatumYandex := TDatum.Create(VHash, CYandexDatumEPSG, VRadiusA, VRadiusB);

  VRadiusA := 6371000;
  VRadiusB := VRadiusA;
  VHash := FHashFunction.CalcHashByDouble(VRadiusA);
  FHashFunction.UpdateHashByDouble(VHash, VRadiusB);
  FDatum53004 := TDatum.Create(VHash, 53004, VRadiusA, VRadiusA);
end;

function TDatumFactory.GetByCode(ADatumEPSG: Integer): IDatum;
begin
  Result := nil;
  case ADatumEPSG of
    CGoogleDatumEPSG: begin
      Result := FDatumGoogle;
    end;
    CYandexDatumEPSG: begin
      Result := FDatumYandex;
    end;
    53004: begin
      Result := FDatum53004;
    end;
  end;
end;

function TDatumFactory.GetByRadius(
  const ARadiusA, ARadiusB: Double
): IDatum;
var
  VEPSG: Integer;
  VHash: THashValue;
begin
  VEPSG := 0;
  if (Abs(ARadiusA - 6378137) < 1) and (Abs(ARadiusB - 6378137) < 1) then begin
    VEPSG := CGoogleDatumEPSG;
  end;
  if (Abs(ARadiusA - 6378137) < 1) and (Abs(ARadiusB - 6356752) < 1) then begin
    VEPSG := CYandexDatumEPSG;
  end;
  if VEPSG > 0 then begin
    Result := GetByCode(VEPSG);
  end else begin
    VHash := FHashFunction.CalcHashByDouble(ARadiusA);
    FHashFunction.UpdateHashByDouble(VHash, ARadiusB);
    Result := TDatum.Create(VHash, 0, ARadiusA, ARadiusB);
  end;
end;

end.
