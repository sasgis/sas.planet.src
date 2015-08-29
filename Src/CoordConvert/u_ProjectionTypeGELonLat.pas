unit u_ProjectionTypeGELonLat;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Datum,
  u_ProjectionTypeBase;

type
  TProjectionTypeGELonLat = class(TProjectionTypeBase)
  protected
    function Relative2LonLatInternal(const APoint: TDoublePoint): TDoublePoint; override;
    function LonLat2RelativeInternal(const APoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(
      const AHash: THashValue;
      const ADatum: IDatum;
      const AProjEPSG: integer
    );
  end;

implementation

uses
  Math;

{ TProjectionTypeGELonLat }

constructor TProjectionTypeGELonLat.Create(
  const AHash: THashValue;
  const ADatum: IDatum;
  const AProjEPSG: integer
);
begin
  inherited Create(AHash, ADatum, AProjEPSG);
end;

function TProjectionTypeGELonLat.LonLat2RelativeInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.x := (0.5 + APoint.x / 360);
  Result.y := (0.5 - APoint.y / 360);
end;

function TProjectionTypeGELonLat.Relative2LonLatInternal(
  const APoint: TDoublePoint
): TDoublePoint;
begin
  Result.X := (APoint.x - 0.5) * 360;
  Result.y := -(APoint.y - 0.5) * 360;
end;

end.

