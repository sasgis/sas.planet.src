unit u_GeoCodePlacemark;

interface

uses
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCodePlacemark = class(TInterfacedObject, IGeoCodePlacemark)
  private
    FPoint: TDoublePoint;
    FAddress: WideString;
    FAccuracy: Integer;
    function GetPoint: TDoublePoint; safecall;
    function GetAddress: WideString; safecall;
    function GetAccuracy: Integer; safecall;
  public
    constructor Create(
      APoint: TDoublePoint;
      AAddress: WideString;
      AAccuracy: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGeoCodePlacemark }

constructor TGeoCodePlacemark.Create(APoint: TDoublePoint;
  AAddress: WideString; AAccuracy: Integer);
begin
  FAddress := AAddress;
  FPoint := APoint;
  FAccuracy := AAccuracy;
end;

destructor TGeoCodePlacemark.Destroy;
begin
  FAddress := '';
  inherited;
end;

function TGeoCodePlacemark.GetAccuracy: Integer;
begin
  Result := FAccuracy;
end;

function TGeoCodePlacemark.GetAddress: WideString;
begin
  Result := FAddress;
end;

function TGeoCodePlacemark.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

end.
 