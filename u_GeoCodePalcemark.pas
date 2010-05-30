unit u_GeoCodePalcemark;

interface

uses
  ActiveX,
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCodePalcemark = class(TInterfacedObject, IGeoCodePalcemark)
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

{ TGeoCodePalcemark }

constructor TGeoCodePalcemark.Create(APoint: TDoublePoint;
  AAddress: WideString; AAccuracy: Integer);
begin
  FAddress := AAddress;
  FPoint := APoint;
  FAccuracy := AAccuracy;
end;

destructor TGeoCodePalcemark.Destroy;
begin
  FAddress := '';
  inherited;
end;

function TGeoCodePalcemark.GetAccuracy: Integer;
begin
  Result := FAccuracy;
end;

function TGeoCodePalcemark.GetAddress: WideString;
begin
  Result := FAddress;
end;

function TGeoCodePalcemark.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

end.
 