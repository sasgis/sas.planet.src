unit u_GeoCodePalcemark;

interface

uses
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCodePalcemark = class(TInterfacedObject, IGeoCodePalcemark)
  private
    FPoint: TExtendedPoint;
    FAddress: WideString;
    FAccuracy: Integer;
    function GetPoint: TExtendedPoint; safecall;
    function GetAddress: WideString; safecall;
    function GetAccuracy: Integer; safecall;
  public
    constructor Create(
      APoint: TExtendedPoint;
      AAddress: WideString;
      AAccuracy: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGeoCodePalcemark }

constructor TGeoCodePalcemark.Create(APoint: TExtendedPoint;
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

function TGeoCodePalcemark.GetPoint: TExtendedPoint;
begin
  Result := FPoint;
end;

end.
 