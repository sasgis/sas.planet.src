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
    FDesc: WideString;
    FFullDesc: WideString;
    FAccuracy: Integer;
    function GetPoint: TDoublePoint; safecall;
    function GetAddress: WideString; safecall;
    function GetDesc: WideString; safecall;
    function GetFullDesc: WideString; safecall;
    function GetAccuracy: Integer; safecall;
  public
    constructor Create(
      APoint: TDoublePoint;
      AAddress: WideString;
      ADesc: WideString;
      AFullDesc: WideString;
      AAccuracy: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGeoCodePlacemark }

constructor TGeoCodePlacemark.Create(APoint: TDoublePoint;
  AAddress: WideString; ADesc: WideString; AFullDesc: WideString; AAccuracy: Integer);
begin
  FAddress := AAddress;
  FDesc := ADesc;
  FFullDesc := AFullDesc;
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

function TGeoCodePlacemark.GetDesc: WideString;
begin
  Result := FDesc;
end;

function TGeoCodePlacemark.GetFullDesc: WideString;
begin
  Result := FFullDesc;
end;

function TGeoCodePlacemark.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

end.
 