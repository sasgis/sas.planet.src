unit u_GPSSatellitesInView;

interface

uses
  SysUtils,
  i_GPS;

type
  TArrayOfIGPSSatelliteInfo = array of IGPSSatelliteInfo;

  TGPSSatellitesInView = class(TInterfacedObject, IGPSSatellitesInView)
  private
    FFixCount: Integer;
    FItems: TArrayOfIGPSSatelliteInfo;
  protected
    function GetCount: Integer; stdcall;
    function GetFixCount: Integer; stdcall;
    function GetItem(AIndex: Integer): IGPSSatelliteInfo; stdcall;
  public
    constructor Create(
      AFixCount: Integer;
      AItems: TArrayOfIGPSSatelliteInfo
    );
    destructor Destroy; override;
  end;

implementation
{ TGPSSatellitesInView }

constructor TGPSSatellitesInView.Create(
  AFixCount: Integer; AItems: TArrayOfIGPSSatelliteInfo);
begin
  FFixCount := AFixCount;
  FItems := AItems;
end;

destructor TGPSSatellitesInView.Destroy;
begin
  FItems := nil;
  inherited;
end;

function TGPSSatellitesInView.GetCount: Integer;
begin
  Result := Length(FItems);
end;

function TGPSSatellitesInView.GetFixCount: Integer;
begin
  Result := FFixCount;
end;

function TGPSSatellitesInView.GetItem(AIndex: Integer): IGPSSatelliteInfo;
begin
  Result := FItems[AIndex];
end;

end.
