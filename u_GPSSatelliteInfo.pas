unit u_GPSSatelliteInfo;

interface

uses
  i_GPS;

type
  TGPSSatelliteInfo = class(TInterfacedObject, IGPSSatelliteInfo)
  private
    FPseudoRandomCode: Integer;
    FElevation: Integer;
    FAzimuth: Integer;
    FSignalToNoiseRatio: Integer;
    FIsFix: Boolean;
  protected
    function GetPseudoRandomCode: Integer; stdcall;
    function GetElevation: Integer; stdcall;
    function GetAzimuth: Integer; stdcall;
    function GetSignalToNoiseRatio: Integer; stdcall;
    function GetIsFix: Boolean; stdcall;
  public
    constructor Create(
      APseudoRandomCode: Integer;
      AElevation: Integer;
      AAzimuth: Integer;
      ASignalToNoiseRatio: Integer;
      AIsFix: Boolean
    );
  end;


implementation

{ TGPSSatelliteInfo }

constructor TGPSSatelliteInfo.Create(APseudoRandomCode, AElevation, AAzimuth,
  ASignalToNoiseRatio: Integer; AIsFix: Boolean);
begin
  FPseudoRandomCode := APseudoRandomCode;
  FElevation := AElevation;
  FAzimuth := AAzimuth;
  FSignalToNoiseRatio := ASignalToNoiseRatio;
  FIsFix := AIsFix;
end;

function TGPSSatelliteInfo.GetAzimuth: Integer;
begin
  Result := FAzimuth;
end;

function TGPSSatelliteInfo.GetElevation: Integer;
begin
  Result := FElevation;
end;

function TGPSSatelliteInfo.GetIsFix: Boolean;
begin
  Result := FIsFix;
end;

function TGPSSatelliteInfo.GetPseudoRandomCode: Integer;
begin
  Result := FPseudoRandomCode;
end;

function TGPSSatelliteInfo.GetSignalToNoiseRatio: Integer;
begin
  Result := FSignalToNoiseRatio;
end;

end.
