unit u_GPSPositionByZylGPS;

interface

uses
  u_GPSPositionUpdatable;

type
  TGPSPositionByZylGPS = class(TGPSPositionUpdatable)
  protected
    procedure GPSReceiver1SatellitesReceive(Sender: TObject);
    procedure GPSReceiverReceive(Sender: TObject; Buffer: string);
    function GetSatActive(pcode:integer;NMEA:string):boolean;
  end;
implementation

uses
  StrUtils,
  SysUtils,
  ZylCustomGPSReceiver,
  t_GeoTypes;

{ TGPSPositionByZylGPS }

const
  CMaxSatCount = 32;

function TGPSPositionByZylGPS.GetSatActive(pcode: integer;
  NMEA: string): boolean;
var str:string;
    i,j,count:integer;
begin
  i:=Pos('GPGSA',NMEA);
  if i<>0 then begin
    i:=PosEx(',',NMEA,i+6);
    i:=PosEx(',',NMEA,i+1);
    count:=0;
    str:='0';
    while (i<>0)and(str<>'')and(str<>',')and(count<12)and(strtoint(str)<>pcode) do begin
      j:=PosEx(',',NMEA,i+1);
      str:=copy(NMEA,i+1,j-i-1);
      inc(count);
      i:=j;
    end;
    if (i<>0)and(str<>'')and(str<>',')and(count<12) then begin
      result:=pcode=strtoint(str);
    end else begin
      result:=false;
    end;
  end else begin
    result:=false;
  end;
end;

procedure TGPSPositionByZylGPS.GPSReceiver1SatellitesReceive(Sender: TObject);
var
  VReceiver: TZylCustomGPSReceiver;
  VFixed: array [0..CMaxSatCount - 1] of Boolean;
  VSatCount: Integer;
  VFixCount: Integer;
  i: Integer;
  VSatellites: TSatellites;
  VSatellite: TSatellite;
begin
  if Sender is TZylCustomGPSReceiver then begin
    VReceiver := TZylCustomGPSReceiver(Sender);
    Lock;
    try
      VSatellites := VReceiver.GetSatellites;
      VSatCount := VSatellites.Count;
      VFixCount := 0;
      for i := 0 to VSatCount - 1 do begin
        VFixed[i] := GetSatActive(VSatellites[i].PseudoRandomCode, VReceiver.GetRawData);
        if VFixed[i] then begin
          Inc(VFixCount);
        end;
      end;
      _UpdateSatellitesCount(VFixCount, VSatCount);
      for i := 0 to VSatCount - 1 do begin
        VSatellite := VSatellites[i];
        _UpdateSattelite(
          i,
          VSatellite.PseudoRandomCode,
          VSatellite.Elevation,
          VSatellite.Azimuth,
          VSatellite.SignalToNoiseRatio,
          VFixed[i]
        );
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TGPSPositionByZylGPS.GPSReceiverReceive(Sender: TObject;
  Buffer: string);
var
  VReceiver: TZylCustomGPSReceiver;
  VPoint: TExtendedPoint;
begin
  if Sender is TZylCustomGPSReceiver then begin
    VReceiver := TZylCustomGPSReceiver(Sender);
    Lock;
    try
      VPoint.X := VReceiver.GetLongitudeAsDecimalDegrees;
      VPoint.Y := VReceiver.GetLatitudeAsDecimalDegrees;
      _UpdatePosition(
        VPoint,
        VReceiver.GetAltitude,
        VReceiver.GetSpeed_KMH,
        VReceiver.GetHeading,
        VReceiver.GetUTCDateTime,
        VReceiver.GetLocalDateTime,
        VReceiver.IsFix,
        VReceiver.GetHDOP,
        VReceiver.GetVDOP,
        VReceiver.GetPDOP
      );
    finally
      UnLock;
    end;
  end;
end;

end.
