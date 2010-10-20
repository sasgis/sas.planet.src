unit u_GPSState;

interface

uses
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  TGPSpar = class
  public
    speed: extended;
    len: extended;
    sspeed: extended;
    allspeed: extended;
    sspeednumentr: integer;
    altitude: extended;
    maxspeed: extended;
    nap: integer;
    azimut: extended;
    Odometr: extended;
    Odometr2: extended;
    SignalStrength: extended;
    SatCount: integer;

    GPS_enab: Boolean;
    //COM-порт, к которому подключен GPS
    GPS_COM: string;
    //Скорость GPS COM порта
    GPS_BaudRate: Integer;
    // Максимальное время ожидания данных от GPS
    GPS_TimeOut: integer;

    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;

    function GetSatActive(pcode:integer;NMEA:string):boolean;
  end;

implementation

uses
  SysUtils,
  StrUtils;

function TGPSpar.GetSatActive(pcode:integer;NMEA:string):boolean;
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

procedure TGPSpar.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  VConfigProvider := AConfigProvider.GetSubItem('GPS');
  if VConfigProvider <> nil then begin
    GPS_enab := VConfigProvider.ReadBool('enbl', false);
    GPS_COM := VConfigProvider.ReadString('com', 'COM0');
    GPS_BaudRate := VConfigProvider.ReadInteger('BaudRate',4800);
    GPS_TimeOut := VConfigProvider.ReadInteger('timeout',300);
  end else begin
    GPS_enab := False;
    GPS_COM := 'COM0';
    GPS_BaudRate := 4800;
    GPS_TimeOut := 300;
  end;
end;

procedure TGPSpar.SaveConfig(AConfigProvider: IConfigDataWriteProvider);
var
  VConfigProvider: IConfigDataWriteProvider;
begin
  inherited;
  VConfigProvider := AConfigProvider.GetOrCreateSubItem('GPS');
  VConfigProvider.WriteBool('enbl', GPS_enab);
  VConfigProvider.WriteString('COM', GPS_COM);
  VConfigProvider.WriteInteger('BaudRate',GPS_BaudRate);
  // Нет сохранения GPS_TimeOut
  
end;

procedure TGPSpar.SendTerminateToThreads;
begin
  // Пока ничего не делаем
end;

procedure TGPSpar.StartThreads;
begin
  // Пока ничего не делаем
end;

end.
