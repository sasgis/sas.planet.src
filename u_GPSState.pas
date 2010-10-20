unit u_GPSState;

interface

uses
  Graphics,
  i_IGPSRecorder,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider;

type
  TGPSpar = class
  private
    FGPSRecorder: IGPSRecorder;
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
    // Интервал между точками от GPS
    GPS_Delay: Integer;
    //Размер указателя направления при GPS-навигации
    GPS_ArrowSize: Integer;
    //Цвет указателя направления при навигацци
    GPS_ArrowColor: TColor;
    // Толщина отображемого GPS трека
    GPS_TrackWidth: Integer;
    //Максимальное количество оотображаемых точек трека
    GPS_NumTrackPoints: integer;
    //Отображать GPS трек
    GPS_ShowPath: Boolean;
    //Центрировать карту на GPS позиции
    GPS_MapMove: Boolean;
    GPS_MapMoveCentered: Boolean;
    //Заисывать GPS трек в файл
    GPS_WriteLog: boolean;
    //Файл для записи GPS трека (Нужно будет заменить отдельным объектом)
    GPS_LogFile: TextFile;
    //Скрывать/показывать панель датчиков при подключении/отключении GPS
    GPS_SensorsAutoShow: boolean;
    //Писать лог NMEA
    GPS_NMEALog: boolean;
    constructor Create();
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;

    function GetSatActive(pcode:integer;NMEA:string):boolean;
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  u_GPSRecorderStuped;

constructor TGPSpar.Create;
begin
  FGPSRecorder := TGPSRecorderStuped.Create;
end;

destructor TGPSpar.Destroy;
begin
  FGPSRecorder := nil;
  inherited;
end;

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
    GPS_BaudRate := VConfigProvider.ReadInteger('BaudRate', 4800);
    GPS_TimeOut := VConfigProvider.ReadInteger('timeout', 300);
    GPS_Delay := VConfigProvider.ReadInteger('update', 1000);
    GPS_ArrowSize := VConfigProvider.ReadInteger('SizeStr', 25);
    GPS_ArrowColor := VConfigProvider.ReadInteger('ColorStr', clRed);
    GPS_TrackWidth := VConfigProvider.ReadInteger('SizeTrack', 5);
    GPS_NumTrackPoints := VConfigProvider.ReadInteger('NumShowTrackPoints', 5000);
    GPS_WriteLog:=VConfigProvider.ReadBool('log',true);
    GPS_NMEALog:=VConfigProvider.ReadBool('NMEAlog',false);
    GPS_ShowPath:=VConfigProvider.ReadBool('path',true);
    GPS_MapMove:=VConfigProvider.ReadBool('go',true);
    GPS_MapMoveCentered:=VConfigProvider.ReadBool('goCentered',false);
    GPS_SensorsAutoShow:=VConfigProvider.ReadBool('SensorsAutoShow',true);

    Odometr:=VConfigProvider.ReadFloat('Odometr',0);
    Odometr2:=VConfigProvider.ReadFloat('Odometr2',0);
  end else begin
    GPS_enab := False;
    GPS_COM := 'COM0';
    GPS_BaudRate := 4800;
    GPS_TimeOut := 300;
    GPS_Delay := 1000;
    GPS_ArrowSize := 25;
    GPS_ArrowColor := clRed;
    GPS_TrackWidth := 5;
    GPS_NumTrackPoints := 5000;
    GPS_WriteLog:=true;
    GPS_NMEALog:=false;
    GPS_ShowPath:=true;
    GPS_MapMove:=true;
    GPS_MapMoveCentered:=false;
    GPS_SensorsAutoShow:=true;
    Odometr:=0;
    Odometr2:=0;
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
  VConfigProvider.WriteInteger('update',GPS_Delay);
  VConfigProvider.WriteInteger('SizeStr',GPS_ArrowSize);
  VConfigProvider.WriteInteger('ColorStr',GPS_ArrowColor);
  VConfigProvider.WriteInteger('SizeTrack',GPS_TrackWidth);
  VConfigProvider.WriteInteger('NumShowTrackPoints',GPS_NumTrackPoints);
  VConfigProvider.WriteBool('path',GPS_ShowPath);
  VConfigProvider.WriteBool('go',GPS_MapMove);
  VConfigProvider.WriteBool('goCentered',GPS_MapMoveCentered);
  VConfigProvider.WriteBool('log',GPS_WriteLog);
  VConfigProvider.WriteBool('NMEALog',GPS_NMEALog);
  VConfigProvider.WriteBool('SensorsAutoShow',GPS_SensorsAutoShow);

  VConfigProvider.WriteFloat('Odometr', Odometr);
  VConfigProvider.WriteFloat('Odometr2', Odometr2);
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
