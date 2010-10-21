unit u_GPSState;

interface

uses
  Graphics,
  i_JclNotify,
  i_IGPSRecorder,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IGPSModule,
  i_IGPSModuleByCOMPortSettings,
  u_GPSModuleByCOMPortSettings;

type
  TGPSpar = class
  private
    FGPSRecorder: IGPSRecorder;
    FSettings: IGPSModuleByCOMPortSettings;
    FSettingsObj: TGPSModuleByCOMPortSettings;
    FGPSModele: IGPSModule;
    GPS_enab: Boolean;
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
    constructor Create();
    destructor Destroy; override;
    procedure LoadConfig(AConfigProvider: IConfigDataProvider); virtual;
    procedure StartThreads; virtual;
    procedure SendTerminateToThreads; virtual;
    procedure SaveConfig(AConfigProvider: IConfigDataWriteProvider); virtual;

    function GetDataReciveNotifier: IJclNotifier;
    function GetConnectErrorNotifier: IJclNotifier;
    function GetConnectNotifier: IJclNotifier;
    function GetDisconnectNotifier: IJclNotifier;
    function GetTimeOutNotifier: IJclNotifier;

    function GetSatActive(pcode:integer;NMEA:string):boolean;
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property GPSSettings: TGPSModuleByCOMPortSettings read FSettingsObj;
    property GPSModele: IGPSModule read FGPSModele;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  u_GPSModuleByZylGPS,
  u_GPSRecorderStuped;

constructor TGPSpar.Create;
begin
  FGPSRecorder := TGPSRecorderStuped.Create;
  FSettingsObj := TGPSModuleByCOMPortSettings.Create;
  FSettings := FSettingsObj;
  FGPSModele := TGPSModuleByZylGPS.Create(FSettings);
end;

destructor TGPSpar.Destroy;
begin
  FGPSRecorder := nil;
  FSettingsObj := nil;
  FSettings := nil;
  FGPSModele := nil;
  inherited;
end;

function TGPSpar.GetConnectErrorNotifier: IJclNotifier;
begin
  Result := FGPSModele.ConnectErrorNotifier;
end;

function TGPSpar.GetConnectNotifier: IJclNotifier;
begin
  Result := FGPSModele.ConnectNotifier;
end;

function TGPSpar.GetDataReciveNotifier: IJclNotifier;
begin
  Result := FGPSModele.DataReciveNotifier;
end;

function TGPSpar.GetDisconnectNotifier: IJclNotifier;
begin
  Result := FGPSModele.DisconnectNotifier;
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

function TGPSpar.GetTimeOutNotifier: IJclNotifier;
begin
  Result := FGPSModele.TimeOutNotifier;
end;

procedure TGPSpar.LoadConfig(AConfigProvider: IConfigDataProvider);
var
  VConfigProvider: IConfigDataProvider;
begin
  VConfigProvider := AConfigProvider.GetSubItem('GPS');
  if VConfigProvider <> nil then begin
    GPS_enab := VConfigProvider.ReadBool('enbl', false);

    FSettingsObj.Port := VConfigProvider.ReadInteger('COM', FSettings.Port);
    FSettingsObj.BaudRate := VConfigProvider.ReadInteger('BaudRate', FSettings.BaudRate);
    FSettingsObj.ConnectionTimeout := VConfigProvider.ReadInteger('timeout', FSettings.ConnectionTimeout);
    FSettingsObj.Delay := VConfigProvider.ReadInteger('update', FSettings.Delay);
    FSettingsObj.NMEALog := VConfigProvider.ReadBool('NMEAlog', FSettings.NMEALog);

    GPS_ArrowSize := VConfigProvider.ReadInteger('SizeStr', 25);
    GPS_ArrowColor := VConfigProvider.ReadInteger('ColorStr', clRed);
    GPS_TrackWidth := VConfigProvider.ReadInteger('SizeTrack', 5);
    GPS_NumTrackPoints := VConfigProvider.ReadInteger('NumShowTrackPoints', 5000);
    GPS_WriteLog:=VConfigProvider.ReadBool('log',true);
    GPS_ShowPath:=VConfigProvider.ReadBool('path',true);
    GPS_MapMove:=VConfigProvider.ReadBool('go',true);
    GPS_MapMoveCentered:=VConfigProvider.ReadBool('goCentered',false);
    GPS_SensorsAutoShow:=VConfigProvider.ReadBool('SensorsAutoShow',true);

    Odometr:=VConfigProvider.ReadFloat('Odometr',0);
    Odometr2:=VConfigProvider.ReadFloat('Odometr2',0);
  end else begin
    GPS_enab := False;
    GPS_ArrowSize := 25;
    GPS_ArrowColor := clRed;
    GPS_TrackWidth := 5;
    GPS_NumTrackPoints := 5000;
    GPS_WriteLog:=true;
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

  VConfigProvider.WriteInteger('COM', FSettings.Port);
  VConfigProvider.WriteInteger('BaudRate',FSettings.BaudRate);
  VConfigProvider.WriteInteger('timeout',FSettings.ConnectionTimeout);
  VConfigProvider.WriteInteger('update',FSettings.Delay);
  VConfigProvider.WriteBool('NMEALog',FSettings.NMEALog);

  VConfigProvider.WriteInteger('SizeStr',GPS_ArrowSize);
  VConfigProvider.WriteInteger('ColorStr',GPS_ArrowColor);
  VConfigProvider.WriteInteger('SizeTrack',GPS_TrackWidth);
  VConfigProvider.WriteInteger('NumShowTrackPoints',GPS_NumTrackPoints);
  VConfigProvider.WriteBool('path',GPS_ShowPath);
  VConfigProvider.WriteBool('go',GPS_MapMove);
  VConfigProvider.WriteBool('goCentered',GPS_MapMoveCentered);
  VConfigProvider.WriteBool('log',GPS_WriteLog);
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
