unit u_GPSLogWriterToPlt;

interface

uses
  SyncObjs,
  SysUtils,
  i_GPS;

type
  TPltLogWriter = class
  private
    FCS: TCriticalSection;
    FStarted: Boolean;
    FPath: string;
    FTrackFileNameFormatSettings: TFormatSettings;
    FPltFormatSettings: TFormatSettings;
    //Файл для записи GPS трека (Нужно будет заменить отдельным объектом)
    GPS_LogFile: TextFile;
    FTrackPartStart: Boolean;
  public
    constructor Create(APath: string);
    destructor Destroy; override;

    procedure StartWrite;
    procedure AddPoint(APosition: IGPSPosition);
    procedure CloseLog;
    property Started: Boolean read FStarted;
  end;

implementation

uses
  t_GeoTypes;

{ TPltLogWriter }

constructor TPltLogWriter.Create(APath: string);
begin
  FCS := TCriticalSection.Create;
  FStarted := False;
  FTrackPartStart := True;
  FPath := APath;

  FTrackFileNameFormatSettings.DecimalSeparator := '.';
  FTrackFileNameFormatSettings.DateSeparator := '.';
  FTrackFileNameFormatSettings.ShortDateFormat := 'yyyy.MM.dd';
  FTrackFileNameFormatSettings.TimeSeparator := ':';
  FTrackFileNameFormatSettings.LongTimeFormat := 'HH-mm-ss';
  FTrackFileNameFormatSettings.ShortTimeFormat := 'HH-mm-ss';
  FTrackFileNameFormatSettings.ListSeparator := ';';
  FTrackFileNameFormatSettings.TwoDigitYearCenturyWindow := 50;

  FPltFormatSettings.DecimalSeparator := '.';
  FPltFormatSettings.DateSeparator := '.';
  FPltFormatSettings.ShortDateFormat := 'dd.MM.yyyy';
  FPltFormatSettings.TimeSeparator := ':';
  FPltFormatSettings.LongTimeFormat := 'HH:mm:ss';
  FPltFormatSettings.ShortTimeFormat := 'HH:mm:ss';
  FPltFormatSettings.ListSeparator := ';';
  FPltFormatSettings.TwoDigitYearCenturyWindow := 50;

end;

destructor TPltLogWriter.Destroy;
begin
  CloseLog;
  FreeAndNil(FCS);
  inherited;
end;

procedure TPltLogWriter.StartWrite;
var
  VFileName: string;
begin
  FCS.Acquire;
  try
    try
      ForceDirectories(FPath);
      VFileName := FPath + DateTimeToStr(Now, FTrackFileNameFormatSettings) +'.plt';
      AssignFile(GPS_LogFile,VFileName);
      rewrite(GPS_LogFile);
      Write(GPS_LogFile,'OziExplorer Track Point File Version 2.0'+#13#10+'WGS 84'+#13#10+'Altitude is in Feet'+#13#10+'Reserved 3'+#13#10+'0,2,255,Track Log File - '+DateTimeToStr(Now)+',1'+#13#10+'0'+#13#10);
      FStarted := True;
    except
      FStarted := False;
      raise;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TPltLogWriter.AddPoint(APosition: IGPSPosition);
var
  VPltString: string;
  VNow: TDateTime;
  sb: string;
  VPoint: TExtendedPoint;
  VAltitude: Extended;
begin
  FCS.Acquire;
  try
    VNow := Now;
    if APosition.IsFix = 0 then begin
      FTrackPartStart := True;
    end else begin
      VPoint := APosition.Position;
      VAltitude := APosition.Altitude;
      if FTrackPartStart then begin
        sb:='1';
      end else begin
        sb:='0';
      end;
      FTrackPartStart := False;
      VPltString:=FloatToStr(VPoint.Y, FPltFormatSettings)+','
        +FloatToStr(VPoint.X, FPltFormatSettings)+','
        +sb+','
        +FloatToStr(VAltitude*3.2808399, FPltFormatSettings)+','
        +FloatToStr(VNow, FPltFormatSettings)+','
        +DateToStr(VNow, FPltFormatSettings)+','
        +TimeToStr(VNow, FPltFormatSettings);
      Writeln(GPS_LogFile,VPltString);
    end;
  finally
    FCS.Release;
  end;
end;

procedure TPltLogWriter.CloseLog;
begin
  FCS.Acquire;
  try
    if FStarted then begin
      CloseFile(GPS_LogFile);
      FStarted := False;
      FTrackPartStart := True;
    end;
  finally
    FCS.Release;
  end;
end;

end.
