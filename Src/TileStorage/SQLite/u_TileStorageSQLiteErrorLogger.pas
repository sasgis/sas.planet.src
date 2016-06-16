unit u_TileStorageSQLiteErrorLogger;

interface

uses
  Windows,
  SysUtils,
  i_TileStorageSQLiteErrorLogger,
  u_BaseInterfacedObject;

type
  TErrorLoggerGetFileNameEvent = procedure (
    var AFullLogFilename: String;
    var AForbidden: Boolean
  ) of object;

  TErrorLoggerToFile = class(TBaseInterfacedObject, IErrorLoggerToFile)
  private
    FFilename: String;
    FEvent: TErrorLoggerGetFileNameEvent;
    // internal
    FLogSync: IReadWriteSync;
    FLogFile: THandle;
  private
    procedure InternalOpenLog;
    procedure InternalCloseLog;
  private
    { IErrorLoggerToFile }
    procedure LogString(const AValue: String);
    procedure Close;
  public
    constructor Create(
      const AFilename: String;
      const AEvent: TErrorLoggerGetFileNameEvent
    );
    destructor Destroy; override;
  end;

implementation

uses
  NTFiles,
  u_Synchronizer;

{ TErrorLoggerToFile }

procedure TErrorLoggerToFile.Close;
begin
  FLogSync.BeginWrite;
  try
    InternalCloseLog;
  finally
    FLogSync.EndWrite;
  end;
end;

constructor TErrorLoggerToFile.Create(
  const AFilename: String;
  const AEvent: TErrorLoggerGetFileNameEvent
);
begin
  Assert(
    (0 < Length(AFilename))
    or
    (Assigned(AEvent))
  );

  inherited Create;

  FFilename := AFilename;
  FEvent := AEvent;
  FLogSync := GSync.SyncStd.Make(Self.ClassName); // spinlock(4096)
  FLogFile := 0;
end;

destructor TErrorLoggerToFile.Destroy;
begin
  Close;
  inherited;
end;

procedure TErrorLoggerToFile.InternalCloseLog;
begin
  if (FLogFile <> 0) then begin
    CloseHandle(FLogFile);
    FLogFile := 0;
  end;
end;

procedure TErrorLoggerToFile.InternalOpenLog;
var
  VFileName: String;
  VForbidden: Boolean;
begin
  if (0 = FLogFile) then begin
    // try to open log file
    VFileName := FFilename;
    if (0 < Length(VFileName)) then begin
      // use it
    end else begin
      // get by event
      if Assigned(FEvent) then begin
        VForbidden := False;
        FEvent(VFileName, VForbidden);
        if VForbidden or (0 = Length(VFileName)) then
          Exit;
      end else begin
        // neither filename nor event
        Exit;
      end;
    end;

    // open
    FLogFile := CreateOrOpenFileWriteOnly(VFileName, False);
    if (INVALID_HANDLE_VALUE = FLogFile) then begin
      FLogFile := 0;
    end;
  end;
end;

procedure TErrorLoggerToFile.LogString(const AValue: String);
var
  VNumberOfBytesWritten: DWORD;
begin
  FLogSync.BeginWrite;
  try
    InternalOpenLog;
    if (0 <> FLogFile) then begin
      SetFilePointer(FLogFile, 0, nil, FILE_END);
      WriteFile(FLogFile, AValue[1], Length(AValue)*SizeOf(Char), VNumberOfBytesWritten, nil);
    end;
  finally
    FLogSync.EndWrite;
  end;
end;

end.