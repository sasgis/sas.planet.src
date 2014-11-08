unit u_AppEnum;

interface

uses
  Windows,
  i_AppEnum;

const
  cEnumMaxSize = 32;

const
  cRiseErrorDef = {$IFDEF DEBUG} True {$ELSE} False {$ENDIF};
  cMutexTimeOutDef = 30000; // 30 sec

type
  TAppEnum = class(TInterfacedObject, IAppEnum)
  private
    type
      TAppSharedInfo = packed record
        Used: array [0..cEnumMaxSize-1] of Boolean;
      end;
      PAppSharedInfo = ^TAppSharedInfo;
  private
    FMutex: THandle;
    FMutexAlreadyExists: Boolean;
    FMutexTimeOut: Cardinal;
    FMapping: THandle;
    FSharedInfo: PAppSharedInfo;
    FCount: Integer;
    FCurrentID: Integer;
    FRiseError: Boolean;
    function DoCreateMutex(const AName: string): Cardinal;
  private
    { IAppEnum }
    function GetCount: Integer;
    function GetCurrentID: Integer;
  public
    constructor Create(
      const ARiseError: Boolean = cRiseErrorDef;
      const AMutexTimeOut: Cardinal = cMutexTimeOutDef
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils;

const
  cAppSharedInfoVersion = #1; // change it every time when modify TAppSharedInfo

const
  cMutexUniqName = 'sasplanet-appenum-shared-mutex' + cAppSharedInfoVersion;
  cMappingUniqName = 'sasplanet-appenum-shared-file' + cAppSharedInfoVersion;

{ TAppEnum }

constructor TAppEnum.Create(
  const ARiseError: Boolean;
  const AMutexTimeOut: Cardinal
);
var
  I: Integer;
begin
  inherited Create;
  
  FRiseError := ARiseError;
  FMutexTimeOut := AMutexTimeOut;

  FCount := 0;
  FCurrentID := -1;

  FMutex := DoCreateMutex(cMutexUniqName);

  FMapping := CreateFileMapping(Cardinal(-1), nil, PAGE_READWRITE, 0, SizeOf(FSharedInfo), cMappingUniqName);
  if FMapping <> 0 then begin
    FSharedInfo := MapViewOfFile(FMapping, FILE_MAP_ALL_ACCESS, 0, 0, 0);
  end else begin
    FSharedInfo := nil;
  end;

  if (FMutex <> 0) and (FSharedInfo <> nil) then begin
    if (WaitForSingleObject(FMutex, FMutexTimeOut) = WAIT_OBJECT_0)  then begin
      try
        for I := 0 to Length(FSharedInfo.Used) - 1 do begin
          if not FSharedInfo.Used[I] then begin
            if FCurrentID = -1 then begin
              FSharedInfo.Used[I] := True;
              FCurrentID := I;
              Inc(FCount);
            end;
          end else begin
            Inc(FCount);
          end;
        end;
      finally
        ReleaseMutex(FMutex);
      end;
    end;
  end;
end;

destructor TAppEnum.Destroy;
begin
  if FSharedInfo <> nil then begin
    if (FCurrentID <> -1) and (FMutex <> 0) then begin
      if WaitForSingleObject(FMutex, FMutexTimeOut) = WAIT_OBJECT_0 then begin
        try
          FSharedInfo.Used[FCurrentID] := False;
        finally
          ReleaseMutex(FMutex);
        end;
      end;
    end;
    UnmapViewOfFile(FSharedInfo);
  end;
  if FMapping <> 0 then CloseHandle(FMapping);
  if FMutex <> 0 then CloseHandle(FMutex);
  inherited Destroy;
end;

function TAppEnum.DoCreateMutex(const AName: string): Cardinal;
var
  SD: TSecurityDescriptor;
  SA: TSecurityAttributes;
begin
  Result := 0;

  if not InitializeSecurityDescriptor(@SD, SECURITY_DESCRIPTOR_REVISION) then begin
    if FRiseError then begin
      raise Exception.CreateFmt('Error InitializeSecurityDescriptor: %s', [SysErrorMessage(GetLastError)]);
    end else begin
      Exit;
    end;
  end;

  SA.nLength := SizeOf(TSecurityAttributes);
  SA.lpSecurityDescriptor := @SD;
  SA.bInheritHandle := False;

  if not SetSecurityDescriptorDacl(SA.lpSecurityDescriptor, True, nil, False) then begin
    if FRiseError then begin
      raise Exception.CreateFmt('Error SetSecurityDescriptorDacl: %s', [SysErrorMessage(GetLastError)]);
    end else begin
      Exit;
    end;
  end;

  Result := CreateMutex(@SA, False, PChar(AName));

  if (Result = 0) and FRiseError then begin
    raise Exception.CreateFmt('Error CreateMutex: %s', [SysErrorMessage(GetLastError)]);
  end else begin
    FMutexAlreadyExists := (GetLastError = ERROR_ALREADY_EXISTS);
  end;
end;

function TAppEnum.GetCount: Integer;
begin
  Result := FCount;
end;

function TAppEnum.GetCurrentID: Integer;
begin
  Result := FCurrentID + 1;
end;

end.
