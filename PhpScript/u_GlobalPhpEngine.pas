unit u_GlobalPhpEngine;

interface

uses
  Windows,
  SysUtils,
  SyncObjs,
  PHPCommon,
  PHP4Delphi;

type
  TGlobalPhpEngine = class
    private
      FPHPEngine: TPHPEngine;
      FInitialized: Boolean;
      FHandleErrors: Boolean;
      FEngineThreadSafe: TCriticalSection;
      procedure Lock;
      procedure Unlock;
      procedure OnScriptError (Sender : TObject; AText : AnsiString;
        AType : TPHPErrorType; AFileName : AnsiString; ALineNo : integer);
    public
      constructor Create;
      destructor Destroy; override;
      function Initialize(AHandleErrors: Boolean): Boolean;
  end;

var
  GPhpEngine: TGlobalPhpEngine = nil;

implementation

{ TGlobalPhpEngine }

constructor TGlobalPhpEngine.Create;
begin
  inherited Create;
  FInitialized := False;
  FHandleErrors := False;
  FEngineThreadSafe := TCriticalSection.Create;
end;

destructor TGlobalPhpEngine.Destroy;
begin
  try
    if FInitialized and Assigned(FPHPEngine) then
    begin
     // FPHPEngine.ShutdownEngine; // исключение в php5ts.dll при выгрузке
     // FPHPEngine.Free;           // что-то где-то забыли освободить или глюк компонента?
    end;
  finally
    FreeAndNil(FEngineThreadSafe);
    inherited Destroy;
  end;
end;

function TGlobalPhpEngine.Initialize(AHandleErrors: Boolean): Boolean;
begin
    Lock;
  try
    if not FInitialized then
    begin
      FPHPEngine := TPHPEngine.Create(nil);
      FPHPEngine.HandleErrors := True;
      FPHPEngine.OnScriptError := Self.OnScriptError;
      FPHPEngine.StartupEngine;
      FHandleErrors := AHandleErrors;
      FInitialized := True;
    end
    else
      if AHandleErrors and not FHandleErrors then
        FHandleErrors := AHandleErrors;

    Result := FInitialized;
  finally
    Unlock;
  end;
end;

procedure TGlobalPhpEngine.OnScriptError (Sender : TObject; AText : AnsiString;
        AType : TPHPErrorType; AFileName : AnsiString; ALineNo : integer);

  function GetPhpErrorAsString(AError: TPHPErrorType): string;
  begin
    case AError of
      etError: Result := 'Error';
      etWarning: Result := 'Warning';
      etParse: Result := 'Parse error';
      etNotice: Result := 'Notice';
      etCoreError: Result := 'CoreError';
      etCoreWarning: Result := 'CoreWarning';
      etCompileError: Result := 'CompileError';
      etCompileWarning: Result := 'CompileWarning';
      etUserError: Result := 'UserError';
      etUserWarning: Result := 'UserWarning';
      etUserNotice: Result := 'UserNotice';
      etUnknown: Result := 'Unknown error';
    end;
  end;

var
  VFilePath: string;
begin
  if ( AFileName <> '' ) and ( AFileName <> '-' ) then
    VFilePath := ' (' + AFileName + ')'
  else
    VFilePath := '';

  if FHandleErrors then
    MessageBoxA( 0,
                 PChar('Error line: ' + IntToStr(ALineNo) + VFilePath + #13#10 +
                       'Error text: ' + AText + #13#10 +
                       'Hint: Set Debug=0 in zmp/params.txt to disable this messages.'),
                 PChar('SAS.Planet PHP Debuger: ' + GetPhpErrorAsString(AType)),
                 MB_OK);
end;

procedure TGlobalPhpEngine.Lock;
begin
  FEngineThreadSafe.Acquire;
end;

procedure TGlobalPhpEngine.Unlock;
begin
  FEngineThreadSafe.Release;
end;

initialization
  GPhpEngine := TGlobalPhpEngine.Create;

finalization
  GPhpEngine.Free;

end.
