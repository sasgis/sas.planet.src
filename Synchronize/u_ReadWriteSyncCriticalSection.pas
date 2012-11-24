unit u_ReadWriteSyncCriticalSection;

interface

uses
  Windows,
  SysUtils,
  u_ReadWriteSyncAbstract,
  i_ReadWriteSyncFactory;

type
  TSynchronizerCS = class(TReadWriteSyncAbstract, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create (const AName: AnsiString);
    destructor Destroy; override;
  end;

  TSynchronizerCSSC = class(TReadWriteSyncAbstract, IReadWriteSync)
  private
    FLock: TRTLCriticalSection;
  protected
    { IReadWriteSync }
    procedure BeginRead;
    procedure EndRead;
    function BeginWrite: Boolean;
    procedure EndWrite;
  public
    constructor Create(const AName: AnsiString; const ASpinCount: Cardinal);
    destructor Destroy; override;
  end;

  TSynchronizerCSFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    function Make(const AName: AnsiString): IReadWriteSync;
  end;

  TSynchronizerCSSCFactory = class(TInterfacedObject, IReadWriteSyncFactory)
  private
    FSpinCount: Cardinal;
  private
    function Make(const AName: AnsiString): IReadWriteSync;
  public
    constructor Create(const ASpinCount: Cardinal);
  end;

implementation

{ TSynchronizerCS }

constructor TSynchronizerCS.Create(const AName: AnsiString);
begin
  inherited Create(AName);
  InitializeCriticalSection(FLock);
end;

destructor TSynchronizerCS.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TSynchronizerCS.BeginRead;
begin
  EnterCriticalSection(FLock);
end;

function TSynchronizerCS.BeginWrite: Boolean;
begin
  EnterCriticalSection(FLock);
  Result := True;
end;

procedure TSynchronizerCS.EndRead;
begin
  LeaveCriticalSection(FLock);
end;

procedure TSynchronizerCS.EndWrite;
begin
  LeaveCriticalSection(FLock);
end;

{ TSynchronizerCSSC }

constructor TSynchronizerCSSC.Create(
  const AName: AnsiString;
  const ASpinCount: Cardinal
);
begin
  inherited Create(AName);
  InitializeCriticalSectionAndSpinCount(FLock, ASpinCount);
end;

destructor TSynchronizerCSSC.Destroy;
begin
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TSynchronizerCSSC.BeginRead;
begin
  EnterCriticalSection(FLock);
end;

function TSynchronizerCSSC.BeginWrite: Boolean;
begin
  EnterCriticalSection(FLock);
  Result := TRUE;
end;

procedure TSynchronizerCSSC.EndRead;
begin
  LeaveCriticalSection(FLock);
end;

procedure TSynchronizerCSSC.EndWrite;
begin
  LeaveCriticalSection(FLock);
end;

{ TSynchronizerCSFactory }

function TSynchronizerCSFactory.Make(const AName: AnsiString): IReadWriteSync;
begin
  Result := TSynchronizerCS.Create(AName);
end;

{ TSynchronizerCSSCFactory }

constructor TSynchronizerCSSCFactory.Create(const ASpinCount: Cardinal);
begin
  inherited Create;
  FSpinCount := ASpinCount;
end;

function TSynchronizerCSSCFactory.Make(const AName: AnsiString): IReadWriteSync;
begin
  Result := TSynchronizerCSSC.Create(AName, FSpinCount);
end;

end.
