unit u_PoolElement;

interface

uses
  Windows,
  i_ISimpleFactory,
  i_IPoolElement;

type
  TPoolElement = class(TObject, IPoolElement)
  protected
    FRefCount: Integer;
    FObject: Iunknown;
    FLastUseTime: Cardinal;
    FFactory: ISimpleFactory;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AFactory: ISimpleFactory);
    destructor Destroy; override;
    function GetLastUseTime: Cardinal;
    function GetObject: IUnknown;
    function TryLock: IPoolElement;
    procedure FreeObjectByTTL(AMinTime: Cardinal);
  end;

implementation

uses
  SysUtils;

constructor TPoolElement.Create(AFactory: ISimpleFactory);
begin
  FFactory := AFactory;
  FLastUseTime := 0;
  FRefCount := 0;
  FObject := nil;
end;

destructor TPoolElement.Destroy;
begin
  if FRefCount <> 0 then begin
    raise Exception.Create('Item locked');
  end;
  FObject := nil;
  FFactory := nil;
  inherited;
end;

// Для Delphi 7 в юните Windows заменить объявление функции InterlockedCompareExchange
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall; external kernel32 name 'InterlockedCompareExchange';

procedure TPoolElement.FreeObjectByTTL(AMinTime: Cardinal);
begin
  if Integer(InterlockedCompareExchange(FRefCount, 1, 0)) = 0 then begin
    if (FLastUseTime > 0) and ((FLastUseTime <= AMinTime) or ((AMinTime < 1 shl 29) and (FLastUseTime > 1 shl 30))) then begin
      Fobject := nil;
      FLastUseTime := 0;
    end;
    InterlockedDecrement(FRefCount);
  end;
end;

function TPoolElement.GetLastUseTime: Cardinal;
begin
  Result := FLastUseTime;
end;

function TPoolElement.GetObject: IUnknown;
begin
  if Fobject = nil then begin
    Fobject := FFactory.CreateInstance;
  end;
  Result := Fobject;
end;

function TPoolElement.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then begin
    Result := 0;
  end else begin
    Result := E_NOINTERFACE;
  end;
end;

function TPoolElement.TryLock: IPoolElement;
begin
  if Integer(InterlockedCompareExchange(FRefCount, 1, 0)) = 0 then begin
    Result := Self;
    _Release;
  end else begin
    Result := nil;
  end;
end;

function TPoolElement._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TPoolElement._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then begin
    FLastUseTime := GetTickCount;
    Sleep(50);
  end;
end;

end.
