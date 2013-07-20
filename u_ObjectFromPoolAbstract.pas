unit u_ObjectFromPoolAbstract;

interface

type
  TObjectFromPoolAbstract = class(TObject, IInterface)
  private
    FRefCount: Integer;
    FNextFree: TObjectFromPoolAbstract;
  protected
    function CheckNeedDestroyObject: Boolean; virtual; abstract;
    procedure InternalCleanup; virtual;
  protected
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    property NextFree: TObjectFromPoolAbstract read FNextFree write FNextFree;
  end;

implementation

uses
  Windows;

{ TObjectFromPoolAbstract }

procedure TObjectFromPoolAbstract.InternalCleanup;
begin
  // Do nothing by default
end;

function TObjectFromPoolAbstract.QueryInterface(const IID: TGUID;
  out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TObjectFromPoolAbstract._AddRef: Integer;
begin
  Assert(FNextFree = nil);
  Result := InterlockedIncrement(FRefCount);
end;

function TObjectFromPoolAbstract._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then begin
    InternalCleanup;
    if CheckNeedDestroyObject then begin
      Destroy;
    end;
  end;
end;

end.
