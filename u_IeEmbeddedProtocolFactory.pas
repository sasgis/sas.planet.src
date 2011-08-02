unit u_IeEmbeddedProtocolFactory;

interface

uses
  Windows,
  ActiveX,
  i_InternalDomainInfoProvider;

type
  TIeEmbeddedProtocolFactory = class(TInterfacedObject, IClassFactory)
  private
    FDomainList: IInternalDomainInfoProviderList;
  protected
    { IClassFactory }
    function CreateInstance(const unkOuter: IUnknown; const iid: TIID;
      out obj): HResult; stdcall;
    function LockServer(fLock: BOOL): HResult; stdcall;
  public
    constructor Create(ADomainList: IInternalDomainInfoProviderList);
  end;

implementation

uses
  SysUtils,
  u_IeEmbeddedProtocol;

{ TIeEmbeddedProtocolFactory }

constructor TIeEmbeddedProtocolFactory.Create(
  ADomainList: IInternalDomainInfoProviderList);
begin
  FDomainList := ADomainList;
end;

function TIeEmbeddedProtocolFactory.CreateInstance(
  const unkOuter: IInterface; const iid: TIID; out obj): HResult;
var
  VProtocol: TIeEmbeddedProtocol;
  VGUIDSTring: string;
begin
  if (unkOuter <> nil) then begin
    Result := CLASS_E_NOAGGREGATION;
    Exit;
  end;
  VGUIDSTring := GUIDToString(iid);
  VProtocol := TIeEmbeddedProtocol.Create(FDomainList);
  if VProtocol.GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
  if VProtocol.RefCount = 0 then VProtocol.Free;
end;

function TIeEmbeddedProtocolFactory.LockServer(fLock: BOOL): HResult;
begin
  Result := E_NOTIMPL;
end;

end.
