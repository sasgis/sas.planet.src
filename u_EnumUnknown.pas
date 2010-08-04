unit u_EnumUnknown;

interface

uses
  Classes,
  ActiveX;

type
  TEnumUnknown = class(TInterfacedObject, IEnumUnknown)
  private
    FList: IInterfaceList;
    FCurrent: Longint;
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  public
    constructor Create(AList: IInterfaceList);
    destructor Destroy; override;
  end;


implementation

{ TEnumUnknown }

function TEnumUnknown.Clone(out enm: IEnumUnknown): HResult;
var
  VClone: TEnumUnknown;
begin
  VClone := TEnumUnknown.Create(FList);
  VClone.FCurrent := FCurrent;
  enm := VClone;
  Result := S_OK;
end;

constructor TEnumUnknown.Create(AList: IInterfaceList);
begin
  FList := AList;
  FCurrent := 0;
end;

destructor TEnumUnknown.Destroy;
begin
  FList := nil;
  inherited;
end;

function TEnumUnknown.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
begin
  if celt <> 1 then begin
    Result := E_NOTIMPL;
  end else begin
    IInterface(elt) := nil;
    FList.Lock;
    try
      if FCurrent < FList.Count then begin
        IInterface(elt) := FList.Items[FCurrent];
        Inc(FCurrent);
        if pceltFetched <> nil then begin
          pceltFetched^ := 1;
        end;
        Result := S_OK;
      end else begin
        if pceltFetched <> nil then begin
          pceltFetched^ := 0;
        end;
        Result := S_FALSE;
      end;;
    finally
      FList.Unlock;
    end;
  end;
end;

function TEnumUnknown.Reset: HResult;
begin
  FCurrent := 0;
  Result := S_OK;
end;

function TEnumUnknown.Skip(celt: Integer): HResult;
begin
  FList.Lock;
  try
    if FCurrent + celt <= FList.Count then begin
      Inc(FCurrent, celt);
      Result := S_OK;
    end else begin
      FCurrent := FList.Count;
      Result := S_FALSE;
    end;;
  finally
    FList.Unlock;
  end;
end;

end.
 