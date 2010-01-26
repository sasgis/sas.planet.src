unit u_EnumUnknownEmpty;

interface

uses
  ActiveX;

type
  TEmptyEnum = class(TInterfacedObject, IEnumUnknown)
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  end;

implementation

{ TEmptyEnum }

function TEmptyEnum.Clone(out enm: IEnumUnknown): HResult;
begin
  enm := Self;
  Result := S_OK;
end;

function TEmptyEnum.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
begin
  pceltFetched^ := 0;
  Result := S_FALSE;
end;

function TEmptyEnum.Reset: HResult;
begin
  Result := S_OK;
end;

function TEmptyEnum.Skip(celt: Integer): HResult;
begin
  Result := S_FALSE;
end;

end.
