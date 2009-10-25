unit u_WideStringsAdapter;

interface

uses
  ComObj,
  StdVCL,
  u_WideStrings;

type
  TWideStringsAdapter = class(TAutoIntfObject, IStrings, IWideStringsAdapter)
  private
    FStrings: TWideStrings;
  protected
    { IWideStringsAdapter }
    procedure ReferenceStrings(S: TWideStrings);
    procedure ReleaseStrings;
    { IStrings }
    function Get_ControlDefault(Index: Integer): OleVariant; safecall;
    procedure Set_ControlDefault(Index: Integer; Value: OleVariant); safecall;
    function Count: Integer; safecall;
    function Get_Item(Index: Integer): OleVariant; safecall;
    procedure Set_Item(Index: Integer; Value: OleVariant); safecall;
    procedure Remove(Index: Integer); safecall;
    procedure Clear; safecall;
    function Add(Item: OleVariant): Integer; safecall;
    function _NewEnum: IUnknown; safecall;
  public
    constructor Create(Strings: TWideStrings);
  end;



implementation
uses
  AxCtrls,
  ActiveX,
  Variants;

type
  TPointerList = array[0..0] of Pointer;

  TWideStringsEnumerator = class(TContainedObject, IEnumString)
  private
    FIndex: Integer;  // index of next unread string
    FStrings: IStrings;
  public
    constructor Create(const Strings: IStrings);
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumString): HResult; stdcall;
  end;

constructor TWideStringsEnumerator.Create(const Strings: IStrings);
begin
  inherited Create(Strings);
  FStrings := Strings;
end;

function TWideStringsEnumerator.Next(celt: Longint; out elt; pceltFetched: PLongint): HResult;
var
  I: Integer;
begin
  I := 0;
  while (I < celt) and (FIndex < FStrings.Count) do
  begin
    TPointerList(elt)[I] := PWideChar(WideString(FStrings.Item[I]));
    Inc(I);
    Inc(FIndex);
  end;
  if pceltFetched <> nil then pceltFetched^ := I;
  if I = celt then Result := S_OK else Result := S_FALSE;
end;

function TWideStringsEnumerator.Skip(celt: Longint): HResult;
begin
  if (FIndex + celt) <= FStrings.Count then
  begin
    Inc(FIndex, celt);
    Result := S_OK;
  end
  else
  begin
    FIndex := FStrings.Count;
    Result := S_FALSE;
  end;
end;

function TWideStringsEnumerator.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;

function TWideStringsEnumerator.Clone(out enm: IEnumString): HResult;
begin
  try
    enm := TWideStringsEnumerator.Create(FStrings);
    Result := S_OK;
  except
    Result := E_UNEXPECTED;
  end;
end;



{ TWideStringsAdapter }

constructor TWideStringsAdapter.Create(Strings: TWideStrings);
var
  StdVcl: ITypeLib;
begin
  OleCheck(LoadRegTypeLib(LIBID_STDVCL, 4, 0, 0, StdVcl));
  inherited Create(StdVcl, IStrings);
  FStrings := Strings;
end;

procedure TWideStringsAdapter.ReferenceStrings(S: TWideStrings);
begin
  FStrings := S;
end;

procedure TWideStringsAdapter.ReleaseStrings;
begin
  FStrings := nil;
end;

function TWideStringsAdapter.Get_ControlDefault(Index: Integer): OleVariant;
begin
  Result := Get_Item(Index);
end;

procedure TWideStringsAdapter.Set_ControlDefault(Index: Integer; Value: OleVariant);
begin
  Set_Item(Index, Value);
end;

function TWideStringsAdapter.Count: Integer;
begin
  Result := 0;
  if FStrings <> nil then Result := FStrings.Count;
end;

function TWideStringsAdapter.Get_Item(Index: Integer): OleVariant;
begin
  Result := NULL;
  if (FStrings <> nil) then Result := FStrings[Index];
end;

procedure TWideStringsAdapter.Set_Item(Index: Integer; Value: OleVariant);
begin
  if (FStrings <> nil) then FStrings[Index] := Value;
end;

procedure TWideStringsAdapter.Remove(Index: Integer);
begin
  if FStrings <> nil then FStrings.Delete(Index);
end;

procedure TWideStringsAdapter.Clear;
begin
  if FStrings <> nil then FStrings.Clear;
end;

function TWideStringsAdapter.Add(Item: OleVariant): Integer;
begin
  Result := -1;
  if FStrings <> nil then Result := FStrings.Add(Item);
end;

function TWideStringsAdapter._NewEnum: IUnknown;
begin
  Result := TWideStringsEnumerator.Create(Self);
end;

end.
