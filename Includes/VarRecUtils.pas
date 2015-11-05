unit VarRecUtils;

interface

type
  TConstArray = array of TVarRec;

// Copies a TVarRec and its contents. If the content is referenced
// the value will be copied to a new location and the reference
// updated.
function CopyVarRec(const Item: TVarRec): TVarRec;

// Creates a TConstArray out of the values given. Uses CopyVarRec
// to make copies of the original elements.
function CreateConstArray(const Elements: array of const): TConstArray;

// TVarRecs created by CopyVarRec must be finalized with this function.
// You should not use it on other TVarRecs.
procedure FinalizeVarRec(var Item: TVarRec);

// A TConstArray contains TVarRecs that must be finalized. This function
// does that for all items in the array.
procedure FinalizeConstArray(var Arr: TConstArray);

implementation

uses
  SysUtils;

function CopyVarRec(const Item: TVarRec): TVarRec;
var
  W: WideString;
begin
  // Copy entire TVarRec first
  Result := Item;

  // Now handle special cases
  case Item.VType of
    vtExtended:
      begin
        New(Result.VExtended);
        Result.VExtended^ := Item.VExtended^;
      end;
    vtString:
      begin
        New(Result.VString);
        Result.VString^ := Item.VString^;
      end;
    vtPChar:
      Result.VPChar := StrNew(Item.VPChar);
    // there is no StrNew for PWideChar
    vtPWideChar:
      begin
        W := Item.VPWideChar;
        GetMem(Result.VPWideChar, 
               (Length(W) + 1) * SizeOf(WideChar));
        Move(PWideChar(W)^, Result.VPWideChar^, 
             (Length(W) + 1) * SizeOf(WideChar));
      end;
    // a little trickier: casting to AnsiString will ensure
    // reference counting is done properly
    vtAnsiString:
      begin
        // nil out first, so no attempt to decrement
        // reference count
        Result.VAnsiString := nil;
        AnsiString(Result.VAnsiString) := AnsiString(Item.VAnsiString);
      end;
    vtCurrency:
      begin
        New(Result.VCurrency);
        Result.VCurrency^ := Item.VCurrency^;
      end;
    vtVariant:
      begin
        New(Result.VVariant);
        Result.VVariant^ := Item.VVariant^;
      end;
    // casting ensures proper reference counting
    vtInterface:
      begin
        Result.VInterface := nil;
        IInterface(Result.VInterface) := IInterface(Item.VInterface);
      end;
    // casting ensures a proper copy is created
    vtWideString:
      begin
        Result.VWideString := nil;
        WideString(Result.VWideString) := WideString(Item.VWideString);
      end;
    {$IFDEF UNICODE}
    vtUnicodeString:
      begin
        Result.VUnicodeString := nil;
        UnicodeString(Result.VUnicodeString) := UnicodeString(Item.VUnicodeString);
      end;
    {$ENDIF}
    vtInt64:
      begin
        New(Result.VInt64);
        Result.VInt64^ := Item.VInt64^;
      end;
    // VPointer and VObject don't have proper copy semantics so it
    // is impossible to write generic code that copies the contents
  end;
end;

function CreateConstArray(const Elements: array of const): TConstArray;
var
  I: Integer;
begin
  SetLength(Result, Length(Elements));
  for I := Low(Elements) to High(Elements) do
    Result[I] := CopyVarRec(Elements[I]);
end;

// use this function on copied TVarRecs only!
procedure FinalizeVarRec(var Item: TVarRec);
begin
  case Item.VType of
    vtExtended: Dispose(Item.VExtended);
    vtString: Dispose(Item.VString);
    vtPChar: StrDispose(Item.VPChar);
    vtPWideChar: FreeMem(Item.VPWideChar);
    vtAnsiString: AnsiString(Item.VAnsiString) := '';
    vtCurrency: Dispose(Item.VCurrency);
    vtVariant: Dispose(Item.VVariant);
    vtInterface: IInterface(Item.VInterface) := nil;
    vtWideString: WideString(Item.VWideString) := '';
    vtInt64: Dispose(Item.VInt64);
    {$IFDEF UNICODE}
    vtUnicodeString: UnicodeString(Item.VUnicodeString) := '';
    {$ENDIF}
  end;
  Item.VInteger := 0;
end;

procedure FinalizeConstArray(var Arr: TConstArray);
var
  I: Integer;
begin
  for I := Low(Arr) to High(Arr) do
    FinalizeVarRec(Arr[I]);
  Finalize(Arr);
  Arr := nil;
end;

end.
