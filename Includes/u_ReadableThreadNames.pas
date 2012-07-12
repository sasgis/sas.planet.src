unit u_ReadableThreadNames;

interface

(*
 * In order to see names for your threads in the Delphi IDE while debugging your
 * Win32 application, call SetCurrentThreadName() in your TThread.Execute method
 *
 *)

procedure SetCurrentThreadName(const AName: string);

implementation

{$IFDEF MSWINDOWS}

uses
  Windows;

type
  TThreadNameInfo = record
    RecType: LongWord;   // must be 0x1000
    Name: PChar;         // pointer to name (in user address space)
    ThreadID: LongWord;  // thread ID (-1 indicates caller thread)
    Flags: LongWord;     // reserved for future use, must be zero
  end;

procedure SetCurrentThreadName(const AName: string);
{$IFDEF DEBUG}
var
  info: TThreadNameInfo;
{$ENDIF}
begin
  {$IFDEF DEBUG}
  if AName <> '' then begin   
    // This code is extremely strange, but it's the documented way of doing it!
    info.RecType := $1000;
    info.Name := PChar(AName);
    info.ThreadID := $FFFFFFFF;
    info.Flags := 0;
    try
      RaiseException($406D1388, 0,
        SizeOf(info) div SizeOf(LongWord), PDWord(@info));
    except
    end;
    end;
  {$ENDIF}
end;

{$ENDIF}

end.