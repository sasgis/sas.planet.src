unit u_ReadableThreadNames;

interface

(*
 * In order to see names for your threads in the Delphi IDE while debugging your
 * application, call SetCurrentThreadName() in your TThread.Execute method
 *
 *)

procedure SetCurrentThreadName(const AName: string); {$IFNDEF DEBUG} inline; {$ENDIF}

implementation

{$IF DEFINED(MSWINDOWS) AND DEFINED(DEBUG)}

uses
  Windows;

{$IF CompilerVersion < 23}
function IsDebuggerPresent: Boolean; stdcall; external 'kernel32.dll';
{$IFEND}

type
  {$A8}
  TThreadNameInfo = record
    dwType     : DWORD;   // must be 0x1000
    szName     : LPCSTR;  // pointer to name (in user addr space)
    dwThreadID : DWORD;   // thread ID (-1 indicates caller thread)
    dwFlags    : DWORD;   // reserved for future use, must be zero
  end;

const
  MS_VC_EXCEPTION: DWORD = $406D1388;

procedure SetCurrentThreadName(const AName: string);
var
  VName: AnsiString;
  VInfo: TThreadNameInfo;
begin
  // This code is extremely strange, but it's the documented way of doing it
  // https://docs.microsoft.com/en-us/visualstudio/debugger/how-to-set-a-thread-name-in-native-code

  if not IsDebuggerPresent or (AName = '') then begin
    Exit;
  end;

  VName := AnsiString(AName);

  VInfo.dwType     := $1000;
  VInfo.szName     := Pointer(VName);
  VInfo.dwThreadID := DWORD(-1);
  VInfo.dwFlags    := 0;

  try
    RaiseException(MS_VC_EXCEPTION, 0, SizeOf(VInfo) div SizeOf(ULONG_PTR), @VInfo);
  except
    // do nothing
  end;
end;

{$ELSE}

procedure SetCurrentThreadName(const AName: string);
begin
  // do nothing
end;

{$IFEND}

end.