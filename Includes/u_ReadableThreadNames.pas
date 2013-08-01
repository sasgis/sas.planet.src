unit u_ReadableThreadNames;

interface

(*
 * In order to see names for your threads in the Delphi IDE while debugging your
 * Win32 application, call SetCurrentThreadName() in your TThread.Execute method
 *
 *)

procedure SetCurrentThreadName(const AName: string);

implementation

{$IF DEFINED(MSWINDOWS) AND DEFINED(DEBUG)}

uses
  Windows;

{$IF CompilerVersion < 23}
function IsDebuggerPresent: Boolean; stdcall; external 'kernel32.dll';
{$IFEND}

type
  TThreadNameInfo = record
    dwType     : DWORD;   // must be 0x1000
    szName     : LPCSTR;  // pointer to name (in user addr space)
    dwThreadID : DWORD;   // thread ID (-1 indicates caller thread)
    dwFlags    : DWORD;   // reserved for future use, must be zero
  end;

const
  cDefaultInfoType    = $1000;
  cSetThreadNameExcep = $406D1388;

procedure SetThreadName(const AName: string; const AThreadID: Cardinal);
var
  VInfo: TThreadNameInfo;
begin
  if IsDebuggerPresent and (AName <> '') then begin

    // This code is extremely strange, but it's the documented way of doing it:
    // http://msdn.microsoft.com/en-us/library/xcb2z8hs(VS.71).aspx

    VInfo.dwType     := cDefaultInfoType;
    VInfo.szName     := PAnsiChar(AnsiString(AName));
    VInfo.dwThreadID := AThreadID;
    VInfo.dwFlags    := 0;

    try
      RaiseException(cSetThreadNameExcep, 0, SizeOf(VInfo) div SizeOf(LongWord), @VInfo);
    except
      // do nothing
    end;
  end;
end;

procedure SetCurrentThreadName(const AName: string);
begin
  SetThreadName(AName, Cardinal(-1));
end;

{$ELSE}

procedure SetCurrentThreadName(const AName: string);
begin
  // do nothing
end;

{$IFEND}

end.