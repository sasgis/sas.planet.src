unit u_QueryPerfCounter;

interface

uses
 Windows;

type
  PLARGE_INTEGER = ^Int64;

  TNtQueryPerformanceCounter = function (
    PerformanceCounter: PLARGE_INTEGER;
    PerformanceFrequency: PLARGE_INTEGER
  ): LongInt; stdcall;
  
function NtQueryPerformanceCounterPtr: Pointer;

implementation

function NtQueryPerformanceCounterPtr: Pointer;
begin
  Result := GetProcAddress(GetModuleHandle('ntdll.dll'), 'NtQueryPerformanceCounter');
end;

end.