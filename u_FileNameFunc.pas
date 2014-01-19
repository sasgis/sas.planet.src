unit u_FileNameFunc;

interface

function PrepareFileName(const AFileName: string): string;

implementation

uses
  SysUtils;

function PrepareFileName(const AFileName: string): string;
begin
  Result := AFileName;
  Result := StringReplace(Result, '\', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '/', '-', [rfReplaceAll]);
  Result := StringReplace(Result, ':', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '*', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '?', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '-', [rfReplaceAll]);
  Result := StringReplace(Result, '|', '-', [rfReplaceAll]);
end;

end.
