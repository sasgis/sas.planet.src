unit u_SafeStrUtil;

interface

function IsAnsi(const P: PChar; const Len: Integer): Boolean;
function SafeStringToAnsi(const s: string): AnsiString;

implementation

uses
  SysUtils;

function IsAnsi(const P: PChar; const Len: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  if P <> nil then begin
    for I := 0 to Len - 1 do begin
      if Ord(P[I]) > 127 then begin
        Exit;
      end;
    end;
  end;
  Result := True;
end;

function SafeStringToAnsi(const s: string): AnsiString;
begin
  if IsAnsi(@s[1], Length(s)) then begin
    Result := AnsiString(s);
  end else begin
    raise Exception.CreateFmt('String "%s" contain non-ansi characters!', [s]);
  end;
end;

end.
