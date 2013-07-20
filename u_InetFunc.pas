unit u_InetFunc;

interface

procedure OpenUrlInBrowser(const URL: string);

function IsGZipped(const AHeader: string): Boolean;

implementation

uses
  Windows,
  ShellAPI;

procedure OpenUrlInBrowser(const URL: string);
begin
  ShellExecute(0, nil, PChar(URL), nil, nil, SW_RESTORE);
end;

function IsGZipped(const AHeader: string): Boolean;
const
  c_Content = 'Content-Encoding';
  c_GZIPped = 'gzip';
var
  VPos: Integer;
  VTxt: AnsiString;
begin
  Result := False;
  VPos := Pos(c_Content, AHeader);
  if (VPos > 0) then begin
    // skip before
    VPos := VPos + Length(c_Content) + 1;
    while (VPos <= Length(AHeader)) and (AHeader[VPos] in [#32,#10,#13,#160,':']) do begin
      Inc(VPos);
    end;
    VTxt := '';
    while (VPos <= Length(AHeader)) and (not (AHeader[VPos] in [#32,#10,#13,#160,':'])) do begin
      VTxt := VTxt + AHeader[VPos];
      Inc(VPos);
    end;
    Result := (VTxt = c_GZIPped);
  end;
end;

end.
