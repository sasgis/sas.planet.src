unit u_UrlGeneratorHelpers;

interface

function Rand(X: Integer): Integer;            
function GetUnixTime: Int64;
function StrLength (const Str: string): Integer;
function GetAfter(SubStr, Str: string): string;
function GetBefore(SubStr, Str: string): string;
function GetBetween(Str, After, Before: string): string;
function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer;
function RegExprGetMatchSubStr(const AStr, AMatchExpr: string): string;
function RegExprReplaseMatchSubStr(const AStr, AMatchExpr, AReplase: string): string;

implementation

uses
  SysUtils,
  DateUtils,
  RegExpr;

function Rand(X: Integer): Integer;
begin
  Result := Random(X);
end;

function GetUnixTime: Int64;
begin
  Result := DateTimeToUnix(now);
end;

function StrLength (const Str: string): Integer;
begin
  Result := Length(Str);
end;

function GetAfter(SubStr, Str: string): string;
begin
  if pos(substr,str) > 0 then
    result := copy(str,pos(substr,str)+length(substr),length(str))
  else
    result := '';
end;

function GetBefore(SubStr, Str: string): string;
begin
  if pos(substr,str)>0 then
    result := copy(str,1,pos(substr,str)-1)
  else
    result := '';
end;

function GetBetween(Str, After, Before: string): string;
begin
  result := GetBefore(Before,GetAfter(After,str));
end;

function SubStrPos(const Str, SubStr: AnsiString; FromPos: Integer): Integer; assembler;
asm
      PUSH EDI
      PUSH ESI
      PUSH EBX
      PUSH EAX
      OR EAX,EAX
      JE @@2
      OR EDX,EDX
      JE @@2
      DEC ECX
      JS @@2

      MOV EBX,[EAX-4]
      SUB EBX,ECX
      JLE @@2
      SUB EBX,[EDX-4]
      JL @@2
      INC EBX

      ADD EAX,ECX
      MOV ECX,EBX
      MOV EBX,[EDX-4]
      DEC EBX
      MOV EDI,EAX
 @@1: MOV ESI,EDX
      LODSB
      REPNE SCASB
      JNE @@2
      MOV EAX,ECX
      PUSH EDI
      MOV ECX,EBX
      REPE CMPSB
      POP EDI
      MOV ECX,EAX
      JNE @@1
      LEA EAX,[EDI-1]
      POP EDX
      SUB EAX,EDX
      INC EAX
      JMP @@3
 @@2: POP EAX
      XOR EAX,EAX
 @@3: POP EBX
      POP ESI
      POP EDI
end;

function RegExprGetMatchSubStr(const AStr, AMatchExpr: string): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
      Result := VRegExpr.Match[1]
    else
      Result := '';
  finally
    FreeAndNil(VRegExpr);
  end;
end;

function RegExprReplaseMatchSubStr(const AStr, AMatchExpr, AReplase: string): string;
var
  VRegExpr: TRegExpr;
begin
    VRegExpr  := TRegExpr.Create;
  try
    VRegExpr.Expression := AMatchExpr;
    if VRegExpr.Exec(AStr) then
      Result := VRegExpr.Replace(AStr, AReplase, True)
    else
      Result := AStr;
  finally
    FreeAndNil(VRegExpr);
  end;
end;

end.
