{                                                                              }
{                        Unicode string functions v3.07                        }
{                                                                              }
{             This unit is copyright © 2002-2004 by David J Butler             }
{                                                                              }
{                  This unit is part of Delphi Fundamentals.                   }
{                   Its original file name is cUnicode.pas                     }
{       The latest version is available from the Fundamentals home page        }
{                     http://fundementals.sourceforge.net/                     }
{                                                                              }
{                I invite you to use this unit, free of charge.                }
{        I invite you to distibute this unit, but it must be for free.         }
{             I also invite you to contribute to its development,              }
{             but do not distribute a modified copy of this file.              }
{                                                                              }
{          A forum is available on SourceForge for general discussion          }
{             http://sourceforge.net/forum/forum.php?forum_id=2117             }
{                                                                              }
{                                                                              }
{ Description:                                                                 }
{   Unicode functions for using WideStrings.                                   }
{                                                                              }
{ Revision history:                                                            }
{   19/04/2002  0.01  Initial version                                          }
{   26/04/2002  0.02  Added WidePos, WideReplace and Append functions.         }
{   28/10/2002  3.03  Refactored for Fundamentals 3.                           }
{   18/12/2002  3.04  Removed dependancy on cUnicodeChar unit.                 }
{   07/09/2003  3.05  Revisions.                                               }
{   10/01/2004  3.06  Unit now uses cUnicodeChar for character functions.      }
{                     Removed dependancy on cUtils unit.                       }
{   01/04/2004  3.07  Compilable with FreePascal-1.92/Win32.                   }
{                                                                              }

{$INCLUDE .\cDefines.inc}
unit cUnicode;

interface

uses
  { Delphi }
  SysUtils,

  { Fundamentals }
  cUnicodeChar;

const
  UnitName      = 'cUnicode';
  UnitVersion   = '3.07';
  UnitCopyright = 'Copyright (c) 2002-2004 David J Butler';



{                                                                              }
{ Unicode errors                                                               }
{                                                                              }
type
  EUnicode = class(Exception);



{                                                                              }
{ WideString functions                                                         }
{                                                                              }
type
  AnsiCharSet = Set of AnsiChar;

function  WideMatchAnsiCharNoCase(const M: Char; const C: WideChar): Boolean;

function  WidePMatchChars(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WidePMatchCharsRev(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WidePMatchAllChars(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WidePMatchAnsiStr(const M: String; const P: PWideChar;
          const CaseSensitive: Boolean = True): Boolean;
function  WidePMatch(const M: WideString; const P: PWideChar): Boolean;

function  WideEqualAnsiStr(const M: String; const S: WideString;
          const CaseSensitive: Boolean = True): Boolean;
function  WideMatchLeftAnsiStr(const M: String; const S: WideString;
          const CaseSensitive: Boolean = True): Boolean;

function  WideZPosChar(const F: WideChar; const P: PWideChar): Integer;
function  WideZPosAnsiChar(const F: Char; const P: PWideChar): Integer;
function  WideZPosAnsiCharSet(const F: AnsiCharSet; const P: PWideChar): Integer;
function  WideZPosAnsiStr(const F: String; const P: PWideChar;
          const CaseSensitive: Boolean = True): Integer;

function  WideZSkipChar(const CharMatchFunc: WideCharMatchFunction;
          var P: PWideChar): Boolean;
function  WideZSkipChars(const CharMatchFunc: WideCharMatchFunction;
          var P: PWideChar): Integer;
function  WidePSkipAnsiChar(const Ch: Char; var P: PWideChar): Boolean;
function  WidePSkipAnsiStr(const M: String; var P: PWideChar;
          const CaseSensitive: Boolean = True): Boolean;

function  WideZExtractBeforeChar(const Ch: WideChar; var P: PWideChar;
          var S: WideString): Boolean;
function  WideZExtractBeforeAnsiChar(const Ch: Char; var P: PWideChar;
          var S: WideString): Boolean;
function  WideZExtractBeforeAnsiCharSet(const C: AnsiCharSet; var P: PWideChar;
          var S: WideString): Boolean;
function  WideZExtractAnsiCharDelimited(const LeftDelimiter, RightDelimiter: Char;
          var P: PWideChar; var S: WideString): Boolean;
function  WideZExtractAnsiCharQuoted(const Delimiter: Char;
          var P: PWideChar; var S: WideString): Boolean;

function  WideDup(const Ch: WideChar; const Count: Integer): WideString;

procedure WideTrimInPlace(var S: WideString;
          const MatchFunc: WideCharMatchfunction = nil);
procedure WideTrimLeftInPlace(var S: WideString;
          const MatchFunc: WideCharMatchfunction = nil);
procedure WideTrimRightInPlace(var S: WideString;
          const MatchFunc: WideCharMatchfunction = nil);

function  WideTrim(const S: WideString;
          const MatchFunc: WideCharMatchfunction = nil): WideString;
function  WideTrimLeft(const S: WideString;
          const MatchFunc: WideCharMatchfunction = nil): WideString;
function  WideTrimRight(const S: WideString;
          const MatchFunc: WideCharMatchfunction = nil): WideString;

function  WideCountChar(const CharMatchFunc: WideCharMatchFunction;
          const S: WideString): Integer; overload;
function  WideCountChar(const Ch: WideChar; const S: WideString): Integer; overload;

function  WidePosChar(const F: WideChar; const S: WideString;
          const StartIndex: Integer = 1): Integer; overload;
function  WidePosAnsiCharSet(const F: AnsiCharSet; const S: WideString;
          const StartIndex: Integer = 1): Integer;
function  WidePos(const F: WideString; const S: WideString;
          const StartIndex: Integer = 1): Integer; overload;

procedure WideReplaceChar(const Find: WideChar; const Replace: WideString;
          var S: WideString);

procedure WideSetLengthAndZero(var S: WideString; const NewLength: Integer);

{$IFDEF DELPHI5}
function  WideUpperCase(const S: WideString): WideString;
function  WideLowerCase(const S: WideString): WideString;
{$ENDIF}

function  WideCopyRange(const S: WideString; const StartIndex, StopIndex: Integer): WideString;
function  WideCopyFrom(const S: WideString; const Index: Integer): WideString;



{                                                                              }
{ Dynamic Array functions                                                      }
{                                                                              }
type
  WideStringArray = Array of WideString;

function  WideAppend(var V : WideStringArray;
          const R: WideString): Integer; overload;
function  WideAppendWideStringArray(var V : WideStringArray;
          const R: WideStringArray): Integer;
function  WideSplit(const S, D: WideString): WideStringArray;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
procedure SelfTest;



implementation



{                                                                              }
{ Match                                                                        }
{                                                                              }
function WideMatchAnsiCharNoCase(const M: Char; const C: WideChar): Boolean;
const ASCIICaseOffset = Ord('a') - Ord('A');
var D, N : Char;
begin
  if Ord(C) > $7F then
    begin
      Result := False;
      exit;
    end;
  D := Char(Ord(C));
  if D in ['A'..'Z'] then
    D := Char(Ord(D) + ASCIICaseOffset);
  N := M;
  if N in ['A'..'Z'] then
    N := Char(Ord(N) + ASCIICaseOffset);
  Result := D = N;
end;

function WidePMatchChars(const CharMatchFunc: WideCharMatchFunction;
    const P: PWideChar; const Length: Integer): Integer;
var Q : PWideChar;
    L : Integer;
    C : WideChar;
begin
  Result := 0;
  Q := P;
  L := Length;
  if not Assigned(Q) or (L = 0) then
    exit;
  C := Q^;
  if (L < 0) and (Ord(C) = 0) then
    exit;
  Repeat
    if not CharMatchFunc(C) then
      exit;
    Inc(Result);
    Inc(Q);
    if L > 0 then
      Dec(L);
    C := Q^;
  Until (L = 0) or ((L < 0) and (Ord(C) = 0));
end;

function WidePMatchCharsRev(const CharMatchFunc: WideCharMatchFunction;
    const P: PWideChar; const Length: Integer): Integer;
var Q : PWideChar;
    L : Integer;
    C : WideChar;
begin
  Result := 0;
  Q := P;
  L := Length;
  if not Assigned(Q) or (L = 0) then
    exit;
  Inc(Q, L - 1);
  C := Q^;
  if (L < 0) and (Ord(C) = 0) then
    exit;
  Repeat
    if not CharMatchFunc(C) then
      exit;
    Inc(Result);
    Dec(Q);
    if L < 0 then
      C := Q^ else
      begin
        Dec(L);
        if L > 0 then
          C := Q^;
      end;
  Until (L = 0) or ((L < 0) and (Ord(C) = 0));
end;

function WidePMatchAllChars(const CharMatchFunc: WideCharMatchFunction; const P: PWideChar; const Length: Integer): Integer;
var Q : PWideChar;
    L : Integer;
    C : WideChar;
begin
  Result := 0;
  Q := P;
  L := Length;
  if not Assigned(Q) or (L = 0) then
    exit;
  C := Q^;
  if (L < 0) and (Ord(C) = 0) then
    exit;
  Repeat
    if CharMatchFunc(C) then
      Inc(Result);
    Inc(Q);
    if L > 0 then
      Dec(L);
    C := Q^;
  Until (L = 0) or ((L < 0) and (Ord(C) = 0));
end;

function WidePMatchAnsiStr(const M: String; const P: PWideChar;
    const CaseSensitive: Boolean): Boolean;
var I, L : Integer;
    Q : PWideChar;
    R : PChar;
begin
  L := Length(M);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  R := Pointer(M);
  Q := P;
  if CaseSensitive then
    begin
      For I := 1 to L do
        if Ord(R^) <> Ord(Q^) then
          begin
            Result := False;
            exit;
          end else
          begin
            Inc(R);
            Inc(Q);
          end;
    end else
    begin
      For I := 1 to L do
        if not WideMatchAnsiCharNoCase(R^, Q^) then
          begin
            Result := False;
            exit;
          end else
          begin
            Inc(R);
            Inc(Q);
          end;
    end;
  Result := True;
end;

function WidePMatch(const M: WideString; const P: PWideChar): Boolean;
var I, L : Integer;
    Q, R : PWideChar;
begin
  L := Length(M);
  if L = 0 then
    begin
      Result := False;
      exit;
    end;
  R := Pointer(M);
  Q := P;
  For I := 1 to L do
    if R^ <> Q^ then
      begin
        Result := False;
        exit;
      end else
      begin
        Inc(R);
        Inc(Q);
      end;
  Result := True;
end;

function WideEqualAnsiStr(const M: String; const S: WideString;
    const CaseSensitive: Boolean): Boolean;
var L : Integer;
begin
  L := Length(M);
  Result := L = Length(S);
  if not Result or (L = 0) then
    exit;
  Result := WidePMatchAnsiStr(M, Pointer(S), CaseSensitive);
end;

function WideMatchLeftAnsiStr(const M: String; const S: WideString;
    const CaseSensitive: Boolean): Boolean;
var L, N : Integer;
begin
  L := Length(M);
  N := Length(S);
  if (L = 0) or (N = 0) or (L > N) then
    begin
      Result := False;
      exit;
    end;
  Result := WidePMatchAnsiStr(M, Pointer(S), CaseSensitive);
end;



{                                                                              }
{ Pos                                                                          }
{                                                                              }
function WideZPosAnsiChar(const F: Char; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Ord(Q^) <> 0 do
    if Ord(Q^) = Ord(F) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;

function WideZPosAnsiCharSet(const F: AnsiCharSet; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Ord(Q^) <> 0 do
    if (Ord(Q^) < $80) and (Char(Ord(Q^)) in F) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;

function WideZPosChar(const F: WideChar; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Ord(Q^) <> 0 do
    if Q^ = F then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;

function WideZPosAnsiStr(const F: String; const P: PWideChar; const CaseSensitive: Boolean): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  While Ord(Q^) <> 0 do
    if WidePMatchAnsiStr(F, Q, CaseSensitive) then
      begin
        Result := I;
        exit;
      end else
      begin
        Inc(Q);
        Inc(I);
      end;
end;



{                                                                              }
{ Skip                                                                         }
{                                                                              }
function WideZSkipChar(const CharMatchFunc: WideCharMatchFunction; var P: PWideChar): Boolean;
var C : WideChar;
begin
  Assert(Assigned(CharMatchFunc));
  C := P^;
  if Ord(C) = 0 then
    begin
      Result := False;
      exit;
    end;
  Result := CharMatchFunc(C);
  if Result then
    Inc(P);
end;

function WideZSkipChars(const CharMatchFunc: WideCharMatchFunction; var P: PWideChar): Integer;
var C : WideChar;
begin
  Assert(Assigned(CharMatchFunc));
  Result := 0;
  if not Assigned(P) then
    exit;
  C := P^;
  While Ord(C) <> 0 do
    if not CharMatchFunc(C) then
      exit
    else
      begin
        Inc(P);
        Inc(Result);
        C := P^;
      end;
end;

function WidePSkipAnsiChar(const Ch: Char; var P: PWideChar): Boolean;
begin
  Result := Ord(P^) = Ord(Ch);
  if Result then
    Inc(P);
end;

function WidePSkipAnsiStr(const M: String; var P: PWideChar; const CaseSensitive: Boolean): Boolean;
begin
  Result := WidePMatchAnsiStr(M, P, CaseSensitive);
  if Result then
    Inc(P, Length(M));
end;



{                                                                              }
{ Extract                                                                      }
{                                                                              }
function WideZExtractBeforeChar(const Ch: WideChar; var P: PWideChar; var S: WideString): Boolean;
var I : Integer;
begin
  I := WideZPosChar(Ch, P);
  Result := I >= 0;
  if I <= 0 then
    begin
      S := '';
      exit;
    end;
  SetLength(S, I);
  Move(P^, Pointer(S)^, I * Sizeof(WideChar));
  Inc(P, I);
end;

function WideZExtractBeforeAnsiChar(const Ch: Char; var P: PWideChar; var S: WideString): Boolean;
var I : Integer;
begin
  I := WideZPosAnsiChar(Ch, P);
  Result := I >= 0;
  if I <= 0 then
    begin
      S := '';
      exit;
    end;
  SetLength(S, I);
  Move(P^, Pointer(S)^, I * Sizeof(WideChar));
  Inc(P, I);
end;

function WideZExtractBeforeAnsiCharSet(const C: AnsiCharSet; var P: PWideChar; var S: WideString): Boolean;
var I : Integer;
begin
  I := WideZPosAnsiCharSet(C, P);
  Result := I >= 0;
  if I <= 0 then
    begin
      S := '';
      exit;
    end;
  SetLength(S, I);
  Move(P^, Pointer(S)^, I * Sizeof(WideChar));
  Inc(P, I);
end;

function WideZExtractAnsiCharDelimited(const LeftDelimiter, RightDelimiter: Char;
         var P: PWideChar; var S: WideString): Boolean;
var Q : PWideChar;
begin
  Q := P;
  Result := Assigned(Q) and (Ord(Q^) < $80) and (Ord(Q^) = Ord(LeftDelimiter));
  if not Result then
    begin
      S := '';
      exit;
    end;
  Inc(Q);
  Result := WideZExtractBeforeAnsiChar(RightDelimiter, Q, S);
  if not Result then
    exit;
  Inc(Q);
  P := Q;
end;

function WideZExtractAnsiCharQuoted(const Delimiter: Char; var P: PWideChar; var S: WideString): Boolean;
begin
  Result := WideZExtractAnsiCharDelimited(Delimiter, Delimiter, P, S);
end;



{                                                                              }
{ Dup                                                                          }
{                                                                              }
function WideDup(const Ch: WideChar; const Count: Integer): WideString;
var I : Integer;
    P : PWideChar;
begin
  if Count <= 0 then
    begin
      Result := '';
      exit;
    end;
  SetLength(Result, Count);
  P := Pointer(Result);
  For I := 1 to Count do
    begin
      P^ := Ch;
      Inc(P);
    end;
end;



{                                                                              }
{ Trim                                                                         }
{                                                                              }
procedure WideTrimLeftInPlace(var S: WideString; const MatchFunc: WideCharMatchFunction);
var I, L : Integer;
    P : PWideChar;
    F: WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWhiteSpace;
  I := 0;
  P := Pointer(S);
  While F(P^) do
    begin
      Inc(I);
      Inc(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  if I = 0 then
    exit;
  S := Copy(S, I + 1, L);
end;

procedure WideTrimRightInPlace(var S: WideString; const MatchFunc: WideCharMatchFunction);
var I, L : Integer;
    P : PWideChar;
    F: WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWhiteSpace;
  I := 0;
  P := Pointer(S);
  Inc(P, L - 1);
  While F(P^) do
    begin
      Inc(I);
      Dec(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  if I = 0 then
    exit;
  SetLength(S, L);
end;

procedure WideTrimInPlace(var S: WideString; const MatchFunc: WideCharMatchFunction);
var I, J, L : Integer;
    P : PWideChar;
    F: WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWhiteSpace;
  I := 0;
  P := Pointer(S);
  Inc(P, L - 1);
  While F(P^) or IsControl(P^) do
    begin
      Inc(I);
      Dec(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  J := 0;
  P := Pointer(S);
  While F(P^) or IsControl(P^) do
    begin
      Inc(J);
      Inc(P);
      Dec(L);
      if L = 0 then
        begin
          S := '';
          exit;
        end;
    end;
  if (I = 0) and (J = 0) then
    exit;
  S := Copy(S, J + 1, L);
end;

function WideTrimLeft(const S: WideString; const MatchFunc: WideCharMatchFunction): WideString;
begin
  Result := S;
  WideTrimLeftInPlace(Result, MatchFunc);
end;

function WideTrimRight(const S: WideString; const MatchFunc: WideCharMatchFunction): WideString;
begin
  Result := S;
  WideTrimRightInPlace(Result, MatchFunc);
end;

function WideTrim(const S: WideString; const MatchFunc: WideCharMatchFunction): WideString;
begin
  Result := S;
  WideTrimInPlace(Result, MatchFunc);
end;



{                                                                              }
{ Count                                                                        }
{                                                                              }
function WideCountChar(const CharMatchFunc: WideCharMatchFunction; const S: WideString): Integer;
begin
  Result := WidePMatchAllChars(CharMatchFunc, Pointer(S), Length(S));
end;

function WideCountChar(const Ch: WideChar; const S: WideString): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := 0;
  Q := PWideChar(S);
  if not Assigned(Q) then
    exit;
  For I := 1 to Length(S) do
    begin
      if Q^ = Ch then
        Inc(Result);
      Inc(Q);
    end;
end;



{                                                                              }
{ Pos                                                                          }
{                                                                              }
function WidePosChar(const F: WideChar; const S: WideString; const StartIndex: Integer): Integer;
var P : PWideChar;
    I, L : Integer;
begin
  L := Length(S);
  if (StartIndex > L) or (StartIndex < 1) then
    begin
      Result := 0;
      exit;
    end;
  P := Pointer(S);
  Inc(P, StartIndex - 1);
  For I := StartIndex to L do
    if P^ = F then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := 0;
end;

function WidePosAnsiCharSet(const F: AnsiCharSet; const S: WideString; const StartIndex: Integer): Integer;
var P : PWideChar;
    I, L : Integer;
begin
  L := Length(S);
  if (StartIndex > L) or (StartIndex < 1) then
    begin
      Result := 0;
      exit;
    end;
  P := Pointer(S);
  Inc(P, StartIndex - 1);
  For I := StartIndex to L do
    if (Ord(P^) <= $FF) and (Char(P^) in F) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := 0;
end;

function WidePos(const F: WideString; const S: WideString; const StartIndex: Integer): Integer;
var P : PWideChar;
    I, L : Integer;
begin
  L := Length(S);
  if (StartIndex > L) or (StartIndex < 1) then
    begin
      Result := 0;
      exit;
    end;
  P := Pointer(S);
  Inc(P, StartIndex - 1);
  For I := StartIndex to L do
    if WidePMatch(F, P) then
      begin
        Result := I;
        exit;
      end
    else
      Inc(P);
  Result := 0;
end;



{                                                                              }
{ Replace                                                                      }
{                                                                              }
procedure WideReplaceChar(const Find: WideChar; const Replace: WideString; var S: WideString);
var C, L, M, I, R : Integer;
    P, Q : PWideChar;
    T : WideString;
begin
  C := WideCountChar(Find, S);
  if C = 0 then
    exit;
  R := Length(Replace);
  M := Length(S);
  L := M + (R - 1) * C;
  if L = 0 then
    begin
      S := '';
      exit;
    end;
  SetLength(T, L);
  P := Pointer(S);
  Q := Pointer(T);
  For I := 1 to M do
    if P^ = Find then
      begin
        if R > 0 then
          begin
            Move(Pointer(Replace)^, Q^, Sizeof(WideChar) * R);
            Inc(Q, R);
          end;
        Inc(P);
      end
    else
      begin
        Q^ := P^;
        Inc(P);
        Inc(Q);
      end;
  S := T;
end;

procedure WideSetLengthAndZero(var S: WideString; const NewLength: Integer);
var L : Integer;
    P : PWideChar;
begin
  L := Length(S);
  if L = NewLength then
    exit;
  SetLength(S, NewLength);
  if L > NewLength then
    exit;
  P := Pointer(S);
  Inc(P, L);
  FillChar(P^, (NewLength - L) * Sizeof(WideChar), #0);
end;

{$IFDEF DELPHI5}
function WideUpperCase(const S: WideString): WideString;
var I : Integer;
begin
  Result := '';
  For I := 1 to Length(S) do
    Result := Result + WideUpCaseFolding(S[I]);
end;

function WideLowerCase(const S: WideString): WideString;
var I : Integer;
begin
  Result := '';
  For I := 1 to Length(S) do
    Result := Result + WideLowCaseFolding(S[I]);
end;
{$ENDIF}

function WideCopyRange(const S: WideString; const StartIndex, StopIndex: Integer): WideString;
var L, I : Integer;
begin
  L := Length(S);
  if (StartIndex > StopIndex) or (StopIndex < 1) or (StartIndex > L) or (L = 0) then
    Result := ''
  else
    begin
      if StartIndex <= 1 then
        if StopIndex >= L then
          begin
            Result := S;
            exit;
          end
        else
          I := 1
      else
        I := StartIndex;
      Result := Copy(S, I, StopIndex - I + 1);
    end;
end;

function WideCopyFrom(const S: WideString; const Index: Integer): WideString;
var L : Integer;
begin
  if Index <= 1 then
    Result := S
  else
    begin
      L := Length(S);
      if (L = 0) or (Index > L) then
        Result := ''
      else
        Result := Copy(S, Index, L - Index + 1);
    end;
end;



{                                                                              }
{ Dynamic Array functions                                                      }
{                                                                              }
function WideAppend(var V : WideStringArray; const R: WideString): Integer;
begin
  Result := Length(V);
  SetLength(V, Result + 1);
  V[Result] := R;
end;

function WideAppendWideStringArray(var V : WideStringArray; const R: WideStringArray): Integer;
var I, LR : Integer;
begin
  Result := Length(V);
  LR := Length(R);
  if LR > 0 then
    begin
      SetLength(V, Result + LR);
      For I := 0 to LR - 1 do
        V[Result + I] := R[I];
    end;
end;

function WideSplit(const S, D: WideString): WideStringArray;
var I, J, L, M: Integer;
begin
  if S = '' then
    begin
      Result := nil;
      exit;
    end;
  M := Length(D);
  if M = 0 then
    begin
      SetLength(Result, 1);
      Result[0] := S;
      exit;
    end;
  L := 0;
  I := 1;
  Repeat
    I := WidePos(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I, M);
  Until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      Result[0] := S;
      exit;
    end;
  L := 0;
  I := 1;
  Repeat
    J := WidePos(D, S, I);
    if J = 0 then
      begin
        Result[L] := WideCopyFrom(S, I);
        break;
      end;
    Result[L] := WideCopyRange(S, I, J - 1);
    Inc(L);
    I := J + M;
  Until False;
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$ASSERTIONS ON}
procedure SelfTest;
var S: WideString;
begin
  Assert(WideMatchAnsiCharNoCase('A', 'a'), 'WideMatchAnsiCharNoCase');
  Assert(WideMatchAnsiCharNoCase('z', 'Z'), 'WideMatchAnsiCharNoCase');
  Assert(WideMatchAnsiCharNoCase('1', '1'), 'WideMatchAnsiCharNoCase');
  Assert(not WideMatchAnsiCharNoCase('A', 'B'), 'WideMatchAnsiCharNoCase');
  Assert(not WideMatchAnsiCharNoCase('0', 'A'), 'WideMatchAnsiCharNoCase');

  Assert(WidePMatchAnsiStr('Unicode', 'uNicode', False), 'WidePMatchAnsiStr');
  Assert(not WidePMatchAnsiStr('Unicode', 'uNicode', True), 'WidePMatchAnsiStr');
  Assert(WidePMatchAnsiStr('Unicode', 'Unicode', True), 'WidePMatchAnsiStr');

  Assert(WideTrimLeft(' X ') = 'X ', 'WideTrimLeft');
  Assert(WideTrimRight(' X ') = ' X', 'WideTrimRight');
  Assert(WideTrim(' X ') = 'X', 'WideTrim');

  Assert(WideDup('X', 0) = '', 'WideDup');
  Assert(WideDup('X', 1) = 'X', 'WideDup');
  Assert(WideDup('A', 4) = 'AAAA', 'WideDup');

  S := 'AXAYAA';
  WideReplaceChar('A', '', S);
  Assert(S = 'XY', 'WideReplaceChar');
  S := 'AXAYAA';
  WideReplaceChar('A', 'B', S);
  Assert(S = 'BXBYBB', 'WideReplaceChar');
  S := 'AXAYAA';
  WideReplaceChar('A', 'CC', S);
  Assert(S = 'CCXCCYCCCC', 'WideReplaceChar');
  S := 'AXAXAA';
  WideReplaceChar('X', 'IJK', S);
  Assert(S = 'AIJKAIJKAA', 'WideReplaceChar');

  Assert(WidePosChar(WideChar('A'), 'XYZABCAACDEF', 1) = 4, 'WidePosChar');
  Assert(WidePosChar(WideChar('A'), 'XYZABCAACDEF', 5) = 7, 'WidePosChar');
  Assert(WidePosChar(WideChar('A'), 'XYZABCAACDEF', 8) = 8, 'WidePosChar');
  Assert(WidePosChar(WideChar('A'), 'XYZABCAACDEF', 9) = 0, 'WidePosChar');
  Assert(WidePosChar(WideChar('Q'), 'XYZABCAACDEF', 1) = 0, 'WidePosChar');
  Assert(WidePos('AB', 'XYZABCAACDEF', 1) = 4, 'WidePos');
  Assert(WidePos('AA', 'XYZABCAACDEF', 1) = 7, 'WidePos');
  Assert(WidePos('A', 'XYZABCAACDEF', 8) = 8, 'WidePos');
  Assert(WidePos('AA', 'XYZABCAACDEF', 8) = 0, 'WidePos');
  Assert(WidePos('AAQ', 'XYZABCAACDEF', 1) = 0, 'WidePos');
  Assert(WideZPosAnsiChar('A', 'XYZABCAACDEF') = 3, 'WideZPosAnsiChar');
  Assert(WideZPosAnsiChar('Q', 'XYZABCAACDEF') = -1, 'WideZPosAnsiChar');
end;



end.

