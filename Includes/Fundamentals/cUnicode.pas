{******************************************************************************}
{                                                                              }
{   Library:          Fundamentals 4.00                                        }
{   File name:        cUnicode.pas                                             }
{   File version:     4.12                                                     }
{   Description:      Unicode character and string functions                   }
{                                                                              }
{   Copyright:        Copyright © 2002-2011, David J Butler                    }
{                     All rights reserved.                                     }
{                     Redistribution and use in source and binary forms, with  }
{                     or without modification, are permitted provided that     }
{                     the following conditions are met:                        }
{                     Redistributions of source code must retain the above     }
{                     copyright notice, this list of conditions and the        }
{                     following disclaimer.                                    }
{                     THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND   }
{                     CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED          }
{                     WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED   }
{                     WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A          }
{                     PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL     }
{                     THE REGENTS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,    }
{                     INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR             }
{                     CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,    }
{                     PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF     }
{                     USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)         }
{                     HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER   }
{                     IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING        }
{                     NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE   }
{                     USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE             }
{                     POSSIBILITY OF SUCH DAMAGE.                              }
{                                                                              }
{   Home page:        http://fundementals.sourceforge.net                      }
{   Forum:            http://sourceforge.net/forum/forum.php?forum_id=2117     }
{   E-mail:           fundamentalslib at gmail.com                             }
{                                                                              }
{ Revision history:                                                            }
{                                                                              }
{   19/04/2002  0.01  Initial version                                          }
{   21/04/2002  0.02  Added case and decomposition functions                   }
{   26/04/2002  0.03  Added WidePos, WideReplace and Append functions.         }
{   28/10/2002  3.04  Refactored for Fundamentals 3.                           }
{   07/09/2003  3.05  Revision.                                                }
{   10/01/2004  3.06  Removed dependancy on cUtils unit.                       }
{   10/01/2004  3.07  Changes to allow smart-linking by the compiler.          }
{                     Typically this saves 100-200K on the executable size.    }
{   01/04/2004  3.08  Compilable with FreePascal-1.92 Win32 i386.              }
{   22/08/2004  3.09  Compilable with Delphi 8.                                }
{   14/07/2005  4.10  Compilable with FreePascal 2 Win32 i386.                 }
{   17/07/2005  4.11  Merged cUnicode and cUnicodeChar units.                  }
{   27/08/2005  4.12  Revised for Fundamentals 4.                              }
{                                                                              }
{ Supported compilers:                                                         }
{                                                                              }
{   Borland Delphi 5/6/7/2005/2006/2007 Win32 i386                             }
{   Borland Delphi 8 .NET                                                      }
{   FreePascal 2 Win32 i386                                                    }
{   FreePascal 2 Linux i386                                                    }
{                                                                              }
{ Notes:                                                                       }
{                                                                              }
{   Most functions in this unit work from tables in source code form.          }
{   All tables were generated from the Unicode 3.2 data.                       }
{                                                                              }
{   The unit doesn't have depend on any other units, including standard        }
{   system units.                                                              }
{                                                                              }
{******************************************************************************}

{$INCLUDE cDefines.inc}
{$IFDEF FREEPASCAL}{$IFDEF DEBUG}
  {$WARNINGS OFF}{$HINTS OFF}
{$ENDIF}{$ENDIF}
unit cUnicode;

interface



{                                                                              }
{ Unicode character constants                                                  }
{                                                                              }
const
  WideNULL = WideChar(#0);
  WideSOH  = WideChar(#1);
  WideSTX  = WideChar(#2);
  WideETX  = WideChar(#3);
  WideEOT  = WideChar(#4);
  WideENQ  = WideChar(#5);
  WideACK  = WideChar(#6);
  WideBEL  = WideChar(#7);
  WideBS   = WideChar(#8);
  WideHT   = WideChar(#9);
  WideLF   = WideChar(#10);
  WideVT   = WideChar(#11);
  WideFF   = WideChar(#12);
  WideCR   = WideChar(#13);
  WideNAK  = WideChar(#21);
  WideSYN  = WideChar(#22);
  WideCAN  = WideChar(#24);
  WideEOF  = WideChar(#26);
  WideESC  = WideChar(#27);
  WideSP   = WideChar(#32);

  WideCRLF : WideString = #13#10;

  WideSingleQuote        = WideChar('''');
  WideDoubleQuote        = WideChar('"');

  WideNoBreakSpace       = WideChar(#$00A0);
  WideLineSeparator      = WideChar(#$2028);
  WideParagraphSeparator = WideChar(#$2029);

  WideBOM_MSB_First      = WideChar(#$FFFE);
  WideBOM_LSB_First      = WideChar(#$FEFF);

  WideObjectReplacement  = WideChar(#$FFFC);
  WideCharReplacement    = WideChar(#$FFFD);
  WideInvalid            = WideChar(#$FFFF);

  WideCopyrightSign      = WideChar(#$00A9);
  WideRegisteredSign     = WideChar(#$00AE);

  WideHighSurrogateFirst        = WideChar(#$D800);
  WideHighSurrogateLast         = WideChar(#$DB7F);
  WideLowSurrogateFirst         = WideChar(#$DC00);
  WideLowSurrogateLast          = WideChar(#$DFFF);
  WidePrivateHighSurrogateFirst = WideChar(#$DB80);
  WidePrivateHighSurrogateLast  = WideChar(#$DBFF);



{                                                                              }
{ Unicode character functions                                                  }
{                                                                              }
{$IFDEF DELPHI5}
type
  UCS4Char = LongWord;
{$ENDIF}
{$IFDEF CLR}
type
  UCS4Char = LongWord;
{$ENDIF}

type
  WideCharMatchFunction = function (const Ch: WideChar): Boolean;

function  IsASCIIChar(const Ch: WideChar): Boolean; {$IFDEF UseInline}inline;{$ENDIF}
function  IsWhiteSpace(const Ch: WideChar): Boolean;
function  IsControl(const Ch: WideChar): Boolean;
function  IsControlOrWhiteSpace(const Ch: WideChar): Boolean;
function  IsIgnorable(const Ch: UCS4Char): Boolean;

function  IsDash(const Ch: WideChar): Boolean;
function  IsHyphen(const Ch: WideChar): Boolean;
function  IsFullStop(const Ch: WideChar): Boolean;
function  IsComma(const Ch: WideChar): Boolean;
function  IsExclamationMark(const Ch: WideChar): Boolean;
function  IsQuestionMark(const Ch: WideChar): Boolean;

function  IsLeftParenthesis(const Ch: WideChar): Boolean;
function  IsLeftBracket(const Ch: WideChar): Boolean;
function  GetRightParenthesis(const LeftParenthesis: WideChar): WideChar;
function  GetRightBracket(const LeftBracket: WideChar): WideChar;

function  IsSingularQuotationMark(const Ch: WideChar): Boolean;
function  IsOpeningQuotationMark(const Ch: WideChar): Boolean;
function  IsClosingQuotationMark(const Ch: WideChar): Boolean;
function  GetClosingQuotationMark(const OpeningQuote: WideChar): WideChar;
function  GetOpeningQuotationMark(const ClosingQuote: WideChar): WideChar;

function  IsPunctuation(const Ch: WideChar): Boolean;

function  IsDecimalDigit(const Ch: UCS4Char): Boolean; overload;
function  IsDecimalDigit(const Ch: WideChar): Boolean; overload;
function  IsASCIIDecimalDigit(const Ch: WideChar): Boolean;
function  DecimalDigitValue(const Ch: UCS4Char): Integer; overload;
function  DecimalDigitValue(const Ch: WideChar): Integer; overload;
function  FractionCharacterValue(const Ch: WideChar; var A, B: Integer): Boolean;
function  RomanNumeralValue(const Ch: WideChar): Integer;

function  IsHexDigit(const Ch: UCS4Char): Boolean; overload;
function  IsHexDigit(const Ch: WideChar): Boolean; overload;
function  IsASCIIHexDigit(const Ch: WideChar): Boolean;
function  HexDigitValue(const Ch: UCS4Char): Integer; overload;
function  HexDigitValue(const Ch: WideChar): Integer; overload;

function  IsUpperCase(const Ch: WideChar): Boolean;
function  IsLowerCase(const Ch: WideChar): Boolean;
function  IsTitleCase(const Ch: WideChar): Boolean;
function  WideUpCase(const Ch: WideChar): WideChar;
function  WideLowCase(const Ch: WideChar): WideChar;
function  WideUpCaseFolding(const Ch: WideChar): WideString;
function  WideLowCaseFolding(const Ch: WideChar): WideString;
function  WideTitleCaseFolding(const Ch: WideChar): WideString;
function  WideIsEqualNoCase(const A, B: WideChar): Boolean;
function  IsLetter(const Ch: WideChar): Boolean;
function  IsAlphabetic(const Ch: WideChar): Boolean;

function  GetCombiningClass(const Ch: WideChar): Byte;
function  GetCharacterDecomposition(const Ch: UCS4Char): WideString; overload;
function  GetCharacterDecomposition(const Ch: WideChar): WideString; overload;



{                                                                              }
{ WideString functions                                                         }
{                                                                              }
type
  AnsiCharSet = Set of AnsiChar;

function  WideMatchAnsiCharNoCase(const M: AnsiChar; const C: WideChar): Boolean;

{$IFNDEF CLR}
function  WidePMatchChars(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WidePMatchCharsRev(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WidePMatchAllChars(const CharMatchFunc: WideCharMatchFunction;
          const P: PWideChar; const Length: Integer = -1): Integer;
function  WidePMatchAnsiStr(const M: AnsiString; const P: PWideChar;
          const CaseSensitive: Boolean = True): Boolean;
function  WidePMatch(const M: WideString; const P: PWideChar): Boolean;
{$ENDIF}

function  WideEqualAnsiStr(const M: AnsiString; const S: WideString;
          const CaseSensitive: Boolean = True): Boolean;
function  WideMatchLeftAnsiStr(const M: AnsiString; const S: WideString;
          const CaseSensitive: Boolean = True): Boolean;

{$IFNDEF CLR}
function  WideZPosChar(const F: WideChar; const P: PWideChar): Integer;
function  WideZPosAnsiChar(const F: AnsiChar; const P: PWideChar): Integer;
function  WideZPosAnsiCharSet(const F: AnsiCharSet; const P: PWideChar): Integer;
function  WideZPosAnsiStr(const F: AnsiString; const P: PWideChar;
          const CaseSensitive: Boolean = True): Integer;

function  WideZSkipChar(const CharMatchFunc: WideCharMatchFunction;
          var P: PWideChar): Boolean;
function  WideZSkipChars(const CharMatchFunc: WideCharMatchFunction;
          var P: PWideChar): Integer;
function  WidePSkipAnsiChar(const Ch: AnsiChar; var P: PWideChar): Boolean;
function  WidePSkipAnsiStr(const M: AnsiString; var P: PWideChar;
          const CaseSensitive: Boolean = True): Boolean;

function  WideZExtractBeforeChar(const Ch: WideChar; var P: PWideChar;
          var S: WideString): Boolean;
function  WideZExtractBeforeAnsiChar(const Ch: AnsiChar; var P: PWideChar;
          var S: WideString): Boolean;
function  WideZExtractBeforeAnsiCharSet(const C: AnsiCharSet; var P: PWideChar;
          var S: WideString): Boolean;
function  WideZExtractAnsiCharDelimited(const LeftDelimiter, RightDelimiter: AnsiChar;
          var P: PWideChar; var S: WideString): Boolean;
function  WideZExtractAnsiCharQuoted(const Delimiter: AnsiChar;
          var P: PWideChar; var S: WideString): Boolean;
{$ENDIF}

function  WideDup(const Ch: WideChar; const Count: Integer): WideString;

procedure WideTrimInPlace(var S: WideString;
          const MatchFunc: WideCharMatchFunction = nil);
procedure WideTrimLeftInPlace(var S: WideString;
          const MatchFunc: WideCharMatchFunction = nil);
procedure WideTrimRightInPlace(var S: WideString;
          const MatchFunc: WideCharMatchFunction = nil);

function  WideTrim(const S: WideString;
          const MatchFunc: WideCharMatchFunction = nil): WideString;
function  WideTrimLeft(const S: WideString;
          const MatchFunc: WideCharMatchFunction = nil): WideString;
function  WideTrimRight(const S: WideString;
          const MatchFunc: WideCharMatchFunction = nil): WideString;

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
{$IFDEF DEBUG}{$IFDEF SELFTEST}
procedure SelfTest;
{$ENDIF}{$ENDIF}



implementation



{                                                                              }
{ Character functions                                                          }
{                                                                              }
function IsASCIIChar(const Ch: WideChar): Boolean;
begin
  Result := Ord(Ch) <= $7F;
end;

function IsWhiteSpace(const Ch: WideChar): Boolean;
begin
  case Ch of
    #$0009..#$000D,    // ASCII CONTROL
    #$0020,            // SPACE
    #$0085,            // <control>
    #$00A0,            // NO-BREAK SPACE
    #$1680,            // OGHAM SPACE MARK
    #$2000..#$200A,    // EN QUAD..HAIR SPACE
    #$2028,            // LINE SEPARATOR
    #$2029,            // PARAGRAPH SEPARATOR
    #$202F,            // NARROW NO-BREAK SPACE
    #$3000 :           // IDEOGRAPHIC SPACE
      Result := True;
  else
    Result := False;
  end;
end;

function IsControl(const Ch: WideChar): Boolean;
begin
  case Ch of
    #$0000..#$001F,
    #$007F..#$009F :
      Result := True;
  else
    Result := False;
  end;
end;

function IsControlOrWhiteSpace(const Ch: WideChar): Boolean;
begin
  Result := IsControl(Ch) or IsWhiteSpace(Ch);
end;

// Derived from 'Cf' + 'Cc' + 'Cs' - White_Space
function IsIgnorable(const Ch: UCS4Char): Boolean;
begin
  case Ch of
    $0000..$0008,     // # Cc   [9] <control>..<control>
    $000E..$001F,     // # Cc  [18] <control>..<control>
    $007F..$0084,     // # Cc   [6] <control>..<control>
    $0086..$009F,     // # Cc  [26] <control>..<control>
    $06DD,            // # Cf       ARABIC END OF AYAH
    $070F,            // # Cf       SYRIAC ABBREVIATION MARK
    $180B..$180D,     // # Mn   [3] MONGOLIAN FREE VARIATION SELECTOR ONE..MONGOLIAN FREE VARIATION SELECTOR THREE
    $180E,            // # Cf       MONGOLIAN VOWEL SEPARATOR
    $200C..$200F,     // # Cf   [4] ZERO WIDTH NON-JOINER..RIGHT-TO-LEFT MARK
    $202A..$202E,     // # Cf   [5] LEFT-TO-RIGHT EMBEDDING..RIGHT-TO-LEFT OVERRIDE
    $2060..$2063,     // # Cf   [4] WORD JOINER..INVISIBLE SEPARATOR
    $2064..$2069,     // # Cn   [6]
    $206A..$206F,     // # Cf   [6] INHIBIT SYMMETRIC SWAPPING..NOMINAL DIGIT SHAPES
    $D800..$DFFF,     // # Cs [2048]
    $FE00..$FE0F,     // # Mn  [16] VARIATION SELECTOR-1..VARIATION SELECTOR-16
    $FEFF,            // # Cf       ZERO WIDTH NO-BREAK SPACE
    $FFF0..$FFF8,     // # Cn   [9]
    $FFF9..$FFFB,     // # Cf   [3] INTERLINEAR ANNOTATION ANCHOR..INTERLINEAR ANNOTATION TERMINATOR
    $1D173..$1D17A,   // # Cf   [8] MUSICAL SYMBOL BEGIN BEAM..MUSICAL SYMBOL END PHRASE
    $E0000,           // # Cn
    $E0001,           // # Cf       LANGUAGE TAG
    $E0002..$E001F,   // # Cn  [30]
    $E0020..$E007F,   // # Cf  [96] TAG SPACE..CANCEL TAG
    $E0080..$E0FFF :  // # Cn [3968]
      Result := True;
  else
    Result := False;
  end;
end;

function IsDash(const Ch: WideChar): Boolean;
begin
  case Ch of
    #$002D,            // HYPHEN-MINUS
    #$00AD,            // SOFT HYPHEN
    #$058A,            // ARMENIAN HYPHEN
    #$1806,            // MONGOLIAN TODO SOFT HYPHEN
    #$2010..#$2015,    // HYPHEN..HORIZONTAL BAR
    #$207B,            // SUPERSCRIPT MINUS
    #$208B,            // SUBSCRIPT MINUS
    #$2212,            // MINUS SIGN
    #$301C,            // WAVE DASH
    #$3030,            // WAVY DASH
    #$FE31..#$FE32,    // PRESENTATION FORM FOR VERTICAL EM DASH..PRESENTATION FORM FOR VERTICAL EN DASH
    #$FE58,            // SMALL EM DASH
    #$FE63,            // SMALL HYPHEN-MINUS
    #$FF0D :           // FULLWIDTH HYPHEN-MINUS
      Result := True;
  else
    Result := False;
  end;
end;

function IsHyphen(const Ch: WideChar): Boolean;
begin
  case Ch of
    #$002D,            // HYPHEN-MINUS
    #$00AD,            // SOFT HYPHEN
    #$058A,            // ARMENIAN HYPHEN
    #$1806,            // MONGOLIAN TODO SOFT HYPHEN
    #$2010..#$2011,    // HYPHEN..NON-BREAKING HYPHEN
    #$30FB,            // KATAKANA MIDDLE DOT
    #$FE63,            // SMALL HYPHEN-MINUS
    #$FF0D,            // FULLWIDTH HYPHEN-MINUS
    #$FF65 :           // HALFWIDTH KATAKANA MIDDLE DOT
      Result := True;
  else
    Result := False;
  end;
end;

function IsFullStop(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $002E,  // FULL STOP
    $0589,  // ARMENIAN FULL STOP
    $06D4,  // ARABIC FULL STOP
    $0701,  // SYRIAC SUPRALINEAR FULL STOP
    $0702,  // SYRIAC SUBLINEAR FULL STOP
    $1362,  // ETHIOPIC FULL STOP
    $166E,  // CANADIAN SYLLABICS FULL STOP
    $1803,  // MONGOLIAN FULL STOP
    $1809,  // MONGOLIAN MANCHU FULL STOP
    $3002,  // IDEOGRAPHIC FULL STOP
    $FE52,  // SMALL FULL STOP
    $FF0E,  // FULLWIDTH FULL STOP
    $FF61 : // HALFWIDTH IDEOGRAPHIC FULL STOP
      Result := True;
  else
    Result := False;
  end;
end;

function IsComma(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $002C,  // COMMA
    $055D,  // ARMENIAN COMMA
    $060C,  // ARABIC COMMA
    $0F14,  // TIBETAN MARK GTER TSHEG
    $1363,  // ETHIOPIC COMMA
    $1802,  // MONGOLIAN COMMA
    $1808,  // MONGOLIAN MANCHU COMMA
    $3001,  // IDEOGRAPHIC COMMA
    $FE50,  // SMALL COMMA
    $FE51,  // SMALL IDEOGRAPHIC COMMA
    $FF0C,  // FULLWIDTH COMMA
    $FF64 : // HALFWIDTH IDEOGRAPHIC COMMA
      Result := True;
  else
    Result := False;
  end;
end;

function IsExclamationMark(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $0021,    // EXCLAMATION MARK
    $00A1,    // INVERTED EXCLAMATION MARK
    $055C,    // ARMENIAN EXCLAMATION MARK
    $203C,    // DOUBLE EXCLAMATION MARK
    $203D,    // INTERROBANG
    $2048,    // QUESTION EXCLAMATION MARK
    $2049,    // EXCLAMATION QUESTION MARK
    $FE57,    // SMALL EXCLAMATION MARK
    $FF01 :   // FULLWIDTH EXCLAMATION MARK
      Result := True;
  else
    Result := False;
  end;
end;

function IsQuestionMark(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $003F,    // QUESTION MARK
    $00BF,    // INVERTED QUESTION MARK
    $037E,    // GREEK QUESTION MARK
    $055E,    // ARMENIAN QUESTION MARK
    $061F,    // ARABIC QUESTION MARK
    $1367,    // ETHIOPIC QUESTION MARK
    $2049,    // EXCLAMATION QUESTION MARK
    $FE56,    // SMALL QUESTION MARK
    $FF1F :   // FULLWIDTH QUESTION MARK
      Result := True;
  else
    Result := False;
  end;
end;

function GetRightParenthesis(const LeftParenthesis: WideChar): WideChar;
begin
  case Ord(LeftParenthesis) of
    $0028 : Result := #$0029;  // PARENTHESIS
    $207D : Result := #$207E;  // SUPERSCRIPT PARENTHESIS
    $208D : Result := #$208E;  // SUBSCRIPT PARENTHESIS
    $FD3E : Result := #$FD3F;  // ORNATE PARENTHESIS
    $FE35 : Result := #$FE36;  // PRESENTATION FORM FOR VERTICAL PARENTHESIS
    $FE59 : Result := #$FE5A;  // SMALL PARENTHESIS
    $FF08 : Result := #$FF09;  // FULLWIDTH PARENTHESIS
  else
    Result := #$0000;
  end;
end;

function IsLeftParenthesis(const Ch: WideChar): Boolean;
begin
  Result := GetRightParenthesis(Ch) <> #$0000;
end;

function GetRightBracket(const LeftBracket: WideChar): WideChar;
begin
  case Ord(LeftBracket) of
    $005B : Result := #$005D;  // SQUARE BRACKET
    $007B : Result := #$007D;  // CURLY BRACKET
    $2045 : Result := #$2046;  // SQUARE BRACKET WITH QUILL
    $2329 : Result := #$232A;  // POINTING ANGLE BRACKET
    $3008 : Result := #$3009;  // ANGLE BRACKET
    $300A : Result := #$300B;  // DOUBLE ANGLE BRACKET
    $300C : Result := #$300D;  // CORNER BRACKET
    $300E : Result := #$300F;  // WHITE CORNER BRACKET
    $3010 : Result := #$3011;  // BLACK LENTICULAR BRACKET
    $3014 : Result := #$3015;  // TORTOISE SHELL BRACKET
    $3016 : Result := #$3017;  // WHITE LENTICULAR BRACKET
    $3018 : Result := #$3019;  // WHITE TORTOISE SHELL BRACKET
    $301A : Result := #$301B;  // WHITE SQUARE BRACKET
    $FE37 : Result := #$FE38;  // PRESENTATION FORM FOR VERTICAL CURLY BRACKET
    $FE39 : Result := #$FE3A;  // PRESENTATION FORM FOR VERTICAL TORTOISE SHELL BRACKET
    $FE3B : Result := #$FE3C;  // PRESENTATION FORM FOR VERTICAL BLACK LENTICULAR BRACKET
    $FE3D : Result := #$FE3E;  // PRESENTATION FORM FOR VERTICAL DOUBLE ANGLE BRACKET
    $FE3F : Result := #$FE40;  // PRESENTATION FORM FOR VERTICAL ANGLE BRACKET
    $FE41 : Result := #$FE42;  // PRESENTATION FORM FOR VERTICAL CORNER BRACKET
    $FE43 : Result := #$FE44;  // PRESENTATION FORM FOR VERTICAL WHITE CORNER BRACKET
    $FE5B : Result := #$FE5C;  // SMALL CURLY BRACKET
    $FE5D : Result := #$FE5E;  // SMALL TORTOISE SHELL BRACKET
    $FF3B : Result := #$FF3D;  // FULLWIDTH SQUARE BRACKET
    $FF5B : Result := #$FF5D;  // FULLWIDTH CURLY BRACKET
    $FF62 : Result := #$FF63;  // HALFWIDTH CORNER BRACKET
  else
    Result := #$0000;
  end;
end;

function IsLeftBracket(const Ch: WideChar): Boolean;
begin
  Result := GetRightBracket(Ch) <> #$0000;
end;

function IsSingularQuotationMark(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $0022,   //        QUOTATION MARK
    $0027,   //        APOSTROPHE
    $FF02,   //        FULLWIDTH QUOTATION MARK
    $FF07 :  //        FULLWIDTH APOSTROPHE
      Result := True;
  else
    Result := False;
  end;
end;

function GetClosingQuotationMark(const OpeningQuote: WideChar): WideChar;
begin
  case Ord(OpeningQuote) of
    $00AB : Result := #$00BB;     // LEFT/RIGHT -POINTING DOUBLE ANGLE QUOTATION MARK
    $2018 : Result := #$2019;     // LEFT/RIGHT SINGLE QUOTATION MARK
    $201A : Result := #$201B;     // SINGLE LOW-9 QUOTATION MARK / SINGLE HIGH-REVERSED-9 QUOTATION MARK
    $201C : Result := #$201D;     // LEFT/RIGHT DOUBLE QUOTATION MARK
    $201E : Result := #$201F;     // DOUBLE LOW-9 QUOTATION MARK / DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    $2039 : Result := #$203A;     // SINGLE LEFT/RIGHT -POINTING ANGLE QUOTATION MARK
    $301D : Result := #$301E;     // REVERSED DOUBLE PRIME QUOTATION MARK / DOUBLE PRIME QUOTATION MARK (also $301F)
  else
    Result := #$0000;
  end;
end;

function IsOpeningQuotationMark(const Ch: WideChar): Boolean;
begin
  Result := GetClosingQuotationMark(Ch) <> #$0000;
end;

function GetOpeningQuotationMark(const ClosingQuote: WideChar): WideChar;
begin
  case Ord(ClosingQuote) of
    $00BB : Result := #$00AB;     // LEFT/RIGHT -POINTING DOUBLE ANGLE QUOTATION MARK
    $2019 : Result := #$2018;     // LEFT/RIGHT SINGLE QUOTATION MARK
    $201B : Result := #$201A;     // SINGLE LOW-9 QUOTATION MARK / SINGLE HIGH-REVERSED-9 QUOTATION MARK
    $201D : Result := #$201C;     // LEFT/RIGHT DOUBLE QUOTATION MARK
    $201F : Result := #$201E;     // DOUBLE LOW-9 QUOTATION MARK / DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    $203A : Result := #$2039;     // SINGLE LEFT/RIGHT -POINTING ANGLE QUOTATION MARK
    $301E : Result := #$301D;     // REVERSED DOUBLE PRIME QUOTATION MARK / DOUBLE PRIME QUOTATION MARK
    $301F : Result := #$301D;     // REVERSED DOUBLE PRIME QUOTATION MARK / LOW DOUBLE PRIME QUOTATION MARK
  else
    Result := #$0000;
  end;
end;

function IsClosingQuotationMark(const Ch: WideChar): Boolean;
begin
  Result := GetOpeningQuotationMark(Ch) <> #$0000;
end;

function IsPunctuation(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $0021,   // EXCLAMATION MARK
    $0022,   // QUOTATION MARK
    $0023,   // NUMBER SIGN
    $0025,   // PERCENT SIGN
    $0026,   // AMPERSAND
    $0027,   // APOSTROPHE
    $0028,   // LEFT PARENTHESIS
    $0029,   // RIGHT PARENTHESIS
    $002A,   // ASTERISK
    $002C,   // COMMA
    $002D,   // HYPHEN-MINUS
    $002E,   // FULL STOP
    $002F,   // SOLIDUS
    $003A,   // COLON
    $003B,   // SEMICOLON
    $003F,   // QUESTION MARK
    $0040,   // COMMERCIAL AT
    $005B,   // LEFT SQUARE BRACKET
    $005C,   // REVERSE SOLIDUS
    $005D,   // RIGHT SQUARE BRACKET
    $005F,   // LOW LINE
    $007B,   // LEFT CURLY BRACKET
    $007D,   // RIGHT CURLY BRACKET
    $00A1,   // INVERTED EXCLAMATION MARK
    $00AB,   // LEFT-POINTING DOUBLE ANGLE QUOTATION MARK
    $00AD,   // SOFT HYPHEN
    $00B7,   // MIDDLE DOT
    $00BB,   // RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK
    $00BF,   // INVERTED QUESTION MARK
    $037E,   // GREEK QUESTION MARK
    $0387,   // GREEK ANO TELEIA
    $055A,   // ARMENIAN APOSTROPHE
    $055B,   // ARMENIAN EMPHASIS MARK
    $055C,   // ARMENIAN EXCLAMATION MARK
    $055D,   // ARMENIAN COMMA
    $055E,   // ARMENIAN QUESTION MARK
    $055F,   // ARMENIAN ABBREVIATION MARK
    $0589,   // ARMENIAN FULL STOP
    $058A,   // ARMENIAN HYPHEN
    $05BE,   // HEBREW PUNCTUATION MAQAF
    $05C0,   // HEBREW PUNCTUATION PASEQ
    $05C3,   // HEBREW PUNCTUATION SOF PASUQ
    $05F3,   // HEBREW PUNCTUATION GERESH
    $05F4,   // HEBREW PUNCTUATION GERSHAYIM
    $060C,   // ARABIC COMMA
    $061B,   // ARABIC SEMICOLON
    $061F,   // ARABIC QUESTION MARK
    $066A,   // ARABIC PERCENT SIGN
    $066B,   // ARABIC DECIMAL SEPARATOR
    $066C,   // ARABIC THOUSANDS SEPARATOR
    $066D,   // ARABIC FIVE POINTED STAR
    $06D4,   // ARABIC FULL STOP
    $0700,   // SYRIAC END OF PARAGRAPH
    $0701,   // SYRIAC SUPRALINEAR FULL STOP
    $0702,   // SYRIAC SUBLINEAR FULL STOP
    $0703,   // SYRIAC SUPRALINEAR COLON
    $0704,   // SYRIAC SUBLINEAR COLON
    $0705,   // SYRIAC HORIZONTAL COLON
    $0706,   // SYRIAC COLON SKEWED LEFT
    $0707,   // SYRIAC COLON SKEWED RIGHT
    $0708,   // SYRIAC SUPRALINEAR COLON SKEWED LEFT
    $0709,   // SYRIAC SUBLINEAR COLON SKEWED RIGHT
    $070A,   // SYRIAC CONTRACTION
    $070B,   // SYRIAC HARKLEAN OBELUS
    $070C,   // SYRIAC HARKLEAN METOBELUS
    $070D,   // SYRIAC HARKLEAN ASTERISCUS
    $0964,   // DEVANAGARI DANDA
    $0965,   // DEVANAGARI DOUBLE DANDA
    $0970,   // DEVANAGARI ABBREVIATION SIGN
    $0DF4,   // SINHALA PUNCTUATION KUNDDALIYA
    $0E4F,   // THAI CHARACTER FONGMAN
    $0E5A,   // THAI CHARACTER ANGKHANKHU
    $0E5B,   // THAI CHARACTER KHOMUT
    $0F04,   // TIBETAN MARK INITIAL YIG MGO MDUN MA
    $0F05,   // TIBETAN MARK CLOSING YIG MGO SGAB MA
    $0F06,   // TIBETAN MARK CARET YIG MGO PHUR SHAD MA
    $0F07,   // TIBETAN MARK YIG MGO TSHEG SHAD MA
    $0F08,   // TIBETAN MARK SBRUL SHAD
    $0F09,   // TIBETAN MARK BSKUR YIG MGO
    $0F0A,   // TIBETAN MARK BKA- SHOG YIG MGO
    $0F0B,   // TIBETAN MARK INTERSYLLABIC TSHEG
    $0F0C,   // TIBETAN MARK DELIMITER TSHEG BSTAR
    $0F0D,   // TIBETAN MARK SHAD
    $0F0E,   // TIBETAN MARK NYIS SHAD
    $0F0F,   // TIBETAN MARK TSHEG SHAD
    $0F10,   // TIBETAN MARK NYIS TSHEG SHAD
    $0F11,   // TIBETAN MARK RIN CHEN SPUNGS SHAD
    $0F12,   // TIBETAN MARK RGYA GRAM SHAD
    $0F3A,   // TIBETAN MARK GUG RTAGS GYON
    $0F3B,   // TIBETAN MARK GUG RTAGS GYAS
    $0F3C,   // TIBETAN MARK ANG KHANG GYON
    $0F3D,   // TIBETAN MARK ANG KHANG GYAS
    $0F85,   // TIBETAN MARK PALUTA
    $104A,   // MYANMAR SIGN LITTLE SECTION
    $104B,   // MYANMAR SIGN SECTION
    $104C,   // MYANMAR SYMBOL LOCATIVE
    $104D,   // MYANMAR SYMBOL COMPLETED
    $104E,   // MYANMAR SYMBOL AFOREMENTIONED
    $104F,   // MYANMAR SYMBOL GENITIVE
    $10FB,   // GEORGIAN PARAGRAPH SEPARATOR
    $1361,   // ETHIOPIC WORDSPACE
    $1362,   // ETHIOPIC FULL STOP
    $1363,   // ETHIOPIC COMMA
    $1364,   // ETHIOPIC SEMICOLON
    $1365,   // ETHIOPIC COLON
    $1366,   // ETHIOPIC PREFACE COLON
    $1367,   // ETHIOPIC QUESTION MARK
    $1368,   // ETHIOPIC PARAGRAPH SEPARATOR
    $166D,   // CANADIAN SYLLABICS CHI SIGN
    $166E,   // CANADIAN SYLLABICS FULL STOP
    $169B,   // OGHAM FEATHER MARK
    $169C,   // OGHAM REVERSED FEATHER MARK
    $16EB,   // RUNIC SINGLE PUNCTUATION
    $16EC,   // RUNIC MULTIPLE PUNCTUATION
    $16ED,   // RUNIC CROSS PUNCTUATION
    $17D4,   // KHMER SIGN KHAN
    $17D5,   // KHMER SIGN BARIYOOSAN
    $17D6,   // KHMER SIGN CAMNUC PII KUUH
    $17D7,   // KHMER SIGN LEK TOO
    $17D8,   // KHMER SIGN BEYYAL
    $17D9,   // KHMER SIGN PHNAEK MUAN
    $17DA,   // KHMER SIGN KOOMUUT
    $17DC,   // KHMER SIGN AVAKRAHASANYA
    $1800,   // MONGOLIAN BIRGA
    $1801,   // MONGOLIAN ELLIPSIS
    $1802,   // MONGOLIAN COMMA
    $1803,   // MONGOLIAN FULL STOP
    $1804,   // MONGOLIAN COLON
    $1805,   // MONGOLIAN FOUR DOTS
    $1806,   // MONGOLIAN TODO SOFT HYPHEN
    $1807,   // MONGOLIAN SIBE SYLLABLE BOUNDARY MARKER
    $1808,   // MONGOLIAN MANCHU COMMA
    $1809,   // MONGOLIAN MANCHU FULL STOP
    $180A,   // MONGOLIAN NIRUGU
    $2010,   // HYPHEN
    $2011,   // NON-BREAKING HYPHEN
    $2012,   // FIGURE DASH
    $2013,   // EN DASH
    $2014,   // EM DASH
    $2015,   // HORIZONTAL BAR
    $2016,   // DOUBLE VERTICAL LINE
    $2017,   // DOUBLE LOW LINE
    $2018,   // LEFT SINGLE QUOTATION MARK
    $2019,   // RIGHT SINGLE QUOTATION MARK
    $201A,   // SINGLE LOW-9 QUOTATION MARK
    $201B,   // SINGLE HIGH-REVERSED-9 QUOTATION MARK
    $201C,   // LEFT DOUBLE QUOTATION MARK
    $201D,   // RIGHT DOUBLE QUOTATION MARK
    $201E,   // DOUBLE LOW-9 QUOTATION MARK
    $201F,   // DOUBLE HIGH-REVERSED-9 QUOTATION MARK
    $2020,   // DAGGER
    $2021,   // DOUBLE DAGGER
    $2022,   // BULLET
    $2023,   // TRIANGULAR BULLET
    $2024,   // ONE DOT LEADER
    $2025,   // TWO DOT LEADER
    $2026,   // HORIZONTAL ELLIPSIS
    $2027,   // HYPHENATION POINT
    $2030,   // PER MILLE SIGN
    $2031,   // PER TEN THOUSAND SIGN
    $2032,   // PRIME
    $2033,   // DOUBLE PRIME
    $2034,   // TRIPLE PRIME
    $2035,   // REVERSED PRIME
    $2036,   // REVERSED DOUBLE PRIME
    $2037,   // REVERSED TRIPLE PRIME
    $2038,   // CARET
    $2039,   // SINGLE LEFT-POINTING ANGLE QUOTATION MARK
    $203A,   // SINGLE RIGHT-POINTING ANGLE QUOTATION MARK
    $203B,   // REFERENCE MARK
    $203C,   // DOUBLE EXCLAMATION MARK
    $203D,   // INTERROBANG
    $203E,   // OVERLINE
    $203F,   // UNDERTIE
    $2040,   // CHARACTER TIE
    $2041,   // CARET INSERTION POINT
    $2042,   // ASTERISM
    $2043,   // HYPHEN BULLET
    $2045,   // LEFT SQUARE BRACKET WITH QUILL
    $2046,   // RIGHT SQUARE BRACKET WITH QUILL
    $2048,   // QUESTION EXCLAMATION MARK
    $2049,   // EXCLAMATION QUESTION MARK
    $204A,   // TIRONIAN SIGN ET
    $204B,   // REVERSED PILCROW SIGN
    $204C,   // BLACK LEFTWARDS BULLET
    $204D,   // BLACK RIGHTWARDS BULLET
    $207D,   // SUPERSCRIPT LEFT PARENTHESIS
    $207E,   // SUPERSCRIPT RIGHT PARENTHESIS
    $208D,   // SUBSCRIPT LEFT PARENTHESIS
    $208E,   // SUBSCRIPT RIGHT PARENTHESIS
    $2329,   // LEFT-POINTING ANGLE BRACKET
    $232A,   // RIGHT-POINTING ANGLE BRACKET
    $3001,   // IDEOGRAPHIC COMMA
    $3002,   // IDEOGRAPHIC FULL STOP
    $3003,   // DITTO MARK
    $3008,   // LEFT ANGLE BRACKET
    $3009,   // RIGHT ANGLE BRACKET
    $300A,   // LEFT DOUBLE ANGLE BRACKET
    $300B,   // RIGHT DOUBLE ANGLE BRACKET
    $300C,   // LEFT CORNER BRACKET
    $300D,   // RIGHT CORNER BRACKET
    $300E,   // LEFT WHITE CORNER BRACKET
    $300F,   // RIGHT WHITE CORNER BRACKET
    $3010,   // LEFT BLACK LENTICULAR BRACKET
    $3011,   // RIGHT BLACK LENTICULAR BRACKET
    $3014,   // LEFT TORTOISE SHELL BRACKET
    $3015,   // RIGHT TORTOISE SHELL BRACKET
    $3016,   // LEFT WHITE LENTICULAR BRACKET
    $3017,   // RIGHT WHITE LENTICULAR BRACKET
    $3018,   // LEFT WHITE TORTOISE SHELL BRACKET
    $3019,   // RIGHT WHITE TORTOISE SHELL BRACKET
    $301A,   // LEFT WHITE SQUARE BRACKET
    $301B,   // RIGHT WHITE SQUARE BRACKET
    $301C,   // WAVE DASH
    $301D,   // REVERSED DOUBLE PRIME QUOTATION MARK
    $301E,   // DOUBLE PRIME QUOTATION MARK
    $301F,   // LOW DOUBLE PRIME QUOTATION MARK
    $3030,   // WAVY DASH
    $30FB,   // KATAKANA MIDDLE DOT
    $FD3E,   // ORNATE LEFT PARENTHESIS
    $FD3F,   // ORNATE RIGHT PARENTHESIS
    $FE30,   // PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
    $FE31,   // PRESENTATION FORM FOR VERTICAL EM DASH
    $FE32,   // PRESENTATION FORM FOR VERTICAL EN DASH
    $FE33,   // PRESENTATION FORM FOR VERTICAL LOW LINE
    $FE34,   // PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
    $FE35,   // PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
    $FE36,   // PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
    $FE37,   // PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
    $FE38,   // PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
    $FE39,   // PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
    $FE3A,   // PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
    $FE3B,   // PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
    $FE3C,   // PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
    $FE3D,   // PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
    $FE3E,   // PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
    $FE3F,   // PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
    $FE40,   // PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
    $FE41,   // PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
    $FE42,   // PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
    $FE43,   // PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
    $FE44,   // PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
    $FE49,   // DASHED OVERLINE
    $FE4A,   // CENTRELINE OVERLINE
    $FE4B,   // WAVY OVERLINE
    $FE4C,   // DOUBLE WAVY OVERLINE
    $FE4D,   // DASHED LOW LINE
    $FE4E,   // CENTRELINE LOW LINE
    $FE4F,   // WAVY LOW LINE
    $FE50,   // SMALL COMMA
    $FE51,   // SMALL IDEOGRAPHIC COMMA
    $FE52,   // SMALL FULL STOP
    $FE54,   // SMALL SEMICOLON
    $FE55,   // SMALL COLON
    $FE56,   // SMALL QUESTION MARK
    $FE57,   // SMALL EXCLAMATION MARK
    $FE58,   // SMALL EM DASH
    $FE59,   // SMALL LEFT PARENTHESIS
    $FE5A,   // SMALL RIGHT PARENTHESIS
    $FE5B,   // SMALL LEFT CURLY BRACKET
    $FE5C,   // SMALL RIGHT CURLY BRACKET
    $FE5D,   // SMALL LEFT TORTOISE SHELL BRACKET
    $FE5E,   // SMALL RIGHT TORTOISE SHELL BRACKET
    $FE5F,   // SMALL NUMBER SIGN
    $FE60,   // SMALL AMPERSAND
    $FE61,   // SMALL ASTERISK
    $FE63,   // SMALL HYPHEN-MINUS
    $FE68,   // SMALL REVERSE SOLIDUS
    $FE6A,   // SMALL PERCENT SIGN
    $FE6B,   // SMALL COMMERCIAL AT
    $FF01,   // FULLWIDTH EXCLAMATION MARK
    $FF02,   // FULLWIDTH QUOTATION MARK
    $FF03,   // FULLWIDTH NUMBER SIGN
    $FF05,   // FULLWIDTH PERCENT SIGN
    $FF06,   // FULLWIDTH AMPERSAND
    $FF07,   // FULLWIDTH APOSTROPHE
    $FF08,   // FULLWIDTH LEFT PARENTHESIS
    $FF09,   // FULLWIDTH RIGHT PARENTHESIS
    $FF0A,   // FULLWIDTH ASTERISK
    $FF0C,   // FULLWIDTH COMMA
    $FF0D,   // FULLWIDTH HYPHEN-MINUS
    $FF0E,   // FULLWIDTH FULL STOP
    $FF0F,   // FULLWIDTH SOLIDUS
    $FF1A,   // FULLWIDTH COLON
    $FF1B,   // FULLWIDTH SEMICOLON
    $FF1F,   // FULLWIDTH QUESTION MARK
    $FF20,   // FULLWIDTH COMMERCIAL AT
    $FF3B,   // FULLWIDTH LEFT SQUARE BRACKET
    $FF3C,   // FULLWIDTH REVERSE SOLIDUS
    $FF3D,   // FULLWIDTH RIGHT SQUARE BRACKET
    $FF3F,   // FULLWIDTH LOW LINE
    $FF5B,   // FULLWIDTH LEFT CURLY BRACKET
    $FF5D,   // FULLWIDTH RIGHT CURLY BRACKET
    $FF61,   // HALFWIDTH IDEOGRAPHIC FULL STOP
    $FF62,   // HALFWIDTH LEFT CORNER BRACKET
    $FF63,   // HALFWIDTH RIGHT CORNER BRACKET
    $FF64,   // HALFWIDTH IDEOGRAPHIC COMMA
    $FF65 :  // HALFWIDTH KATAKANA MIDDLE DOT
      Result := True;
  else
    Result := False;
  end;
end;

function DecimalDigitBase(const Ch: UCS4Char): UCS4Char;
begin
  case Ch of
    $0030..$0039   : Result := $0030;  // DIGIT
    $0660..$0669   : Result := $0660;  // ARABIC-INDIC DIGIT
    $06F0..$06F9   : Result := $06F0;  // EXTENDED ARABIC-INDIC DIGIT
    $0966..$096F   : Result := $0966;  // DEVANAGARI DIGIT
    $09E6..$09EF   : Result := $09E6;  // BENGALI DIGIT
    $0A66..$0A6F   : Result := $0A66;  // GURMUKHI DIGIT
    $0AE6..$0AEF   : Result := $0AE6;  // GUJARATI DIGIT
    $0B66..$0B6F   : Result := $0B66;  // ORIYA DIGIT
    $0C66..$0C6F   : Result := $0C66;  // TELUGU DIGIT
    $0CE6..$0CEF   : Result := $0CE6;  // KANNADA DIGIT
    $0D66..$0D6F   : Result := $0D66;  // MALAYALAM DIGIT
    $0E50..$0E59   : Result := $0E50;  // THAI DIGIT
    $0ED0..$0ED9   : Result := $0ED0;  // LAO DIGIT
    $0F20..$0F29   : Result := $0F20;  // TIBETAN DIGIT
    $1040..$1049   : Result := $1040;  // MYANMAR DIGIT
    $17E0..$17E9   : Result := $17E0;  // KHMER DIGIT
    $1810..$1819   : Result := $1810;  // MONGOLIAN DIGIT
    $2070..$2079   : Result := $2070;  // SUPERSCRIPT DIGIT
    $2080..$2089   : Result := $2080;  // SUBSCRIPT DIGIT
    $FF10..$FF19   : Result := $FF10;  // FULLWIDTH DIGIT
    $1D7CE..$1D7D7 : Result := $1D7CE; // MATHEMATICAL BOLD DIGIT
    $1D7D8..$1D7E1 : Result := $1D7D8; // MATHEMATICAL DOUBLE-STRUCK DIGIT
    $1D7E2..$1D7EB : Result := $1D7E2; // MATHEMATICAL SANS-SERIF DIGIT
    $1D7EC..$1D7F5 : Result := $1D7EC; // MATHEMATICAL SANS-SERIF BOLD DIGIT
    $1D7F6..$1D7FF : Result := $1D7F6; // MATHEMATICAL MONOSPACE DIGIT
  else
    Result := 0;
  end;
end;

function DecimalDigitValue(const Ch: UCS4Char): Integer;
var I : LongWord;
begin
  I := DecimalDigitBase(Ch);
  if I = 0 then
    Result := -1
  else
    Result := Ch - I;
end;

function DecimalDigitValue(const Ch: WideChar): Integer;
begin
  Result := DecimalDigitValue(Ord(Ch));
end;

function IsDecimalDigit(const Ch: UCS4Char): Boolean;
begin
  Result := DecimalDigitBase(Ch) <> 0;
end;

function IsDecimalDigit(const Ch: WideChar): Boolean;
begin
  Result := DecimalDigitBase(Ord(Ch)) <> 0;
end;

function IsASCIIDecimalDigit(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $0030..$0039 : Result := True;
  else
    Result := False;
  end;
end;

function FractionCharacterValue(const Ch: WideChar; var A, B : Integer): Boolean;
begin
  case Ord(Ch) of
    $00BC : begin A := 1; B := 4; end;       // # No       VULGAR FRACTION ONE QUARTER
    $00BD : begin A := 1; B := 2; end;       // # No       VULGAR FRACTION ONE HALF
    $00BE : begin A := 3; B := 4; end;       // # No       VULGAR FRACTION THREE QUARTERS
    $0F2A : begin A := 1; B := 2; end;       // # No       TIBETAN DIGIT HALF ONE
    $2153 : begin A := 1; B := 3; end;       // # No       VULGAR FRACTION ONE THIRD
    $2154 : begin A := 2; B := 3; end;       // # No       VULGAR FRACTION TWO THIRDS
    $2155 : begin A := 1; B := 5; end;       // # No       VULGAR FRACTION ONE FIFTH
    $2156 : begin A := 2; B := 5; end;       // # No       VULGAR FRACTION TWO FIFTHS
    $2157 : begin A := 3; B := 5; end;       // # No       VULGAR FRACTION THREE FIFTHS
    $2158 : begin A := 4; B := 5; end;       // # No       VULGAR FRACTION FOUR FIFTHS
    $2159 : begin A := 1; B := 6; end;       // # No       VULGAR FRACTION ONE SIXTH
    $215A : begin A := 5; B := 6; end;       // # No       VULGAR FRACTION FIVE SIXTHS
    $215B : begin A := 1; B := 8; end;       // # No       VULGAR FRACTION ONE EIGHTH
    $215C : begin A := 3; B := 8; end;       // # No       VULGAR FRACTION THREE EIGHTHS
    $215D : begin A := 5; B := 8; end;       // # No       VULGAR FRACTION FIVE EIGHTHS
    $215E : begin A := 7; B := 8; end;       // # No       VULGAR FRACTION SEVEN EIGHTHS
  else
    begin A := 0; B := 0; end;
  end;
  Result := B <> 0;
end;

function RomanNumeralValue(const Ch: WideChar): Integer;
begin
  case Ord(Ch) of
    $2160        : Result := 1;     //  Nl       ROMAN NUMERAL ONE
    $2161        : Result := 2;     //  Nl       ROMAN NUMERAL TWO
    $2162        : Result := 3;     //  Nl       ROMAN NUMERAL THREE
    $2163        : Result := 4;     //  Nl       ROMAN NUMERAL FOUR
    $2164        : Result := 5;     //  Nl       ROMAN NUMERAL FIVE
    $2165        : Result := 6;     //  Nl       ROMAN NUMERAL SIX
    $2166        : Result := 7;     //  Nl       ROMAN NUMERAL SEVEN
    $2167        : Result := 8;     //  Nl       ROMAN NUMERAL EIGHT
    $2168        : Result := 9;     //  Nl       ROMAN NUMERAL NINE
    $2169        : Result := 10;    //  Nl       ROMAN NUMERAL TEN
    $216A        : Result := 11;    //  Nl       ROMAN NUMERAL ELEVEN
    $216B        : Result := 12;    //  Nl       ROMAN NUMERAL TWELVE
    $216C        : Result := 50;    //  Nl       ROMAN NUMERAL FIFTY
    $216D        : Result := 100;   //  Nl       ROMAN NUMERAL ONE HUNDRED
    $216E        : Result := 500;   //  Nl       ROMAN NUMERAL FIVE HUNDRED
    $216F        : Result := 1000;  //  Nl       ROMAN NUMERAL ONE THOUSAND
    $2170        : Result := 1;     //  Nl       SMALL ROMAN NUMERAL ONE
    $2171        : Result := 2;     //  Nl       SMALL ROMAN NUMERAL TWO
    $2172        : Result := 3;     //  Nl       SMALL ROMAN NUMERAL THREE
    $2173        : Result := 4;     //  Nl       SMALL ROMAN NUMERAL FOUR
    $2174        : Result := 5;     //  Nl       SMALL ROMAN NUMERAL FIVE
    $2175        : Result := 6;     //  Nl       SMALL ROMAN NUMERAL SIX
    $2176        : Result := 7;     //  Nl       SMALL ROMAN NUMERAL SEVEN
    $2177        : Result := 8;     //  Nl       SMALL ROMAN NUMERAL EIGHT
    $2178        : Result := 9;     //  Nl       SMALL ROMAN NUMERAL NINE
    $2179        : Result := 10;    //  Nl       SMALL ROMAN NUMERAL TEN
    $217A        : Result := 11;    //  Nl       SMALL ROMAN NUMERAL ELEVEN
    $217B        : Result := 12;    //  Nl       SMALL ROMAN NUMERAL TWELVE
    $217C        : Result := 50;    //  Nl       SMALL ROMAN NUMERAL FIFTY
    $217D        : Result := 100;   //  Nl       SMALL ROMAN NUMERAL ONE HUNDRED
    $217E        : Result := 500;   //  Nl       SMALL ROMAN NUMERAL FIVE HUNDRED
    $217F..$2180 : Result := 1000;  //  Nl   [2] SMALL ROMAN NUMERAL ONE THOUSAND..ROMAN NUMERAL ONE THOUSAND C D
    $2181        : Result := 5000;  //  Nl       ROMAN NUMERAL FIVE THOUSAND
    $2182        : Result := 10000; //  Nl       ROMAN NUMERAL TEN THOUSAND
  else
    Result := 0;
  end;
end;

function LatinAlphaCharBase(const Ch: WideChar): UCS4Char;
begin
  case Ord(Ch) of
    $0041..$005A : Result := $0041;  // LATIN CAPITAL LETTER
    $0061..$007A : Result := $0061;  // LATIN SMALL LETTER
    $FF21..$FF3A : Result := $FF21;  // FULLWIDTH LATIN CAPITAL LETTER
    $FF41..$FF5A : Result := $FF41;  // FULLWIDTH LATIN SMALL LETTER
  else
    Result := 0;
  end;
end;

function HexAlphaDigitBase(const Ch: WideChar): UCS4Char; overload;
begin
  Result := LatinAlphaCharBase(Ch);
  if Result = 0 then
    exit;
  if Ord(Ch) - Result > 5 then
    Result := 0;
end;

function HexAlphaDigitBase(const Ch: UCS4Char): UCS4Char; overload;
begin
  if Ch <= $FFFF then
    Result := HexAlphaDigitBase(WideChar(Ch))
  else
    case Ch of
      $1D400..$1D405 : Result := $1D400;  // MATHEMATICAL BOLD CAPITAL
      $1D41A..$1D41F : Result := $1D41A;  // MATHEMATICAL BOLD SMALL
      $1D434..$1D439 : Result := $1D434;  // MATHEMATICAL ITALIC CAPITAL
      $1D44E..$1D453 : Result := $1D44E;  // MATHEMATICAL ITALIC SMALL
      $1D468..$1D46D : Result := $1D468;  // MATHEMATICAL BOLD ITALIC CAPITAL
      $1D482..$1D487 : Result := $1D482;  // MATHEMATICAL BOLD ITALIC SMALL
      $1D49C..$1D4A1 : Result := $1D49C;  // MATHEMATICAL SCRIPT CAPITAL
      $1D4B6..$1D4BB : Result := $1D4B6;  // MATHEMATICAL SCRIPT SMALL
      $1D4D0..$1D4D5 : Result := $1D4D0;  // MATHEMATICAL BOLD SCRIPT CAPITAL
      $1D4EA..$1D4EF : Result := $1D4EA;  // MATHEMATICAL BOLD SCRIPT SMALL
      $1D504..$1D509 : Result := $1D504;  // MATHEMATICAL FRAKTUR CAPITAL
      $1D51E..$1D523 : Result := $1D51E;  // MATHEMATICAL FRAKTUR SMALL
      $1D538..$1D53D : Result := $1D538;  // MATHEMATICAL DOUBLE-STRUCK CAPITAL
      $1D552..$1D557 : Result := $1D552;  // MATHEMATICAL DOUBLE-STRUCK SMALL
      $1D56C..$1D571 : Result := $1D56C;  // MATHEMATICAL BOLD FRAKTUR CAPITAL
      $1D586..$1D58B : Result := $1D586;  // MATHEMATICAL BOLD FRAKTUR SMALL
      $1D5A0..$1D5A5 : Result := $1D5A0;  // MATHEMATICAL SANS-SERIF CAPITAL
      $1D5BA..$1D5BF : Result := $1D5BA;  // MATHEMATICAL SANS-SERIF SMALL
      $1D5D4..$1D5D9 : Result := $1D5D4;  // MATHEMATICAL SANS-SERIF BOLD CAPITAL
      $1D5EE..$1D5F3 : Result := $1D5EE;  // MATHEMATICAL SANS-SERIF BOLD SMALL
      $1D608..$1D60D : Result := $1D608;  // MATHEMATICAL SANS-SERIF ITALIC CAPITAL
      $1D622..$1D627 : Result := $1D622;  // MATHEMATICAL SANS-SERIF ITALIC SMALL
      $1D63C..$1D641 : Result := $1D63C;  // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL
      $1D656..$1D65B : Result := $1D656;  // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL
      $1D670..$1D675 : Result := $1D670;  // MATHEMATICAL MONOSPACE CAPITAL
      $1D68A..$1D68F : Result := $1D68A;  // MATHEMATICAL MONOSPACE SMALL
      $E0041..$E0046 : Result := $E0041;  // TAG LATIN CAPITAL LETTER
    else
      Result := 0;
    end;
end;

function HexDigitValue(const Ch: UCS4Char): Integer;
var I : UCS4Char;
begin
  Result := DecimalDigitValue(Ch);
  if Result >= 0 then
    exit;
  I := HexAlphaDigitBase(Ch);
  if I > 0 then
    Result := Ch - I + 10;
end;

function HexDigitValue(const Ch: WideChar): Integer;
var I : UCS4Char;
begin
  Result := DecimalDigitValue(Ch);
  if Result >= 0 then
    exit;
  I := HexAlphaDigitBase(Ch);
  if I > 0 then
    Result := Ord(Ch) - I + 10;
end;

function IsHexDigit(const Ch: UCS4Char): Boolean;
begin
  Result := HexDigitValue(Ch) >= 0;
end;

function IsHexDigit(const Ch: WideChar): Boolean;
begin
  Result := HexDigitValue(Ch) >= 0;
end;

function IsASCIIHexDigit(const Ch: WideChar): Boolean;
begin
  case Ord(Ch) of
    $0030..$0039,
    $0041..$0046,
    $0061..$0066  : Result := True;
  else
    Result := False;
  end;
end;

{ Unicode letter table                                                         }
type
  TUnicodeLetterAttr = (laUpper, laLower);
  TUnicodeLetterInfo = packed record
    Unicode  : WideChar;
    Attr     : TUnicodeLetterAttr;
    CaseCode : WideChar;
  end;
  PUnicodeLetterInfo = ^TUnicodeLetterInfo;

const
  // Derived from 'Lu' and 'Ll' class
  UnicodeLetterEntries = 1492; // ~7K table
  UnicodeLetterInfo : Array[0..UnicodeLetterEntries - 1] of TUnicodeLetterInfo = (
    (Unicode:#$0041; Attr:laUpper; CaseCode:#$0061),   // LATIN CAPITAL LETTER A
    (Unicode:#$0042; Attr:laUpper; CaseCode:#$0062),   // LATIN CAPITAL LETTER B
    (Unicode:#$0043; Attr:laUpper; CaseCode:#$0063),   // LATIN CAPITAL LETTER C
    (Unicode:#$0044; Attr:laUpper; CaseCode:#$0064),   // LATIN CAPITAL LETTER D
    (Unicode:#$0045; Attr:laUpper; CaseCode:#$0065),   // LATIN CAPITAL LETTER E
    (Unicode:#$0046; Attr:laUpper; CaseCode:#$0066),   // LATIN CAPITAL LETTER F
    (Unicode:#$0047; Attr:laUpper; CaseCode:#$0067),   // LATIN CAPITAL LETTER G
    (Unicode:#$0048; Attr:laUpper; CaseCode:#$0068),   // LATIN CAPITAL LETTER H
    (Unicode:#$0049; Attr:laUpper; CaseCode:#$0069),   // LATIN CAPITAL LETTER I
    (Unicode:#$004A; Attr:laUpper; CaseCode:#$006A),   // LATIN CAPITAL LETTER J
    (Unicode:#$004B; Attr:laUpper; CaseCode:#$006B),   // LATIN CAPITAL LETTER K
    (Unicode:#$004C; Attr:laUpper; CaseCode:#$006C),   // LATIN CAPITAL LETTER L
    (Unicode:#$004D; Attr:laUpper; CaseCode:#$006D),   // LATIN CAPITAL LETTER M
    (Unicode:#$004E; Attr:laUpper; CaseCode:#$006E),   // LATIN CAPITAL LETTER N
    (Unicode:#$004F; Attr:laUpper; CaseCode:#$006F),   // LATIN CAPITAL LETTER O
    (Unicode:#$0050; Attr:laUpper; CaseCode:#$0070),   // LATIN CAPITAL LETTER P
    (Unicode:#$0051; Attr:laUpper; CaseCode:#$0071),   // LATIN CAPITAL LETTER Q
    (Unicode:#$0052; Attr:laUpper; CaseCode:#$0072),   // LATIN CAPITAL LETTER R
    (Unicode:#$0053; Attr:laUpper; CaseCode:#$0073),   // LATIN CAPITAL LETTER S
    (Unicode:#$0054; Attr:laUpper; CaseCode:#$0074),   // LATIN CAPITAL LETTER T
    (Unicode:#$0055; Attr:laUpper; CaseCode:#$0075),   // LATIN CAPITAL LETTER U
    (Unicode:#$0056; Attr:laUpper; CaseCode:#$0076),   // LATIN CAPITAL LETTER V
    (Unicode:#$0057; Attr:laUpper; CaseCode:#$0077),   // LATIN CAPITAL LETTER W
    (Unicode:#$0058; Attr:laUpper; CaseCode:#$0078),   // LATIN CAPITAL LETTER X
    (Unicode:#$0059; Attr:laUpper; CaseCode:#$0079),   // LATIN CAPITAL LETTER Y
    (Unicode:#$005A; Attr:laUpper; CaseCode:#$007A),   // LATIN CAPITAL LETTER Z
    (Unicode:#$0061; Attr:laLower; CaseCode:#$0041),   // LATIN SMALL LETTER A
    (Unicode:#$0062; Attr:laLower; CaseCode:#$0042),   // LATIN SMALL LETTER B
    (Unicode:#$0063; Attr:laLower; CaseCode:#$0043),   // LATIN SMALL LETTER C
    (Unicode:#$0064; Attr:laLower; CaseCode:#$0044),   // LATIN SMALL LETTER D
    (Unicode:#$0065; Attr:laLower; CaseCode:#$0045),   // LATIN SMALL LETTER E
    (Unicode:#$0066; Attr:laLower; CaseCode:#$0046),   // LATIN SMALL LETTER F
    (Unicode:#$0067; Attr:laLower; CaseCode:#$0047),   // LATIN SMALL LETTER G
    (Unicode:#$0068; Attr:laLower; CaseCode:#$0048),   // LATIN SMALL LETTER H
    (Unicode:#$0069; Attr:laLower; CaseCode:#$0049),   // LATIN SMALL LETTER I
    (Unicode:#$006A; Attr:laLower; CaseCode:#$004A),   // LATIN SMALL LETTER J
    (Unicode:#$006B; Attr:laLower; CaseCode:#$004B),   // LATIN SMALL LETTER K
    (Unicode:#$006C; Attr:laLower; CaseCode:#$004C),   // LATIN SMALL LETTER L
    (Unicode:#$006D; Attr:laLower; CaseCode:#$004D),   // LATIN SMALL LETTER M
    (Unicode:#$006E; Attr:laLower; CaseCode:#$004E),   // LATIN SMALL LETTER N
    (Unicode:#$006F; Attr:laLower; CaseCode:#$004F),   // LATIN SMALL LETTER O
    (Unicode:#$0070; Attr:laLower; CaseCode:#$0050),   // LATIN SMALL LETTER P
    (Unicode:#$0071; Attr:laLower; CaseCode:#$0051),   // LATIN SMALL LETTER Q
    (Unicode:#$0072; Attr:laLower; CaseCode:#$0052),   // LATIN SMALL LETTER R
    (Unicode:#$0073; Attr:laLower; CaseCode:#$0053),   // LATIN SMALL LETTER S
    (Unicode:#$0074; Attr:laLower; CaseCode:#$0054),   // LATIN SMALL LETTER T
    (Unicode:#$0075; Attr:laLower; CaseCode:#$0055),   // LATIN SMALL LETTER U
    (Unicode:#$0076; Attr:laLower; CaseCode:#$0056),   // LATIN SMALL LETTER V
    (Unicode:#$0077; Attr:laLower; CaseCode:#$0057),   // LATIN SMALL LETTER W
    (Unicode:#$0078; Attr:laLower; CaseCode:#$0058),   // LATIN SMALL LETTER X
    (Unicode:#$0079; Attr:laLower; CaseCode:#$0059),   // LATIN SMALL LETTER Y
    (Unicode:#$007A; Attr:laLower; CaseCode:#$005A),   // LATIN SMALL LETTER Z
    (Unicode:#$00AA; Attr:laLower; CaseCode:#$FFFF),   // FEMININE ORDINAL INDICATOR
    (Unicode:#$00B5; Attr:laLower; CaseCode:#$039C),   // MICRO SIGN
    (Unicode:#$00BA; Attr:laLower; CaseCode:#$FFFF),   // MASCULINE ORDINAL INDICATOR
    (Unicode:#$00C0; Attr:laUpper; CaseCode:#$00E0),   // LATIN CAPITAL LETTER A WITH GRAVE
    (Unicode:#$00C1; Attr:laUpper; CaseCode:#$00E1),   // LATIN CAPITAL LETTER A WITH ACUTE
    (Unicode:#$00C2; Attr:laUpper; CaseCode:#$00E2),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00C3; Attr:laUpper; CaseCode:#$00E3),   // LATIN CAPITAL LETTER A WITH TILDE
    (Unicode:#$00C4; Attr:laUpper; CaseCode:#$00E4),   // LATIN CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$00C5; Attr:laUpper; CaseCode:#$00E5),   // LATIN CAPITAL LETTER A WITH RING ABOVE
    (Unicode:#$00C6; Attr:laUpper; CaseCode:#$00E6),   // LATIN CAPITAL LETTER AE
    (Unicode:#$00C7; Attr:laUpper; CaseCode:#$00E7),   // LATIN CAPITAL LETTER C WITH CEDILLA
    (Unicode:#$00C8; Attr:laUpper; CaseCode:#$00E8),   // LATIN CAPITAL LETTER E WITH GRAVE
    (Unicode:#$00C9; Attr:laUpper; CaseCode:#$00E9),   // LATIN CAPITAL LETTER E WITH ACUTE
    (Unicode:#$00CA; Attr:laUpper; CaseCode:#$00EA),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00CB; Attr:laUpper; CaseCode:#$00EB),   // LATIN CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$00CC; Attr:laUpper; CaseCode:#$00EC),   // LATIN CAPITAL LETTER I WITH GRAVE
    (Unicode:#$00CD; Attr:laUpper; CaseCode:#$00ED),   // LATIN CAPITAL LETTER I WITH ACUTE
    (Unicode:#$00CE; Attr:laUpper; CaseCode:#$00EE),   // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00CF; Attr:laUpper; CaseCode:#$00EF),   // LATIN CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$00D0; Attr:laUpper; CaseCode:#$00F0),   // LATIN CAPITAL LETTER ETH
    (Unicode:#$00D1; Attr:laUpper; CaseCode:#$00F1),   // LATIN CAPITAL LETTER N WITH TILDE
    (Unicode:#$00D2; Attr:laUpper; CaseCode:#$00F2),   // LATIN CAPITAL LETTER O WITH GRAVE
    (Unicode:#$00D3; Attr:laUpper; CaseCode:#$00F3),   // LATIN CAPITAL LETTER O WITH ACUTE
    (Unicode:#$00D4; Attr:laUpper; CaseCode:#$00F4),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00D5; Attr:laUpper; CaseCode:#$00F5),   // LATIN CAPITAL LETTER O WITH TILDE
    (Unicode:#$00D6; Attr:laUpper; CaseCode:#$00F6),   // LATIN CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$00D8; Attr:laUpper; CaseCode:#$00F8),   // LATIN CAPITAL LETTER O WITH STROKE
    (Unicode:#$00D9; Attr:laUpper; CaseCode:#$00F9),   // LATIN CAPITAL LETTER U WITH GRAVE
    (Unicode:#$00DA; Attr:laUpper; CaseCode:#$00FA),   // LATIN CAPITAL LETTER U WITH ACUTE
    (Unicode:#$00DB; Attr:laUpper; CaseCode:#$00FB),   // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00DC; Attr:laUpper; CaseCode:#$00FC),   // LATIN CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$00DD; Attr:laUpper; CaseCode:#$00FD),   // LATIN CAPITAL LETTER Y WITH ACUTE
    (Unicode:#$00DE; Attr:laUpper; CaseCode:#$00FE),   // LATIN CAPITAL LETTER THORN
    (Unicode:#$00DF; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SHARP S
    (Unicode:#$00E0; Attr:laLower; CaseCode:#$00C0),   // LATIN SMALL LETTER A WITH GRAVE
    (Unicode:#$00E1; Attr:laLower; CaseCode:#$00C1),   // LATIN SMALL LETTER A WITH ACUTE
    (Unicode:#$00E2; Attr:laLower; CaseCode:#$00C2),   // LATIN SMALL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00E3; Attr:laLower; CaseCode:#$00C3),   // LATIN SMALL LETTER A WITH TILDE
    (Unicode:#$00E4; Attr:laLower; CaseCode:#$00C4),   // LATIN SMALL LETTER A WITH DIAERESIS
    (Unicode:#$00E5; Attr:laLower; CaseCode:#$00C5),   // LATIN SMALL LETTER A WITH RING ABOVE
    (Unicode:#$00E6; Attr:laLower; CaseCode:#$00C6),   // LATIN SMALL LETTER AE
    (Unicode:#$00E7; Attr:laLower; CaseCode:#$00C7),   // LATIN SMALL LETTER C WITH CEDILLA
    (Unicode:#$00E8; Attr:laLower; CaseCode:#$00C8),   // LATIN SMALL LETTER E WITH GRAVE
    (Unicode:#$00E9; Attr:laLower; CaseCode:#$00C9),   // LATIN SMALL LETTER E WITH ACUTE
    (Unicode:#$00EA; Attr:laLower; CaseCode:#$00CA),   // LATIN SMALL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00EB; Attr:laLower; CaseCode:#$00CB),   // LATIN SMALL LETTER E WITH DIAERESIS
    (Unicode:#$00EC; Attr:laLower; CaseCode:#$00CC),   // LATIN SMALL LETTER I WITH GRAVE
    (Unicode:#$00ED; Attr:laLower; CaseCode:#$00CD),   // LATIN SMALL LETTER I WITH ACUTE
    (Unicode:#$00EE; Attr:laLower; CaseCode:#$00CE),   // LATIN SMALL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00EF; Attr:laLower; CaseCode:#$00CF),   // LATIN SMALL LETTER I WITH DIAERESIS
    (Unicode:#$00F0; Attr:laLower; CaseCode:#$00D0),   // LATIN SMALL LETTER ETH
    (Unicode:#$00F1; Attr:laLower; CaseCode:#$00D1),   // LATIN SMALL LETTER N WITH TILDE
    (Unicode:#$00F2; Attr:laLower; CaseCode:#$00D2),   // LATIN SMALL LETTER O WITH GRAVE
    (Unicode:#$00F3; Attr:laLower; CaseCode:#$00D3),   // LATIN SMALL LETTER O WITH ACUTE
    (Unicode:#$00F4; Attr:laLower; CaseCode:#$00D4),   // LATIN SMALL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00F5; Attr:laLower; CaseCode:#$00D5),   // LATIN SMALL LETTER O WITH TILDE
    (Unicode:#$00F6; Attr:laLower; CaseCode:#$00D6),   // LATIN SMALL LETTER O WITH DIAERESIS
    (Unicode:#$00F8; Attr:laLower; CaseCode:#$00D8),   // LATIN SMALL LETTER O WITH STROKE
    (Unicode:#$00F9; Attr:laLower; CaseCode:#$00D9),   // LATIN SMALL LETTER U WITH GRAVE
    (Unicode:#$00FA; Attr:laLower; CaseCode:#$00DA),   // LATIN SMALL LETTER U WITH ACUTE
    (Unicode:#$00FB; Attr:laLower; CaseCode:#$00DB),   // LATIN SMALL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00FC; Attr:laLower; CaseCode:#$00DC),   // LATIN SMALL LETTER U WITH DIAERESIS
    (Unicode:#$00FD; Attr:laLower; CaseCode:#$00DD),   // LATIN SMALL LETTER Y WITH ACUTE
    (Unicode:#$00FE; Attr:laLower; CaseCode:#$00DE),   // LATIN SMALL LETTER THORN
    (Unicode:#$00FF; Attr:laLower; CaseCode:#$0178),   // LATIN SMALL LETTER Y WITH DIAERESIS
    (Unicode:#$0100; Attr:laUpper; CaseCode:#$0101),   // LATIN CAPITAL LETTER A WITH MACRON
    (Unicode:#$0101; Attr:laLower; CaseCode:#$0100),   // LATIN SMALL LETTER A WITH MACRON
    (Unicode:#$0102; Attr:laUpper; CaseCode:#$0103),   // LATIN CAPITAL LETTER A WITH BREVE
    (Unicode:#$0103; Attr:laLower; CaseCode:#$0102),   // LATIN SMALL LETTER A WITH BREVE
    (Unicode:#$0104; Attr:laUpper; CaseCode:#$0105),   // LATIN CAPITAL LETTER A WITH OGONEK
    (Unicode:#$0105; Attr:laLower; CaseCode:#$0104),   // LATIN SMALL LETTER A WITH OGONEK
    (Unicode:#$0106; Attr:laUpper; CaseCode:#$0107),   // LATIN CAPITAL LETTER C WITH ACUTE
    (Unicode:#$0107; Attr:laLower; CaseCode:#$0106),   // LATIN SMALL LETTER C WITH ACUTE
    (Unicode:#$0108; Attr:laUpper; CaseCode:#$0109),   // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    (Unicode:#$0109; Attr:laLower; CaseCode:#$0108),   // LATIN SMALL LETTER C WITH CIRCUMFLEX
    (Unicode:#$010A; Attr:laUpper; CaseCode:#$010B),   // LATIN CAPITAL LETTER C WITH DOT ABOVE
    (Unicode:#$010B; Attr:laLower; CaseCode:#$010A),   // LATIN SMALL LETTER C WITH DOT ABOVE
    (Unicode:#$010C; Attr:laUpper; CaseCode:#$010D),   // LATIN CAPITAL LETTER C WITH CARON
    (Unicode:#$010D; Attr:laLower; CaseCode:#$010C),   // LATIN SMALL LETTER C WITH CARON
    (Unicode:#$010E; Attr:laUpper; CaseCode:#$010F),   // LATIN CAPITAL LETTER D WITH CARON
    (Unicode:#$010F; Attr:laLower; CaseCode:#$010E),   // LATIN SMALL LETTER D WITH CARON
    (Unicode:#$0110; Attr:laUpper; CaseCode:#$0111),   // LATIN CAPITAL LETTER D WITH STROKE
    (Unicode:#$0111; Attr:laLower; CaseCode:#$0110),   // LATIN SMALL LETTER D WITH STROKE
    (Unicode:#$0112; Attr:laUpper; CaseCode:#$0113),   // LATIN CAPITAL LETTER E WITH MACRON
    (Unicode:#$0113; Attr:laLower; CaseCode:#$0112),   // LATIN SMALL LETTER E WITH MACRON
    (Unicode:#$0114; Attr:laUpper; CaseCode:#$0115),   // LATIN CAPITAL LETTER E WITH BREVE
    (Unicode:#$0115; Attr:laLower; CaseCode:#$0114),   // LATIN SMALL LETTER E WITH BREVE
    (Unicode:#$0116; Attr:laUpper; CaseCode:#$0117),   // LATIN CAPITAL LETTER E WITH DOT ABOVE
    (Unicode:#$0117; Attr:laLower; CaseCode:#$0116),   // LATIN SMALL LETTER E WITH DOT ABOVE
    (Unicode:#$0118; Attr:laUpper; CaseCode:#$0119),   // LATIN CAPITAL LETTER E WITH OGONEK
    (Unicode:#$0119; Attr:laLower; CaseCode:#$0118),   // LATIN SMALL LETTER E WITH OGONEK
    (Unicode:#$011A; Attr:laUpper; CaseCode:#$011B),   // LATIN CAPITAL LETTER E WITH CARON
    (Unicode:#$011B; Attr:laLower; CaseCode:#$011A),   // LATIN SMALL LETTER E WITH CARON
    (Unicode:#$011C; Attr:laUpper; CaseCode:#$011D),   // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011D; Attr:laLower; CaseCode:#$011C),   // LATIN SMALL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011E; Attr:laUpper; CaseCode:#$011F),   // LATIN CAPITAL LETTER G WITH BREVE
    (Unicode:#$011F; Attr:laLower; CaseCode:#$011E),   // LATIN SMALL LETTER G WITH BREVE
    (Unicode:#$0120; Attr:laUpper; CaseCode:#$0121),   // LATIN CAPITAL LETTER G WITH DOT ABOVE
    (Unicode:#$0121; Attr:laLower; CaseCode:#$0120),   // LATIN SMALL LETTER G WITH DOT ABOVE
    (Unicode:#$0122; Attr:laUpper; CaseCode:#$0123),   // LATIN CAPITAL LETTER G WITH CEDILLA
    (Unicode:#$0123; Attr:laLower; CaseCode:#$0122),   // LATIN SMALL LETTER G WITH CEDILLA
    (Unicode:#$0124; Attr:laUpper; CaseCode:#$0125),   // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0125; Attr:laLower; CaseCode:#$0124),   // LATIN SMALL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0126; Attr:laUpper; CaseCode:#$0127),   // LATIN CAPITAL LETTER H WITH STROKE
    (Unicode:#$0127; Attr:laLower; CaseCode:#$0126),   // LATIN SMALL LETTER H WITH STROKE
    (Unicode:#$0128; Attr:laUpper; CaseCode:#$0129),   // LATIN CAPITAL LETTER I WITH TILDE
    (Unicode:#$0129; Attr:laLower; CaseCode:#$0128),   // LATIN SMALL LETTER I WITH TILDE
    (Unicode:#$012A; Attr:laUpper; CaseCode:#$012B),   // LATIN CAPITAL LETTER I WITH MACRON
    (Unicode:#$012B; Attr:laLower; CaseCode:#$012A),   // LATIN SMALL LETTER I WITH MACRON
    (Unicode:#$012C; Attr:laUpper; CaseCode:#$012D),   // LATIN CAPITAL LETTER I WITH BREVE
    (Unicode:#$012D; Attr:laLower; CaseCode:#$012C),   // LATIN SMALL LETTER I WITH BREVE
    (Unicode:#$012E; Attr:laUpper; CaseCode:#$012F),   // LATIN CAPITAL LETTER I WITH OGONEK
    (Unicode:#$012F; Attr:laLower; CaseCode:#$012E),   // LATIN SMALL LETTER I WITH OGONEK
    (Unicode:#$0130; Attr:laUpper; CaseCode:#$0069),   // LATIN CAPITAL LETTER I WITH DOT ABOVE
    (Unicode:#$0131; Attr:laLower; CaseCode:#$0049),   // LATIN SMALL LETTER DOTLESS I
    (Unicode:#$0132; Attr:laUpper; CaseCode:#$0133),   // LATIN CAPITAL LIGATURE IJ
    (Unicode:#$0133; Attr:laLower; CaseCode:#$0132),   // LATIN SMALL LIGATURE IJ
    (Unicode:#$0134; Attr:laUpper; CaseCode:#$0135),   // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0135; Attr:laLower; CaseCode:#$0134),   // LATIN SMALL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0136; Attr:laUpper; CaseCode:#$0137),   // LATIN CAPITAL LETTER K WITH CEDILLA
    (Unicode:#$0137; Attr:laLower; CaseCode:#$0136),   // LATIN SMALL LETTER K WITH CEDILLA
    (Unicode:#$0138; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER KRA
    (Unicode:#$0139; Attr:laUpper; CaseCode:#$013A),   // LATIN CAPITAL LETTER L WITH ACUTE
    (Unicode:#$013A; Attr:laLower; CaseCode:#$0139),   // LATIN SMALL LETTER L WITH ACUTE
    (Unicode:#$013B; Attr:laUpper; CaseCode:#$013C),   // LATIN CAPITAL LETTER L WITH CEDILLA
    (Unicode:#$013C; Attr:laLower; CaseCode:#$013B),   // LATIN SMALL LETTER L WITH CEDILLA
    (Unicode:#$013D; Attr:laUpper; CaseCode:#$013E),   // LATIN CAPITAL LETTER L WITH CARON
    (Unicode:#$013E; Attr:laLower; CaseCode:#$013D),   // LATIN SMALL LETTER L WITH CARON
    (Unicode:#$013F; Attr:laUpper; CaseCode:#$0140),   // LATIN CAPITAL LETTER L WITH MIDDLE DOT
    (Unicode:#$0140; Attr:laLower; CaseCode:#$013F),   // LATIN SMALL LETTER L WITH MIDDLE DOT
    (Unicode:#$0141; Attr:laUpper; CaseCode:#$0142),   // LATIN CAPITAL LETTER L WITH STROKE
    (Unicode:#$0142; Attr:laLower; CaseCode:#$0141),   // LATIN SMALL LETTER L WITH STROKE
    (Unicode:#$0143; Attr:laUpper; CaseCode:#$0144),   // LATIN CAPITAL LETTER N WITH ACUTE
    (Unicode:#$0144; Attr:laLower; CaseCode:#$0143),   // LATIN SMALL LETTER N WITH ACUTE
    (Unicode:#$0145; Attr:laUpper; CaseCode:#$0146),   // LATIN CAPITAL LETTER N WITH CEDILLA
    (Unicode:#$0146; Attr:laLower; CaseCode:#$0145),   // LATIN SMALL LETTER N WITH CEDILLA
    (Unicode:#$0147; Attr:laUpper; CaseCode:#$0148),   // LATIN CAPITAL LETTER N WITH CARON
    (Unicode:#$0148; Attr:laLower; CaseCode:#$0147),   // LATIN SMALL LETTER N WITH CARON
    (Unicode:#$0149; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    (Unicode:#$014A; Attr:laUpper; CaseCode:#$014B),   // LATIN CAPITAL LETTER ENG
    (Unicode:#$014B; Attr:laLower; CaseCode:#$014A),   // LATIN SMALL LETTER ENG
    (Unicode:#$014C; Attr:laUpper; CaseCode:#$014D),   // LATIN CAPITAL LETTER O WITH MACRON
    (Unicode:#$014D; Attr:laLower; CaseCode:#$014C),   // LATIN SMALL LETTER O WITH MACRON
    (Unicode:#$014E; Attr:laUpper; CaseCode:#$014F),   // LATIN CAPITAL LETTER O WITH BREVE
    (Unicode:#$014F; Attr:laLower; CaseCode:#$014E),   // LATIN SMALL LETTER O WITH BREVE
    (Unicode:#$0150; Attr:laUpper; CaseCode:#$0151),   // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0151; Attr:laLower; CaseCode:#$0150),   // LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0152; Attr:laUpper; CaseCode:#$0153),   // LATIN CAPITAL LIGATURE OE
    (Unicode:#$0153; Attr:laLower; CaseCode:#$0152),   // LATIN SMALL LIGATURE OE
    (Unicode:#$0154; Attr:laUpper; CaseCode:#$0155),   // LATIN CAPITAL LETTER R WITH ACUTE
    (Unicode:#$0155; Attr:laLower; CaseCode:#$0154),   // LATIN SMALL LETTER R WITH ACUTE
    (Unicode:#$0156; Attr:laUpper; CaseCode:#$0157),   // LATIN CAPITAL LETTER R WITH CEDILLA
    (Unicode:#$0157; Attr:laLower; CaseCode:#$0156),   // LATIN SMALL LETTER R WITH CEDILLA
    (Unicode:#$0158; Attr:laUpper; CaseCode:#$0159),   // LATIN CAPITAL LETTER R WITH CARON
    (Unicode:#$0159; Attr:laLower; CaseCode:#$0158),   // LATIN SMALL LETTER R WITH CARON
    (Unicode:#$015A; Attr:laUpper; CaseCode:#$015B),   // LATIN CAPITAL LETTER S WITH ACUTE
    (Unicode:#$015B; Attr:laLower; CaseCode:#$015A),   // LATIN SMALL LETTER S WITH ACUTE
    (Unicode:#$015C; Attr:laUpper; CaseCode:#$015D),   // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015D; Attr:laLower; CaseCode:#$015C),   // LATIN SMALL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015E; Attr:laUpper; CaseCode:#$015F),   // LATIN CAPITAL LETTER S WITH CEDILLA
    (Unicode:#$015F; Attr:laLower; CaseCode:#$015E),   // LATIN SMALL LETTER S WITH CEDILLA
    (Unicode:#$0160; Attr:laUpper; CaseCode:#$0161),   // LATIN CAPITAL LETTER S WITH CARON
    (Unicode:#$0161; Attr:laLower; CaseCode:#$0160),   // LATIN SMALL LETTER S WITH CARON
    (Unicode:#$0162; Attr:laUpper; CaseCode:#$0163),   // LATIN CAPITAL LETTER T WITH CEDILLA
    (Unicode:#$0163; Attr:laLower; CaseCode:#$0162),   // LATIN SMALL LETTER T WITH CEDILLA
    (Unicode:#$0164; Attr:laUpper; CaseCode:#$0165),   // LATIN CAPITAL LETTER T WITH CARON
    (Unicode:#$0165; Attr:laLower; CaseCode:#$0164),   // LATIN SMALL LETTER T WITH CARON
    (Unicode:#$0166; Attr:laUpper; CaseCode:#$0167),   // LATIN CAPITAL LETTER T WITH STROKE
    (Unicode:#$0167; Attr:laLower; CaseCode:#$0166),   // LATIN SMALL LETTER T WITH STROKE
    (Unicode:#$0168; Attr:laUpper; CaseCode:#$0169),   // LATIN CAPITAL LETTER U WITH TILDE
    (Unicode:#$0169; Attr:laLower; CaseCode:#$0168),   // LATIN SMALL LETTER U WITH TILDE
    (Unicode:#$016A; Attr:laUpper; CaseCode:#$016B),   // LATIN CAPITAL LETTER U WITH MACRON
    (Unicode:#$016B; Attr:laLower; CaseCode:#$016A),   // LATIN SMALL LETTER U WITH MACRON
    (Unicode:#$016C; Attr:laUpper; CaseCode:#$016D),   // LATIN CAPITAL LETTER U WITH BREVE
    (Unicode:#$016D; Attr:laLower; CaseCode:#$016C),   // LATIN SMALL LETTER U WITH BREVE
    (Unicode:#$016E; Attr:laUpper; CaseCode:#$016F),   // LATIN CAPITAL LETTER U WITH RING ABOVE
    (Unicode:#$016F; Attr:laLower; CaseCode:#$016E),   // LATIN SMALL LETTER U WITH RING ABOVE
    (Unicode:#$0170; Attr:laUpper; CaseCode:#$0171),   // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0171; Attr:laLower; CaseCode:#$0170),   // LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0172; Attr:laUpper; CaseCode:#$0173),   // LATIN CAPITAL LETTER U WITH OGONEK
    (Unicode:#$0173; Attr:laLower; CaseCode:#$0172),   // LATIN SMALL LETTER U WITH OGONEK
    (Unicode:#$0174; Attr:laUpper; CaseCode:#$0175),   // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0175; Attr:laLower; CaseCode:#$0174),   // LATIN SMALL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0176; Attr:laUpper; CaseCode:#$0177),   // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0177; Attr:laLower; CaseCode:#$0176),   // LATIN SMALL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0178; Attr:laUpper; CaseCode:#$00FF),   // LATIN CAPITAL LETTER Y WITH DIAERESIS
    (Unicode:#$0179; Attr:laUpper; CaseCode:#$017A),   // LATIN CAPITAL LETTER Z WITH ACUTE
    (Unicode:#$017A; Attr:laLower; CaseCode:#$0179),   // LATIN SMALL LETTER Z WITH ACUTE
    (Unicode:#$017B; Attr:laUpper; CaseCode:#$017C),   // LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (Unicode:#$017C; Attr:laLower; CaseCode:#$017B),   // LATIN SMALL LETTER Z WITH DOT ABOVE
    (Unicode:#$017D; Attr:laUpper; CaseCode:#$017E),   // LATIN CAPITAL LETTER Z WITH CARON
    (Unicode:#$017E; Attr:laLower; CaseCode:#$017D),   // LATIN SMALL LETTER Z WITH CARON
    (Unicode:#$017F; Attr:laLower; CaseCode:#$0053),   // LATIN SMALL LETTER LONG S
    (Unicode:#$0180; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER B WITH STROKE
    (Unicode:#$0181; Attr:laUpper; CaseCode:#$0253),   // LATIN CAPITAL LETTER B WITH HOOK
    (Unicode:#$0182; Attr:laUpper; CaseCode:#$0183),   // LATIN CAPITAL LETTER B WITH TOPBAR
    (Unicode:#$0183; Attr:laLower; CaseCode:#$0182),   // LATIN SMALL LETTER B WITH TOPBAR
    (Unicode:#$0184; Attr:laUpper; CaseCode:#$0185),   // LATIN CAPITAL LETTER TONE SIX
    (Unicode:#$0185; Attr:laLower; CaseCode:#$0184),   // LATIN SMALL LETTER TONE SIX
    (Unicode:#$0186; Attr:laUpper; CaseCode:#$0254),   // LATIN CAPITAL LETTER OPEN O
    (Unicode:#$0187; Attr:laUpper; CaseCode:#$0188),   // LATIN CAPITAL LETTER C WITH HOOK
    (Unicode:#$0188; Attr:laLower; CaseCode:#$0187),   // LATIN SMALL LETTER C WITH HOOK
    (Unicode:#$0189; Attr:laUpper; CaseCode:#$0256),   // LATIN CAPITAL LETTER AFRICAN D
    (Unicode:#$018A; Attr:laUpper; CaseCode:#$0257),   // LATIN CAPITAL LETTER D WITH HOOK
    (Unicode:#$018B; Attr:laUpper; CaseCode:#$018C),   // LATIN CAPITAL LETTER D WITH TOPBAR
    (Unicode:#$018C; Attr:laLower; CaseCode:#$018B),   // LATIN SMALL LETTER D WITH TOPBAR
    (Unicode:#$018D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED DELTA
    (Unicode:#$018E; Attr:laUpper; CaseCode:#$01DD),   // LATIN CAPITAL LETTER REVERSED E
    (Unicode:#$018F; Attr:laUpper; CaseCode:#$0259),   // LATIN CAPITAL LETTER SCHWA
    (Unicode:#$0190; Attr:laUpper; CaseCode:#$025B),   // LATIN CAPITAL LETTER OPEN E
    (Unicode:#$0191; Attr:laUpper; CaseCode:#$0192),   // LATIN CAPITAL LETTER F WITH HOOK
    (Unicode:#$0192; Attr:laLower; CaseCode:#$0191),   // LATIN SMALL LETTER F WITH HOOK
    (Unicode:#$0193; Attr:laUpper; CaseCode:#$0260),   // LATIN CAPITAL LETTER G WITH HOOK
    (Unicode:#$0194; Attr:laUpper; CaseCode:#$0263),   // LATIN CAPITAL LETTER GAMMA
    (Unicode:#$0195; Attr:laLower; CaseCode:#$01F6),   // LATIN SMALL LETTER HV
    (Unicode:#$0196; Attr:laUpper; CaseCode:#$0269),   // LATIN CAPITAL LETTER IOTA
    (Unicode:#$0197; Attr:laUpper; CaseCode:#$0268),   // LATIN CAPITAL LETTER I WITH STROKE
    (Unicode:#$0198; Attr:laUpper; CaseCode:#$0199),   // LATIN CAPITAL LETTER K WITH HOOK
    (Unicode:#$0199; Attr:laLower; CaseCode:#$0198),   // LATIN SMALL LETTER K WITH HOOK
    (Unicode:#$019A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH BAR
    (Unicode:#$019B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LAMBDA WITH STROKE
    (Unicode:#$019C; Attr:laUpper; CaseCode:#$026F),   // LATIN CAPITAL LETTER TURNED M
    (Unicode:#$019D; Attr:laUpper; CaseCode:#$0272),   // LATIN CAPITAL LETTER N WITH LEFT HOOK
    (Unicode:#$019E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N WITH LONG RIGHT LEG
    (Unicode:#$019F; Attr:laUpper; CaseCode:#$0275),   // LATIN CAPITAL LETTER O WITH MIDDLE TILDE
    (Unicode:#$01A0; Attr:laUpper; CaseCode:#$01A1),   // LATIN CAPITAL LETTER O WITH HORN
    (Unicode:#$01A1; Attr:laLower; CaseCode:#$01A0),   // LATIN SMALL LETTER O WITH HORN
    (Unicode:#$01A2; Attr:laUpper; CaseCode:#$01A3),   // LATIN CAPITAL LETTER OI
    (Unicode:#$01A3; Attr:laLower; CaseCode:#$01A2),   // LATIN SMALL LETTER OI
    (Unicode:#$01A4; Attr:laUpper; CaseCode:#$01A5),   // LATIN CAPITAL LETTER P WITH HOOK
    (Unicode:#$01A5; Attr:laLower; CaseCode:#$01A4),   // LATIN SMALL LETTER P WITH HOOK
    (Unicode:#$01A6; Attr:laUpper; CaseCode:#$0280),   // LATIN LETTER YR
    (Unicode:#$01A7; Attr:laUpper; CaseCode:#$01A8),   // LATIN CAPITAL LETTER TONE TWO
    (Unicode:#$01A8; Attr:laLower; CaseCode:#$01A7),   // LATIN SMALL LETTER TONE TWO
    (Unicode:#$01A9; Attr:laUpper; CaseCode:#$0283),   // LATIN CAPITAL LETTER ESH
    (Unicode:#$01AA; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER REVERSED ESH LOOP
    (Unicode:#$01AB; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER T WITH PALATAL HOOK
    (Unicode:#$01AC; Attr:laUpper; CaseCode:#$01AD),   // LATIN CAPITAL LETTER T WITH HOOK
    (Unicode:#$01AD; Attr:laLower; CaseCode:#$01AC),   // LATIN SMALL LETTER T WITH HOOK
    (Unicode:#$01AE; Attr:laUpper; CaseCode:#$0288),   // LATIN CAPITAL LETTER T WITH RETROFLEX HOOK
    (Unicode:#$01AF; Attr:laUpper; CaseCode:#$01B0),   // LATIN CAPITAL LETTER U WITH HORN
    (Unicode:#$01B0; Attr:laLower; CaseCode:#$01AF),   // LATIN SMALL LETTER U WITH HORN
    (Unicode:#$01B1; Attr:laUpper; CaseCode:#$028A),   // LATIN CAPITAL LETTER UPSILON
    (Unicode:#$01B2; Attr:laUpper; CaseCode:#$028B),   // LATIN CAPITAL LETTER V WITH HOOK
    (Unicode:#$01B3; Attr:laUpper; CaseCode:#$01B4),   // LATIN CAPITAL LETTER Y WITH HOOK
    (Unicode:#$01B4; Attr:laLower; CaseCode:#$01B3),   // LATIN SMALL LETTER Y WITH HOOK
    (Unicode:#$01B5; Attr:laUpper; CaseCode:#$01B6),   // LATIN CAPITAL LETTER Z WITH STROKE
    (Unicode:#$01B6; Attr:laLower; CaseCode:#$01B5),   // LATIN SMALL LETTER Z WITH STROKE
    (Unicode:#$01B7; Attr:laUpper; CaseCode:#$0292),   // LATIN CAPITAL LETTER EZH
    (Unicode:#$01B8; Attr:laUpper; CaseCode:#$01B9),   // LATIN CAPITAL LETTER EZH REVERSED
    (Unicode:#$01B9; Attr:laLower; CaseCode:#$01B8),   // LATIN SMALL LETTER EZH REVERSED
    (Unicode:#$01BA; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER EZH WITH TAIL
    (Unicode:#$01BC; Attr:laUpper; CaseCode:#$01BD),   // LATIN CAPITAL LETTER TONE FIVE
    (Unicode:#$01BD; Attr:laLower; CaseCode:#$01BC),   // LATIN SMALL LETTER TONE FIVE
    (Unicode:#$01BE; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER INVERTED GLOTTAL STOP WITH STROKE
    (Unicode:#$01BF; Attr:laLower; CaseCode:#$01F7),   // LATIN LETTER WYNN
    (Unicode:#$01C4; Attr:laUpper; CaseCode:#$01C6),   // LATIN CAPITAL LETTER DZ WITH CARON
    (Unicode:#$01C6; Attr:laLower; CaseCode:#$01C4),   // LATIN SMALL LETTER DZ WITH CARON
    (Unicode:#$01C7; Attr:laUpper; CaseCode:#$01C9),   // LATIN CAPITAL LETTER LJ
    (Unicode:#$01C9; Attr:laLower; CaseCode:#$01C7),   // LATIN SMALL LETTER LJ
    (Unicode:#$01CA; Attr:laUpper; CaseCode:#$01CC),   // LATIN CAPITAL LETTER NJ
    (Unicode:#$01CC; Attr:laLower; CaseCode:#$01CA),   // LATIN SMALL LETTER NJ
    (Unicode:#$01CD; Attr:laUpper; CaseCode:#$01CE),   // LATIN CAPITAL LETTER A WITH CARON
    (Unicode:#$01CE; Attr:laLower; CaseCode:#$01CD),   // LATIN SMALL LETTER A WITH CARON
    (Unicode:#$01CF; Attr:laUpper; CaseCode:#$01D0),   // LATIN CAPITAL LETTER I WITH CARON
    (Unicode:#$01D0; Attr:laLower; CaseCode:#$01CF),   // LATIN SMALL LETTER I WITH CARON
    (Unicode:#$01D1; Attr:laUpper; CaseCode:#$01D2),   // LATIN CAPITAL LETTER O WITH CARON
    (Unicode:#$01D2; Attr:laLower; CaseCode:#$01D1),   // LATIN SMALL LETTER O WITH CARON
    (Unicode:#$01D3; Attr:laUpper; CaseCode:#$01D4),   // LATIN CAPITAL LETTER U WITH CARON
    (Unicode:#$01D4; Attr:laLower; CaseCode:#$01D3),   // LATIN SMALL LETTER U WITH CARON
    (Unicode:#$01D5; Attr:laUpper; CaseCode:#$01D6),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D6; Attr:laLower; CaseCode:#$01D5),   // LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D7; Attr:laUpper; CaseCode:#$01D8),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D8; Attr:laLower; CaseCode:#$01D7),   // LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D9; Attr:laUpper; CaseCode:#$01DA),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DA; Attr:laLower; CaseCode:#$01D9),   // LATIN SMALL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DB; Attr:laUpper; CaseCode:#$01DC),   // LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DC; Attr:laLower; CaseCode:#$01DB),   // LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DD; Attr:laLower; CaseCode:#$018E),   // LATIN SMALL LETTER TURNED E
    (Unicode:#$01DE; Attr:laUpper; CaseCode:#$01DF),   // LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01DF; Attr:laLower; CaseCode:#$01DE),   // LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01E0; Attr:laUpper; CaseCode:#$01E1),   // LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E1; Attr:laLower; CaseCode:#$01E0),   // LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E2; Attr:laUpper; CaseCode:#$01E3),   // LATIN CAPITAL LETTER AE WITH MACRON
    (Unicode:#$01E3; Attr:laLower; CaseCode:#$01E2),   // LATIN SMALL LETTER AE WITH MACRON
    (Unicode:#$01E4; Attr:laUpper; CaseCode:#$01E5),   // LATIN CAPITAL LETTER G WITH STROKE
    (Unicode:#$01E5; Attr:laLower; CaseCode:#$01E4),   // LATIN SMALL LETTER G WITH STROKE
    (Unicode:#$01E6; Attr:laUpper; CaseCode:#$01E7),   // LATIN CAPITAL LETTER G WITH CARON
    (Unicode:#$01E7; Attr:laLower; CaseCode:#$01E6),   // LATIN SMALL LETTER G WITH CARON
    (Unicode:#$01E8; Attr:laUpper; CaseCode:#$01E9),   // LATIN CAPITAL LETTER K WITH CARON
    (Unicode:#$01E9; Attr:laLower; CaseCode:#$01E8),   // LATIN SMALL LETTER K WITH CARON
    (Unicode:#$01EA; Attr:laUpper; CaseCode:#$01EB),   // LATIN CAPITAL LETTER O WITH OGONEK
    (Unicode:#$01EB; Attr:laLower; CaseCode:#$01EA),   // LATIN SMALL LETTER O WITH OGONEK
    (Unicode:#$01EC; Attr:laUpper; CaseCode:#$01ED),   // LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01ED; Attr:laLower; CaseCode:#$01EC),   // LATIN SMALL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01EE; Attr:laUpper; CaseCode:#$01EF),   // LATIN CAPITAL LETTER EZH WITH CARON
    (Unicode:#$01EF; Attr:laLower; CaseCode:#$01EE),   // LATIN SMALL LETTER EZH WITH CARON
    (Unicode:#$01F0; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER J WITH CARON
    (Unicode:#$01F1; Attr:laUpper; CaseCode:#$01F3),   // LATIN CAPITAL LETTER DZ
    (Unicode:#$01F3; Attr:laLower; CaseCode:#$01F1),   // LATIN SMALL LETTER DZ
    (Unicode:#$01F4; Attr:laUpper; CaseCode:#$01F5),   // LATIN CAPITAL LETTER G WITH ACUTE
    (Unicode:#$01F5; Attr:laLower; CaseCode:#$01F4),   // LATIN SMALL LETTER G WITH ACUTE
    (Unicode:#$01F6; Attr:laUpper; CaseCode:#$0195),   // LATIN CAPITAL LETTER HWAIR
    (Unicode:#$01F7; Attr:laUpper; CaseCode:#$01BF),   // LATIN CAPITAL LETTER WYNN
    (Unicode:#$01F8; Attr:laUpper; CaseCode:#$01F9),   // LATIN CAPITAL LETTER N WITH GRAVE
    (Unicode:#$01F9; Attr:laLower; CaseCode:#$01F8),   // LATIN SMALL LETTER N WITH GRAVE
    (Unicode:#$01FA; Attr:laUpper; CaseCode:#$01FB),   // LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FB; Attr:laLower; CaseCode:#$01FA),   // LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FC; Attr:laUpper; CaseCode:#$01FD),   // LATIN CAPITAL LETTER AE WITH ACUTE
    (Unicode:#$01FD; Attr:laLower; CaseCode:#$01FC),   // LATIN SMALL LETTER AE WITH ACUTE
    (Unicode:#$01FE; Attr:laUpper; CaseCode:#$01FF),   // LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$01FF; Attr:laLower; CaseCode:#$01FE),   // LATIN SMALL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$0200; Attr:laUpper; CaseCode:#$0201),   // LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0201; Attr:laLower; CaseCode:#$0200),   // LATIN SMALL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0202; Attr:laUpper; CaseCode:#$0203),   // LATIN CAPITAL LETTER A WITH INVERTED BREVE
    (Unicode:#$0203; Attr:laLower; CaseCode:#$0202),   // LATIN SMALL LETTER A WITH INVERTED BREVE
    (Unicode:#$0204; Attr:laUpper; CaseCode:#$0205),   // LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0205; Attr:laLower; CaseCode:#$0204),   // LATIN SMALL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0206; Attr:laUpper; CaseCode:#$0207),   // LATIN CAPITAL LETTER E WITH INVERTED BREVE
    (Unicode:#$0207; Attr:laLower; CaseCode:#$0206),   // LATIN SMALL LETTER E WITH INVERTED BREVE
    (Unicode:#$0208; Attr:laUpper; CaseCode:#$0209),   // LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$0209; Attr:laLower; CaseCode:#$0208),   // LATIN SMALL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$020A; Attr:laUpper; CaseCode:#$020B),   // LATIN CAPITAL LETTER I WITH INVERTED BREVE
    (Unicode:#$020B; Attr:laLower; CaseCode:#$020A),   // LATIN SMALL LETTER I WITH INVERTED BREVE
    (Unicode:#$020C; Attr:laUpper; CaseCode:#$020D),   // LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020D; Attr:laLower; CaseCode:#$020C),   // LATIN SMALL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020E; Attr:laUpper; CaseCode:#$020F),   // LATIN CAPITAL LETTER O WITH INVERTED BREVE
    (Unicode:#$020F; Attr:laLower; CaseCode:#$020E),   // LATIN SMALL LETTER O WITH INVERTED BREVE
    (Unicode:#$0210; Attr:laUpper; CaseCode:#$0211),   // LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0211; Attr:laLower; CaseCode:#$0210),   // LATIN SMALL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0212; Attr:laUpper; CaseCode:#$0213),   // LATIN CAPITAL LETTER R WITH INVERTED BREVE
    (Unicode:#$0213; Attr:laLower; CaseCode:#$0212),   // LATIN SMALL LETTER R WITH INVERTED BREVE
    (Unicode:#$0214; Attr:laUpper; CaseCode:#$0215),   // LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0215; Attr:laLower; CaseCode:#$0214),   // LATIN SMALL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0216; Attr:laUpper; CaseCode:#$0217),   // LATIN CAPITAL LETTER U WITH INVERTED BREVE
    (Unicode:#$0217; Attr:laLower; CaseCode:#$0216),   // LATIN SMALL LETTER U WITH INVERTED BREVE
    (Unicode:#$0218; Attr:laUpper; CaseCode:#$0219),   // LATIN CAPITAL LETTER S WITH COMMA BELOW
    (Unicode:#$0219; Attr:laLower; CaseCode:#$0218),   // LATIN SMALL LETTER S WITH COMMA BELOW
    (Unicode:#$021A; Attr:laUpper; CaseCode:#$021B),   // LATIN CAPITAL LETTER T WITH COMMA BELOW
    (Unicode:#$021B; Attr:laLower; CaseCode:#$021A),   // LATIN SMALL LETTER T WITH COMMA BELOW
    (Unicode:#$021C; Attr:laUpper; CaseCode:#$021D),   // LATIN CAPITAL LETTER YOGH
    (Unicode:#$021D; Attr:laLower; CaseCode:#$021C),   // LATIN SMALL LETTER YOGH
    (Unicode:#$021E; Attr:laUpper; CaseCode:#$021F),   // LATIN CAPITAL LETTER H WITH CARON
    (Unicode:#$021F; Attr:laLower; CaseCode:#$021E),   // LATIN SMALL LETTER H WITH CARON
    (Unicode:#$0222; Attr:laUpper; CaseCode:#$0223),   // LATIN CAPITAL LETTER OU
    (Unicode:#$0223; Attr:laLower; CaseCode:#$0222),   // LATIN SMALL LETTER OU
    (Unicode:#$0224; Attr:laUpper; CaseCode:#$0225),   // LATIN CAPITAL LETTER Z WITH HOOK
    (Unicode:#$0225; Attr:laLower; CaseCode:#$0224),   // LATIN SMALL LETTER Z WITH HOOK
    (Unicode:#$0226; Attr:laUpper; CaseCode:#$0227),   // LATIN CAPITAL LETTER A WITH DOT ABOVE
    (Unicode:#$0227; Attr:laLower; CaseCode:#$0226),   // LATIN SMALL LETTER A WITH DOT ABOVE
    (Unicode:#$0228; Attr:laUpper; CaseCode:#$0229),   // LATIN CAPITAL LETTER E WITH CEDILLA
    (Unicode:#$0229; Attr:laLower; CaseCode:#$0228),   // LATIN SMALL LETTER E WITH CEDILLA
    (Unicode:#$022A; Attr:laUpper; CaseCode:#$022B),   // LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022B; Attr:laLower; CaseCode:#$022A),   // LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022C; Attr:laUpper; CaseCode:#$022D),   // LATIN CAPITAL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022D; Attr:laLower; CaseCode:#$022C),   // LATIN SMALL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022E; Attr:laUpper; CaseCode:#$022F),   // LATIN CAPITAL LETTER O WITH DOT ABOVE
    (Unicode:#$022F; Attr:laLower; CaseCode:#$022E),   // LATIN SMALL LETTER O WITH DOT ABOVE
    (Unicode:#$0230; Attr:laUpper; CaseCode:#$0231),   // LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0231; Attr:laLower; CaseCode:#$0230),   // LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0232; Attr:laUpper; CaseCode:#$0233),   // LATIN CAPITAL LETTER Y WITH MACRON
    (Unicode:#$0233; Attr:laLower; CaseCode:#$0232),   // LATIN SMALL LETTER Y WITH MACRON
    (Unicode:#$0250; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED A
    (Unicode:#$0251; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER ALPHA
    (Unicode:#$0252; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED ALPHA
    (Unicode:#$0253; Attr:laLower; CaseCode:#$0181),   // LATIN SMALL LETTER B WITH HOOK
    (Unicode:#$0254; Attr:laLower; CaseCode:#$0186),   // LATIN SMALL LETTER OPEN O
    (Unicode:#$0255; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER C WITH CURL
    (Unicode:#$0256; Attr:laLower; CaseCode:#$0189),   // LATIN SMALL LETTER D WITH TAIL
    (Unicode:#$0257; Attr:laLower; CaseCode:#$018A),   // LATIN SMALL LETTER D WITH HOOK
    (Unicode:#$0258; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED E
    (Unicode:#$0259; Attr:laLower; CaseCode:#$018F),   // LATIN SMALL LETTER SCHWA
    (Unicode:#$025A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SCHWA WITH HOOK
    (Unicode:#$025B; Attr:laLower; CaseCode:#$0190),   // LATIN SMALL LETTER OPEN E
    (Unicode:#$025C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED OPEN E
    (Unicode:#$025D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED OPEN E WITH HOOK
    (Unicode:#$025E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED REVERSED OPEN E
    (Unicode:#$025F; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DOTLESS J WITH STROKE
    (Unicode:#$0260; Attr:laLower; CaseCode:#$0193),   // LATIN SMALL LETTER G WITH HOOK
    (Unicode:#$0261; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SCRIPT G
    (Unicode:#$0262; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL G
    (Unicode:#$0263; Attr:laLower; CaseCode:#$0194),   // LATIN SMALL LETTER GAMMA
    (Unicode:#$0264; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER RAMS HORN
    (Unicode:#$0265; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED H
    (Unicode:#$0266; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER H WITH HOOK
    (Unicode:#$0267; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER HENG WITH HOOK
    (Unicode:#$0268; Attr:laLower; CaseCode:#$0197),   // LATIN SMALL LETTER I WITH STROKE
    (Unicode:#$0269; Attr:laLower; CaseCode:#$0196),   // LATIN SMALL LETTER IOTA
    (Unicode:#$026A; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL I
    (Unicode:#$026B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH MIDDLE TILDE
    (Unicode:#$026C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH BELT
    (Unicode:#$026D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER L WITH RETROFLEX HOOK
    (Unicode:#$026E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LEZH
    (Unicode:#$026F; Attr:laLower; CaseCode:#$019C),   // LATIN SMALL LETTER TURNED M
    (Unicode:#$0270; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED M WITH LONG LEG
    (Unicode:#$0271; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER M WITH HOOK
    (Unicode:#$0272; Attr:laLower; CaseCode:#$019D),   // LATIN SMALL LETTER N WITH LEFT HOOK
    (Unicode:#$0273; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER N WITH RETROFLEX HOOK
    (Unicode:#$0274; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL N
    (Unicode:#$0275; Attr:laLower; CaseCode:#$019F),   // LATIN SMALL LETTER BARRED O
    (Unicode:#$0276; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL OE
    (Unicode:#$0277; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED OMEGA
    (Unicode:#$0278; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER PHI
    (Unicode:#$0279; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R
    (Unicode:#$027A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R WITH LONG LEG
    (Unicode:#$027B; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED R WITH HOOK
    (Unicode:#$027C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH LONG LEG
    (Unicode:#$027D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH TAIL
    (Unicode:#$027E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER R WITH FISHHOOK
    (Unicode:#$027F; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER REVERSED R WITH FISHHOOK
    (Unicode:#$0280; Attr:laLower; CaseCode:#$01A6),   // LATIN LETTER SMALL CAPITAL R
    (Unicode:#$0281; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL INVERTED R
    (Unicode:#$0282; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER S WITH HOOK
    (Unicode:#$0283; Attr:laLower; CaseCode:#$01A9),   // LATIN SMALL LETTER ESH
    (Unicode:#$0284; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DOTLESS J WITH STROKE AND HOOK
    (Unicode:#$0285; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER SQUAT REVERSED ESH
    (Unicode:#$0286; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER ESH WITH CURL
    (Unicode:#$0287; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED T
    (Unicode:#$0288; Attr:laLower; CaseCode:#$01AE),   // LATIN SMALL LETTER T WITH RETROFLEX HOOK
    (Unicode:#$0289; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER U BAR
    (Unicode:#$028A; Attr:laLower; CaseCode:#$01B1),   // LATIN SMALL LETTER UPSILON
    (Unicode:#$028B; Attr:laLower; CaseCode:#$01B2),   // LATIN SMALL LETTER V WITH HOOK
    (Unicode:#$028C; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED V
    (Unicode:#$028D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED W
    (Unicode:#$028E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED Y
    (Unicode:#$028F; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL Y
    (Unicode:#$0290; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Z WITH RETROFLEX HOOK
    (Unicode:#$0291; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Z WITH CURL
    (Unicode:#$0292; Attr:laLower; CaseCode:#$01B7),   // LATIN SMALL LETTER EZH
    (Unicode:#$0293; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER EZH WITH CURL
    (Unicode:#$0294; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER GLOTTAL STOP
    (Unicode:#$0295; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER PHARYNGEAL VOICED FRICATIVE
    (Unicode:#$0296; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER INVERTED GLOTTAL STOP
    (Unicode:#$0297; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER STRETCHED C
    (Unicode:#$0298; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BILABIAL CLICK
    (Unicode:#$0299; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL B
    (Unicode:#$029A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER CLOSED OPEN E
    (Unicode:#$029B; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL G WITH HOOK
    (Unicode:#$029C; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL H
    (Unicode:#$029D; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER J WITH CROSSED-TAIL
    (Unicode:#$029E; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TURNED K
    (Unicode:#$029F; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER SMALL CAPITAL L
    (Unicode:#$02A0; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Q WITH HOOK
    (Unicode:#$02A1; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER GLOTTAL STOP WITH STROKE
    (Unicode:#$02A2; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER REVERSED GLOTTAL STOP WITH STROKE
    (Unicode:#$02A3; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DZ DIGRAPH
    (Unicode:#$02A4; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DEZH DIGRAPH
    (Unicode:#$02A5; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER DZ DIGRAPH WITH CURL
    (Unicode:#$02A6; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TS DIGRAPH
    (Unicode:#$02A7; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TESH DIGRAPH
    (Unicode:#$02A8; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER TC DIGRAPH WITH CURL
    (Unicode:#$02A9; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER FENG DIGRAPH
    (Unicode:#$02AA; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LS DIGRAPH
    (Unicode:#$02AB; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER LZ DIGRAPH
    (Unicode:#$02AC; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BILABIAL PERCUSSIVE
    (Unicode:#$02AD; Attr:laLower; CaseCode:#$FFFF),   // LATIN LETTER BIDENTAL PERCUSSIVE
    (Unicode:#$0386; Attr:laUpper; CaseCode:#$03AC),   // GREEK CAPITAL LETTER ALPHA WITH TONOS
    (Unicode:#$0388; Attr:laUpper; CaseCode:#$03AD),   // GREEK CAPITAL LETTER EPSILON WITH TONOS
    (Unicode:#$0389; Attr:laUpper; CaseCode:#$03AE),   // GREEK CAPITAL LETTER ETA WITH TONOS
    (Unicode:#$038A; Attr:laUpper; CaseCode:#$03AF),   // GREEK CAPITAL LETTER IOTA WITH TONOS
    (Unicode:#$038C; Attr:laUpper; CaseCode:#$03CC),   // GREEK CAPITAL LETTER OMICRON WITH TONOS
    (Unicode:#$038E; Attr:laUpper; CaseCode:#$03CD),   // GREEK CAPITAL LETTER UPSILON WITH TONOS
    (Unicode:#$038F; Attr:laUpper; CaseCode:#$03CE),   // GREEK CAPITAL LETTER OMEGA WITH TONOS
    (Unicode:#$0390; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (Unicode:#$0391; Attr:laUpper; CaseCode:#$03B1),   // GREEK CAPITAL LETTER ALPHA
    (Unicode:#$0392; Attr:laUpper; CaseCode:#$03B2),   // GREEK CAPITAL LETTER BETA
    (Unicode:#$0393; Attr:laUpper; CaseCode:#$03B3),   // GREEK CAPITAL LETTER GAMMA
    (Unicode:#$0394; Attr:laUpper; CaseCode:#$03B4),   // GREEK CAPITAL LETTER DELTA
    (Unicode:#$0395; Attr:laUpper; CaseCode:#$03B5),   // GREEK CAPITAL LETTER EPSILON
    (Unicode:#$0396; Attr:laUpper; CaseCode:#$03B6),   // GREEK CAPITAL LETTER ZETA
    (Unicode:#$0397; Attr:laUpper; CaseCode:#$03B7),   // GREEK CAPITAL LETTER ETA
    (Unicode:#$0398; Attr:laUpper; CaseCode:#$03B8),   // GREEK CAPITAL LETTER THETA
    (Unicode:#$0399; Attr:laUpper; CaseCode:#$03B9),   // GREEK CAPITAL LETTER IOTA
    (Unicode:#$039A; Attr:laUpper; CaseCode:#$03BA),   // GREEK CAPITAL LETTER KAPPA
    (Unicode:#$039B; Attr:laUpper; CaseCode:#$03BB),   // GREEK CAPITAL LETTER LAMDA
    (Unicode:#$039C; Attr:laUpper; CaseCode:#$03BC),   // GREEK CAPITAL LETTER MU
    (Unicode:#$039D; Attr:laUpper; CaseCode:#$03BD),   // GREEK CAPITAL LETTER NU
    (Unicode:#$039E; Attr:laUpper; CaseCode:#$03BE),   // GREEK CAPITAL LETTER XI
    (Unicode:#$039F; Attr:laUpper; CaseCode:#$03BF),   // GREEK CAPITAL LETTER OMICRON
    (Unicode:#$03A0; Attr:laUpper; CaseCode:#$03C0),   // GREEK CAPITAL LETTER PI
    (Unicode:#$03A1; Attr:laUpper; CaseCode:#$03C1),   // GREEK CAPITAL LETTER RHO
    (Unicode:#$03A3; Attr:laUpper; CaseCode:#$03C3),   // GREEK CAPITAL LETTER SIGMA
    (Unicode:#$03A4; Attr:laUpper; CaseCode:#$03C4),   // GREEK CAPITAL LETTER TAU
    (Unicode:#$03A5; Attr:laUpper; CaseCode:#$03C5),   // GREEK CAPITAL LETTER UPSILON
    (Unicode:#$03A6; Attr:laUpper; CaseCode:#$03C6),   // GREEK CAPITAL LETTER PHI
    (Unicode:#$03A7; Attr:laUpper; CaseCode:#$03C7),   // GREEK CAPITAL LETTER CHI
    (Unicode:#$03A8; Attr:laUpper; CaseCode:#$03C8),   // GREEK CAPITAL LETTER PSI
    (Unicode:#$03A9; Attr:laUpper; CaseCode:#$03C9),   // GREEK CAPITAL LETTER OMEGA
    (Unicode:#$03AA; Attr:laUpper; CaseCode:#$03CA),   // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03AB; Attr:laUpper; CaseCode:#$03CB),   // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03AC; Attr:laLower; CaseCode:#$0386),   // GREEK SMALL LETTER ALPHA WITH TONOS
    (Unicode:#$03AD; Attr:laLower; CaseCode:#$0388),   // GREEK SMALL LETTER EPSILON WITH TONOS
    (Unicode:#$03AE; Attr:laLower; CaseCode:#$0389),   // GREEK SMALL LETTER ETA WITH TONOS
    (Unicode:#$03AF; Attr:laLower; CaseCode:#$038A),   // GREEK SMALL LETTER IOTA WITH TONOS
    (Unicode:#$03B0; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (Unicode:#$03B1; Attr:laLower; CaseCode:#$0391),   // GREEK SMALL LETTER ALPHA
    (Unicode:#$03B2; Attr:laLower; CaseCode:#$0392),   // GREEK SMALL LETTER BETA
    (Unicode:#$03B3; Attr:laLower; CaseCode:#$0393),   // GREEK SMALL LETTER GAMMA
    (Unicode:#$03B4; Attr:laLower; CaseCode:#$0394),   // GREEK SMALL LETTER DELTA
    (Unicode:#$03B5; Attr:laLower; CaseCode:#$0395),   // GREEK SMALL LETTER EPSILON
    (Unicode:#$03B6; Attr:laLower; CaseCode:#$0396),   // GREEK SMALL LETTER ZETA
    (Unicode:#$03B7; Attr:laLower; CaseCode:#$0397),   // GREEK SMALL LETTER ETA
    (Unicode:#$03B8; Attr:laLower; CaseCode:#$0398),   // GREEK SMALL LETTER THETA
    (Unicode:#$03B9; Attr:laLower; CaseCode:#$0399),   // GREEK SMALL LETTER IOTA
    (Unicode:#$03BA; Attr:laLower; CaseCode:#$039A),   // GREEK SMALL LETTER KAPPA
    (Unicode:#$03BB; Attr:laLower; CaseCode:#$039B),   // GREEK SMALL LETTER LAMDA
    (Unicode:#$03BC; Attr:laLower; CaseCode:#$039C),   // GREEK SMALL LETTER MU
    (Unicode:#$03BD; Attr:laLower; CaseCode:#$039D),   // GREEK SMALL LETTER NU
    (Unicode:#$03BE; Attr:laLower; CaseCode:#$039E),   // GREEK SMALL LETTER XI
    (Unicode:#$03BF; Attr:laLower; CaseCode:#$039F),   // GREEK SMALL LETTER OMICRON
    (Unicode:#$03C0; Attr:laLower; CaseCode:#$03A0),   // GREEK SMALL LETTER PI
    (Unicode:#$03C1; Attr:laLower; CaseCode:#$03A1),   // GREEK SMALL LETTER RHO
    (Unicode:#$03C2; Attr:laLower; CaseCode:#$03A3),   // GREEK SMALL LETTER FINAL SIGMA
    (Unicode:#$03C3; Attr:laLower; CaseCode:#$03A3),   // GREEK SMALL LETTER SIGMA
    (Unicode:#$03C4; Attr:laLower; CaseCode:#$03A4),   // GREEK SMALL LETTER TAU
    (Unicode:#$03C5; Attr:laLower; CaseCode:#$03A5),   // GREEK SMALL LETTER UPSILON
    (Unicode:#$03C6; Attr:laLower; CaseCode:#$03A6),   // GREEK SMALL LETTER PHI
    (Unicode:#$03C7; Attr:laLower; CaseCode:#$03A7),   // GREEK SMALL LETTER CHI
    (Unicode:#$03C8; Attr:laLower; CaseCode:#$03A8),   // GREEK SMALL LETTER PSI
    (Unicode:#$03C9; Attr:laLower; CaseCode:#$03A9),   // GREEK SMALL LETTER OMEGA
    (Unicode:#$03CA; Attr:laLower; CaseCode:#$03AA),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03CB; Attr:laLower; CaseCode:#$03AB),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03CC; Attr:laLower; CaseCode:#$038C),   // GREEK SMALL LETTER OMICRON WITH TONOS
    (Unicode:#$03CD; Attr:laLower; CaseCode:#$038E),   // GREEK SMALL LETTER UPSILON WITH TONOS
    (Unicode:#$03CE; Attr:laLower; CaseCode:#$038F),   // GREEK SMALL LETTER OMEGA WITH TONOS
    (Unicode:#$03D0; Attr:laLower; CaseCode:#$0392),   // GREEK BETA SYMBOL
    (Unicode:#$03D1; Attr:laLower; CaseCode:#$0398),   // GREEK THETA SYMBOL
    (Unicode:#$03D2; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH HOOK SYMBOL
    (Unicode:#$03D3; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
    (Unicode:#$03D4; Attr:laUpper; CaseCode:#$FFFF),   // GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
    (Unicode:#$03D5; Attr:laLower; CaseCode:#$03A6),   // GREEK PHI SYMBOL
    (Unicode:#$03D6; Attr:laLower; CaseCode:#$03A0),   // GREEK PI SYMBOL
    (Unicode:#$03D7; Attr:laLower; CaseCode:#$FFFF),   // GREEK KAI SYMBOL
    (Unicode:#$03DA; Attr:laUpper; CaseCode:#$03DB),   // GREEK LETTER STIGMA
    (Unicode:#$03DB; Attr:laLower; CaseCode:#$03DA),   // GREEK SMALL LETTER STIGMA
    (Unicode:#$03DC; Attr:laUpper; CaseCode:#$03DD),   // GREEK LETTER DIGAMMA
    (Unicode:#$03DD; Attr:laLower; CaseCode:#$03DC),   // GREEK SMALL LETTER DIGAMMA
    (Unicode:#$03DE; Attr:laUpper; CaseCode:#$03DF),   // GREEK LETTER KOPPA
    (Unicode:#$03DF; Attr:laLower; CaseCode:#$03DE),   // GREEK SMALL LETTER KOPPA
    (Unicode:#$03E0; Attr:laUpper; CaseCode:#$03E1),   // GREEK LETTER SAMPI
    (Unicode:#$03E1; Attr:laLower; CaseCode:#$03E0),   // GREEK SMALL LETTER SAMPI
    (Unicode:#$03E2; Attr:laUpper; CaseCode:#$03E3),   // COPTIC CAPITAL LETTER SHEI
    (Unicode:#$03E3; Attr:laLower; CaseCode:#$03E2),   // COPTIC SMALL LETTER SHEI
    (Unicode:#$03E4; Attr:laUpper; CaseCode:#$03E5),   // COPTIC CAPITAL LETTER FEI
    (Unicode:#$03E5; Attr:laLower; CaseCode:#$03E4),   // COPTIC SMALL LETTER FEI
    (Unicode:#$03E6; Attr:laUpper; CaseCode:#$03E7),   // COPTIC CAPITAL LETTER KHEI
    (Unicode:#$03E7; Attr:laLower; CaseCode:#$03E6),   // COPTIC SMALL LETTER KHEI
    (Unicode:#$03E8; Attr:laUpper; CaseCode:#$03E9),   // COPTIC CAPITAL LETTER HORI
    (Unicode:#$03E9; Attr:laLower; CaseCode:#$03E8),   // COPTIC SMALL LETTER HORI
    (Unicode:#$03EA; Attr:laUpper; CaseCode:#$03EB),   // COPTIC CAPITAL LETTER GANGIA
    (Unicode:#$03EB; Attr:laLower; CaseCode:#$03EA),   // COPTIC SMALL LETTER GANGIA
    (Unicode:#$03EC; Attr:laUpper; CaseCode:#$03ED),   // COPTIC CAPITAL LETTER SHIMA
    (Unicode:#$03ED; Attr:laLower; CaseCode:#$03EC),   // COPTIC SMALL LETTER SHIMA
    (Unicode:#$03EE; Attr:laUpper; CaseCode:#$03EF),   // COPTIC CAPITAL LETTER DEI
    (Unicode:#$03EF; Attr:laLower; CaseCode:#$03EE),   // COPTIC SMALL LETTER DEI
    (Unicode:#$03F0; Attr:laLower; CaseCode:#$039A),   // GREEK KAPPA SYMBOL
    (Unicode:#$03F1; Attr:laLower; CaseCode:#$03A1),   // GREEK RHO SYMBOL
    (Unicode:#$03F2; Attr:laLower; CaseCode:#$03A3),   // GREEK LUNATE SIGMA SYMBOL
    (Unicode:#$03F3; Attr:laLower; CaseCode:#$FFFF),   // GREEK LETTER YOT
    (Unicode:#$03F4; Attr:laUpper; CaseCode:#$03B8),   // GREEK CAPITAL THETA SYMBOL
    (Unicode:#$03F5; Attr:laLower; CaseCode:#$0395),   // GREEK LUNATE EPSILON SYMBOL
    (Unicode:#$0400; Attr:laUpper; CaseCode:#$0450),   // CYRILLIC CAPITAL LETTER IE WITH GRAVE
    (Unicode:#$0401; Attr:laUpper; CaseCode:#$0451),   // CYRILLIC CAPITAL LETTER IO
    (Unicode:#$0402; Attr:laUpper; CaseCode:#$0452),   // CYRILLIC CAPITAL LETTER DJE
    (Unicode:#$0403; Attr:laUpper; CaseCode:#$0453),   // CYRILLIC CAPITAL LETTER GJE
    (Unicode:#$0404; Attr:laUpper; CaseCode:#$0454),   // CYRILLIC CAPITAL LETTER UKRAINIAN IE
    (Unicode:#$0405; Attr:laUpper; CaseCode:#$0455),   // CYRILLIC CAPITAL LETTER DZE
    (Unicode:#$0406; Attr:laUpper; CaseCode:#$0456),   // CYRILLIC CAPITAL LETTER BYELORUSSIAN-UKRAINIAN I
    (Unicode:#$0407; Attr:laUpper; CaseCode:#$0457),   // CYRILLIC CAPITAL LETTER YI
    (Unicode:#$0408; Attr:laUpper; CaseCode:#$0458),   // CYRILLIC CAPITAL LETTER JE
    (Unicode:#$0409; Attr:laUpper; CaseCode:#$0459),   // CYRILLIC CAPITAL LETTER LJE
    (Unicode:#$040A; Attr:laUpper; CaseCode:#$045A),   // CYRILLIC CAPITAL LETTER NJE
    (Unicode:#$040B; Attr:laUpper; CaseCode:#$045B),   // CYRILLIC CAPITAL LETTER TSHE
    (Unicode:#$040C; Attr:laUpper; CaseCode:#$045C),   // CYRILLIC CAPITAL LETTER KJE
    (Unicode:#$040D; Attr:laUpper; CaseCode:#$045D),   // CYRILLIC CAPITAL LETTER I WITH GRAVE
    (Unicode:#$040E; Attr:laUpper; CaseCode:#$045E),   // CYRILLIC CAPITAL LETTER SHORT U
    (Unicode:#$040F; Attr:laUpper; CaseCode:#$045F),   // CYRILLIC CAPITAL LETTER DZHE
    (Unicode:#$0410; Attr:laUpper; CaseCode:#$0430),   // CYRILLIC CAPITAL LETTER A
    (Unicode:#$0411; Attr:laUpper; CaseCode:#$0431),   // CYRILLIC CAPITAL LETTER BE
    (Unicode:#$0412; Attr:laUpper; CaseCode:#$0432),   // CYRILLIC CAPITAL LETTER VE
    (Unicode:#$0413; Attr:laUpper; CaseCode:#$0433),   // CYRILLIC CAPITAL LETTER GHE
    (Unicode:#$0414; Attr:laUpper; CaseCode:#$0434),   // CYRILLIC CAPITAL LETTER DE
    (Unicode:#$0415; Attr:laUpper; CaseCode:#$0435),   // CYRILLIC CAPITAL LETTER IE
    (Unicode:#$0416; Attr:laUpper; CaseCode:#$0436),   // CYRILLIC CAPITAL LETTER ZHE
    (Unicode:#$0417; Attr:laUpper; CaseCode:#$0437),   // CYRILLIC CAPITAL LETTER ZE
    (Unicode:#$0418; Attr:laUpper; CaseCode:#$0438),   // CYRILLIC CAPITAL LETTER I
    (Unicode:#$0419; Attr:laUpper; CaseCode:#$0439),   // CYRILLIC CAPITAL LETTER SHORT I
    (Unicode:#$041A; Attr:laUpper; CaseCode:#$043A),   // CYRILLIC CAPITAL LETTER KA
    (Unicode:#$041B; Attr:laUpper; CaseCode:#$043B),   // CYRILLIC CAPITAL LETTER EL
    (Unicode:#$041C; Attr:laUpper; CaseCode:#$043C),   // CYRILLIC CAPITAL LETTER EM
    (Unicode:#$041D; Attr:laUpper; CaseCode:#$043D),   // CYRILLIC CAPITAL LETTER EN
    (Unicode:#$041E; Attr:laUpper; CaseCode:#$043E),   // CYRILLIC CAPITAL LETTER O
    (Unicode:#$041F; Attr:laUpper; CaseCode:#$043F),   // CYRILLIC CAPITAL LETTER PE
    (Unicode:#$0420; Attr:laUpper; CaseCode:#$0440),   // CYRILLIC CAPITAL LETTER ER
    (Unicode:#$0421; Attr:laUpper; CaseCode:#$0441),   // CYRILLIC CAPITAL LETTER ES
    (Unicode:#$0422; Attr:laUpper; CaseCode:#$0442),   // CYRILLIC CAPITAL LETTER TE
    (Unicode:#$0423; Attr:laUpper; CaseCode:#$0443),   // CYRILLIC CAPITAL LETTER U
    (Unicode:#$0424; Attr:laUpper; CaseCode:#$0444),   // CYRILLIC CAPITAL LETTER EF
    (Unicode:#$0425; Attr:laUpper; CaseCode:#$0445),   // CYRILLIC CAPITAL LETTER HA
    (Unicode:#$0426; Attr:laUpper; CaseCode:#$0446),   // CYRILLIC CAPITAL LETTER TSE
    (Unicode:#$0427; Attr:laUpper; CaseCode:#$0447),   // CYRILLIC CAPITAL LETTER CHE
    (Unicode:#$0428; Attr:laUpper; CaseCode:#$0448),   // CYRILLIC CAPITAL LETTER SHA
    (Unicode:#$0429; Attr:laUpper; CaseCode:#$0449),   // CYRILLIC CAPITAL LETTER SHCHA
    (Unicode:#$042A; Attr:laUpper; CaseCode:#$044A),   // CYRILLIC CAPITAL LETTER HARD SIGN
    (Unicode:#$042B; Attr:laUpper; CaseCode:#$044B),   // CYRILLIC CAPITAL LETTER YERU
    (Unicode:#$042C; Attr:laUpper; CaseCode:#$044C),   // CYRILLIC CAPITAL LETTER SOFT SIGN
    (Unicode:#$042D; Attr:laUpper; CaseCode:#$044D),   // CYRILLIC CAPITAL LETTER E
    (Unicode:#$042E; Attr:laUpper; CaseCode:#$044E),   // CYRILLIC CAPITAL LETTER YU
    (Unicode:#$042F; Attr:laUpper; CaseCode:#$044F),   // CYRILLIC CAPITAL LETTER YA
    (Unicode:#$0430; Attr:laLower; CaseCode:#$0410),   // CYRILLIC SMALL LETTER A
    (Unicode:#$0431; Attr:laLower; CaseCode:#$0411),   // CYRILLIC SMALL LETTER BE
    (Unicode:#$0432; Attr:laLower; CaseCode:#$0412),   // CYRILLIC SMALL LETTER VE
    (Unicode:#$0433; Attr:laLower; CaseCode:#$0413),   // CYRILLIC SMALL LETTER GHE
    (Unicode:#$0434; Attr:laLower; CaseCode:#$0414),   // CYRILLIC SMALL LETTER DE
    (Unicode:#$0435; Attr:laLower; CaseCode:#$0415),   // CYRILLIC SMALL LETTER IE
    (Unicode:#$0436; Attr:laLower; CaseCode:#$0416),   // CYRILLIC SMALL LETTER ZHE
    (Unicode:#$0437; Attr:laLower; CaseCode:#$0417),   // CYRILLIC SMALL LETTER ZE
    (Unicode:#$0438; Attr:laLower; CaseCode:#$0418),   // CYRILLIC SMALL LETTER I
    (Unicode:#$0439; Attr:laLower; CaseCode:#$0419),   // CYRILLIC SMALL LETTER SHORT I
    (Unicode:#$043A; Attr:laLower; CaseCode:#$041A),   // CYRILLIC SMALL LETTER KA
    (Unicode:#$043B; Attr:laLower; CaseCode:#$041B),   // CYRILLIC SMALL LETTER EL
    (Unicode:#$043C; Attr:laLower; CaseCode:#$041C),   // CYRILLIC SMALL LETTER EM
    (Unicode:#$043D; Attr:laLower; CaseCode:#$041D),   // CYRILLIC SMALL LETTER EN
    (Unicode:#$043E; Attr:laLower; CaseCode:#$041E),   // CYRILLIC SMALL LETTER O
    (Unicode:#$043F; Attr:laLower; CaseCode:#$041F),   // CYRILLIC SMALL LETTER PE
    (Unicode:#$0440; Attr:laLower; CaseCode:#$0420),   // CYRILLIC SMALL LETTER ER
    (Unicode:#$0441; Attr:laLower; CaseCode:#$0421),   // CYRILLIC SMALL LETTER ES
    (Unicode:#$0442; Attr:laLower; CaseCode:#$0422),   // CYRILLIC SMALL LETTER TE
    (Unicode:#$0443; Attr:laLower; CaseCode:#$0423),   // CYRILLIC SMALL LETTER U
    (Unicode:#$0444; Attr:laLower; CaseCode:#$0424),   // CYRILLIC SMALL LETTER EF
    (Unicode:#$0445; Attr:laLower; CaseCode:#$0425),   // CYRILLIC SMALL LETTER HA
    (Unicode:#$0446; Attr:laLower; CaseCode:#$0426),   // CYRILLIC SMALL LETTER TSE
    (Unicode:#$0447; Attr:laLower; CaseCode:#$0427),   // CYRILLIC SMALL LETTER CHE
    (Unicode:#$0448; Attr:laLower; CaseCode:#$0428),   // CYRILLIC SMALL LETTER SHA
    (Unicode:#$0449; Attr:laLower; CaseCode:#$0429),   // CYRILLIC SMALL LETTER SHCHA
    (Unicode:#$044A; Attr:laLower; CaseCode:#$042A),   // CYRILLIC SMALL LETTER HARD SIGN
    (Unicode:#$044B; Attr:laLower; CaseCode:#$042B),   // CYRILLIC SMALL LETTER YERU
    (Unicode:#$044C; Attr:laLower; CaseCode:#$042C),   // CYRILLIC SMALL LETTER SOFT SIGN
    (Unicode:#$044D; Attr:laLower; CaseCode:#$042D),   // CYRILLIC SMALL LETTER E
    (Unicode:#$044E; Attr:laLower; CaseCode:#$042E),   // CYRILLIC SMALL LETTER YU
    (Unicode:#$044F; Attr:laLower; CaseCode:#$042F),   // CYRILLIC SMALL LETTER YA
    (Unicode:#$0450; Attr:laLower; CaseCode:#$0400),   // CYRILLIC SMALL LETTER IE WITH GRAVE
    (Unicode:#$0451; Attr:laLower; CaseCode:#$0401),   // CYRILLIC SMALL LETTER IO
    (Unicode:#$0452; Attr:laLower; CaseCode:#$0402),   // CYRILLIC SMALL LETTER DJE
    (Unicode:#$0453; Attr:laLower; CaseCode:#$0403),   // CYRILLIC SMALL LETTER GJE
    (Unicode:#$0454; Attr:laLower; CaseCode:#$0404),   // CYRILLIC SMALL LETTER UKRAINIAN IE
    (Unicode:#$0455; Attr:laLower; CaseCode:#$0405),   // CYRILLIC SMALL LETTER DZE
    (Unicode:#$0456; Attr:laLower; CaseCode:#$0406),   // CYRILLIC SMALL LETTER BYELORUSSIAN-UKRAINIAN I
    (Unicode:#$0457; Attr:laLower; CaseCode:#$0407),   // CYRILLIC SMALL LETTER YI
    (Unicode:#$0458; Attr:laLower; CaseCode:#$0408),   // CYRILLIC SMALL LETTER JE
    (Unicode:#$0459; Attr:laLower; CaseCode:#$0409),   // CYRILLIC SMALL LETTER LJE
    (Unicode:#$045A; Attr:laLower; CaseCode:#$040A),   // CYRILLIC SMALL LETTER NJE
    (Unicode:#$045B; Attr:laLower; CaseCode:#$040B),   // CYRILLIC SMALL LETTER TSHE
    (Unicode:#$045C; Attr:laLower; CaseCode:#$040C),   // CYRILLIC SMALL LETTER KJE
    (Unicode:#$045D; Attr:laLower; CaseCode:#$040D),   // CYRILLIC SMALL LETTER I WITH GRAVE
    (Unicode:#$045E; Attr:laLower; CaseCode:#$040E),   // CYRILLIC SMALL LETTER SHORT U
    (Unicode:#$045F; Attr:laLower; CaseCode:#$040F),   // CYRILLIC SMALL LETTER DZHE
    (Unicode:#$0460; Attr:laUpper; CaseCode:#$0461),   // CYRILLIC CAPITAL LETTER OMEGA
    (Unicode:#$0461; Attr:laLower; CaseCode:#$0460),   // CYRILLIC SMALL LETTER OMEGA
    (Unicode:#$0462; Attr:laUpper; CaseCode:#$0463),   // CYRILLIC CAPITAL LETTER YAT
    (Unicode:#$0463; Attr:laLower; CaseCode:#$0462),   // CYRILLIC SMALL LETTER YAT
    (Unicode:#$0464; Attr:laUpper; CaseCode:#$0465),   // CYRILLIC CAPITAL LETTER IOTIFIED E
    (Unicode:#$0465; Attr:laLower; CaseCode:#$0464),   // CYRILLIC SMALL LETTER IOTIFIED E
    (Unicode:#$0466; Attr:laUpper; CaseCode:#$0467),   // CYRILLIC CAPITAL LETTER LITTLE YUS
    (Unicode:#$0467; Attr:laLower; CaseCode:#$0466),   // CYRILLIC SMALL LETTER LITTLE YUS
    (Unicode:#$0468; Attr:laUpper; CaseCode:#$0469),   // CYRILLIC CAPITAL LETTER IOTIFIED LITTLE YUS
    (Unicode:#$0469; Attr:laLower; CaseCode:#$0468),   // CYRILLIC SMALL LETTER IOTIFIED LITTLE YUS
    (Unicode:#$046A; Attr:laUpper; CaseCode:#$046B),   // CYRILLIC CAPITAL LETTER BIG YUS
    (Unicode:#$046B; Attr:laLower; CaseCode:#$046A),   // CYRILLIC SMALL LETTER BIG YUS
    (Unicode:#$046C; Attr:laUpper; CaseCode:#$046D),   // CYRILLIC CAPITAL LETTER IOTIFIED BIG YUS
    (Unicode:#$046D; Attr:laLower; CaseCode:#$046C),   // CYRILLIC SMALL LETTER IOTIFIED BIG YUS
    (Unicode:#$046E; Attr:laUpper; CaseCode:#$046F),   // CYRILLIC CAPITAL LETTER KSI
    (Unicode:#$046F; Attr:laLower; CaseCode:#$046E),   // CYRILLIC SMALL LETTER KSI
    (Unicode:#$0470; Attr:laUpper; CaseCode:#$0471),   // CYRILLIC CAPITAL LETTER PSI
    (Unicode:#$0471; Attr:laLower; CaseCode:#$0470),   // CYRILLIC SMALL LETTER PSI
    (Unicode:#$0472; Attr:laUpper; CaseCode:#$0473),   // CYRILLIC CAPITAL LETTER FITA
    (Unicode:#$0473; Attr:laLower; CaseCode:#$0472),   // CYRILLIC SMALL LETTER FITA
    (Unicode:#$0474; Attr:laUpper; CaseCode:#$0475),   // CYRILLIC CAPITAL LETTER IZHITSA
    (Unicode:#$0475; Attr:laLower; CaseCode:#$0474),   // CYRILLIC SMALL LETTER IZHITSA
    (Unicode:#$0476; Attr:laUpper; CaseCode:#$0477),   // CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0477; Attr:laLower; CaseCode:#$0476),   // CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0478; Attr:laUpper; CaseCode:#$0479),   // CYRILLIC CAPITAL LETTER UK
    (Unicode:#$0479; Attr:laLower; CaseCode:#$0478),   // CYRILLIC SMALL LETTER UK
    (Unicode:#$047A; Attr:laUpper; CaseCode:#$047B),   // CYRILLIC CAPITAL LETTER ROUND OMEGA
    (Unicode:#$047B; Attr:laLower; CaseCode:#$047A),   // CYRILLIC SMALL LETTER ROUND OMEGA
    (Unicode:#$047C; Attr:laUpper; CaseCode:#$047D),   // CYRILLIC CAPITAL LETTER OMEGA WITH TITLO
    (Unicode:#$047D; Attr:laLower; CaseCode:#$047C),   // CYRILLIC SMALL LETTER OMEGA WITH TITLO
    (Unicode:#$047E; Attr:laUpper; CaseCode:#$047F),   // CYRILLIC CAPITAL LETTER OT
    (Unicode:#$047F; Attr:laLower; CaseCode:#$047E),   // CYRILLIC SMALL LETTER OT
    (Unicode:#$0480; Attr:laUpper; CaseCode:#$0481),   // CYRILLIC CAPITAL LETTER KOPPA
    (Unicode:#$0481; Attr:laLower; CaseCode:#$0480),   // CYRILLIC SMALL LETTER KOPPA
    (Unicode:#$048C; Attr:laUpper; CaseCode:#$048D),   // CYRILLIC CAPITAL LETTER SEMISOFT SIGN
    (Unicode:#$048D; Attr:laLower; CaseCode:#$048C),   // CYRILLIC SMALL LETTER SEMISOFT SIGN
    (Unicode:#$048E; Attr:laUpper; CaseCode:#$048F),   // CYRILLIC CAPITAL LETTER ER WITH TICK
    (Unicode:#$048F; Attr:laLower; CaseCode:#$048E),   // CYRILLIC SMALL LETTER ER WITH TICK
    (Unicode:#$0490; Attr:laUpper; CaseCode:#$0491),   // CYRILLIC CAPITAL LETTER GHE WITH UPTURN
    (Unicode:#$0491; Attr:laLower; CaseCode:#$0490),   // CYRILLIC SMALL LETTER GHE WITH UPTURN
    (Unicode:#$0492; Attr:laUpper; CaseCode:#$0493),   // CYRILLIC CAPITAL LETTER GHE WITH STROKE
    (Unicode:#$0493; Attr:laLower; CaseCode:#$0492),   // CYRILLIC SMALL LETTER GHE WITH STROKE
    (Unicode:#$0494; Attr:laUpper; CaseCode:#$0495),   // CYRILLIC CAPITAL LETTER GHE WITH MIDDLE HOOK
    (Unicode:#$0495; Attr:laLower; CaseCode:#$0494),   // CYRILLIC SMALL LETTER GHE WITH MIDDLE HOOK
    (Unicode:#$0496; Attr:laUpper; CaseCode:#$0497),   // CYRILLIC CAPITAL LETTER ZHE WITH DESCENDER
    (Unicode:#$0497; Attr:laLower; CaseCode:#$0496),   // CYRILLIC SMALL LETTER ZHE WITH DESCENDER
    (Unicode:#$0498; Attr:laUpper; CaseCode:#$0499),   // CYRILLIC CAPITAL LETTER ZE WITH DESCENDER
    (Unicode:#$0499; Attr:laLower; CaseCode:#$0498),   // CYRILLIC SMALL LETTER ZE WITH DESCENDER
    (Unicode:#$049A; Attr:laUpper; CaseCode:#$049B),   // CYRILLIC CAPITAL LETTER KA WITH DESCENDER
    (Unicode:#$049B; Attr:laLower; CaseCode:#$049A),   // CYRILLIC SMALL LETTER KA WITH DESCENDER
    (Unicode:#$049C; Attr:laUpper; CaseCode:#$049D),   // CYRILLIC CAPITAL LETTER KA WITH VERTICAL STROKE
    (Unicode:#$049D; Attr:laLower; CaseCode:#$049C),   // CYRILLIC SMALL LETTER KA WITH VERTICAL STROKE
    (Unicode:#$049E; Attr:laUpper; CaseCode:#$049F),   // CYRILLIC CAPITAL LETTER KA WITH STROKE
    (Unicode:#$049F; Attr:laLower; CaseCode:#$049E),   // CYRILLIC SMALL LETTER KA WITH STROKE
    (Unicode:#$04A0; Attr:laUpper; CaseCode:#$04A1),   // CYRILLIC CAPITAL LETTER BASHKIR KA
    (Unicode:#$04A1; Attr:laLower; CaseCode:#$04A0),   // CYRILLIC SMALL LETTER BASHKIR KA
    (Unicode:#$04A2; Attr:laUpper; CaseCode:#$04A3),   // CYRILLIC CAPITAL LETTER EN WITH DESCENDER
    (Unicode:#$04A3; Attr:laLower; CaseCode:#$04A2),   // CYRILLIC SMALL LETTER EN WITH DESCENDER
    (Unicode:#$04A4; Attr:laUpper; CaseCode:#$04A5),   // CYRILLIC CAPITAL LIGATURE EN GHE
    (Unicode:#$04A5; Attr:laLower; CaseCode:#$04A4),   // CYRILLIC SMALL LIGATURE EN GHE
    (Unicode:#$04A6; Attr:laUpper; CaseCode:#$04A7),   // CYRILLIC CAPITAL LETTER PE WITH MIDDLE HOOK
    (Unicode:#$04A7; Attr:laLower; CaseCode:#$04A6),   // CYRILLIC SMALL LETTER PE WITH MIDDLE HOOK
    (Unicode:#$04A8; Attr:laUpper; CaseCode:#$04A9),   // CYRILLIC CAPITAL LETTER ABKHASIAN HA
    (Unicode:#$04A9; Attr:laLower; CaseCode:#$04A8),   // CYRILLIC SMALL LETTER ABKHASIAN HA
    (Unicode:#$04AA; Attr:laUpper; CaseCode:#$04AB),   // CYRILLIC CAPITAL LETTER ES WITH DESCENDER
    (Unicode:#$04AB; Attr:laLower; CaseCode:#$04AA),   // CYRILLIC SMALL LETTER ES WITH DESCENDER
    (Unicode:#$04AC; Attr:laUpper; CaseCode:#$04AD),   // CYRILLIC CAPITAL LETTER TE WITH DESCENDER
    (Unicode:#$04AD; Attr:laLower; CaseCode:#$04AC),   // CYRILLIC SMALL LETTER TE WITH DESCENDER
    (Unicode:#$04AE; Attr:laUpper; CaseCode:#$04AF),   // CYRILLIC CAPITAL LETTER STRAIGHT U
    (Unicode:#$04AF; Attr:laLower; CaseCode:#$04AE),   // CYRILLIC SMALL LETTER STRAIGHT U
    (Unicode:#$04B0; Attr:laUpper; CaseCode:#$04B1),   // CYRILLIC CAPITAL LETTER STRAIGHT U WITH STROKE
    (Unicode:#$04B1; Attr:laLower; CaseCode:#$04B0),   // CYRILLIC SMALL LETTER STRAIGHT U WITH STROKE
    (Unicode:#$04B2; Attr:laUpper; CaseCode:#$04B3),   // CYRILLIC CAPITAL LETTER HA WITH DESCENDER
    (Unicode:#$04B3; Attr:laLower; CaseCode:#$04B2),   // CYRILLIC SMALL LETTER HA WITH DESCENDER
    (Unicode:#$04B4; Attr:laUpper; CaseCode:#$04B5),   // CYRILLIC CAPITAL LIGATURE TE TSE
    (Unicode:#$04B5; Attr:laLower; CaseCode:#$04B4),   // CYRILLIC SMALL LIGATURE TE TSE
    (Unicode:#$04B6; Attr:laUpper; CaseCode:#$04B7),   // CYRILLIC CAPITAL LETTER CHE WITH DESCENDER
    (Unicode:#$04B7; Attr:laLower; CaseCode:#$04B6),   // CYRILLIC SMALL LETTER CHE WITH DESCENDER
    (Unicode:#$04B8; Attr:laUpper; CaseCode:#$04B9),   // CYRILLIC CAPITAL LETTER CHE WITH VERTICAL STROKE
    (Unicode:#$04B9; Attr:laLower; CaseCode:#$04B8),   // CYRILLIC SMALL LETTER CHE WITH VERTICAL STROKE
    (Unicode:#$04BA; Attr:laUpper; CaseCode:#$04BB),   // CYRILLIC CAPITAL LETTER SHHA
    (Unicode:#$04BB; Attr:laLower; CaseCode:#$04BA),   // CYRILLIC SMALL LETTER SHHA
    (Unicode:#$04BC; Attr:laUpper; CaseCode:#$04BD),   // CYRILLIC CAPITAL LETTER ABKHASIAN CHE
    (Unicode:#$04BD; Attr:laLower; CaseCode:#$04BC),   // CYRILLIC SMALL LETTER ABKHASIAN CHE
    (Unicode:#$04BE; Attr:laUpper; CaseCode:#$04BF),   // CYRILLIC CAPITAL LETTER ABKHASIAN CHE WITH DESCENDER
    (Unicode:#$04BF; Attr:laLower; CaseCode:#$04BE),   // CYRILLIC SMALL LETTER ABKHASIAN CHE WITH DESCENDER
    (Unicode:#$04C0; Attr:laUpper; CaseCode:#$FFFF),   // CYRILLIC LETTER PALOCHKA
    (Unicode:#$04C1; Attr:laUpper; CaseCode:#$04C2),   // CYRILLIC CAPITAL LETTER ZHE WITH BREVE
    (Unicode:#$04C2; Attr:laLower; CaseCode:#$04C1),   // CYRILLIC SMALL LETTER ZHE WITH BREVE
    (Unicode:#$04C3; Attr:laUpper; CaseCode:#$04C4),   // CYRILLIC CAPITAL LETTER KA WITH HOOK
    (Unicode:#$04C4; Attr:laLower; CaseCode:#$04C3),   // CYRILLIC SMALL LETTER KA WITH HOOK
    (Unicode:#$04C7; Attr:laUpper; CaseCode:#$04C8),   // CYRILLIC CAPITAL LETTER EN WITH HOOK
    (Unicode:#$04C8; Attr:laLower; CaseCode:#$04C7),   // CYRILLIC SMALL LETTER EN WITH HOOK
    (Unicode:#$04CB; Attr:laUpper; CaseCode:#$04CC),   // CYRILLIC CAPITAL LETTER KHAKASSIAN CHE
    (Unicode:#$04CC; Attr:laLower; CaseCode:#$04CB),   // CYRILLIC SMALL LETTER KHAKASSIAN CHE
    (Unicode:#$04D0; Attr:laUpper; CaseCode:#$04D1),   // CYRILLIC CAPITAL LETTER A WITH BREVE
    (Unicode:#$04D1; Attr:laLower; CaseCode:#$04D0),   // CYRILLIC SMALL LETTER A WITH BREVE
    (Unicode:#$04D2; Attr:laUpper; CaseCode:#$04D3),   // CYRILLIC CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$04D3; Attr:laLower; CaseCode:#$04D2),   // CYRILLIC SMALL LETTER A WITH DIAERESIS
    (Unicode:#$04D4; Attr:laUpper; CaseCode:#$04D5),   // CYRILLIC CAPITAL LIGATURE A IE
    (Unicode:#$04D5; Attr:laLower; CaseCode:#$04D4),   // CYRILLIC SMALL LIGATURE A IE
    (Unicode:#$04D6; Attr:laUpper; CaseCode:#$04D7),   // CYRILLIC CAPITAL LETTER IE WITH BREVE
    (Unicode:#$04D7; Attr:laLower; CaseCode:#$04D6),   // CYRILLIC SMALL LETTER IE WITH BREVE
    (Unicode:#$04D8; Attr:laUpper; CaseCode:#$04D9),   // CYRILLIC CAPITAL LETTER SCHWA
    (Unicode:#$04D9; Attr:laLower; CaseCode:#$04D8),   // CYRILLIC SMALL LETTER SCHWA
    (Unicode:#$04DA; Attr:laUpper; CaseCode:#$04DB),   // CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DB; Attr:laLower; CaseCode:#$04DA),   // CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DC; Attr:laUpper; CaseCode:#$04DD),   // CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DD; Attr:laLower; CaseCode:#$04DC),   // CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DE; Attr:laUpper; CaseCode:#$04DF),   // CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
    (Unicode:#$04DF; Attr:laLower; CaseCode:#$04DE),   // CYRILLIC SMALL LETTER ZE WITH DIAERESIS
    (Unicode:#$04E0; Attr:laUpper; CaseCode:#$04E1),   // CYRILLIC CAPITAL LETTER ABKHASIAN DZE
    (Unicode:#$04E1; Attr:laLower; CaseCode:#$04E0),   // CYRILLIC SMALL LETTER ABKHASIAN DZE
    (Unicode:#$04E2; Attr:laUpper; CaseCode:#$04E3),   // CYRILLIC CAPITAL LETTER I WITH MACRON
    (Unicode:#$04E3; Attr:laLower; CaseCode:#$04E2),   // CYRILLIC SMALL LETTER I WITH MACRON
    (Unicode:#$04E4; Attr:laUpper; CaseCode:#$04E5),   // CYRILLIC CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$04E5; Attr:laLower; CaseCode:#$04E4),   // CYRILLIC SMALL LETTER I WITH DIAERESIS
    (Unicode:#$04E6; Attr:laUpper; CaseCode:#$04E7),   // CYRILLIC CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$04E7; Attr:laLower; CaseCode:#$04E6),   // CYRILLIC SMALL LETTER O WITH DIAERESIS
    (Unicode:#$04E8; Attr:laUpper; CaseCode:#$04E9),   // CYRILLIC CAPITAL LETTER BARRED O
    (Unicode:#$04E9; Attr:laLower; CaseCode:#$04E8),   // CYRILLIC SMALL LETTER BARRED O
    (Unicode:#$04EA; Attr:laUpper; CaseCode:#$04EB),   // CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EB; Attr:laLower; CaseCode:#$04EA),   // CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EC; Attr:laUpper; CaseCode:#$04ED),   // CYRILLIC CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$04ED; Attr:laLower; CaseCode:#$04EC),   // CYRILLIC SMALL LETTER E WITH DIAERESIS
    (Unicode:#$04EE; Attr:laUpper; CaseCode:#$04EF),   // CYRILLIC CAPITAL LETTER U WITH MACRON
    (Unicode:#$04EF; Attr:laLower; CaseCode:#$04EE),   // CYRILLIC SMALL LETTER U WITH MACRON
    (Unicode:#$04F0; Attr:laUpper; CaseCode:#$04F1),   // CYRILLIC CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$04F1; Attr:laLower; CaseCode:#$04F0),   // CYRILLIC SMALL LETTER U WITH DIAERESIS
    (Unicode:#$04F2; Attr:laUpper; CaseCode:#$04F3),   // CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F3; Attr:laLower; CaseCode:#$04F2),   // CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F4; Attr:laUpper; CaseCode:#$04F5),   // CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F5; Attr:laLower; CaseCode:#$04F4),   // CYRILLIC SMALL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F8; Attr:laUpper; CaseCode:#$04F9),   // CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
    (Unicode:#$04F9; Attr:laLower; CaseCode:#$04F8),   // CYRILLIC SMALL LETTER YERU WITH DIAERESIS
    (Unicode:#$0531; Attr:laUpper; CaseCode:#$0561),   // ARMENIAN CAPITAL LETTER AYB
    (Unicode:#$0532; Attr:laUpper; CaseCode:#$0562),   // ARMENIAN CAPITAL LETTER BEN
    (Unicode:#$0533; Attr:laUpper; CaseCode:#$0563),   // ARMENIAN CAPITAL LETTER GIM
    (Unicode:#$0534; Attr:laUpper; CaseCode:#$0564),   // ARMENIAN CAPITAL LETTER DA
    (Unicode:#$0535; Attr:laUpper; CaseCode:#$0565),   // ARMENIAN CAPITAL LETTER ECH
    (Unicode:#$0536; Attr:laUpper; CaseCode:#$0566),   // ARMENIAN CAPITAL LETTER ZA
    (Unicode:#$0537; Attr:laUpper; CaseCode:#$0567),   // ARMENIAN CAPITAL LETTER EH
    (Unicode:#$0538; Attr:laUpper; CaseCode:#$0568),   // ARMENIAN CAPITAL LETTER ET
    (Unicode:#$0539; Attr:laUpper; CaseCode:#$0569),   // ARMENIAN CAPITAL LETTER TO
    (Unicode:#$053A; Attr:laUpper; CaseCode:#$056A),   // ARMENIAN CAPITAL LETTER ZHE
    (Unicode:#$053B; Attr:laUpper; CaseCode:#$056B),   // ARMENIAN CAPITAL LETTER INI
    (Unicode:#$053C; Attr:laUpper; CaseCode:#$056C),   // ARMENIAN CAPITAL LETTER LIWN
    (Unicode:#$053D; Attr:laUpper; CaseCode:#$056D),   // ARMENIAN CAPITAL LETTER XEH
    (Unicode:#$053E; Attr:laUpper; CaseCode:#$056E),   // ARMENIAN CAPITAL LETTER CA
    (Unicode:#$053F; Attr:laUpper; CaseCode:#$056F),   // ARMENIAN CAPITAL LETTER KEN
    (Unicode:#$0540; Attr:laUpper; CaseCode:#$0570),   // ARMENIAN CAPITAL LETTER HO
    (Unicode:#$0541; Attr:laUpper; CaseCode:#$0571),   // ARMENIAN CAPITAL LETTER JA
    (Unicode:#$0542; Attr:laUpper; CaseCode:#$0572),   // ARMENIAN CAPITAL LETTER GHAD
    (Unicode:#$0543; Attr:laUpper; CaseCode:#$0573),   // ARMENIAN CAPITAL LETTER CHEH
    (Unicode:#$0544; Attr:laUpper; CaseCode:#$0574),   // ARMENIAN CAPITAL LETTER MEN
    (Unicode:#$0545; Attr:laUpper; CaseCode:#$0575),   // ARMENIAN CAPITAL LETTER YI
    (Unicode:#$0546; Attr:laUpper; CaseCode:#$0576),   // ARMENIAN CAPITAL LETTER NOW
    (Unicode:#$0547; Attr:laUpper; CaseCode:#$0577),   // ARMENIAN CAPITAL LETTER SHA
    (Unicode:#$0548; Attr:laUpper; CaseCode:#$0578),   // ARMENIAN CAPITAL LETTER VO
    (Unicode:#$0549; Attr:laUpper; CaseCode:#$0579),   // ARMENIAN CAPITAL LETTER CHA
    (Unicode:#$054A; Attr:laUpper; CaseCode:#$057A),   // ARMENIAN CAPITAL LETTER PEH
    (Unicode:#$054B; Attr:laUpper; CaseCode:#$057B),   // ARMENIAN CAPITAL LETTER JHEH
    (Unicode:#$054C; Attr:laUpper; CaseCode:#$057C),   // ARMENIAN CAPITAL LETTER RA
    (Unicode:#$054D; Attr:laUpper; CaseCode:#$057D),   // ARMENIAN CAPITAL LETTER SEH
    (Unicode:#$054E; Attr:laUpper; CaseCode:#$057E),   // ARMENIAN CAPITAL LETTER VEW
    (Unicode:#$054F; Attr:laUpper; CaseCode:#$057F),   // ARMENIAN CAPITAL LETTER TIWN
    (Unicode:#$0550; Attr:laUpper; CaseCode:#$0580),   // ARMENIAN CAPITAL LETTER REH
    (Unicode:#$0551; Attr:laUpper; CaseCode:#$0581),   // ARMENIAN CAPITAL LETTER CO
    (Unicode:#$0552; Attr:laUpper; CaseCode:#$0582),   // ARMENIAN CAPITAL LETTER YIWN
    (Unicode:#$0553; Attr:laUpper; CaseCode:#$0583),   // ARMENIAN CAPITAL LETTER PIWR
    (Unicode:#$0554; Attr:laUpper; CaseCode:#$0584),   // ARMENIAN CAPITAL LETTER KEH
    (Unicode:#$0555; Attr:laUpper; CaseCode:#$0585),   // ARMENIAN CAPITAL LETTER OH
    (Unicode:#$0556; Attr:laUpper; CaseCode:#$0586),   // ARMENIAN CAPITAL LETTER FEH
    (Unicode:#$0561; Attr:laLower; CaseCode:#$0531),   // ARMENIAN SMALL LETTER AYB
    (Unicode:#$0562; Attr:laLower; CaseCode:#$0532),   // ARMENIAN SMALL LETTER BEN
    (Unicode:#$0563; Attr:laLower; CaseCode:#$0533),   // ARMENIAN SMALL LETTER GIM
    (Unicode:#$0564; Attr:laLower; CaseCode:#$0534),   // ARMENIAN SMALL LETTER DA
    (Unicode:#$0565; Attr:laLower; CaseCode:#$0535),   // ARMENIAN SMALL LETTER ECH
    (Unicode:#$0566; Attr:laLower; CaseCode:#$0536),   // ARMENIAN SMALL LETTER ZA
    (Unicode:#$0567; Attr:laLower; CaseCode:#$0537),   // ARMENIAN SMALL LETTER EH
    (Unicode:#$0568; Attr:laLower; CaseCode:#$0538),   // ARMENIAN SMALL LETTER ET
    (Unicode:#$0569; Attr:laLower; CaseCode:#$0539),   // ARMENIAN SMALL LETTER TO
    (Unicode:#$056A; Attr:laLower; CaseCode:#$053A),   // ARMENIAN SMALL LETTER ZHE
    (Unicode:#$056B; Attr:laLower; CaseCode:#$053B),   // ARMENIAN SMALL LETTER INI
    (Unicode:#$056C; Attr:laLower; CaseCode:#$053C),   // ARMENIAN SMALL LETTER LIWN
    (Unicode:#$056D; Attr:laLower; CaseCode:#$053D),   // ARMENIAN SMALL LETTER XEH
    (Unicode:#$056E; Attr:laLower; CaseCode:#$053E),   // ARMENIAN SMALL LETTER CA
    (Unicode:#$056F; Attr:laLower; CaseCode:#$053F),   // ARMENIAN SMALL LETTER KEN
    (Unicode:#$0570; Attr:laLower; CaseCode:#$0540),   // ARMENIAN SMALL LETTER HO
    (Unicode:#$0571; Attr:laLower; CaseCode:#$0541),   // ARMENIAN SMALL LETTER JA
    (Unicode:#$0572; Attr:laLower; CaseCode:#$0542),   // ARMENIAN SMALL LETTER GHAD
    (Unicode:#$0573; Attr:laLower; CaseCode:#$0543),   // ARMENIAN SMALL LETTER CHEH
    (Unicode:#$0574; Attr:laLower; CaseCode:#$0544),   // ARMENIAN SMALL LETTER MEN
    (Unicode:#$0575; Attr:laLower; CaseCode:#$0545),   // ARMENIAN SMALL LETTER YI
    (Unicode:#$0576; Attr:laLower; CaseCode:#$0546),   // ARMENIAN SMALL LETTER NOW
    (Unicode:#$0577; Attr:laLower; CaseCode:#$0547),   // ARMENIAN SMALL LETTER SHA
    (Unicode:#$0578; Attr:laLower; CaseCode:#$0548),   // ARMENIAN SMALL LETTER VO
    (Unicode:#$0579; Attr:laLower; CaseCode:#$0549),   // ARMENIAN SMALL LETTER CHA
    (Unicode:#$057A; Attr:laLower; CaseCode:#$054A),   // ARMENIAN SMALL LETTER PEH
    (Unicode:#$057B; Attr:laLower; CaseCode:#$054B),   // ARMENIAN SMALL LETTER JHEH
    (Unicode:#$057C; Attr:laLower; CaseCode:#$054C),   // ARMENIAN SMALL LETTER RA
    (Unicode:#$057D; Attr:laLower; CaseCode:#$054D),   // ARMENIAN SMALL LETTER SEH
    (Unicode:#$057E; Attr:laLower; CaseCode:#$054E),   // ARMENIAN SMALL LETTER VEW
    (Unicode:#$057F; Attr:laLower; CaseCode:#$054F),   // ARMENIAN SMALL LETTER TIWN
    (Unicode:#$0580; Attr:laLower; CaseCode:#$0550),   // ARMENIAN SMALL LETTER REH
    (Unicode:#$0581; Attr:laLower; CaseCode:#$0551),   // ARMENIAN SMALL LETTER CO
    (Unicode:#$0582; Attr:laLower; CaseCode:#$0552),   // ARMENIAN SMALL LETTER YIWN
    (Unicode:#$0583; Attr:laLower; CaseCode:#$0553),   // ARMENIAN SMALL LETTER PIWR
    (Unicode:#$0584; Attr:laLower; CaseCode:#$0554),   // ARMENIAN SMALL LETTER KEH
    (Unicode:#$0585; Attr:laLower; CaseCode:#$0555),   // ARMENIAN SMALL LETTER OH
    (Unicode:#$0586; Attr:laLower; CaseCode:#$0556),   // ARMENIAN SMALL LETTER FEH
    (Unicode:#$0587; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE ECH YIWN
    (Unicode:#$10A0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER AN
    (Unicode:#$10A1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER BAN
    (Unicode:#$10A2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER GAN
    (Unicode:#$10A3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER DON
    (Unicode:#$10A4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER EN
    (Unicode:#$10A5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER VIN
    (Unicode:#$10A6; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ZEN
    (Unicode:#$10A7; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER TAN
    (Unicode:#$10A8; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER IN
    (Unicode:#$10A9; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER KAN
    (Unicode:#$10AA; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER LAS
    (Unicode:#$10AB; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER MAN
    (Unicode:#$10AC; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER NAR
    (Unicode:#$10AD; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ON
    (Unicode:#$10AE; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER PAR
    (Unicode:#$10AF; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER ZHAR
    (Unicode:#$10B0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER RAE
    (Unicode:#$10B1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER SAN
    (Unicode:#$10B2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER TAR
    (Unicode:#$10B3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER UN
    (Unicode:#$10B4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER PHAR
    (Unicode:#$10B5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER KHAR
    (Unicode:#$10B6; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER GHAN
    (Unicode:#$10B7; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER QAR
    (Unicode:#$10B8; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER SHIN
    (Unicode:#$10B9; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CHIN
    (Unicode:#$10BA; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CAN
    (Unicode:#$10BB; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER JIL
    (Unicode:#$10BC; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CIL
    (Unicode:#$10BD; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER CHAR
    (Unicode:#$10BE; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER XAN
    (Unicode:#$10BF; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER JHAN
    (Unicode:#$10C0; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HAE
    (Unicode:#$10C1; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HE
    (Unicode:#$10C2; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HIE
    (Unicode:#$10C3; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER WE
    (Unicode:#$10C4; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HAR
    (Unicode:#$10C5; Attr:laUpper; CaseCode:#$FFFF),   // GEORGIAN CAPITAL LETTER HOE
    (Unicode:#$1E00; Attr:laUpper; CaseCode:#$1E01),   // LATIN CAPITAL LETTER A WITH RING BELOW
    (Unicode:#$1E01; Attr:laLower; CaseCode:#$1E00),   // LATIN SMALL LETTER A WITH RING BELOW
    (Unicode:#$1E02; Attr:laUpper; CaseCode:#$1E03),   // LATIN CAPITAL LETTER B WITH DOT ABOVE
    (Unicode:#$1E03; Attr:laLower; CaseCode:#$1E02),   // LATIN SMALL LETTER B WITH DOT ABOVE
    (Unicode:#$1E04; Attr:laUpper; CaseCode:#$1E05),   // LATIN CAPITAL LETTER B WITH DOT BELOW
    (Unicode:#$1E05; Attr:laLower; CaseCode:#$1E04),   // LATIN SMALL LETTER B WITH DOT BELOW
    (Unicode:#$1E06; Attr:laUpper; CaseCode:#$1E07),   // LATIN CAPITAL LETTER B WITH LINE BELOW
    (Unicode:#$1E07; Attr:laLower; CaseCode:#$1E06),   // LATIN SMALL LETTER B WITH LINE BELOW
    (Unicode:#$1E08; Attr:laUpper; CaseCode:#$1E09),   // LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E09; Attr:laLower; CaseCode:#$1E08),   // LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E0A; Attr:laUpper; CaseCode:#$1E0B),   // LATIN CAPITAL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0B; Attr:laLower; CaseCode:#$1E0A),   // LATIN SMALL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0C; Attr:laUpper; CaseCode:#$1E0D),   // LATIN CAPITAL LETTER D WITH DOT BELOW
    (Unicode:#$1E0D; Attr:laLower; CaseCode:#$1E0C),   // LATIN SMALL LETTER D WITH DOT BELOW
    (Unicode:#$1E0E; Attr:laUpper; CaseCode:#$1E0F),   // LATIN CAPITAL LETTER D WITH LINE BELOW
    (Unicode:#$1E0F; Attr:laLower; CaseCode:#$1E0E),   // LATIN SMALL LETTER D WITH LINE BELOW
    (Unicode:#$1E10; Attr:laUpper; CaseCode:#$1E11),   // LATIN CAPITAL LETTER D WITH CEDILLA
    (Unicode:#$1E11; Attr:laLower; CaseCode:#$1E10),   // LATIN SMALL LETTER D WITH CEDILLA
    (Unicode:#$1E12; Attr:laUpper; CaseCode:#$1E13),   // LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E13; Attr:laLower; CaseCode:#$1E12),   // LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E14; Attr:laUpper; CaseCode:#$1E15),   // LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E15; Attr:laLower; CaseCode:#$1E14),   // LATIN SMALL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E16; Attr:laUpper; CaseCode:#$1E17),   // LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E17; Attr:laLower; CaseCode:#$1E16),   // LATIN SMALL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E18; Attr:laUpper; CaseCode:#$1E19),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E19; Attr:laLower; CaseCode:#$1E18),   // LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E1A; Attr:laUpper; CaseCode:#$1E1B),   // LATIN CAPITAL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1B; Attr:laLower; CaseCode:#$1E1A),   // LATIN SMALL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1C; Attr:laUpper; CaseCode:#$1E1D),   // LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1D; Attr:laLower; CaseCode:#$1E1C),   // LATIN SMALL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1E; Attr:laUpper; CaseCode:#$1E1F),   // LATIN CAPITAL LETTER F WITH DOT ABOVE
    (Unicode:#$1E1F; Attr:laLower; CaseCode:#$1E1E),   // LATIN SMALL LETTER F WITH DOT ABOVE
    (Unicode:#$1E20; Attr:laUpper; CaseCode:#$1E21),   // LATIN CAPITAL LETTER G WITH MACRON
    (Unicode:#$1E21; Attr:laLower; CaseCode:#$1E20),   // LATIN SMALL LETTER G WITH MACRON
    (Unicode:#$1E22; Attr:laUpper; CaseCode:#$1E23),   // LATIN CAPITAL LETTER H WITH DOT ABOVE
    (Unicode:#$1E23; Attr:laLower; CaseCode:#$1E22),   // LATIN SMALL LETTER H WITH DOT ABOVE
    (Unicode:#$1E24; Attr:laUpper; CaseCode:#$1E25),   // LATIN CAPITAL LETTER H WITH DOT BELOW
    (Unicode:#$1E25; Attr:laLower; CaseCode:#$1E24),   // LATIN SMALL LETTER H WITH DOT BELOW
    (Unicode:#$1E26; Attr:laUpper; CaseCode:#$1E27),   // LATIN CAPITAL LETTER H WITH DIAERESIS
    (Unicode:#$1E27; Attr:laLower; CaseCode:#$1E26),   // LATIN SMALL LETTER H WITH DIAERESIS
    (Unicode:#$1E28; Attr:laUpper; CaseCode:#$1E29),   // LATIN CAPITAL LETTER H WITH CEDILLA
    (Unicode:#$1E29; Attr:laLower; CaseCode:#$1E28),   // LATIN SMALL LETTER H WITH CEDILLA
    (Unicode:#$1E2A; Attr:laUpper; CaseCode:#$1E2B),   // LATIN CAPITAL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2B; Attr:laLower; CaseCode:#$1E2A),   // LATIN SMALL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2C; Attr:laUpper; CaseCode:#$1E2D),   // LATIN CAPITAL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2D; Attr:laLower; CaseCode:#$1E2C),   // LATIN SMALL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2E; Attr:laUpper; CaseCode:#$1E2F),   // LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E2F; Attr:laLower; CaseCode:#$1E2E),   // LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E30; Attr:laUpper; CaseCode:#$1E31),   // LATIN CAPITAL LETTER K WITH ACUTE
    (Unicode:#$1E31; Attr:laLower; CaseCode:#$1E30),   // LATIN SMALL LETTER K WITH ACUTE
    (Unicode:#$1E32; Attr:laUpper; CaseCode:#$1E33),   // LATIN CAPITAL LETTER K WITH DOT BELOW
    (Unicode:#$1E33; Attr:laLower; CaseCode:#$1E32),   // LATIN SMALL LETTER K WITH DOT BELOW
    (Unicode:#$1E34; Attr:laUpper; CaseCode:#$1E35),   // LATIN CAPITAL LETTER K WITH LINE BELOW
    (Unicode:#$1E35; Attr:laLower; CaseCode:#$1E34),   // LATIN SMALL LETTER K WITH LINE BELOW
    (Unicode:#$1E36; Attr:laUpper; CaseCode:#$1E37),   // LATIN CAPITAL LETTER L WITH DOT BELOW
    (Unicode:#$1E37; Attr:laLower; CaseCode:#$1E36),   // LATIN SMALL LETTER L WITH DOT BELOW
    (Unicode:#$1E38; Attr:laUpper; CaseCode:#$1E39),   // LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E39; Attr:laLower; CaseCode:#$1E38),   // LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E3A; Attr:laUpper; CaseCode:#$1E3B),   // LATIN CAPITAL LETTER L WITH LINE BELOW
    (Unicode:#$1E3B; Attr:laLower; CaseCode:#$1E3A),   // LATIN SMALL LETTER L WITH LINE BELOW
    (Unicode:#$1E3C; Attr:laUpper; CaseCode:#$1E3D),   // LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3D; Attr:laLower; CaseCode:#$1E3C),   // LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3E; Attr:laUpper; CaseCode:#$1E3F),   // LATIN CAPITAL LETTER M WITH ACUTE
    (Unicode:#$1E3F; Attr:laLower; CaseCode:#$1E3E),   // LATIN SMALL LETTER M WITH ACUTE
    (Unicode:#$1E40; Attr:laUpper; CaseCode:#$1E41),   // LATIN CAPITAL LETTER M WITH DOT ABOVE
    (Unicode:#$1E41; Attr:laLower; CaseCode:#$1E40),   // LATIN SMALL LETTER M WITH DOT ABOVE
    (Unicode:#$1E42; Attr:laUpper; CaseCode:#$1E43),   // LATIN CAPITAL LETTER M WITH DOT BELOW
    (Unicode:#$1E43; Attr:laLower; CaseCode:#$1E42),   // LATIN SMALL LETTER M WITH DOT BELOW
    (Unicode:#$1E44; Attr:laUpper; CaseCode:#$1E45),   // LATIN CAPITAL LETTER N WITH DOT ABOVE
    (Unicode:#$1E45; Attr:laLower; CaseCode:#$1E44),   // LATIN SMALL LETTER N WITH DOT ABOVE
    (Unicode:#$1E46; Attr:laUpper; CaseCode:#$1E47),   // LATIN CAPITAL LETTER N WITH DOT BELOW
    (Unicode:#$1E47; Attr:laLower; CaseCode:#$1E46),   // LATIN SMALL LETTER N WITH DOT BELOW
    (Unicode:#$1E48; Attr:laUpper; CaseCode:#$1E49),   // LATIN CAPITAL LETTER N WITH LINE BELOW
    (Unicode:#$1E49; Attr:laLower; CaseCode:#$1E48),   // LATIN SMALL LETTER N WITH LINE BELOW
    (Unicode:#$1E4A; Attr:laUpper; CaseCode:#$1E4B),   // LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4B; Attr:laLower; CaseCode:#$1E4A),   // LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4C; Attr:laUpper; CaseCode:#$1E4D),   // LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4D; Attr:laLower; CaseCode:#$1E4C),   // LATIN SMALL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4E; Attr:laUpper; CaseCode:#$1E4F),   // LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E4F; Attr:laLower; CaseCode:#$1E4E),   // LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E50; Attr:laUpper; CaseCode:#$1E51),   // LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E51; Attr:laLower; CaseCode:#$1E50),   // LATIN SMALL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E52; Attr:laUpper; CaseCode:#$1E53),   // LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E53; Attr:laLower; CaseCode:#$1E52),   // LATIN SMALL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E54; Attr:laUpper; CaseCode:#$1E55),   // LATIN CAPITAL LETTER P WITH ACUTE
    (Unicode:#$1E55; Attr:laLower; CaseCode:#$1E54),   // LATIN SMALL LETTER P WITH ACUTE
    (Unicode:#$1E56; Attr:laUpper; CaseCode:#$1E57),   // LATIN CAPITAL LETTER P WITH DOT ABOVE
    (Unicode:#$1E57; Attr:laLower; CaseCode:#$1E56),   // LATIN SMALL LETTER P WITH DOT ABOVE
    (Unicode:#$1E58; Attr:laUpper; CaseCode:#$1E59),   // LATIN CAPITAL LETTER R WITH DOT ABOVE
    (Unicode:#$1E59; Attr:laLower; CaseCode:#$1E58),   // LATIN SMALL LETTER R WITH DOT ABOVE
    (Unicode:#$1E5A; Attr:laUpper; CaseCode:#$1E5B),   // LATIN CAPITAL LETTER R WITH DOT BELOW
    (Unicode:#$1E5B; Attr:laLower; CaseCode:#$1E5A),   // LATIN SMALL LETTER R WITH DOT BELOW
    (Unicode:#$1E5C; Attr:laUpper; CaseCode:#$1E5D),   // LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5D; Attr:laLower; CaseCode:#$1E5C),   // LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5E; Attr:laUpper; CaseCode:#$1E5F),   // LATIN CAPITAL LETTER R WITH LINE BELOW
    (Unicode:#$1E5F; Attr:laLower; CaseCode:#$1E5E),   // LATIN SMALL LETTER R WITH LINE BELOW
    (Unicode:#$1E60; Attr:laUpper; CaseCode:#$1E61),   // LATIN CAPITAL LETTER S WITH DOT ABOVE
    (Unicode:#$1E61; Attr:laLower; CaseCode:#$1E60),   // LATIN SMALL LETTER S WITH DOT ABOVE
    (Unicode:#$1E62; Attr:laUpper; CaseCode:#$1E63),   // LATIN CAPITAL LETTER S WITH DOT BELOW
    (Unicode:#$1E63; Attr:laLower; CaseCode:#$1E62),   // LATIN SMALL LETTER S WITH DOT BELOW
    (Unicode:#$1E64; Attr:laUpper; CaseCode:#$1E65),   // LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E65; Attr:laLower; CaseCode:#$1E64),   // LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E66; Attr:laUpper; CaseCode:#$1E67),   // LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E67; Attr:laLower; CaseCode:#$1E66),   // LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E68; Attr:laUpper; CaseCode:#$1E69),   // LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E69; Attr:laLower; CaseCode:#$1E68),   // LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E6A; Attr:laUpper; CaseCode:#$1E6B),   // LATIN CAPITAL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6B; Attr:laLower; CaseCode:#$1E6A),   // LATIN SMALL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6C; Attr:laUpper; CaseCode:#$1E6D),   // LATIN CAPITAL LETTER T WITH DOT BELOW
    (Unicode:#$1E6D; Attr:laLower; CaseCode:#$1E6C),   // LATIN SMALL LETTER T WITH DOT BELOW
    (Unicode:#$1E6E; Attr:laUpper; CaseCode:#$1E6F),   // LATIN CAPITAL LETTER T WITH LINE BELOW
    (Unicode:#$1E6F; Attr:laLower; CaseCode:#$1E6E),   // LATIN SMALL LETTER T WITH LINE BELOW
    (Unicode:#$1E70; Attr:laUpper; CaseCode:#$1E71),   // LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E71; Attr:laLower; CaseCode:#$1E70),   // LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E72; Attr:laUpper; CaseCode:#$1E73),   // LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E73; Attr:laLower; CaseCode:#$1E72),   // LATIN SMALL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E74; Attr:laUpper; CaseCode:#$1E75),   // LATIN CAPITAL LETTER U WITH TILDE BELOW
    (Unicode:#$1E75; Attr:laLower; CaseCode:#$1E74),   // LATIN SMALL LETTER U WITH TILDE BELOW
    (Unicode:#$1E76; Attr:laUpper; CaseCode:#$1E77),   // LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E77; Attr:laLower; CaseCode:#$1E76),   // LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E78; Attr:laUpper; CaseCode:#$1E79),   // LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E79; Attr:laLower; CaseCode:#$1E78),   // LATIN SMALL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E7A; Attr:laUpper; CaseCode:#$1E7B),   // LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7B; Attr:laLower; CaseCode:#$1E7A),   // LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7C; Attr:laUpper; CaseCode:#$1E7D),   // LATIN CAPITAL LETTER V WITH TILDE
    (Unicode:#$1E7D; Attr:laLower; CaseCode:#$1E7C),   // LATIN SMALL LETTER V WITH TILDE
    (Unicode:#$1E7E; Attr:laUpper; CaseCode:#$1E7F),   // LATIN CAPITAL LETTER V WITH DOT BELOW
    (Unicode:#$1E7F; Attr:laLower; CaseCode:#$1E7E),   // LATIN SMALL LETTER V WITH DOT BELOW
    (Unicode:#$1E80; Attr:laUpper; CaseCode:#$1E81),   // LATIN CAPITAL LETTER W WITH GRAVE
    (Unicode:#$1E81; Attr:laLower; CaseCode:#$1E80),   // LATIN SMALL LETTER W WITH GRAVE
    (Unicode:#$1E82; Attr:laUpper; CaseCode:#$1E83),   // LATIN CAPITAL LETTER W WITH ACUTE
    (Unicode:#$1E83; Attr:laLower; CaseCode:#$1E82),   // LATIN SMALL LETTER W WITH ACUTE
    (Unicode:#$1E84; Attr:laUpper; CaseCode:#$1E85),   // LATIN CAPITAL LETTER W WITH DIAERESIS
    (Unicode:#$1E85; Attr:laLower; CaseCode:#$1E84),   // LATIN SMALL LETTER W WITH DIAERESIS
    (Unicode:#$1E86; Attr:laUpper; CaseCode:#$1E87),   // LATIN CAPITAL LETTER W WITH DOT ABOVE
    (Unicode:#$1E87; Attr:laLower; CaseCode:#$1E86),   // LATIN SMALL LETTER W WITH DOT ABOVE
    (Unicode:#$1E88; Attr:laUpper; CaseCode:#$1E89),   // LATIN CAPITAL LETTER W WITH DOT BELOW
    (Unicode:#$1E89; Attr:laLower; CaseCode:#$1E88),   // LATIN SMALL LETTER W WITH DOT BELOW
    (Unicode:#$1E8A; Attr:laUpper; CaseCode:#$1E8B),   // LATIN CAPITAL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8B; Attr:laLower; CaseCode:#$1E8A),   // LATIN SMALL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8C; Attr:laUpper; CaseCode:#$1E8D),   // LATIN CAPITAL LETTER X WITH DIAERESIS
    (Unicode:#$1E8D; Attr:laLower; CaseCode:#$1E8C),   // LATIN SMALL LETTER X WITH DIAERESIS
    (Unicode:#$1E8E; Attr:laUpper; CaseCode:#$1E8F),   // LATIN CAPITAL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E8F; Attr:laLower; CaseCode:#$1E8E),   // LATIN SMALL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E90; Attr:laUpper; CaseCode:#$1E91),   // LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E91; Attr:laLower; CaseCode:#$1E90),   // LATIN SMALL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E92; Attr:laUpper; CaseCode:#$1E93),   // LATIN CAPITAL LETTER Z WITH DOT BELOW
    (Unicode:#$1E93; Attr:laLower; CaseCode:#$1E92),   // LATIN SMALL LETTER Z WITH DOT BELOW
    (Unicode:#$1E94; Attr:laUpper; CaseCode:#$1E95),   // LATIN CAPITAL LETTER Z WITH LINE BELOW
    (Unicode:#$1E95; Attr:laLower; CaseCode:#$1E94),   // LATIN SMALL LETTER Z WITH LINE BELOW
    (Unicode:#$1E96; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER H WITH LINE BELOW
    (Unicode:#$1E97; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER T WITH DIAERESIS
    (Unicode:#$1E98; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER W WITH RING ABOVE
    (Unicode:#$1E99; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER Y WITH RING ABOVE
    (Unicode:#$1E9A; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LETTER A WITH RIGHT HALF RING
    (Unicode:#$1E9B; Attr:laLower; CaseCode:#$1E60),   // LATIN SMALL LETTER LONG S WITH DOT ABOVE
    (Unicode:#$1EA0; Attr:laUpper; CaseCode:#$1EA1),   // LATIN CAPITAL LETTER A WITH DOT BELOW
    (Unicode:#$1EA1; Attr:laLower; CaseCode:#$1EA0),   // LATIN SMALL LETTER A WITH DOT BELOW
    (Unicode:#$1EA2; Attr:laUpper; CaseCode:#$1EA3),   // LATIN CAPITAL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA3; Attr:laLower; CaseCode:#$1EA2),   // LATIN SMALL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA4; Attr:laUpper; CaseCode:#$1EA5),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA5; Attr:laLower; CaseCode:#$1EA4),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA6; Attr:laUpper; CaseCode:#$1EA7),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA7; Attr:laLower; CaseCode:#$1EA6),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA8; Attr:laUpper; CaseCode:#$1EA9),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EA9; Attr:laLower; CaseCode:#$1EA8),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EAA; Attr:laUpper; CaseCode:#$1EAB),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAB; Attr:laLower; CaseCode:#$1EAA),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAC; Attr:laUpper; CaseCode:#$1EAD),   // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAD; Attr:laLower; CaseCode:#$1EAC),   // LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAE; Attr:laUpper; CaseCode:#$1EAF),   // LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EAF; Attr:laLower; CaseCode:#$1EAE),   // LATIN SMALL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EB0; Attr:laUpper; CaseCode:#$1EB1),   // LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB1; Attr:laLower; CaseCode:#$1EB0),   // LATIN SMALL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB2; Attr:laUpper; CaseCode:#$1EB3),   // LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB3; Attr:laLower; CaseCode:#$1EB2),   // LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB4; Attr:laUpper; CaseCode:#$1EB5),   // LATIN CAPITAL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB5; Attr:laLower; CaseCode:#$1EB4),   // LATIN SMALL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB6; Attr:laUpper; CaseCode:#$1EB7),   // LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB7; Attr:laLower; CaseCode:#$1EB6),   // LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB8; Attr:laUpper; CaseCode:#$1EB9),   // LATIN CAPITAL LETTER E WITH DOT BELOW
    (Unicode:#$1EB9; Attr:laLower; CaseCode:#$1EB8),   // LATIN SMALL LETTER E WITH DOT BELOW
    (Unicode:#$1EBA; Attr:laUpper; CaseCode:#$1EBB),   // LATIN CAPITAL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBB; Attr:laLower; CaseCode:#$1EBA),   // LATIN SMALL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBC; Attr:laUpper; CaseCode:#$1EBD),   // LATIN CAPITAL LETTER E WITH TILDE
    (Unicode:#$1EBD; Attr:laLower; CaseCode:#$1EBC),   // LATIN SMALL LETTER E WITH TILDE
    (Unicode:#$1EBE; Attr:laUpper; CaseCode:#$1EBF),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EBF; Attr:laLower; CaseCode:#$1EBE),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EC0; Attr:laUpper; CaseCode:#$1EC1),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC1; Attr:laLower; CaseCode:#$1EC0),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC2; Attr:laUpper; CaseCode:#$1EC3),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC3; Attr:laLower; CaseCode:#$1EC2),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC4; Attr:laUpper; CaseCode:#$1EC5),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC5; Attr:laLower; CaseCode:#$1EC4),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC6; Attr:laUpper; CaseCode:#$1EC7),   // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC7; Attr:laLower; CaseCode:#$1EC6),   // LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC8; Attr:laUpper; CaseCode:#$1EC9),   // LATIN CAPITAL LETTER I WITH HOOK ABOVE
    (Unicode:#$1EC9; Attr:laLower; CaseCode:#$1EC8),   // LATIN SMALL LETTER I WITH HOOK ABOVE
    (Unicode:#$1ECA; Attr:laUpper; CaseCode:#$1ECB),   // LATIN CAPITAL LETTER I WITH DOT BELOW
    (Unicode:#$1ECB; Attr:laLower; CaseCode:#$1ECA),   // LATIN SMALL LETTER I WITH DOT BELOW
    (Unicode:#$1ECC; Attr:laUpper; CaseCode:#$1ECD),   // LATIN CAPITAL LETTER O WITH DOT BELOW
    (Unicode:#$1ECD; Attr:laLower; CaseCode:#$1ECC),   // LATIN SMALL LETTER O WITH DOT BELOW
    (Unicode:#$1ECE; Attr:laUpper; CaseCode:#$1ECF),   // LATIN CAPITAL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ECF; Attr:laLower; CaseCode:#$1ECE),   // LATIN SMALL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ED0; Attr:laUpper; CaseCode:#$1ED1),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED1; Attr:laLower; CaseCode:#$1ED0),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED2; Attr:laUpper; CaseCode:#$1ED3),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED3; Attr:laLower; CaseCode:#$1ED2),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED4; Attr:laUpper; CaseCode:#$1ED5),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED5; Attr:laLower; CaseCode:#$1ED4),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED6; Attr:laUpper; CaseCode:#$1ED7),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED7; Attr:laLower; CaseCode:#$1ED6),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED8; Attr:laUpper; CaseCode:#$1ED9),   // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1ED9; Attr:laLower; CaseCode:#$1ED8),   // LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EDA; Attr:laUpper; CaseCode:#$1EDB),   // LATIN CAPITAL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDB; Attr:laLower; CaseCode:#$1EDA),   // LATIN SMALL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDC; Attr:laUpper; CaseCode:#$1EDD),   // LATIN CAPITAL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDD; Attr:laLower; CaseCode:#$1EDC),   // LATIN SMALL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDE; Attr:laUpper; CaseCode:#$1EDF),   // LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EDF; Attr:laLower; CaseCode:#$1EDE),   // LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EE0; Attr:laUpper; CaseCode:#$1EE1),   // LATIN CAPITAL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE1; Attr:laLower; CaseCode:#$1EE0),   // LATIN SMALL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE2; Attr:laUpper; CaseCode:#$1EE3),   // LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE3; Attr:laLower; CaseCode:#$1EE2),   // LATIN SMALL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE4; Attr:laUpper; CaseCode:#$1EE5),   // LATIN CAPITAL LETTER U WITH DOT BELOW
    (Unicode:#$1EE5; Attr:laLower; CaseCode:#$1EE4),   // LATIN SMALL LETTER U WITH DOT BELOW
    (Unicode:#$1EE6; Attr:laUpper; CaseCode:#$1EE7),   // LATIN CAPITAL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE7; Attr:laLower; CaseCode:#$1EE6),   // LATIN SMALL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE8; Attr:laUpper; CaseCode:#$1EE9),   // LATIN CAPITAL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EE9; Attr:laLower; CaseCode:#$1EE8),   // LATIN SMALL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EEA; Attr:laUpper; CaseCode:#$1EEB),   // LATIN CAPITAL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEB; Attr:laLower; CaseCode:#$1EEA),   // LATIN SMALL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEC; Attr:laUpper; CaseCode:#$1EED),   // LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EED; Attr:laLower; CaseCode:#$1EEC),   // LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EEE; Attr:laUpper; CaseCode:#$1EEF),   // LATIN CAPITAL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EEF; Attr:laLower; CaseCode:#$1EEE),   // LATIN SMALL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EF0; Attr:laUpper; CaseCode:#$1EF1),   // LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF1; Attr:laLower; CaseCode:#$1EF0),   // LATIN SMALL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF2; Attr:laUpper; CaseCode:#$1EF3),   // LATIN CAPITAL LETTER Y WITH GRAVE
    (Unicode:#$1EF3; Attr:laLower; CaseCode:#$1EF2),   // LATIN SMALL LETTER Y WITH GRAVE
    (Unicode:#$1EF4; Attr:laUpper; CaseCode:#$1EF5),   // LATIN CAPITAL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF5; Attr:laLower; CaseCode:#$1EF4),   // LATIN SMALL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF6; Attr:laUpper; CaseCode:#$1EF7),   // LATIN CAPITAL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF7; Attr:laLower; CaseCode:#$1EF6),   // LATIN SMALL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF8; Attr:laUpper; CaseCode:#$1EF9),   // LATIN CAPITAL LETTER Y WITH TILDE
    (Unicode:#$1EF9; Attr:laLower; CaseCode:#$1EF8),   // LATIN SMALL LETTER Y WITH TILDE
    (Unicode:#$1F00; Attr:laLower; CaseCode:#$1F08),   // GREEK SMALL LETTER ALPHA WITH PSILI
    (Unicode:#$1F01; Attr:laLower; CaseCode:#$1F09),   // GREEK SMALL LETTER ALPHA WITH DASIA
    (Unicode:#$1F02; Attr:laLower; CaseCode:#$1F0A),   // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F03; Attr:laLower; CaseCode:#$1F0B),   // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F04; Attr:laLower; CaseCode:#$1F0C),   // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F05; Attr:laLower; CaseCode:#$1F0D),   // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F06; Attr:laLower; CaseCode:#$1F0E),   // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F07; Attr:laLower; CaseCode:#$1F0F),   // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F08; Attr:laUpper; CaseCode:#$1F00),   // GREEK CAPITAL LETTER ALPHA WITH PSILI
    (Unicode:#$1F09; Attr:laUpper; CaseCode:#$1F01),   // GREEK CAPITAL LETTER ALPHA WITH DASIA
    (Unicode:#$1F0A; Attr:laUpper; CaseCode:#$1F02),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F0B; Attr:laUpper; CaseCode:#$1F03),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F0C; Attr:laUpper; CaseCode:#$1F04),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F0D; Attr:laUpper; CaseCode:#$1F05),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F0E; Attr:laUpper; CaseCode:#$1F06),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F0F; Attr:laUpper; CaseCode:#$1F07),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F10; Attr:laLower; CaseCode:#$1F18),   // GREEK SMALL LETTER EPSILON WITH PSILI
    (Unicode:#$1F11; Attr:laLower; CaseCode:#$1F19),   // GREEK SMALL LETTER EPSILON WITH DASIA
    (Unicode:#$1F12; Attr:laLower; CaseCode:#$1F1A),   // GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F13; Attr:laLower; CaseCode:#$1F1B),   // GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F14; Attr:laLower; CaseCode:#$1F1C),   // GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F15; Attr:laLower; CaseCode:#$1F1D),   // GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F18; Attr:laUpper; CaseCode:#$1F10),   // GREEK CAPITAL LETTER EPSILON WITH PSILI
    (Unicode:#$1F19; Attr:laUpper; CaseCode:#$1F11),   // GREEK CAPITAL LETTER EPSILON WITH DASIA
    (Unicode:#$1F1A; Attr:laUpper; CaseCode:#$1F12),   // GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F1B; Attr:laUpper; CaseCode:#$1F13),   // GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F1C; Attr:laUpper; CaseCode:#$1F14),   // GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F1D; Attr:laUpper; CaseCode:#$1F15),   // GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F20; Attr:laLower; CaseCode:#$1F28),   // GREEK SMALL LETTER ETA WITH PSILI
    (Unicode:#$1F21; Attr:laLower; CaseCode:#$1F29),   // GREEK SMALL LETTER ETA WITH DASIA
    (Unicode:#$1F22; Attr:laLower; CaseCode:#$1F2A),   // GREEK SMALL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F23; Attr:laLower; CaseCode:#$1F2B),   // GREEK SMALL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F24; Attr:laLower; CaseCode:#$1F2C),   // GREEK SMALL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F25; Attr:laLower; CaseCode:#$1F2D),   // GREEK SMALL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F26; Attr:laLower; CaseCode:#$1F2E),   // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F27; Attr:laLower; CaseCode:#$1F2F),   // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F28; Attr:laUpper; CaseCode:#$1F20),   // GREEK CAPITAL LETTER ETA WITH PSILI
    (Unicode:#$1F29; Attr:laUpper; CaseCode:#$1F21),   // GREEK CAPITAL LETTER ETA WITH DASIA
    (Unicode:#$1F2A; Attr:laUpper; CaseCode:#$1F22),   // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F2B; Attr:laUpper; CaseCode:#$1F23),   // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F2C; Attr:laUpper; CaseCode:#$1F24),   // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F2D; Attr:laUpper; CaseCode:#$1F25),   // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F2E; Attr:laUpper; CaseCode:#$1F26),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F2F; Attr:laUpper; CaseCode:#$1F27),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F30; Attr:laLower; CaseCode:#$1F38),   // GREEK SMALL LETTER IOTA WITH PSILI
    (Unicode:#$1F31; Attr:laLower; CaseCode:#$1F39),   // GREEK SMALL LETTER IOTA WITH DASIA
    (Unicode:#$1F32; Attr:laLower; CaseCode:#$1F3A),   // GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F33; Attr:laLower; CaseCode:#$1F3B),   // GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F34; Attr:laLower; CaseCode:#$1F3C),   // GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F35; Attr:laLower; CaseCode:#$1F3D),   // GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F36; Attr:laLower; CaseCode:#$1F3E),   // GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F37; Attr:laLower; CaseCode:#$1F3F),   // GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F38; Attr:laUpper; CaseCode:#$1F30),   // GREEK CAPITAL LETTER IOTA WITH PSILI
    (Unicode:#$1F39; Attr:laUpper; CaseCode:#$1F31),   // GREEK CAPITAL LETTER IOTA WITH DASIA
    (Unicode:#$1F3A; Attr:laUpper; CaseCode:#$1F32),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F3B; Attr:laUpper; CaseCode:#$1F33),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F3C; Attr:laUpper; CaseCode:#$1F34),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F3D; Attr:laUpper; CaseCode:#$1F35),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F3E; Attr:laUpper; CaseCode:#$1F36),   // GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F3F; Attr:laUpper; CaseCode:#$1F37),   // GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F40; Attr:laLower; CaseCode:#$1F48),   // GREEK SMALL LETTER OMICRON WITH PSILI
    (Unicode:#$1F41; Attr:laLower; CaseCode:#$1F49),   // GREEK SMALL LETTER OMICRON WITH DASIA
    (Unicode:#$1F42; Attr:laLower; CaseCode:#$1F4A),   // GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F43; Attr:laLower; CaseCode:#$1F4B),   // GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F44; Attr:laLower; CaseCode:#$1F4C),   // GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F45; Attr:laLower; CaseCode:#$1F4D),   // GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F48; Attr:laUpper; CaseCode:#$1F40),   // GREEK CAPITAL LETTER OMICRON WITH PSILI
    (Unicode:#$1F49; Attr:laUpper; CaseCode:#$1F41),   // GREEK CAPITAL LETTER OMICRON WITH DASIA
    (Unicode:#$1F4A; Attr:laUpper; CaseCode:#$1F42),   // GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F4B; Attr:laUpper; CaseCode:#$1F43),   // GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F4C; Attr:laUpper; CaseCode:#$1F44),   // GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F4D; Attr:laUpper; CaseCode:#$1F45),   // GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F50; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI
    (Unicode:#$1F51; Attr:laLower; CaseCode:#$1F59),   // GREEK SMALL LETTER UPSILON WITH DASIA
    (Unicode:#$1F52; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    (Unicode:#$1F53; Attr:laLower; CaseCode:#$1F5B),   // GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F54; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
    (Unicode:#$1F55; Attr:laLower; CaseCode:#$1F5D),   // GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F56; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
    (Unicode:#$1F57; Attr:laLower; CaseCode:#$1F5F),   // GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F59; Attr:laUpper; CaseCode:#$1F51),   // GREEK CAPITAL LETTER UPSILON WITH DASIA
    (Unicode:#$1F5B; Attr:laUpper; CaseCode:#$1F53),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F5D; Attr:laUpper; CaseCode:#$1F55),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F5F; Attr:laUpper; CaseCode:#$1F57),   // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F60; Attr:laLower; CaseCode:#$1F68),   // GREEK SMALL LETTER OMEGA WITH PSILI
    (Unicode:#$1F61; Attr:laLower; CaseCode:#$1F69),   // GREEK SMALL LETTER OMEGA WITH DASIA
    (Unicode:#$1F62; Attr:laLower; CaseCode:#$1F6A),   // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F63; Attr:laLower; CaseCode:#$1F6B),   // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F64; Attr:laLower; CaseCode:#$1F6C),   // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F65; Attr:laLower; CaseCode:#$1F6D),   // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F66; Attr:laLower; CaseCode:#$1F6E),   // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F67; Attr:laLower; CaseCode:#$1F6F),   // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F68; Attr:laUpper; CaseCode:#$1F60),   // GREEK CAPITAL LETTER OMEGA WITH PSILI
    (Unicode:#$1F69; Attr:laUpper; CaseCode:#$1F61),   // GREEK CAPITAL LETTER OMEGA WITH DASIA
    (Unicode:#$1F6A; Attr:laUpper; CaseCode:#$1F62),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F6B; Attr:laUpper; CaseCode:#$1F63),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F6C; Attr:laUpper; CaseCode:#$1F64),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F6D; Attr:laUpper; CaseCode:#$1F65),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F6E; Attr:laUpper; CaseCode:#$1F66),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F6F; Attr:laUpper; CaseCode:#$1F67),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F70; Attr:laLower; CaseCode:#$1FBA),   // GREEK SMALL LETTER ALPHA WITH VARIA
    (Unicode:#$1F71; Attr:laLower; CaseCode:#$1FBB),   // GREEK SMALL LETTER ALPHA WITH OXIA
    (Unicode:#$1F72; Attr:laLower; CaseCode:#$1FC8),   // GREEK SMALL LETTER EPSILON WITH VARIA
    (Unicode:#$1F73; Attr:laLower; CaseCode:#$1FC9),   // GREEK SMALL LETTER EPSILON WITH OXIA
    (Unicode:#$1F74; Attr:laLower; CaseCode:#$1FCA),   // GREEK SMALL LETTER ETA WITH VARIA
    (Unicode:#$1F75; Attr:laLower; CaseCode:#$1FCB),   // GREEK SMALL LETTER ETA WITH OXIA
    (Unicode:#$1F76; Attr:laLower; CaseCode:#$1FDA),   // GREEK SMALL LETTER IOTA WITH VARIA
    (Unicode:#$1F77; Attr:laLower; CaseCode:#$1FDB),   // GREEK SMALL LETTER IOTA WITH OXIA
    (Unicode:#$1F78; Attr:laLower; CaseCode:#$1FF8),   // GREEK SMALL LETTER OMICRON WITH VARIA
    (Unicode:#$1F79; Attr:laLower; CaseCode:#$1FF9),   // GREEK SMALL LETTER OMICRON WITH OXIA
    (Unicode:#$1F7A; Attr:laLower; CaseCode:#$1FEA),   // GREEK SMALL LETTER UPSILON WITH VARIA
    (Unicode:#$1F7B; Attr:laLower; CaseCode:#$1FEB),   // GREEK SMALL LETTER UPSILON WITH OXIA
    (Unicode:#$1F7C; Attr:laLower; CaseCode:#$1FFA),   // GREEK SMALL LETTER OMEGA WITH VARIA
    (Unicode:#$1F7D; Attr:laLower; CaseCode:#$1FFB),   // GREEK SMALL LETTER OMEGA WITH OXIA
    (Unicode:#$1F80; Attr:laLower; CaseCode:#$1F88),   // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F81; Attr:laLower; CaseCode:#$1F89),   // GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F82; Attr:laLower; CaseCode:#$1F8A),   // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F83; Attr:laLower; CaseCode:#$1F8B),   // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F84; Attr:laLower; CaseCode:#$1F8C),   // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F85; Attr:laLower; CaseCode:#$1F8D),   // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F86; Attr:laLower; CaseCode:#$1F8E),   // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F87; Attr:laLower; CaseCode:#$1F8F),   // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F90; Attr:laLower; CaseCode:#$1F98),   // GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F91; Attr:laLower; CaseCode:#$1F99),   // GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F92; Attr:laLower; CaseCode:#$1F9A),   // GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F93; Attr:laLower; CaseCode:#$1F9B),   // GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F94; Attr:laLower; CaseCode:#$1F9C),   // GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F95; Attr:laLower; CaseCode:#$1F9D),   // GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F96; Attr:laLower; CaseCode:#$1F9E),   // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F97; Attr:laLower; CaseCode:#$1F9F),   // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA0; Attr:laLower; CaseCode:#$1FA8),   // GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1FA1; Attr:laLower; CaseCode:#$1FA9),   // GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1FA2; Attr:laLower; CaseCode:#$1FAA),   // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA3; Attr:laLower; CaseCode:#$1FAB),   // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA4; Attr:laLower; CaseCode:#$1FAC),   // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA5; Attr:laLower; CaseCode:#$1FAD),   // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA6; Attr:laLower; CaseCode:#$1FAE),   // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA7; Attr:laLower; CaseCode:#$1FAF),   // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB0; Attr:laLower; CaseCode:#$1FB8),   // GREEK SMALL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB1; Attr:laLower; CaseCode:#$1FB9),   // GREEK SMALL LETTER ALPHA WITH MACRON
    (Unicode:#$1FB2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FB3; Attr:laLower; CaseCode:#$1FBC),   // GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
    (Unicode:#$1FB4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FB6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH PERISPOMENI
    (Unicode:#$1FB7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB8; Attr:laUpper; CaseCode:#$1FB0),   // GREEK CAPITAL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB9; Attr:laUpper; CaseCode:#$1FB1),   // GREEK CAPITAL LETTER ALPHA WITH MACRON
    (Unicode:#$1FBA; Attr:laUpper; CaseCode:#$1F70),   // GREEK CAPITAL LETTER ALPHA WITH VARIA
    (Unicode:#$1FBB; Attr:laUpper; CaseCode:#$1F71),   // GREEK CAPITAL LETTER ALPHA WITH OXIA
    (Unicode:#$1FBE; Attr:laLower; CaseCode:#$0399),   // GREEK PROSGEGRAMMENI
    (Unicode:#$1FC2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FC3; Attr:laLower; CaseCode:#$1FCC),   // GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
    (Unicode:#$1FC4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FC6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH PERISPOMENI
    (Unicode:#$1FC7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FC8; Attr:laUpper; CaseCode:#$1F72),   // GREEK CAPITAL LETTER EPSILON WITH VARIA
    (Unicode:#$1FC9; Attr:laUpper; CaseCode:#$1F73),   // GREEK CAPITAL LETTER EPSILON WITH OXIA
    (Unicode:#$1FCA; Attr:laUpper; CaseCode:#$1F74),   // GREEK CAPITAL LETTER ETA WITH VARIA
    (Unicode:#$1FCB; Attr:laUpper; CaseCode:#$1F75),   // GREEK CAPITAL LETTER ETA WITH OXIA
    (Unicode:#$1FD0; Attr:laLower; CaseCode:#$1FD8),   // GREEK SMALL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD1; Attr:laLower; CaseCode:#$1FD9),   // GREEK SMALL LETTER IOTA WITH MACRON
    (Unicode:#$1FD2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
    (Unicode:#$1FD3; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
    (Unicode:#$1FD6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH PERISPOMENI
    (Unicode:#$1FD7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FD8; Attr:laUpper; CaseCode:#$1FD0),   // GREEK CAPITAL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD9; Attr:laUpper; CaseCode:#$1FD1),   // GREEK CAPITAL LETTER IOTA WITH MACRON
    (Unicode:#$1FDA; Attr:laUpper; CaseCode:#$1F76),   // GREEK CAPITAL LETTER IOTA WITH VARIA
    (Unicode:#$1FDB; Attr:laUpper; CaseCode:#$1F77),   // GREEK CAPITAL LETTER IOTA WITH OXIA
    (Unicode:#$1FE0; Attr:laLower; CaseCode:#$1FE8),   // GREEK SMALL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE1; Attr:laLower; CaseCode:#$1FE9),   // GREEK SMALL LETTER UPSILON WITH MACRON
    (Unicode:#$1FE2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
    (Unicode:#$1FE3; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
    (Unicode:#$1FE4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER RHO WITH PSILI
    (Unicode:#$1FE5; Attr:laLower; CaseCode:#$1FEC),   // GREEK SMALL LETTER RHO WITH DASIA
    (Unicode:#$1FE6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH PERISPOMENI
    (Unicode:#$1FE7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FE8; Attr:laUpper; CaseCode:#$1FE0),   // GREEK CAPITAL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE9; Attr:laUpper; CaseCode:#$1FE1),   // GREEK CAPITAL LETTER UPSILON WITH MACRON
    (Unicode:#$1FEA; Attr:laUpper; CaseCode:#$1F7A),   // GREEK CAPITAL LETTER UPSILON WITH VARIA
    (Unicode:#$1FEB; Attr:laUpper; CaseCode:#$1F7B),   // GREEK CAPITAL LETTER UPSILON WITH OXIA
    (Unicode:#$1FEC; Attr:laUpper; CaseCode:#$1FE5),   // GREEK CAPITAL LETTER RHO WITH DASIA
    (Unicode:#$1FF2; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FF3; Attr:laLower; CaseCode:#$1FFC),   // GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
    (Unicode:#$1FF4; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FF6; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH PERISPOMENI
    (Unicode:#$1FF7; Attr:laLower; CaseCode:#$FFFF),   // GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FF8; Attr:laUpper; CaseCode:#$1F78),   // GREEK CAPITAL LETTER OMICRON WITH VARIA
    (Unicode:#$1FF9; Attr:laUpper; CaseCode:#$1F79),   // GREEK CAPITAL LETTER OMICRON WITH OXIA
    (Unicode:#$1FFA; Attr:laUpper; CaseCode:#$1F7C),   // GREEK CAPITAL LETTER OMEGA WITH VARIA
    (Unicode:#$1FFB; Attr:laUpper; CaseCode:#$1F7D),   // GREEK CAPITAL LETTER OMEGA WITH OXIA
    (Unicode:#$207F; Attr:laLower; CaseCode:#$FFFF),   // SUPERSCRIPT LATIN SMALL LETTER N
    (Unicode:#$2102; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL C
    (Unicode:#$2107; Attr:laUpper; CaseCode:#$FFFF),   // EULER CONSTANT
    (Unicode:#$210A; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL G
    (Unicode:#$210B; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL H
    (Unicode:#$210C; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL H
    (Unicode:#$210D; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL H
    (Unicode:#$210E; Attr:laLower; CaseCode:#$FFFF),   // PLANCK CONSTANT
    (Unicode:#$210F; Attr:laLower; CaseCode:#$FFFF),   // PLANCK CONSTANT OVER TWO PI
    (Unicode:#$2110; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL I
    (Unicode:#$2111; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL I
    (Unicode:#$2112; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL L
    (Unicode:#$2113; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL L
    (Unicode:#$2115; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL N
    (Unicode:#$2119; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL P
    (Unicode:#$211A; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL Q
    (Unicode:#$211B; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL R
    (Unicode:#$211C; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL R
    (Unicode:#$211D; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL R
    (Unicode:#$2124; Attr:laUpper; CaseCode:#$FFFF),   // DOUBLE-STRUCK CAPITAL Z
    (Unicode:#$2126; Attr:laUpper; CaseCode:#$03C9),   // OHM SIGN
    (Unicode:#$2128; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL Z
    (Unicode:#$212A; Attr:laUpper; CaseCode:#$006B),   // KELVIN SIGN
    (Unicode:#$212B; Attr:laUpper; CaseCode:#$00E5),   // ANGSTROM SIGN
    (Unicode:#$212C; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL B
    (Unicode:#$212D; Attr:laUpper; CaseCode:#$FFFF),   // BLACK-LETTER CAPITAL C
    (Unicode:#$212F; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL E
    (Unicode:#$2130; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL E
    (Unicode:#$2131; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL F
    (Unicode:#$2133; Attr:laUpper; CaseCode:#$FFFF),   // SCRIPT CAPITAL M
    (Unicode:#$2134; Attr:laLower; CaseCode:#$FFFF),   // SCRIPT SMALL O
    (Unicode:#$2139; Attr:laLower; CaseCode:#$FFFF),   // INFORMATION SOURCE
    (Unicode:#$FB00; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FF
    (Unicode:#$FB01; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FI
    (Unicode:#$FB02; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FL
    (Unicode:#$FB03; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FFI
    (Unicode:#$FB04; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE FFL
    (Unicode:#$FB05; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE LONG S T
    (Unicode:#$FB06; Attr:laLower; CaseCode:#$FFFF),   // LATIN SMALL LIGATURE ST
    (Unicode:#$FB13; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN NOW
    (Unicode:#$FB14; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN ECH
    (Unicode:#$FB15; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN INI
    (Unicode:#$FB16; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE VEW NOW
    (Unicode:#$FB17; Attr:laLower; CaseCode:#$FFFF),   // ARMENIAN SMALL LIGATURE MEN XEH
    (Unicode:#$FF21; Attr:laUpper; CaseCode:#$FF41),   // FULLWIDTH LATIN CAPITAL LETTER A
    (Unicode:#$FF22; Attr:laUpper; CaseCode:#$FF42),   // FULLWIDTH LATIN CAPITAL LETTER B
    (Unicode:#$FF23; Attr:laUpper; CaseCode:#$FF43),   // FULLWIDTH LATIN CAPITAL LETTER C
    (Unicode:#$FF24; Attr:laUpper; CaseCode:#$FF44),   // FULLWIDTH LATIN CAPITAL LETTER D
    (Unicode:#$FF25; Attr:laUpper; CaseCode:#$FF45),   // FULLWIDTH LATIN CAPITAL LETTER E
    (Unicode:#$FF26; Attr:laUpper; CaseCode:#$FF46),   // FULLWIDTH LATIN CAPITAL LETTER F
    (Unicode:#$FF27; Attr:laUpper; CaseCode:#$FF47),   // FULLWIDTH LATIN CAPITAL LETTER G
    (Unicode:#$FF28; Attr:laUpper; CaseCode:#$FF48),   // FULLWIDTH LATIN CAPITAL LETTER H
    (Unicode:#$FF29; Attr:laUpper; CaseCode:#$FF49),   // FULLWIDTH LATIN CAPITAL LETTER I
    (Unicode:#$FF2A; Attr:laUpper; CaseCode:#$FF4A),   // FULLWIDTH LATIN CAPITAL LETTER J
    (Unicode:#$FF2B; Attr:laUpper; CaseCode:#$FF4B),   // FULLWIDTH LATIN CAPITAL LETTER K
    (Unicode:#$FF2C; Attr:laUpper; CaseCode:#$FF4C),   // FULLWIDTH LATIN CAPITAL LETTER L
    (Unicode:#$FF2D; Attr:laUpper; CaseCode:#$FF4D),   // FULLWIDTH LATIN CAPITAL LETTER M
    (Unicode:#$FF2E; Attr:laUpper; CaseCode:#$FF4E),   // FULLWIDTH LATIN CAPITAL LETTER N
    (Unicode:#$FF2F; Attr:laUpper; CaseCode:#$FF4F),   // FULLWIDTH LATIN CAPITAL LETTER O
    (Unicode:#$FF30; Attr:laUpper; CaseCode:#$FF50),   // FULLWIDTH LATIN CAPITAL LETTER P
    (Unicode:#$FF31; Attr:laUpper; CaseCode:#$FF51),   // FULLWIDTH LATIN CAPITAL LETTER Q
    (Unicode:#$FF32; Attr:laUpper; CaseCode:#$FF52),   // FULLWIDTH LATIN CAPITAL LETTER R
    (Unicode:#$FF33; Attr:laUpper; CaseCode:#$FF53),   // FULLWIDTH LATIN CAPITAL LETTER S
    (Unicode:#$FF34; Attr:laUpper; CaseCode:#$FF54),   // FULLWIDTH LATIN CAPITAL LETTER T
    (Unicode:#$FF35; Attr:laUpper; CaseCode:#$FF55),   // FULLWIDTH LATIN CAPITAL LETTER U
    (Unicode:#$FF36; Attr:laUpper; CaseCode:#$FF56),   // FULLWIDTH LATIN CAPITAL LETTER V
    (Unicode:#$FF37; Attr:laUpper; CaseCode:#$FF57),   // FULLWIDTH LATIN CAPITAL LETTER W
    (Unicode:#$FF38; Attr:laUpper; CaseCode:#$FF58),   // FULLWIDTH LATIN CAPITAL LETTER X
    (Unicode:#$FF39; Attr:laUpper; CaseCode:#$FF59),   // FULLWIDTH LATIN CAPITAL LETTER Y
    (Unicode:#$FF3A; Attr:laUpper; CaseCode:#$FF5A),   // FULLWIDTH LATIN CAPITAL LETTER Z
    (Unicode:#$FF41; Attr:laLower; CaseCode:#$FF21),   // FULLWIDTH LATIN SMALL LETTER A
    (Unicode:#$FF42; Attr:laLower; CaseCode:#$FF22),   // FULLWIDTH LATIN SMALL LETTER B
    (Unicode:#$FF43; Attr:laLower; CaseCode:#$FF23),   // FULLWIDTH LATIN SMALL LETTER C
    (Unicode:#$FF44; Attr:laLower; CaseCode:#$FF24),   // FULLWIDTH LATIN SMALL LETTER D
    (Unicode:#$FF45; Attr:laLower; CaseCode:#$FF25),   // FULLWIDTH LATIN SMALL LETTER E
    (Unicode:#$FF46; Attr:laLower; CaseCode:#$FF26),   // FULLWIDTH LATIN SMALL LETTER F
    (Unicode:#$FF47; Attr:laLower; CaseCode:#$FF27),   // FULLWIDTH LATIN SMALL LETTER G
    (Unicode:#$FF48; Attr:laLower; CaseCode:#$FF28),   // FULLWIDTH LATIN SMALL LETTER H
    (Unicode:#$FF49; Attr:laLower; CaseCode:#$FF29),   // FULLWIDTH LATIN SMALL LETTER I
    (Unicode:#$FF4A; Attr:laLower; CaseCode:#$FF2A),   // FULLWIDTH LATIN SMALL LETTER J
    (Unicode:#$FF4B; Attr:laLower; CaseCode:#$FF2B),   // FULLWIDTH LATIN SMALL LETTER K
    (Unicode:#$FF4C; Attr:laLower; CaseCode:#$FF2C),   // FULLWIDTH LATIN SMALL LETTER L
    (Unicode:#$FF4D; Attr:laLower; CaseCode:#$FF2D),   // FULLWIDTH LATIN SMALL LETTER M
    (Unicode:#$FF4E; Attr:laLower; CaseCode:#$FF2E),   // FULLWIDTH LATIN SMALL LETTER N
    (Unicode:#$FF4F; Attr:laLower; CaseCode:#$FF2F),   // FULLWIDTH LATIN SMALL LETTER O
    (Unicode:#$FF50; Attr:laLower; CaseCode:#$FF30),   // FULLWIDTH LATIN SMALL LETTER P
    (Unicode:#$FF51; Attr:laLower; CaseCode:#$FF31),   // FULLWIDTH LATIN SMALL LETTER Q
    (Unicode:#$FF52; Attr:laLower; CaseCode:#$FF32),   // FULLWIDTH LATIN SMALL LETTER R
    (Unicode:#$FF53; Attr:laLower; CaseCode:#$FF33),   // FULLWIDTH LATIN SMALL LETTER S
    (Unicode:#$FF54; Attr:laLower; CaseCode:#$FF34),   // FULLWIDTH LATIN SMALL LETTER T
    (Unicode:#$FF55; Attr:laLower; CaseCode:#$FF35),   // FULLWIDTH LATIN SMALL LETTER U
    (Unicode:#$FF56; Attr:laLower; CaseCode:#$FF36),   // FULLWIDTH LATIN SMALL LETTER V
    (Unicode:#$FF57; Attr:laLower; CaseCode:#$FF37),   // FULLWIDTH LATIN SMALL LETTER W
    (Unicode:#$FF58; Attr:laLower; CaseCode:#$FF38),   // FULLWIDTH LATIN SMALL LETTER X
    (Unicode:#$FF59; Attr:laLower; CaseCode:#$FF39),   // FULLWIDTH LATIN SMALL LETTER Y
    (Unicode:#$FF5A; Attr:laLower; CaseCode:#$FF3A)    // FULLWIDTH LATIN SMALL LETTER Z
    );

function LocateLetterInfo(const Ch: WideChar): Integer;
var L, H, I : Integer;
    D       : WideChar;
begin
  // Binary search [Avg number of comparisons = Log2(UnicodeLetterEntries) = 10]
  L := 0;
  H := UnicodeLetterEntries - 1;
  repeat
    I := (L + H) div 2;
    D := UnicodeLetterInfo[I].Unicode;
    if D = Ch then
      begin
        Result := I;
        exit;
      end else
    if D > Ch then
      H := I - 1
    else
      L := I + 1;
  until L > H;
  Result := -1;
end;

function LocateOtherLowerCase(const Ch: WideChar): WideChar;
begin
  case Ord(Ch) of
    $2170..$217F : Result := WideChar(Ord(Ch) - $2170 + $2160);    // # Nl  [16] SMALL ROMAN NUMERAL ONE..SMALL ROMAN NUMERAL ONE THOUSAND
    $24D0..$24E9 : Result := WideChar(Ord(Ch) - $24D0 + $24B6);    // # So  [26] CIRCLED LATIN SMALL LETTER A..CIRCLED LATIN SMALL LETTER Z
  else
    Result := #$0000;
  end;
end;

function LocateOtherUpperCase(const Ch: WideChar): WideChar;
begin
  case Ord(Ch) of
    $2160..$216F : Result := WideChar(Ord(Ch) - $2160 + $2170);    // # Nl  [16] ROMAN NUMERAL ONE..ROMAN NUMERAL ONE THOUSAND
    $24B6..$24CF : Result := WideChar(Ord(Ch) - $24B6 + $24D0);    // # So  [26] CIRCLED LATIN CAPITAL LETTER A..CIRCLED LATIN CAPITAL LETTER Z
  else
    Result := #$0000;
  end;
end;

function LocateFoldingTitleCase(const Ch: WideChar): WideString;
begin
  if Ord(Ch) < $00DF then
    Result := '' else
  if Ord(Ch) <= $0587 then
    case Ord(Ch) of
      $00DF : Result := #$0053#$0073;         // # LATIN SMALL LETTER SHARP S
      $0149 : Result := #$02BC#$004E;         // # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
      $01F0 : Result := #$004A#$030C;         // # LATIN SMALL LETTER J WITH CARON
      $0390 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $03B0 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $0587 : Result := #$0535#$0582;         // # ARMENIAN SMALL LIGATURE ECH YIWN
    else
      Result := '';
    end else
  if Ord(Ch) < $1E96 then
    Result := '' else
  if Ord(Ch) <= $1FF7 then
    case Ord(Ch) of
      $1E96 : Result := #$0048#$0331;         // # LATIN SMALL LETTER H WITH LINE BELOW
      $1E97 : Result := #$0054#$0308;         // # LATIN SMALL LETTER T WITH DIAERESIS
      $1E98 : Result := #$0057#$030A;         // # LATIN SMALL LETTER W WITH RING ABOVE
      $1E99 : Result := #$0059#$030A;         // # LATIN SMALL LETTER Y WITH RING ABOVE
      $1E9A : Result := #$0041#$02BE;         // # LATIN SMALL LETTER A WITH RIGHT HALF RING
      $1F50 : Result := #$03A5#$0313;         // # GREEK SMALL LETTER UPSILON WITH PSILI
      $1F52 : Result := #$03A5#$0313#$0300;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
      $1F54 : Result := #$03A5#$0313#$0301;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
      $1F56 : Result := #$03A5#$0313#$0342;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
      $1FB2 : Result := #$1FBA#$0345;         // # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
      $1FB4 : Result := #$0386#$0345;         // # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
      $1FB6 : Result := #$0391#$0342;         // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
      $1FB7 : Result := #$0391#$0342#$0345;   // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FC2 : Result := #$1FCA#$0345;         // # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
      $1FC4 : Result := #$0389#$0345;         // # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
      $1FC6 : Result := #$0397#$0342;         // # GREEK SMALL LETTER ETA WITH PERISPOMENI
      $1FC7 : Result := #$0397#$0342#$0345;   // # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FD2 : Result := #$0399#$0308#$0300;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
      $1FD3 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
      $1FD6 : Result := #$0399#$0342;         // # GREEK SMALL LETTER IOTA WITH PERISPOMENI
      $1FD7 : Result := #$0399#$0308#$0342;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
      $1FE2 : Result := #$03A5#$0308#$0300;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
      $1FE3 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
      $1FE4 : Result := #$03A1#$0313;         // # GREEK SMALL LETTER RHO WITH PSILI
      $1FE6 : Result := #$03A5#$0342;         // # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
      $1FE7 : Result := #$03A5#$0308#$0342;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
      $1FF2 : Result := #$1FFA#$0345;         // # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
      $1FF4 : Result := #$038F#$0345;         // # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
      $1FF6 : Result := #$03A9#$0342;         // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
      $1FF7 : Result := #$03A9#$0342#$0345;   // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    else
      Result := '';
    end else
  if Ord(Ch) < $FB00 then
    Result := '' else
  if Ord(Ch) <= $FB17 then
    case Ord(Ch) of
      $FB00 : Result := #$0046#$0066;         // # LATIN SMALL LIGATURE FF
      $FB01 : Result := #$0046#$0069;         // # LATIN SMALL LIGATURE FI
      $FB02 : Result := #$0046#$006C;         // # LATIN SMALL LIGATURE FL
      $FB03 : Result := #$0046#$0066#$0069;   // # LATIN SMALL LIGATURE FFI
      $FB04 : Result := #$0046#$0066#$006C;   // # LATIN SMALL LIGATURE FFL
      $FB05 : Result := #$0053#$0074;         // # LATIN SMALL LIGATURE LONG S T
      $FB06 : Result := #$0053#$0074;         // # LATIN SMALL LIGATURE ST
      $FB13 : Result := #$0544#$0576;         // # ARMENIAN SMALL LIGATURE MEN NOW
      $FB14 : Result := #$0544#$0565;         // # ARMENIAN SMALL LIGATURE MEN ECH
      $FB15 : Result := #$0544#$056B;         // # ARMENIAN SMALL LIGATURE MEN INI
      $FB16 : Result := #$054E#$0576;         // # ARMENIAN SMALL LIGATURE VEW NOW
      $FB17 : Result := #$0544#$056D;         // # ARMENIAN SMALL LIGATURE MEN XEH
    else
      Result := '';
    end
  else
    Result := '';
end;

function LocateFoldingUpperCase(const Ch: WideChar): WideString;
begin
  if Ord(Ch) < $00DF then
    Result := '' else
  if Ord(Ch) <= $0587 then
    case Ord(Ch) of
      $00DF : Result := #$0053#$0053;         // # LATIN SMALL LETTER SHARP S
      $0149 : Result := #$02BC#$004E;         // # LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
      $01F0 : Result := #$004A#$030C;         // # LATIN SMALL LETTER J WITH CARON
      $0390 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
      $03B0 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
      $0587 : Result := #$0535#$0552;         // # ARMENIAN SMALL LIGATURE ECH YIWN
    else
      Result := '';
    end else
  if Ord(Ch) < $1E96 then
    Result := '' else
  if Ord(Ch) <= $1FFC then
    case Ord(Ch) of
      $1E96 : Result := #$0048#$0331;         // # LATIN SMALL LETTER H WITH LINE BELOW
      $1E97 : Result := #$0054#$0308;         // # LATIN SMALL LETTER T WITH DIAERESIS
      $1E98 : Result := #$0057#$030A;         // # LATIN SMALL LETTER W WITH RING ABOVE
      $1E99 : Result := #$0059#$030A;         // # LATIN SMALL LETTER Y WITH RING ABOVE
      $1E9A : Result := #$0041#$02BE;         // # LATIN SMALL LETTER A WITH RIGHT HALF RING
      $1F50 : Result := #$03A5#$0313;         // # GREEK SMALL LETTER UPSILON WITH PSILI
      $1F52 : Result := #$03A5#$0313#$0300;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
      $1F54 : Result := #$03A5#$0313#$0301;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
      $1F56 : Result := #$03A5#$0313#$0342;   // # GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
      $1F80 : Result := #$1F08#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
      $1F81 : Result := #$1F09#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
      $1F82 : Result := #$1F0A#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1F83 : Result := #$1F0B#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1F84 : Result := #$1F0C#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1F85 : Result := #$1F0D#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1F86 : Result := #$1F0E#$0399;         // # GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1F87 : Result := #$1F0F#$0399;         // # GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1F88 : Result := #$1F08#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
      $1F89 : Result := #$1F09#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
      $1F8A : Result := #$1F0A#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1F8B : Result := #$1F0B#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1F8C : Result := #$1F0C#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1F8D : Result := #$1F0D#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1F8E : Result := #$1F0E#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1F8F : Result := #$1F0F#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1F90 : Result := #$1F28#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
      $1F91 : Result := #$1F29#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
      $1F92 : Result := #$1F2A#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1F93 : Result := #$1F2B#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1F94 : Result := #$1F2C#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1F95 : Result := #$1F2D#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1F96 : Result := #$1F2E#$0399;         // # GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1F97 : Result := #$1F2F#$0399;         // # GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1F98 : Result := #$1F28#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
      $1F99 : Result := #$1F29#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
      $1F9A : Result := #$1F2A#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1F9B : Result := #$1F2B#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1F9C : Result := #$1F2C#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1F9D : Result := #$1F2D#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1F9E : Result := #$1F2E#$0399;         // # GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1F9F : Result := #$1F2F#$0399;         // # GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1FA0 : Result := #$1F68#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
      $1FA1 : Result := #$1F69#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
      $1FA2 : Result := #$1F6A#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
      $1FA3 : Result := #$1F6B#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
      $1FA4 : Result := #$1F6C#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
      $1FA5 : Result := #$1F6D#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
      $1FA6 : Result := #$1F6E#$0399;         // # GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
      $1FA7 : Result := #$1F6F#$0399;         // # GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
      $1FA8 : Result := #$1F68#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
      $1FA9 : Result := #$1F69#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
      $1FAA : Result := #$1F6A#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
      $1FAB : Result := #$1F6B#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
      $1FAC : Result := #$1F6C#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
      $1FAD : Result := #$1F6D#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
      $1FAE : Result := #$1F6E#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
      $1FAF : Result := #$1F6F#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
      $1FB2 : Result := #$1FBA#$0399;         // # GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
      $1FB3 : Result := #$0391#$0399;         // # GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
      $1FB4 : Result := #$0386#$0399;         // # GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
      $1FB6 : Result := #$0391#$0342;         // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI
      $1FB7 : Result := #$0391#$0342#$0399;   // # GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FBC : Result := #$0391#$0399;         // # GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
      $1FC2 : Result := #$1FCA#$0399;         // # GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
      $1FC3 : Result := #$0397#$0399;         // # GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
      $1FC4 : Result := #$0389#$0399;         // # GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
      $1FC6 : Result := #$0397#$0342;         // # GREEK SMALL LETTER ETA WITH PERISPOMENI
      $1FC7 : Result := #$0397#$0342#$0399;   // # GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FCC : Result := #$0397#$0399;         // # GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
      $1FD2 : Result := #$0399#$0308#$0300;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
      $1FD3 : Result := #$0399#$0308#$0301;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
      $1FD6 : Result := #$0399#$0342;         // # GREEK SMALL LETTER IOTA WITH PERISPOMENI
      $1FD7 : Result := #$0399#$0308#$0342;   // # GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
      $1FE2 : Result := #$03A5#$0308#$0300;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
      $1FE3 : Result := #$03A5#$0308#$0301;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
      $1FE4 : Result := #$03A1#$0313;         // # GREEK SMALL LETTER RHO WITH PSILI
      $1FE6 : Result := #$03A5#$0342;         // # GREEK SMALL LETTER UPSILON WITH PERISPOMENI
      $1FE7 : Result := #$03A5#$0308#$0342;   // # GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
      $1FF2 : Result := #$1FFA#$0399;         // # GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
      $1FF3 : Result := #$03A9#$0399;         // # GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
      $1FF4 : Result := #$038F#$0399;         // # GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
      $1FF6 : Result := #$03A9#$0342;         // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI
      $1FF7 : Result := #$03A9#$0342#$0399;   // # GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
      $1FFC : Result := #$03A9#$0399;         // # GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    else
      Result := '';
    end else
  if Ord(Ch) < $FB00 then
    Result := '' else
  if Ord(Ch) <= $FB17 then
    case Ord(Ch) of
      $FB00 : Result := #$0046#$0046;         // # LATIN SMALL LIGATURE FF
      $FB01 : Result := #$0046#$0049;         // # LATIN SMALL LIGATURE FI
      $FB02 : Result := #$0046#$004C;         // # LATIN SMALL LIGATURE FL
      $FB03 : Result := #$0046#$0046#$0049;   // # LATIN SMALL LIGATURE FFI
      $FB04 : Result := #$0046#$0046#$004C;   // # LATIN SMALL LIGATURE FFL
      $FB05 : Result := #$0053#$0054;         // # LATIN SMALL LIGATURE LONG S T
      $FB06 : Result := #$0053#$0054;         // # LATIN SMALL LIGATURE ST
      $FB13 : Result := #$0544#$0546;         // # ARMENIAN SMALL LIGATURE MEN NOW
      $FB14 : Result := #$0544#$0535;         // # ARMENIAN SMALL LIGATURE MEN ECH
      $FB15 : Result := #$0544#$053B;         // # ARMENIAN SMALL LIGATURE MEN INI
      $FB16 : Result := #$054E#$0546;         // # ARMENIAN SMALL LIGATURE VEW NOW
      $FB17 : Result := #$0544#$053D;         // # ARMENIAN SMALL LIGATURE MEN XEH
    else
      Result := '';
    end
  else
    Result := '';
end;

function LocateFoldingLowerCase(const Ch: WideChar): WideString;
begin
  if Ch = #$0130 then
    Result := #$0069#$0307
  else
    Result := '';
end;

function IsUpperCase(const Ch: WideChar): Boolean;
var I : Integer;
begin
  I := LocateLetterInfo(Ch);
  if I >= 0 then
    Result := UnicodeLetterInfo[I].Attr = laUpper
  else
    Result := LocateOtherUpperCase(Ch) <> #$0000;
end;

function IsLowerCase(const Ch: WideChar): Boolean;
var I : Integer;
begin
  I := LocateLetterInfo(Ch);
  if I >= 0 then
    Result := UnicodeLetterInfo[I].Attr = laLower
  else
    Result := LocateOtherLowerCase(Ch) <> #$0000;
end;

type
  TUnicodeTitleCaseLetterInfo = packed record
    Unicode : WideChar;
    Upper   : WideChar;
    Lower   : WideChar;
  end;
  PUnicodeTitleCaseLetterInfo = ^TUnicodeTitleCaseLetterInfo;

const
  // Derived from 'Lt' class
  UnicodeTitleCaseLetterEntries = 31;
  UnicodeTitleCaseLetterInfo : Array[0..UnicodeTitleCaseLetterEntries - 1] of TUnicodeTitleCaseLetterInfo = (
    (Unicode:#$01C5; Upper:#$01C4; Lower:#$01C6),   // LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
    (Unicode:#$01C8; Upper:#$01C7; Lower:#$01C9),   // LATIN CAPITAL LETTER L WITH SMALL LETTER J
    (Unicode:#$01CB; Upper:#$01CA; Lower:#$01CC),   // LATIN CAPITAL LETTER N WITH SMALL LETTER J
    (Unicode:#$01F2; Upper:#$01F1; Lower:#$01F3),   // LATIN CAPITAL LETTER D WITH SMALL LETTER Z
    (Unicode:#$1F88; Upper:#$FFFF; Lower:#$1F80),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F89; Upper:#$FFFF; Lower:#$1F81),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F8A; Upper:#$FFFF; Lower:#$1F82),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8B; Upper:#$FFFF; Lower:#$1F83),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8C; Upper:#$FFFF; Lower:#$1F84),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8D; Upper:#$FFFF; Lower:#$1F85),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8E; Upper:#$FFFF; Lower:#$1F86),   // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F8F; Upper:#$FFFF; Lower:#$1F87),   // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F98; Upper:#$FFFF; Lower:#$1F90),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F99; Upper:#$FFFF; Lower:#$1F91),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F9A; Upper:#$FFFF; Lower:#$1F92),   // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9B; Upper:#$FFFF; Lower:#$1F93),   // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9C; Upper:#$FFFF; Lower:#$1F94),   // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9D; Upper:#$FFFF; Lower:#$1F95),   // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9E; Upper:#$FFFF; Lower:#$1F96),   // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F9F; Upper:#$FFFF; Lower:#$1F97),   // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FA8; Upper:#$FFFF; Lower:#$1FA0),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1FA9; Upper:#$FFFF; Lower:#$1FA1),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1FAA; Upper:#$FFFF; Lower:#$1FA2),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAB; Upper:#$FFFF; Lower:#$1FA3),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAC; Upper:#$FFFF; Lower:#$1FA4),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAD; Upper:#$FFFF; Lower:#$1FA5),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAE; Upper:#$FFFF; Lower:#$1FA6),   // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FAF; Upper:#$FFFF; Lower:#$1FA7),   // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FBC; Upper:#$FFFF; Lower:#$1FB3),   // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
    (Unicode:#$1FCC; Upper:#$FFFF; Lower:#$1FC3),   // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
    (Unicode:#$1FFC; Upper:#$FFFF; Lower:#$1FF3)    // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    );

function LocateTitleCaseLetterInfo(const Ch: WideChar): Integer;
var I : Integer;
begin
  if (Ord(Ch) < $01C5) or (Ord(Ch) > $1FFC) then
    Result := -1 else
  if (Ord(Ch) > $01F2) and (Ord(Ch) < $1F88) then
    Result := -1 else
    begin
      for I := 0 to UnicodeTitleCaseLetterEntries - 1 do
        if UnicodeTitleCaseLetterInfo[I].Unicode = Ch then
          begin
            Result := I;
            exit;
          end;
      Result := -1;
    end;
end;

function IsTitleCase(const Ch: WideChar): Boolean;
begin
  Result := LocateTitleCaseLetterInfo(Ch) >= 0;
end;

{$IFDEF CLR}
function WideUpCase(const Ch: WideChar): WideChar;
var I : Integer;
    J : Integer;
    C : WideChar;
    P : TUnicodeLetterInfo;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    begin
      if AnsiChar(Ord(Ch)) in ['a'..'z'] then
        Result := WideChar(Ord(Ch) - (Ord('a') - Ord('A'))) else
        Result := Ch;
    end else
    begin
      I := LocateLetterInfo(Ch);
      if I >= 0 then
        begin
          P := UnicodeLetterInfo[I];
          if P.Attr = laUpper then
            Result := Ch else
            begin
              C := P.CaseCode;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end;
        end else
        begin
          J := LocateTitleCaseLetterInfo(Ch);
          if J >= 0 then
            begin
              C := UnicodeTitleCaseLetterInfo[J].Upper;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end else
            begin
              C := LocateOtherLowerCase(Ch);
              if C = #$0000 then
                Result := Ch else
                Result := C;
            end;
        end;
    end;
end;
{$ELSE}
function WideUpCase(const Ch: WideChar): WideChar;
var I : Integer;
    J : Integer;
    C : WideChar;
    P : PUnicodeLetterInfo;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    begin
      if AnsiChar(Ord(Ch)) in ['a'..'z'] then
        Result := WideChar(Ord(Ch) - (Ord('a') - Ord('A'))) else
        Result := Ch;
    end else
    begin
      I := LocateLetterInfo(Ch);
      if I >= 0 then
        begin
          P := @UnicodeLetterInfo[I];
          if P^.Attr = laUpper then
            Result := Ch else
            begin
              C := P^.CaseCode;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end;
        end else
        begin
          J := LocateTitleCaseLetterInfo(Ch);
          if J >= 0 then
            begin
              C := UnicodeTitleCaseLetterInfo[J].Upper;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end else
            begin
              C := LocateOtherLowerCase(Ch);
              if C = #$0000 then
                Result := Ch else
                Result := C;
            end;
        end;
    end;
end;
{$ENDIF}

function WideUpCaseFolding(const Ch: WideChar): WideString;
var R : WideChar;
begin
  R := WideUpCase(Ch);
  if R = Ch then
    begin
      Result := LocateFoldingUpperCase(Ch);
      if Result = '' then
        Result := Ch;
    end
  else
    Result := R;
end;

{$IFDEF CLR}
function WideLowCase(const Ch: WideChar): WideChar;
var I : Integer;
    J : Integer;
    C : WideChar;
    P : TUnicodeLetterInfo;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    begin
      if AnsiChar(Ord(Ch)) in ['A'..'Z'] then
        Result := WideChar(Ord(Ch) + (Ord('a') - Ord('A'))) else
        Result := Ch;
    end else
    begin
      I := LocateLetterInfo(Ch);
      if I >= 0 then
        begin
          P := UnicodeLetterInfo[I];
          if P.Attr = laLower then
            Result := Ch else
            begin
              C := P.CaseCode;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end;
        end else
        begin
          J := LocateTitleCaseLetterInfo(Ch);
          if J >= 0 then
            begin
              C := UnicodeTitleCaseLetterInfo[J].Lower;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end else
            begin
              C := LocateOtherUpperCase(Ch);
              if C = #$0000 then
                Result := Ch else
                Result := C;
            end;
        end;
    end;
end;
{$ELSE}
function WideLowCase(const Ch: WideChar): WideChar;
var I : Integer;
    J : Integer;
    C : WideChar;
    P : PUnicodeLetterInfo;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    begin
      if AnsiChar(Ord(Ch)) in ['A'..'Z'] then
        Result := WideChar(Ord(Ch) + (Ord('a') - Ord('A'))) else
        Result := Ch;
    end else
    begin
      I := LocateLetterInfo(Ch);
      if I >= 0 then
        begin
          P := @UnicodeLetterInfo[I];
          if P^.Attr = laLower then
            Result := Ch else
            begin
              C := P^.CaseCode;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end;
        end else
        begin
          J := LocateTitleCaseLetterInfo(Ch);
          if J >= 0 then
            begin
              C := UnicodeTitleCaseLetterInfo[J].Lower;
              if C = #$FFFF then
                Result := Ch else
                Result := C;
            end else
            begin
              C := LocateOtherUpperCase(Ch);
              if C = #$0000 then
                Result := Ch else
                Result := C;
            end;
        end;
    end;
end;
{$ENDIF}

function WideLowCaseFolding(const Ch: WideChar): WideString;
var R : WideChar;
begin
  R := WideLowCase(Ch);
  if R = Ch then
    begin
      Result := LocateFoldingLowerCase(Ch);
      if Result = '' then
        Result := Ch;
    end
  else
    Result := R;
end;

function WideTitleCaseFolding(const Ch: WideChar): WideString;
begin
  Result := LocateFoldingTitleCase(Ch);
  if Result = '' then
    Result := Ch;
end;

{$IFDEF CLR}
function WideIsEqualNoCase(const A, B: WideChar): Boolean;
var I    : Integer;
    J    : Integer;
    C, D : AnsiChar;
    E, F : WideChar;
    P    : TUnicodeTitleCaseLetterInfo;
begin
  Result := A = B;
  if Result then
    exit;
  if (Ord(A) < $80) and (Ord(B) < $80) then // ASCII short-cut
    begin
      if AnsiChar(Ord(A)) in ['A'..'Z'] then
        C := AnsiChar(Byte(Ord(A)) + (Ord('a') - Ord('A'))) else
        C := AnsiChar(Ord(A));
      if AnsiChar(Ord(B)) in ['A'..'Z'] then
        D := AnsiChar(Byte (Ord(B)) + (Ord('a') - Ord('A'))) else
        D := AnsiChar(Ord(B));
      Result := C = D;
      exit;
    end;
  I := LocateLetterInfo(A);
  if I >= 0 then
    begin
      E := UnicodeLetterInfo[I].CaseCode;
      if E = #$FFFF then
        Result := False else
        Result := E = B;
      exit;
    end;
  J := LocateTitleCaseLetterInfo(A);
  if J >= 0 then
    begin
      P := UnicodeTitleCaseLetterInfo[J];
      E := P.Upper;
      F := P.Lower;
      Result := ((E <> #$FFFF) and (E = B)) or
                ((F <> #$FFFF) and (F = B));
      exit;
    end;
  E := LocateOtherLowerCase(A);
  if E <> #$0000 then
    Result := E = B
  else
    Result := False;
end;
{$ELSE}
function WideIsEqualNoCase(const A, B: WideChar): Boolean;
var I    : Integer;
    J    : Integer;
    C, D : AnsiChar;
    E, F : WideChar;
    P    : PUnicodeTitleCaseLetterInfo;
begin
  Result := A = B;
  if Result then
    exit;
  if (Ord(A) < $80) and (Ord(B) < $80) then // ASCII short-cut
    begin
      if AnsiChar(Ord(A)) in ['A'..'Z'] then
        C := AnsiChar(Byte(Ord(A)) + (Ord('a') - Ord('A'))) else
        C := AnsiChar(Ord(A));
      if AnsiChar(Ord(B)) in ['A'..'Z'] then
        D := AnsiChar(Byte (Ord(B)) + (Ord('a') - Ord('A'))) else
        D := AnsiChar(Ord(B));
      Result := C = D;
      exit;
    end;
  I := LocateLetterInfo(A);
  if I >= 0 then
    begin
      E := UnicodeLetterInfo[I].CaseCode;
      if E = #$FFFF then
        Result := False else
        Result := E = B;
      exit;
    end;
  J := LocateTitleCaseLetterInfo(A);
  if J >= 0 then
    begin
      P := @UnicodeTitleCaseLetterInfo[J];
      E := P^.Upper;
      F := P^.Lower;
      Result := ((E <> #$FFFF) and (E = B)) or
                ((F <> #$FFFF) and (F = B));
      exit;
    end;
  E := LocateOtherLowerCase(A);
  if E <> #$0000 then
    Result := E = B
  else
    Result := False;
end;
{$ENDIF}

// Derived from 'Lo' class
function IsOtherLetter(const Ch: UCS4Char): Boolean;
begin
  case Ch of
    $01BB,              //       LATIN LETTER TWO WITH STROKE
    $01C0..$01C3,       //   [4] LATIN LETTER DENTAL CLICK..LATIN LETTER RETROFLEX CLICK
    $05D0..$05EA,       //  [27] HEBREW LETTER ALEF..HEBREW LETTER TAV
    $05F0..$05F2,       //   [3] HEBREW LIGATURE YIDDISH DOUBLE VAV..HEBREW LIGATURE YIDDISH DOUBLE YOD
    $0621..$063A,       //  [26] ARABIC LETTER HAMZA..ARABIC LETTER GHAIN
    $0641..$064A,       //  [10] ARABIC LETTER FEH..ARABIC LETTER YEH
    $066E..$066F,       //   [2] ARABIC LETTER DOTLESS BEH..ARABIC LETTER DOTLESS QAF
    $0671..$06D3,       //  [99] ARABIC LETTER ALEF WASLA..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
    $06D5,              //       ARABIC LETTER AE
    $06FA..$06FC,       //   [3] ARABIC LETTER SHEEN WITH DOT BELOW..ARABIC LETTER GHAIN WITH DOT BELOW
    $0710,              //       SYRIAC LETTER ALAPH
    $0712..$072C,       //  [27] SYRIAC LETTER BETH..SYRIAC LETTER TAW
    $0780..$07A5,       //  [38] THAANA LETTER HAA..THAANA LETTER WAAVU
    $07B1,              //       THAANA LETTER NAA
    $0905..$0939,       //  [53] DEVANAGARI LETTER A..DEVANAGARI LETTER HA
    $093D,              //       DEVANAGARI SIGN AVAGRAHA
    $0950,              //       DEVANAGARI OM
    $0958..$0961,       //  [10] DEVANAGARI LETTER QA..DEVANAGARI LETTER VOCALIC LL
    $0985..$098C,       //   [8] BENGALI LETTER A..BENGALI LETTER VOCALIC L
    $098F..$0990,       //   [2] BENGALI LETTER E..BENGALI LETTER AI
    $0993..$09A8,       //  [22] BENGALI LETTER O..BENGALI LETTER NA
    $09AA..$09B0,       //   [7] BENGALI LETTER PA..BENGALI LETTER RA
    $09B2,              //       BENGALI LETTER LA
    $09B6..$09B9,       //   [4] BENGALI LETTER SHA..BENGALI LETTER HA
    $09DC..$09DD,       //   [2] BENGALI LETTER RRA..BENGALI LETTER RHA
    $09DF..$09E1,       //   [3] BENGALI LETTER YYA..BENGALI LETTER VOCALIC LL
    $09F0..$09F1,       //   [2] BENGALI LETTER RA WITH MIDDLE DIAGONAL..BENGALI LETTER RA WITH LOWER DIAGONAL
    $0A05..$0A0A,       //   [6] GURMUKHI LETTER A..GURMUKHI LETTER UU
    $0A0F..$0A10,       //   [2] GURMUKHI LETTER EE..GURMUKHI LETTER AI
    $0A13..$0A28,       //  [22] GURMUKHI LETTER OO..GURMUKHI LETTER NA
    $0A2A..$0A30,       //   [7] GURMUKHI LETTER PA..GURMUKHI LETTER RA
    $0A32..$0A33,       //   [2] GURMUKHI LETTER LA..GURMUKHI LETTER LLA
    $0A35..$0A36,       //   [2] GURMUKHI LETTER VA..GURMUKHI LETTER SHA
    $0A38..$0A39,       //   [2] GURMUKHI LETTER SA..GURMUKHI LETTER HA
    $0A59..$0A5C,       //   [4] GURMUKHI LETTER KHHA..GURMUKHI LETTER RRA
    $0A5E,              //       GURMUKHI LETTER FA
    $0A72..$0A74,       //   [3] GURMUKHI IRI..GURMUKHI EK ONKAR
    $0A85..$0A8B,       //   [7] GUJARATI LETTER A..GUJARATI LETTER VOCALIC R
    $0A8D,              //       GUJARATI VOWEL CANDRA E
    $0A8F..$0A91,       //   [3] GUJARATI LETTER E..GUJARATI VOWEL CANDRA O
    $0A93..$0AA8,       //  [22] GUJARATI LETTER O..GUJARATI LETTER NA
    $0AAA..$0AB0,       //   [7] GUJARATI LETTER PA..GUJARATI LETTER RA
    $0AB2..$0AB3,       //   [2] GUJARATI LETTER LA..GUJARATI LETTER LLA
    $0AB5..$0AB9,       //   [5] GUJARATI LETTER VA..GUJARATI LETTER HA
    $0ABD,              //       GUJARATI SIGN AVAGRAHA
    $0AD0,              //       GUJARATI OM
    $0AE0,              //       GUJARATI LETTER VOCALIC RR
    $0B05..$0B0C,       //   [8] ORIYA LETTER A..ORIYA LETTER VOCALIC L
    $0B0F..$0B10,       //   [2] ORIYA LETTER E..ORIYA LETTER AI
    $0B13..$0B28,       //  [22] ORIYA LETTER O..ORIYA LETTER NA
    $0B2A..$0B30,       //   [7] ORIYA LETTER PA..ORIYA LETTER RA
    $0B32..$0B33,       //   [2] ORIYA LETTER LA..ORIYA LETTER LLA
    $0B36..$0B39,       //   [4] ORIYA LETTER SHA..ORIYA LETTER HA
    $0B3D,              //       ORIYA SIGN AVAGRAHA
    $0B5C..$0B5D,       //   [2] ORIYA LETTER RRA..ORIYA LETTER RHA
    $0B5F..$0B61,       //   [3] ORIYA LETTER YYA..ORIYA LETTER VOCALIC LL
    $0B83,              //       TAMIL SIGN VISARGA
    $0B85..$0B8A,       //   [6] TAMIL LETTER A..TAMIL LETTER UU
    $0B8E..$0B90,       //   [3] TAMIL LETTER E..TAMIL LETTER AI
    $0B92..$0B95,       //   [4] TAMIL LETTER O..TAMIL LETTER KA
    $0B99..$0B9A,       //   [2] TAMIL LETTER NGA..TAMIL LETTER CA
    $0B9C,              //       TAMIL LETTER JA
    $0B9E..$0B9F,       //   [2] TAMIL LETTER NYA..TAMIL LETTER TTA
    $0BA3..$0BA4,       //   [2] TAMIL LETTER NNA..TAMIL LETTER TA
    $0BA8..$0BAA,       //   [3] TAMIL LETTER NA..TAMIL LETTER PA
    $0BAE..$0BB5,       //   [8] TAMIL LETTER MA..TAMIL LETTER VA
    $0BB7..$0BB9,       //   [3] TAMIL LETTER SSA..TAMIL LETTER HA
    $0C05..$0C0C,       //   [8] TELUGU LETTER A..TELUGU LETTER VOCALIC L
    $0C0E..$0C10,       //   [3] TELUGU LETTER E..TELUGU LETTER AI
    $0C12..$0C28,       //  [23] TELUGU LETTER O..TELUGU LETTER NA
    $0C2A..$0C33,       //  [10] TELUGU LETTER PA..TELUGU LETTER LLA
    $0C35..$0C39,       //   [5] TELUGU LETTER VA..TELUGU LETTER HA
    $0C60..$0C61,       //   [2] TELUGU LETTER VOCALIC RR..TELUGU LETTER VOCALIC LL
    $0C85..$0C8C,       //   [8] KANNADA LETTER A..KANNADA LETTER VOCALIC L
    $0C8E..$0C90,       //   [3] KANNADA LETTER E..KANNADA LETTER AI
    $0C92..$0CA8,       //  [23] KANNADA LETTER O..KANNADA LETTER NA
    $0CAA..$0CB3,       //  [10] KANNADA LETTER PA..KANNADA LETTER LLA
    $0CB5..$0CB9,       //   [5] KANNADA LETTER VA..KANNADA LETTER HA
    $0CDE,              //       KANNADA LETTER FA
    $0CE0..$0CE1,       //   [2] KANNADA LETTER VOCALIC RR..KANNADA LETTER VOCALIC LL
    $0D05..$0D0C,       //   [8] MALAYALAM LETTER A..MALAYALAM LETTER VOCALIC L
    $0D0E..$0D10,       //   [3] MALAYALAM LETTER E..MALAYALAM LETTER AI
    $0D12..$0D28,       //  [23] MALAYALAM LETTER O..MALAYALAM LETTER NA
    $0D2A..$0D39,       //  [16] MALAYALAM LETTER PA..MALAYALAM LETTER HA
    $0D60..$0D61,       //   [2] MALAYALAM LETTER VOCALIC RR..MALAYALAM LETTER VOCALIC LL
    $0D85..$0D96,       //  [18] SINHALA LETTER AYANNA..SINHALA LETTER AUYANNA
    $0D9A..$0DB1,       //  [24] SINHALA LETTER ALPAPRAANA KAYANNA..SINHALA LETTER DANTAJA NAYANNA
    $0DB3..$0DBB,       //   [9] SINHALA LETTER SANYAKA DAYANNA..SINHALA LETTER RAYANNA
    $0DBD,              //       SINHALA LETTER DANTAJA LAYANNA
    $0DC0..$0DC6,       //   [7] SINHALA LETTER VAYANNA..SINHALA LETTER FAYANNA
    $0E01..$0E30,       //  [48] THAI CHARACTER KO KAI..THAI CHARACTER SARA A
    $0E32..$0E33,       //   [2] THAI CHARACTER SARA AA..THAI CHARACTER SARA AM
    $0E40..$0E45,       //   [6] THAI CHARACTER SARA E..THAI CHARACTER LAKKHANGYAO
    $0E81..$0E82,       //   [2] LAO LETTER KO..LAO LETTER KHO SUNG
    $0E84,              //       LAO LETTER KHO TAM
    $0E87..$0E88,       //   [2] LAO LETTER NGO..LAO LETTER CO
    $0E8A,              //       LAO LETTER SO TAM
    $0E8D,              //       LAO LETTER NYO
    $0E94..$0E97,       //   [4] LAO LETTER DO..LAO LETTER THO TAM
    $0E99..$0E9F,       //   [7] LAO LETTER NO..LAO LETTER FO SUNG
    $0EA1..$0EA3,       //   [3] LAO LETTER MO..LAO LETTER LO LING
    $0EA5,              //       LAO LETTER LO LOOT
    $0EA7,              //       LAO LETTER WO
    $0EAA..$0EAB,       //   [2] LAO LETTER SO SUNG..LAO LETTER HO SUNG
    $0EAD..$0EB0,       //   [4] LAO LETTER O..LAO VOWEL SIGN A
    $0EB2..$0EB3,       //   [2] LAO VOWEL SIGN AA..LAO VOWEL SIGN AM
    $0EBD,              //       LAO SEMIVOWEL SIGN NYO
    $0EC0..$0EC4,       //   [5] LAO VOWEL SIGN E..LAO VOWEL SIGN AI
    $0EDC..$0EDD,       //   [2] LAO HO NO..LAO HO MO
    $0F00,              //       TIBETAN SYLLABLE OM
    $0F40..$0F47,       //   [8] TIBETAN LETTER KA..TIBETAN LETTER JA
    $0F49..$0F6A,       //  [34] TIBETAN LETTER NYA..TIBETAN LETTER FIXED-FORM RA
    $0F88..$0F8B,       //   [4] TIBETAN SIGN LCE TSA CAN..TIBETAN SIGN GRU MED RGYINGS
    $1000..$1021,       //  [34] MYANMAR LETTER KA..MYANMAR LETTER A
    $1023..$1027,       //   [5] MYANMAR LETTER I..MYANMAR LETTER E
    $1029..$102A,       //   [2] MYANMAR LETTER O..MYANMAR LETTER AU
    $1050..$1055,       //   [6] MYANMAR LETTER SHA..MYANMAR LETTER VOCALIC LL
    $10D0..$10F8,       //  [41] GEORGIAN LETTER AN..GEORGIAN LETTER ELIFI
    $1100..$1159,       //  [90] HANGUL CHOSEONG KIYEOK..HANGUL CHOSEONG YEORINHIEUH
    $115F..$11A2,       //  [68] HANGUL CHOSEONG FILLER..HANGUL JUNGSEONG SSANGARAEA
    $11A8..$11F9,       //  [82] HANGUL JONGSEONG KIYEOK..HANGUL JONGSEONG YEORINHIEUH
    $1200..$1206,       //   [7] ETHIOPIC SYLLABLE HA..ETHIOPIC SYLLABLE HO
    $1208..$1246,       //  [63] ETHIOPIC SYLLABLE LA..ETHIOPIC SYLLABLE QO
    $1248,              //       ETHIOPIC SYLLABLE QWA
    $124A..$124D,       //   [4] ETHIOPIC SYLLABLE QWI..ETHIOPIC SYLLABLE QWE
    $1250..$1256,       //   [7] ETHIOPIC SYLLABLE QHA..ETHIOPIC SYLLABLE QHO
    $1258,              //       ETHIOPIC SYLLABLE QHWA
    $125A..$125D,       //   [4] ETHIOPIC SYLLABLE QHWI..ETHIOPIC SYLLABLE QHWE
    $1260..$1286,       //  [39] ETHIOPIC SYLLABLE BA..ETHIOPIC SYLLABLE XO
    $1288,              //       ETHIOPIC SYLLABLE XWA
    $128A..$128D,       //   [4] ETHIOPIC SYLLABLE XWI..ETHIOPIC SYLLABLE XWE
    $1290..$12AE,       //  [31] ETHIOPIC SYLLABLE NA..ETHIOPIC SYLLABLE KO
    $12B0,              //       ETHIOPIC SYLLABLE KWA
    $12B2..$12B5,       //   [4] ETHIOPIC SYLLABLE KWI..ETHIOPIC SYLLABLE KWE
    $12B8..$12BE,       //   [7] ETHIOPIC SYLLABLE KXA..ETHIOPIC SYLLABLE KXO
    $12C0,              //       ETHIOPIC SYLLABLE KXWA
    $12C2..$12C5,       //   [4] ETHIOPIC SYLLABLE KXWI..ETHIOPIC SYLLABLE KXWE
    $12C8..$12CE,       //   [7] ETHIOPIC SYLLABLE WA..ETHIOPIC SYLLABLE WO
    $12D0..$12D6,       //   [7] ETHIOPIC SYLLABLE PHARYNGEAL A..ETHIOPIC SYLLABLE PHARYNGEAL O
    $12D8..$12EE,       //  [23] ETHIOPIC SYLLABLE ZA..ETHIOPIC SYLLABLE YO
    $12F0..$130E,       //  [31] ETHIOPIC SYLLABLE DA..ETHIOPIC SYLLABLE GO
    $1310,              //       ETHIOPIC SYLLABLE GWA
    $1312..$1315,       //   [4] ETHIOPIC SYLLABLE GWI..ETHIOPIC SYLLABLE GWE
    $1318..$131E,       //   [7] ETHIOPIC SYLLABLE GGA..ETHIOPIC SYLLABLE GGO
    $1320..$1346,       //  [39] ETHIOPIC SYLLABLE THA..ETHIOPIC SYLLABLE TZO
    $1348..$135A,       //  [19] ETHIOPIC SYLLABLE FA..ETHIOPIC SYLLABLE FYA
    $13A0..$13F4,       //  [85] CHEROKEE LETTER A..CHEROKEE LETTER YV
    $1401..$166C,       // [620] CANADIAN SYLLABICS E..CANADIAN SYLLABICS CARRIER TTSA
    $166F..$1676,       //   [8] CANADIAN SYLLABICS QAI..CANADIAN SYLLABICS NNGAA
    $1681..$169A,       //  [26] OGHAM LETTER BEITH..OGHAM LETTER PEITH
    $16A0..$16EA,       //  [75] RUNIC LETTER FEHU FEOH FE F..RUNIC LETTER X
    $1700..$170C,       //  [13] TAGALOG LETTER A..TAGALOG LETTER YA
    $170E..$1711,       //   [4] TAGALOG LETTER LA..TAGALOG LETTER HA
    $1720..$1731,       //  [18] HANUNOO LETTER A..HANUNOO LETTER HA
    $1740..$1751,       //  [18] BUHID LETTER A..BUHID LETTER HA
    $1760..$176C,       //  [13] TAGBANWA LETTER A..TAGBANWA LETTER YA
    $176E..$1770,       //   [3] TAGBANWA LETTER LA..TAGBANWA LETTER SA
    $1780..$17B3,       //  [52] KHMER LETTER KA..KHMER INDEPENDENT VOWEL QAU
    $17DC,              //       KHMER SIGN AVAKRAHASANYA
    $1820..$1842,       //  [35] MONGOLIAN LETTER A..MONGOLIAN LETTER CHI
    $1844..$1877,       //  [52] MONGOLIAN LETTER TODO E..MONGOLIAN LETTER MANCHU ZHA
    $1880..$18A8,       //  [41] MONGOLIAN LETTER ALI GALI ANUSVARA ONE..MONGOLIAN LETTER MANCHU ALI GALI BHA
    $2135..$2138,       //   [4] ALEF SYMBOL..DALET SYMBOL
    $3006,              //       IDEOGRAPHIC CLOSING MARK
    $303C,              //       MASU MARK
    $3041..$3096,       //  [86] HIRAGANA LETTER SMALL A..HIRAGANA LETTER SMALL KE
    $309F,              //       HIRAGANA DIGRAPH YORI
    $30A1..$30FA,       //  [90] KATAKANA LETTER SMALL A..KATAKANA LETTER VO
    $30FF,              //       KATAKANA DIGRAPH KOTO
    $3105..$312C,       //  [40] BOPOMOFO LETTER B..BOPOMOFO LETTER GN
    $3131..$318E,       //  [94] HANGUL LETTER KIYEOK..HANGUL LETTER ARAEAE
    $31A0..$31B7,       //  [24] BOPOMOFO LETTER BU..BOPOMOFO FINAL LETTER H
    $31F0..$31FF,       //  [16] KATAKANA LETTER SMALL KU..KATAKANA LETTER SMALL RO
    $3400..$4DB5,       // [6582] CJK UNIFIED IDEOGRAPH-3400..CJK UNIFIED IDEOGRAPH-4DB5
    $4E00..$9FA5,       // [20902] CJK UNIFIED IDEOGRAPH-4E00..CJK UNIFIED IDEOGRAPH-9FA5
    $A000..$A48C,       // [1165] YI SYLLABLE IT..YI SYLLABLE YYR
    $AC00..$D7A3,       // [11172] HANGUL SYLLABLE GA..HANGUL SYLLABLE HIH
    $F900..$FA2D,       // [302] CJK COMPATIBILITY IDEOGRAPH-F900..CJK COMPATIBILITY IDEOGRAPH-FA2D
    $FA30..$FA6A,       //  [59] CJK COMPATIBILITY IDEOGRAPH-FA30..CJK COMPATIBILITY IDEOGRAPH-FA6A
    $FB1D,              //       HEBREW LETTER YOD WITH HIRIQ
    $FB1F..$FB28,       //  [10] HEBREW LIGATURE YIDDISH YOD YOD PATAH..HEBREW LETTER WIDE TAV
    $FB2A..$FB36,       //  [13] HEBREW LETTER SHIN WITH SHIN DOT..HEBREW LETTER ZAYIN WITH DAGESH
    $FB38..$FB3C,       //   [5] HEBREW LETTER TET WITH DAGESH..HEBREW LETTER LAMED WITH DAGESH
    $FB3E,              //       HEBREW LETTER MEM WITH DAGESH
    $FB40..$FB41,       //   [2] HEBREW LETTER NUN WITH DAGESH..HEBREW LETTER SAMEKH WITH DAGESH
    $FB43..$FB44,       //   [2] HEBREW LETTER FINAL PE WITH DAGESH..HEBREW LETTER PE WITH DAGESH
    $FB46..$FBB1,       // [108] HEBREW LETTER TSADI WITH DAGESH..ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
    $FBD3..$FD3D,       // [363] ARABIC LETTER NG ISOLATED FORM..ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
    $FD50..$FD8F,       //  [64] ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM..ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
    $FD92..$FDC7,       //  [54] ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM..ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
    $FDF0..$FDFB,       //  [12] ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM..ARABIC LIGATURE JALLAJALALOUHOU
    $FE70..$FE74,       //   [5] ARABIC FATHATAN ISOLATED FORM..ARABIC KASRATAN ISOLATED FORM
    $FE76..$FEFC,       // [135] ARABIC FATHA ISOLATED FORM..ARABIC LIGATURE LAM WITH ALEF FINAL FORM
    $FF66..$FF6F,       //  [10] HALFWIDTH KATAKANA LETTER WO..HALFWIDTH KATAKANA LETTER SMALL TU
    $FF71..$FF9D,       //  [45] HALFWIDTH KATAKANA LETTER A..HALFWIDTH KATAKANA LETTER N
    $FFA0..$FFBE,       //  [31] HALFWIDTH HANGUL FILLER..HALFWIDTH HANGUL LETTER HIEUH
    $FFC2..$FFC7,       //   [6] HALFWIDTH HANGUL LETTER A..HALFWIDTH HANGUL LETTER E
    $FFCA..$FFCF,       //   [6] HALFWIDTH HANGUL LETTER YEO..HALFWIDTH HANGUL LETTER OE
    $FFD2..$FFD7,       //   [6] HALFWIDTH HANGUL LETTER YO..HALFWIDTH HANGUL LETTER YU
    $FFDA..$FFDC,       //   [3] HALFWIDTH HANGUL LETTER EU..HALFWIDTH HANGUL LETTER I
    $10300..$1031E,     //  [31] OLD ITALIC LETTER A..OLD ITALIC LETTER UU
    $10330..$10349,     //  [26] GOTHIC LETTER AHSA..GOTHIC LETTER OTHAL
    $20000..$2A6D6,     // [42711] CJK UNIFIED IDEOGRAPH-20000..CJK UNIFIED IDEOGRAPH-2A6D6
    $2F800..$2FA1D :    // [542] CJK COMPATIBILITY IDEOGRAPH-2F800..CJK COMPATIBILITY IDEOGRAPH-2FA1D
      Result := True;
  else
    Result := False;
  end;
end;

function IsLetter(const Ch: WideChar): Boolean;
begin
  if Ord(Ch) < $80 then // ASCII short-cut
    Result := AnsiChar(Ord(Ch)) in ['A'..'Z', 'a'..'z']
  else
    begin
      Result := LocateLetterInfo(Ch) >= 0;
      if Result then
        exit;
      Result := LocateTitleCaseLetterInfo(Ch) >= 0;
      if Result then
        exit;
      Result := IsOtherLetter(Ord(Ch));
    end;
end;

function IsAlphabetic(const Ch: WideChar): Boolean;
begin
  Result := IsLetter(Ch);
  if Result then
    exit;
  case Ord(Ch) of
    $02B0..$02B8,   // # Lm   [9] MODIFIER LETTER SMALL H..MODIFIER LETTER SMALL Y
    $02BB..$02C1,   // # Lm   [7] MODIFIER LETTER TURNED COMMA..MODIFIER LETTER REVERSED GLOTTAL STOP
    $02D0..$02D1,   // # Lm   [2] MODIFIER LETTER TRIANGULAR COLON..MODIFIER LETTER HALF TRIANGULAR COLON
    $02E0..$02E4,   // # Lm   [5] MODIFIER LETTER SMALL GAMMA..MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
    $02EE,          // # Lm       MODIFIER LETTER DOUBLE APOSTROPHE
    $0345,          // # Mn       COMBINING GREEK YPOGEGRAMMENI
    $037A,          // # Lm       GREEK YPOGEGRAMMENI
    $0559,          // # Lm       ARMENIAN MODIFIER LETTER LEFT HALF RING
    $05B0..$05B9,   // # Mn  [10] HEBREW POINT SHEVA..HEBREW POINT HOLAM
    $05BB..$05BD,   // # Mn   [3] HEBREW POINT QUBUTS..HEBREW POINT METEG
    $05BF,          // # Mn       HEBREW POINT RAFE
    $05C1..$05C2,   // # Mn   [2] HEBREW POINT SHIN DOT..HEBREW POINT SIN DOT
    $05C4,          // # Mn       HEBREW MARK UPPER DOT
    $0640,          // # Lm       ARABIC TATWEEL
    $064B..$0655,   // # Mn  [11] ARABIC FATHATAN..ARABIC HAMZA BELOW
    $0670,          // # Mn       ARABIC LETTER SUPERSCRIPT ALEF
    $06D6..$06DC,   // # Mn   [7] ARABIC SMALL HIGH LIGATURE SAD WITH LAM WITH ALEF MAKSURA..ARABIC SMALL HIGH SEEN
    $06E1..$06E4,   // # Mn   [4] ARABIC SMALL HIGH DOTLESS HEAD OF KHAH..ARABIC SMALL HIGH MADDA
    $06E5..$06E6,   // # Lm   [2] ARABIC SMALL WAW..ARABIC SMALL YEH
    $06E7..$06E8,   // # Mn   [2] ARABIC SMALL HIGH YEH..ARABIC SMALL HIGH NOON
    $06ED,          // # Mn       ARABIC SMALL LOW MEEM
    $0711,          // # Mn       SYRIAC LETTER SUPERSCRIPT ALAPH
    $0730..$073F,   // # Mn  [16] SYRIAC PTHAHA ABOVE..SYRIAC RWAHA
    $07A6..$07B0,   // # Mn  [11] THAANA ABAFILI..THAANA SUKUN
    $0901..$0902,   // # Mn   [2] DEVANAGARI SIGN CANDRABINDU..DEVANAGARI SIGN ANUSVARA
    $0903,          // # Mc       DEVANAGARI SIGN VISARGA
    $093E..$0940,   // # Mc   [3] DEVANAGARI VOWEL SIGN AA..DEVANAGARI VOWEL SIGN II
    $0941..$0948,   // # Mn   [8] DEVANAGARI VOWEL SIGN U..DEVANAGARI VOWEL SIGN AI
    $0949..$094C,   // # Mc   [4] DEVANAGARI VOWEL SIGN CANDRA O..DEVANAGARI VOWEL SIGN AU
    $0962..$0963,   // # Mn   [2] DEVANAGARI VOWEL SIGN VOCALIC L..DEVANAGARI VOWEL SIGN VOCALIC LL
    $0981,          // # Mn       BENGALI SIGN CANDRABINDU
    $0982..$0983,   // # Mc   [2] BENGALI SIGN ANUSVARA..BENGALI SIGN VISARGA
    $09BE..$09C0,   // # Mc   [3] BENGALI VOWEL SIGN AA..BENGALI VOWEL SIGN II
    $09C1..$09C4,   // # Mn   [4] BENGALI VOWEL SIGN U..BENGALI VOWEL SIGN VOCALIC RR
    $09C7..$09C8,   // # Mc   [2] BENGALI VOWEL SIGN E..BENGALI VOWEL SIGN AI
    $09CB..$09CC,   // # Mc   [2] BENGALI VOWEL SIGN O..BENGALI VOWEL SIGN AU
    $09D7,          // # Mc       BENGALI AU LENGTH MARK
    $09E2..$09E3,   // # Mn   [2] BENGALI VOWEL SIGN VOCALIC L..BENGALI VOWEL SIGN VOCALIC LL
    $0A02,          // # Mn       GURMUKHI SIGN BINDI
    $0A3E..$0A40,   // # Mc   [3] GURMUKHI VOWEL SIGN AA..GURMUKHI VOWEL SIGN II
    $0A41..$0A42,   // # Mn   [2] GURMUKHI VOWEL SIGN U..GURMUKHI VOWEL SIGN UU
    $0A47..$0A48,   // # Mn   [2] GURMUKHI VOWEL SIGN EE..GURMUKHI VOWEL SIGN AI
    $0A4B..$0A4C,   // # Mn   [2] GURMUKHI VOWEL SIGN OO..GURMUKHI VOWEL SIGN AU
    $0A70..$0A71,   // # Mn   [2] GURMUKHI TIPPI..GURMUKHI ADDAK
    $0A81..$0A82,   // # Mn   [2] GUJARATI SIGN CANDRABINDU..GUJARATI SIGN ANUSVARA
    $0A83,          // # Mc       GUJARATI SIGN VISARGA
    $0ABE..$0AC0,   // # Mc   [3] GUJARATI VOWEL SIGN AA..GUJARATI VOWEL SIGN II
    $0AC1..$0AC5,   // # Mn   [5] GUJARATI VOWEL SIGN U..GUJARATI VOWEL SIGN CANDRA E
    $0AC7..$0AC8,   // # Mn   [2] GUJARATI VOWEL SIGN E..GUJARATI VOWEL SIGN AI
    $0AC9,          // # Mc       GUJARATI VOWEL SIGN CANDRA O
    $0ACB..$0ACC,   // # Mc   [2] GUJARATI VOWEL SIGN O..GUJARATI VOWEL SIGN AU
    $0B01,          // # Mn       ORIYA SIGN CANDRABINDU
    $0B02..$0B03,   // # Mc   [2] ORIYA SIGN ANUSVARA..ORIYA SIGN VISARGA
    $0B3E,          // # Mc       ORIYA VOWEL SIGN AA
    $0B3F,          // # Mn       ORIYA VOWEL SIGN I
    $0B40,          // # Mc       ORIYA VOWEL SIGN II
    $0B41..$0B43,   // # Mn   [3] ORIYA VOWEL SIGN U..ORIYA VOWEL SIGN VOCALIC R
    $0B47..$0B48,   // # Mc   [2] ORIYA VOWEL SIGN E..ORIYA VOWEL SIGN AI
    $0B4B..$0B4C,   // # Mc   [2] ORIYA VOWEL SIGN O..ORIYA VOWEL SIGN AU
    $0B56,          // # Mn       ORIYA AI LENGTH MARK
    $0B57,          // # Mc       ORIYA AU LENGTH MARK
    $0B82,          // # Mn       TAMIL SIGN ANUSVARA
    $0BBE..$0BBF,   // # Mc   [2] TAMIL VOWEL SIGN AA..TAMIL VOWEL SIGN I
    $0BC0,          // # Mn       TAMIL VOWEL SIGN II
    $0BC1..$0BC2,   // # Mc   [2] TAMIL VOWEL SIGN U..TAMIL VOWEL SIGN UU
    $0BC6..$0BC8,   // # Mc   [3] TAMIL VOWEL SIGN E..TAMIL VOWEL SIGN AI
    $0BCA..$0BCC,   // # Mc   [3] TAMIL VOWEL SIGN O..TAMIL VOWEL SIGN AU
    $0BD7,          // # Mc       TAMIL AU LENGTH MARK
    $0C01..$0C03,   // # Mc   [3] TELUGU SIGN CANDRABINDU..TELUGU SIGN VISARGA
    $0C3E..$0C40,   // # Mn   [3] TELUGU VOWEL SIGN AA..TELUGU VOWEL SIGN II
    $0C41..$0C44,   // # Mc   [4] TELUGU VOWEL SIGN U..TELUGU VOWEL SIGN VOCALIC RR
    $0C46..$0C48,   // # Mn   [3] TELUGU VOWEL SIGN E..TELUGU VOWEL SIGN AI
    $0C4A..$0C4C,   // # Mn   [3] TELUGU VOWEL SIGN O..TELUGU VOWEL SIGN AU
    $0C55..$0C56,   // # Mn   [2] TELUGU LENGTH MARK..TELUGU AI LENGTH MARK
    $0C82..$0C83,   // # Mc   [2] KANNADA SIGN ANUSVARA..KANNADA SIGN VISARGA
    $0CBE,          // # Mc       KANNADA VOWEL SIGN AA
    $0CBF,          // # Mn       KANNADA VOWEL SIGN I
    $0CC0..$0CC4,   // # Mc   [5] KANNADA VOWEL SIGN II..KANNADA VOWEL SIGN VOCALIC RR
    $0CC6,          // # Mn       KANNADA VOWEL SIGN E
    $0CC7..$0CC8,   // # Mc   [2] KANNADA VOWEL SIGN EE..KANNADA VOWEL SIGN AI
    $0CCA..$0CCB,   // # Mc   [2] KANNADA VOWEL SIGN O..KANNADA VOWEL SIGN OO
    $0CCC,          // # Mn       KANNADA VOWEL SIGN AU
    $0CD5..$0CD6,   // # Mc   [2] KANNADA LENGTH MARK..KANNADA AI LENGTH MARK
    $0D02..$0D03,   // # Mc   [2] MALAYALAM SIGN ANUSVARA..MALAYALAM SIGN VISARGA
    $0D3E..$0D40,   // # Mc   [3] MALAYALAM VOWEL SIGN AA..MALAYALAM VOWEL SIGN II
    $0D41..$0D43,   // # Mn   [3] MALAYALAM VOWEL SIGN U..MALAYALAM VOWEL SIGN VOCALIC R
    $0D46..$0D48,   // # Mc   [3] MALAYALAM VOWEL SIGN E..MALAYALAM VOWEL SIGN AI
    $0D4A..$0D4C,   // # Mc   [3] MALAYALAM VOWEL SIGN O..MALAYALAM VOWEL SIGN AU
    $0D57,          // # Mc       MALAYALAM AU LENGTH MARK
    $0D82..$0D83,   // # Mc   [2] SINHALA SIGN ANUSVARAYA..SINHALA SIGN VISARGAYA
    $0DCF..$0DD1,   // # Mc   [3] SINHALA VOWEL SIGN AELA-PILLA..SINHALA VOWEL SIGN DIGA AEDA-PILLA
    $0DD2..$0DD4,   // # Mn   [3] SINHALA VOWEL SIGN KETTI IS-PILLA..SINHALA VOWEL SIGN KETTI PAA-PILLA
    $0DD6,          // # Mn       SINHALA VOWEL SIGN DIGA PAA-PILLA
    $0DD8..$0DDF,   // # Mc   [8] SINHALA VOWEL SIGN GAETTA-PILLA..SINHALA VOWEL SIGN GAYANUKITTA
    $0DF2..$0DF3,   // # Mc   [2] SINHALA VOWEL SIGN DIGA GAETTA-PILLA..SINHALA VOWEL SIGN DIGA GAYANUKITTA
    $0E31,          // # Mn       THAI CHARACTER MAI HAN-AKAT
    $0E34..$0E3A,   // # Mn   [7] THAI CHARACTER SARA I..THAI CHARACTER PHINTHU
    $0E46,          // # Lm       THAI CHARACTER MAIYAMOK
    $0E4D,          // # Mn       THAI CHARACTER NIKHAHIT
    $0EB1,          // # Mn       LAO VOWEL SIGN MAI KAN
    $0EB4..$0EB9,   // # Mn   [6] LAO VOWEL SIGN I..LAO VOWEL SIGN UU
    $0EBB..$0EBC,   // # Mn   [2] LAO VOWEL SIGN MAI KON..LAO SEMIVOWEL SIGN LO
    $0EC6,          // # Lm       LAO KO LA
    $0ECD,          // # Mn       LAO NIGGAHITA
    $0F71..$0F7E,   // # Mn  [14] TIBETAN VOWEL SIGN AA..TIBETAN SIGN RJES SU NGA RO
    $0F7F,          // # Mc       TIBETAN SIGN RNAM BCAD
    $0F80..$0F81,   // # Mn   [2] TIBETAN VOWEL SIGN REVERSED I..TIBETAN VOWEL SIGN REVERSED II
    $0F90..$0F97,   // # Mn   [8] TIBETAN SUBJOINED LETTER KA..TIBETAN SUBJOINED LETTER JA
    $0F99..$0FBC,   // # Mn  [36] TIBETAN SUBJOINED LETTER NYA..TIBETAN SUBJOINED LETTER FIXED-FORM RA
    $102C,          // # Mc       MYANMAR VOWEL SIGN AA
    $102D..$1030,   // # Mn   [4] MYANMAR VOWEL SIGN I..MYANMAR VOWEL SIGN UU
    $1031,          // # Mc       MYANMAR VOWEL SIGN E
    $1032,          // # Mn       MYANMAR VOWEL SIGN AI
    $1036,          // # Mn       MYANMAR SIGN ANUSVARA
    $1038,          // # Mc       MYANMAR SIGN VISARGA
    $1056..$1057,   // # Mc   [2] MYANMAR VOWEL SIGN VOCALIC R..MYANMAR VOWEL SIGN VOCALIC RR
    $1058..$1059,   // # Mn   [2] MYANMAR VOWEL SIGN VOCALIC L..MYANMAR VOWEL SIGN VOCALIC LL
    $16EE..$16F0,   // # Nl   [3] RUNIC ARLAUG SYMBOL..RUNIC BELGTHOR SYMBOL
    $1712..$1713,   // # Mn   [2] TAGALOG VOWEL SIGN I..TAGALOG VOWEL SIGN U
    $1732..$1733,   // # Mn   [2] HANUNOO VOWEL SIGN I..HANUNOO VOWEL SIGN U
    $1752..$1753,   // # Mn   [2] BUHID VOWEL SIGN I..BUHID VOWEL SIGN U
    $1772..$1773,   // # Mn   [2] TAGBANWA VOWEL SIGN I..TAGBANWA VOWEL SIGN U
    $17B4..$17B6,   // # Mc   [3] KHMER VOWEL INHERENT AQ..KHMER VOWEL SIGN AA
    $17B7..$17BD,   // # Mn   [7] KHMER VOWEL SIGN I..KHMER VOWEL SIGN UA
    $17BE..$17C5,   // # Mc   [8] KHMER VOWEL SIGN OE..KHMER VOWEL SIGN AU
    $17C6,          // # Mn       KHMER SIGN NIKAHIT
    $17C7..$17C8,   // # Mc   [2] KHMER SIGN REAHMUK..KHMER SIGN YUUKALEAPINTU
    $17D7,          // # Lm       KHMER SIGN LEK TOO
    $1843,          // # Lm       MONGOLIAN LETTER TODO LONG VOWEL SIGN
    $18A9,          // # Mn       MONGOLIAN LETTER ALI GALI DAGALGA
    $2160..$2183,   // # Nl  [36] ROMAN NUMERAL ONE..ROMAN NUMERAL REVERSED ONE HUNDRED
    $3005,          // # Lm       IDEOGRAPHIC ITERATION MARK
    $3007,          // # Nl       IDEOGRAPHIC NUMBER ZERO
    $3021..$3029,   // # Nl   [9] HANGZHOU NUMERAL ONE..HANGZHOU NUMERAL NINE
    $3031..$3035,   // # Lm   [5] VERTICAL KANA REPEAT MARK..VERTICAL KANA REPEAT MARK LOWER HALF
    $3038..$303A,   // # Nl   [3] HANGZHOU NUMERAL TEN..HANGZHOU NUMERAL THIRTY
    $303B,          // # Lm       VERTICAL IDEOGRAPHIC ITERATION MARK
    $309D..$309E,   // # Lm   [2] HIRAGANA ITERATION MARK..HIRAGANA VOICED ITERATION MARK
    $30FC..$30FE,   // # Lm   [3] KATAKANA-HIRAGANA PROLONGED SOUND MARK..KATAKANA VOICED ITERATION MARK
    $FB1E,          // # Mn       HEBREW POINT JUDEO-SPANISH VARIKA
    $FF70,          // # Lm       HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
    $FF9E..$FF9F :  // # Lm   [2] HALFWIDTH KATAKANA VOICED SOUND MARK..HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
      Result := True;
  else
    Result := False;
  end;
end;

function GetCombiningClass(const Ch: WideChar): Byte;
begin
  if Ord(Ch) < $0300 then
    Result := 0 else
  case Ord(Ch) of
    $0300..$0319 : Result := 230;
    $031A        : Result := 232;
    $031B        : Result := 216;
    $031C..$0320 : Result := 220;
    $0321..$0322 : Result := 202;
    $0323..$0326 : Result := 220;
    $0327..$0328 : Result := 202;
    $0329..$0333 : Result := 220;
    $0334..$0338 : Result := 1;
    $0339..$033C : Result := 220;
    $033D..$0344 : Result := 230;
    $0345        : Result := 240;
    $0346        : Result := 230;
    $0347..$0349 : Result := 220;
    $034A..$034C : Result := 230;
    $034D..$034E : Result := 220;
    $0360..$0361 : Result := 234;
    $0362        : Result := 233;
    $0483..$0486 : Result := 230;
    $0591        : Result := 220;
    $0592..$0595 : Result := 230;
    $0596        : Result := 220;
    $0597..$0599 : Result := 230;
    $059A        : Result := 222;
    $059B        : Result := 220;
    $059C..$05A1 : Result := 230;
    $05A3..$05A4 : Result := 220;
    $05A8..$05A9 : Result := 230;
    $05AA        : Result := 220;
    $05AB..$05AC : Result := 230;
    $05AD        : Result := 222;
    $05AE        : Result := 228;
    $05AF        : Result := 230;
    $05B0..$05B9 : Result := Ord(Ch) - $05B0 + 10;
    $05BB        : Result := 20;
    $05BC        : Result := 21;
    $05BD        : Result := 22;
    $05BF        : Result := 23;
    $05C1        : Result := 24;
    $05C2        : Result := 25;
    $05C4        : Result := 230;
    $064B..$0652 : Result := Ord(Ch) - $064B + 27;
    $0653..$0654 : Result := 230;
    $0655        : Result := 220;
    $0670        : Result := 35;
    $06D6..$06DC : Result := 230;
    $06DF..$06E2 : Result := 230;
    $06E3        : Result := 220;
    $06E4        : Result := 230;
    $06E7..$06E8 : Result := 230;
    $06EA        : Result := 220;
    $06EB..$06EC : Result := 230;
    $06ED        : Result := 220;
    $0711        : Result := 36;
    $0730        : Result := 230;
    $0731        : Result := 220;
    $0732..$0733 : Result := 230;
    $0734        : Result := 220;
    $0735..$0736 : Result := 230;
    $0737..$0739 : Result := 220;
    $073A        : Result := 230;
    $073B..$073C : Result := 220;
    $073D        : Result := 230;
    $073E        : Result := 220;
    $073F..$0741 : Result := 230;
    $0742        : Result := 220;
    $0743        : Result := 230;
    $0744        : Result := 220;
    $0745        : Result := 230;
    $0746        : Result := 220;
    $0747        : Result := 230;
    $0748        : Result := 220;
    $0749..$074A : Result := 230;
    $093C        : Result := 7;
    $094D        : Result := 9;
    $0951        : Result := 230;
    $0952        : Result := 220;
    $0953..$0954 : Result := 230;
    $09BC        : Result := 7;
    $09CD        : Result := 9;
    $0A3C        : Result := 7;
    $0A4D        : Result := 9;
    $0ABC        : Result := 7;
    $0ACD        : Result := 9;
    $0B3C        : Result := 7;
    $0B4D        : Result := 9;
    $0BCD        : Result := 9;
    $0C4D        : Result := 9;
    $0C55        : Result := 84;
    $0C56        : Result := 91;
    $0CCD        : Result := 9;
    $0D4D        : Result := 9;
    $0DCA        : Result := 9;
    $0E38..$0E39 : Result := 103;
    $0E3A        : Result := 9;
    $0E48..$0E4B : Result := 107;
    $0EB8..$0EB9 : Result := 118;
    $0EC8..$0ECB : Result := 122;
    $0F18..$0F19 : Result := 220;
    $0F35        : Result := 220;
    $0F37        : Result := 220;
    $0F39        : Result := 216;
    $0F71        : Result := 129;
    $0F72        : Result := 130;
    $0F74        : Result := 132;
    $0F7A..$0F7D : Result := 130;
    $0F80        : Result := 130;
    $0F82..$0F83 : Result := 230;
    $0F84        : Result := 9;
    $0F86..$0F87 : Result := 230;
    $0FC6        : Result := 220;
    $1037        : Result := 7;
    $1039        : Result := 9;
    $17D2        : Result := 9;
    $18A9        : Result := 228;
    $20D0..$20D1 : Result := 230;
    $20D2..$20D3 : Result := 1;
    $20D4..$20D7 : Result := 230;
    $20D8..$20DA : Result := 1;
    $20DB..$20DC : Result := 230;
    $20E1        : Result := 230;
    $302A        : Result := 218;
    $302B        : Result := 228;
    $302C        : Result := 232;
    $302D        : Result := 222;
    $302E..$302F : Result := 224;
    $3099        : Result := 8;
    $309A        : Result := 8;
    $FB1E        : Result := 26;
    $FE20..$FE23 : Result := 230;
  else
    Result := 0;
  end;
end;

type
  TUnicodeDecompositionAttr = (daNone, daNoBreak, daCompat, daSuper,
      daFraction, daSub, daFont, daCircle, daWide, daSquare, daIsolated,
      daInitial, daFinal, daMedial, daVertical, daSmall, daNarrow);
  TUnicodeDecompositionInfo = packed record
    Unicode : WideChar;
    Attr    : TUnicodeDecompositionAttr;
    Ch1     : WideChar;
    Ch2     : WideChar;
    Ch3     : WideChar;
    Ch4     : WideChar;
    Ch5     : WideChar;
  end;
  PUnicodeDecompositionInfo = ^TUnicodeDecompositionInfo;

const
  UnicodeDecompositionEntries = 3481; // ~ 45K
  UnicodeDecompositionInfo : Array[0..UnicodeDecompositionEntries - 1] of TUnicodeDecompositionInfo = (
    (Unicode:#$00A0; Attr:daNoBreak; Ch1:#$0020; Ch2:#$FFFF),                          // NO-BREAK SPACE
    (Unicode:#$00A8; Attr:daCompat; Ch1:#$0020; Ch2:#$0308; Ch3:#$FFFF),               // DIAERESIS
    (Unicode:#$00AA; Attr:daSuper; Ch1:#$0061; Ch2:#$FFFF),                            // FEMININE ORDINAL INDICATOR
    (Unicode:#$00AF; Attr:daCompat; Ch1:#$0020; Ch2:#$0304; Ch3:#$FFFF),               // MACRON
    (Unicode:#$00B2; Attr:daSuper; Ch1:#$0032; Ch2:#$FFFF),                            // SUPERSCRIPT TWO
    (Unicode:#$00B3; Attr:daSuper; Ch1:#$0033; Ch2:#$FFFF),                            // SUPERSCRIPT THREE
    (Unicode:#$00B4; Attr:daCompat; Ch1:#$0020; Ch2:#$0301; Ch3:#$FFFF),               // ACUTE ACCENT
    (Unicode:#$00B5; Attr:daCompat; Ch1:#$03BC; Ch2:#$FFFF),                           // MICRO SIGN
    (Unicode:#$00B8; Attr:daCompat; Ch1:#$0020; Ch2:#$0327; Ch3:#$FFFF),               // CEDILLA
    (Unicode:#$00B9; Attr:daSuper; Ch1:#$0031; Ch2:#$FFFF),                            // SUPERSCRIPT ONE
    (Unicode:#$00BA; Attr:daSuper; Ch1:#$006F; Ch2:#$FFFF),                            // MASCULINE ORDINAL INDICATOR
    (Unicode:#$00BC; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$0034; Ch4:#$FFFF), // VULGAR FRACTION ONE QUARTER
    (Unicode:#$00BD; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$0032; Ch4:#$FFFF), // VULGAR FRACTION ONE HALF
    (Unicode:#$00BE; Attr:daFraction; Ch1:#$0033; Ch2:#$2044; Ch3:#$0034; Ch4:#$FFFF), // VULGAR FRACTION THREE QUARTERS
    (Unicode:#$00C0; Attr:daNone; Ch1:#$0041; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH GRAVE
    (Unicode:#$00C1; Attr:daNone; Ch1:#$0041; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH ACUTE
    (Unicode:#$00C2; Attr:daNone; Ch1:#$0041; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00C3; Attr:daNone; Ch1:#$0041; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH TILDE
    (Unicode:#$00C4; Attr:daNone; Ch1:#$0041; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$00C5; Attr:daNone; Ch1:#$0041; Ch2:#$030A; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH RING ABOVE
    (Unicode:#$00C7; Attr:daNone; Ch1:#$0043; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER C WITH CEDILLA
    (Unicode:#$00C8; Attr:daNone; Ch1:#$0045; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH GRAVE
    (Unicode:#$00C9; Attr:daNone; Ch1:#$0045; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH ACUTE
    (Unicode:#$00CA; Attr:daNone; Ch1:#$0045; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00CB; Attr:daNone; Ch1:#$0045; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$00CC; Attr:daNone; Ch1:#$0049; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH GRAVE
    (Unicode:#$00CD; Attr:daNone; Ch1:#$0049; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH ACUTE
    (Unicode:#$00CE; Attr:daNone; Ch1:#$0049; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00CF; Attr:daNone; Ch1:#$0049; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$00D1; Attr:daNone; Ch1:#$004E; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH TILDE
    (Unicode:#$00D2; Attr:daNone; Ch1:#$004F; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH GRAVE
    (Unicode:#$00D3; Attr:daNone; Ch1:#$004F; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH ACUTE
    (Unicode:#$00D4; Attr:daNone; Ch1:#$004F; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00D5; Attr:daNone; Ch1:#$004F; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH TILDE
    (Unicode:#$00D6; Attr:daNone; Ch1:#$004F; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$00D9; Attr:daNone; Ch1:#$0055; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH GRAVE
    (Unicode:#$00DA; Attr:daNone; Ch1:#$0055; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH ACUTE
    (Unicode:#$00DB; Attr:daNone; Ch1:#$0055; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00DC; Attr:daNone; Ch1:#$0055; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$00DD; Attr:daNone; Ch1:#$0059; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH ACUTE
    (Unicode:#$00E0; Attr:daNone; Ch1:#$0061; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH GRAVE
    (Unicode:#$00E1; Attr:daNone; Ch1:#$0061; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH ACUTE
    (Unicode:#$00E2; Attr:daNone; Ch1:#$0061; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CIRCUMFLEX
    (Unicode:#$00E3; Attr:daNone; Ch1:#$0061; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH TILDE
    (Unicode:#$00E4; Attr:daNone; Ch1:#$0061; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH DIAERESIS
    (Unicode:#$00E5; Attr:daNone; Ch1:#$0061; Ch2:#$030A; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH RING ABOVE
    (Unicode:#$00E7; Attr:daNone; Ch1:#$0063; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER C WITH CEDILLA
    (Unicode:#$00E8; Attr:daNone; Ch1:#$0065; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH GRAVE
    (Unicode:#$00E9; Attr:daNone; Ch1:#$0065; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH ACUTE
    (Unicode:#$00EA; Attr:daNone; Ch1:#$0065; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX
    (Unicode:#$00EB; Attr:daNone; Ch1:#$0065; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH DIAERESIS
    (Unicode:#$00EC; Attr:daNone; Ch1:#$0069; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH GRAVE
    (Unicode:#$00ED; Attr:daNone; Ch1:#$0069; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH ACUTE
    (Unicode:#$00EE; Attr:daNone; Ch1:#$0069; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH CIRCUMFLEX
    (Unicode:#$00EF; Attr:daNone; Ch1:#$0069; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH DIAERESIS
    (Unicode:#$00F1; Attr:daNone; Ch1:#$006E; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH TILDE
    (Unicode:#$00F2; Attr:daNone; Ch1:#$006F; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH GRAVE
    (Unicode:#$00F3; Attr:daNone; Ch1:#$006F; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH ACUTE
    (Unicode:#$00F4; Attr:daNone; Ch1:#$006F; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CIRCUMFLEX
    (Unicode:#$00F5; Attr:daNone; Ch1:#$006F; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH TILDE
    (Unicode:#$00F6; Attr:daNone; Ch1:#$006F; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DIAERESIS
    (Unicode:#$00F9; Attr:daNone; Ch1:#$0075; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH GRAVE
    (Unicode:#$00FA; Attr:daNone; Ch1:#$0075; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH ACUTE
    (Unicode:#$00FB; Attr:daNone; Ch1:#$0075; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH CIRCUMFLEX
    (Unicode:#$00FC; Attr:daNone; Ch1:#$0075; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DIAERESIS
    (Unicode:#$00FD; Attr:daNone; Ch1:#$0079; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH ACUTE
    (Unicode:#$00FF; Attr:daNone; Ch1:#$0079; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH DIAERESIS
    (Unicode:#$0100; Attr:daNone; Ch1:#$0041; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH MACRON
    (Unicode:#$0101; Attr:daNone; Ch1:#$0061; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH MACRON
    (Unicode:#$0102; Attr:daNone; Ch1:#$0041; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH BREVE
    (Unicode:#$0103; Attr:daNone; Ch1:#$0061; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH BREVE
    (Unicode:#$0104; Attr:daNone; Ch1:#$0041; Ch2:#$0328; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH OGONEK
    (Unicode:#$0105; Attr:daNone; Ch1:#$0061; Ch2:#$0328; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH OGONEK
    (Unicode:#$0106; Attr:daNone; Ch1:#$0043; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER C WITH ACUTE
    (Unicode:#$0107; Attr:daNone; Ch1:#$0063; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER C WITH ACUTE
    (Unicode:#$0108; Attr:daNone; Ch1:#$0043; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER C WITH CIRCUMFLEX
    (Unicode:#$0109; Attr:daNone; Ch1:#$0063; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER C WITH CIRCUMFLEX
    (Unicode:#$010A; Attr:daNone; Ch1:#$0043; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER C WITH DOT ABOVE
    (Unicode:#$010B; Attr:daNone; Ch1:#$0063; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER C WITH DOT ABOVE
    (Unicode:#$010C; Attr:daNone; Ch1:#$0043; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER C WITH CARON
    (Unicode:#$010D; Attr:daNone; Ch1:#$0063; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER C WITH CARON
    (Unicode:#$010E; Attr:daNone; Ch1:#$0044; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER D WITH CARON
    (Unicode:#$010F; Attr:daNone; Ch1:#$0064; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER D WITH CARON
    (Unicode:#$0112; Attr:daNone; Ch1:#$0045; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH MACRON
    (Unicode:#$0113; Attr:daNone; Ch1:#$0065; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH MACRON
    (Unicode:#$0114; Attr:daNone; Ch1:#$0045; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH BREVE
    (Unicode:#$0115; Attr:daNone; Ch1:#$0065; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH BREVE
    (Unicode:#$0116; Attr:daNone; Ch1:#$0045; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH DOT ABOVE
    (Unicode:#$0117; Attr:daNone; Ch1:#$0065; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH DOT ABOVE
    (Unicode:#$0118; Attr:daNone; Ch1:#$0045; Ch2:#$0328; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH OGONEK
    (Unicode:#$0119; Attr:daNone; Ch1:#$0065; Ch2:#$0328; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH OGONEK
    (Unicode:#$011A; Attr:daNone; Ch1:#$0045; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CARON
    (Unicode:#$011B; Attr:daNone; Ch1:#$0065; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CARON
    (Unicode:#$011C; Attr:daNone; Ch1:#$0047; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011D; Attr:daNone; Ch1:#$0067; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH CIRCUMFLEX
    (Unicode:#$011E; Attr:daNone; Ch1:#$0047; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH BREVE
    (Unicode:#$011F; Attr:daNone; Ch1:#$0067; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH BREVE
    (Unicode:#$0120; Attr:daNone; Ch1:#$0047; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH DOT ABOVE
    (Unicode:#$0121; Attr:daNone; Ch1:#$0067; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH DOT ABOVE
    (Unicode:#$0122; Attr:daNone; Ch1:#$0047; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH CEDILLA
    (Unicode:#$0123; Attr:daNone; Ch1:#$0067; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH CEDILLA
    (Unicode:#$0124; Attr:daNone; Ch1:#$0048; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0125; Attr:daNone; Ch1:#$0068; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH CIRCUMFLEX
    (Unicode:#$0128; Attr:daNone; Ch1:#$0049; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH TILDE
    (Unicode:#$0129; Attr:daNone; Ch1:#$0069; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH TILDE
    (Unicode:#$012A; Attr:daNone; Ch1:#$0049; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH MACRON
    (Unicode:#$012B; Attr:daNone; Ch1:#$0069; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH MACRON
    (Unicode:#$012C; Attr:daNone; Ch1:#$0049; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH BREVE
    (Unicode:#$012D; Attr:daNone; Ch1:#$0069; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH BREVE
    (Unicode:#$012E; Attr:daNone; Ch1:#$0049; Ch2:#$0328; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH OGONEK
    (Unicode:#$012F; Attr:daNone; Ch1:#$0069; Ch2:#$0328; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH OGONEK
    (Unicode:#$0130; Attr:daNone; Ch1:#$0049; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH DOT ABOVE
    (Unicode:#$0132; Attr:daCompat; Ch1:#$0049; Ch2:#$004A; Ch3:#$FFFF),  // LATIN CAPITAL LIGATURE IJ
    (Unicode:#$0133; Attr:daCompat; Ch1:#$0069; Ch2:#$006A; Ch3:#$FFFF),  // LATIN SMALL LIGATURE IJ
    (Unicode:#$0134; Attr:daNone; Ch1:#$004A; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0135; Attr:daNone; Ch1:#$006A; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER J WITH CIRCUMFLEX
    (Unicode:#$0136; Attr:daNone; Ch1:#$004B; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER K WITH CEDILLA
    (Unicode:#$0137; Attr:daNone; Ch1:#$006B; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER K WITH CEDILLA
    (Unicode:#$0139; Attr:daNone; Ch1:#$004C; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH ACUTE
    (Unicode:#$013A; Attr:daNone; Ch1:#$006C; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH ACUTE
    (Unicode:#$013B; Attr:daNone; Ch1:#$004C; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH CEDILLA
    (Unicode:#$013C; Attr:daNone; Ch1:#$006C; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH CEDILLA
    (Unicode:#$013D; Attr:daNone; Ch1:#$004C; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH CARON
    (Unicode:#$013E; Attr:daNone; Ch1:#$006C; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH CARON
    (Unicode:#$013F; Attr:daCompat; Ch1:#$004C; Ch2:#$00B7; Ch3:#$FFFF),  // LATIN CAPITAL LETTER L WITH MIDDLE DOT
    (Unicode:#$0140; Attr:daCompat; Ch1:#$006C; Ch2:#$00B7; Ch3:#$FFFF),  // LATIN SMALL LETTER L WITH MIDDLE DOT
    (Unicode:#$0143; Attr:daNone; Ch1:#$004E; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH ACUTE
    (Unicode:#$0144; Attr:daNone; Ch1:#$006E; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH ACUTE
    (Unicode:#$0145; Attr:daNone; Ch1:#$004E; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH CEDILLA
    (Unicode:#$0146; Attr:daNone; Ch1:#$006E; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH CEDILLA
    (Unicode:#$0147; Attr:daNone; Ch1:#$004E; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH CARON
    (Unicode:#$0148; Attr:daNone; Ch1:#$006E; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH CARON
    (Unicode:#$0149; Attr:daCompat; Ch1:#$02BC; Ch2:#$006E; Ch3:#$FFFF),  // LATIN SMALL LETTER N PRECEDED BY APOSTROPHE
    (Unicode:#$014C; Attr:daNone; Ch1:#$004F; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH MACRON
    (Unicode:#$014D; Attr:daNone; Ch1:#$006F; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH MACRON
    (Unicode:#$014E; Attr:daNone; Ch1:#$004F; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH BREVE
    (Unicode:#$014F; Attr:daNone; Ch1:#$006F; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH BREVE
    (Unicode:#$0150; Attr:daNone; Ch1:#$004F; Ch2:#$030B; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0151; Attr:daNone; Ch1:#$006F; Ch2:#$030B; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DOUBLE ACUTE
    (Unicode:#$0154; Attr:daNone; Ch1:#$0052; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH ACUTE
    (Unicode:#$0155; Attr:daNone; Ch1:#$0072; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH ACUTE
    (Unicode:#$0156; Attr:daNone; Ch1:#$0052; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH CEDILLA
    (Unicode:#$0157; Attr:daNone; Ch1:#$0072; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH CEDILLA
    (Unicode:#$0158; Attr:daNone; Ch1:#$0052; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH CARON
    (Unicode:#$0159; Attr:daNone; Ch1:#$0072; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH CARON
    (Unicode:#$015A; Attr:daNone; Ch1:#$0053; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH ACUTE
    (Unicode:#$015B; Attr:daNone; Ch1:#$0073; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH ACUTE
    (Unicode:#$015C; Attr:daNone; Ch1:#$0053; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015D; Attr:daNone; Ch1:#$0073; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH CIRCUMFLEX
    (Unicode:#$015E; Attr:daNone; Ch1:#$0053; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH CEDILLA
    (Unicode:#$015F; Attr:daNone; Ch1:#$0073; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH CEDILLA
    (Unicode:#$0160; Attr:daNone; Ch1:#$0053; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH CARON
    (Unicode:#$0161; Attr:daNone; Ch1:#$0073; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH CARON
    (Unicode:#$0162; Attr:daNone; Ch1:#$0054; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH CEDILLA
    (Unicode:#$0163; Attr:daNone; Ch1:#$0074; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH CEDILLA
    (Unicode:#$0164; Attr:daNone; Ch1:#$0054; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH CARON
    (Unicode:#$0165; Attr:daNone; Ch1:#$0074; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH CARON
    (Unicode:#$0168; Attr:daNone; Ch1:#$0055; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH TILDE
    (Unicode:#$0169; Attr:daNone; Ch1:#$0075; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH TILDE
    (Unicode:#$016A; Attr:daNone; Ch1:#$0055; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH MACRON
    (Unicode:#$016B; Attr:daNone; Ch1:#$0075; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH MACRON
    (Unicode:#$016C; Attr:daNone; Ch1:#$0055; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH BREVE
    (Unicode:#$016D; Attr:daNone; Ch1:#$0075; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH BREVE
    (Unicode:#$016E; Attr:daNone; Ch1:#$0055; Ch2:#$030A; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH RING ABOVE
    (Unicode:#$016F; Attr:daNone; Ch1:#$0075; Ch2:#$030A; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH RING ABOVE
    (Unicode:#$0170; Attr:daNone; Ch1:#$0055; Ch2:#$030B; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0171; Attr:daNone; Ch1:#$0075; Ch2:#$030B; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$0172; Attr:daNone; Ch1:#$0055; Ch2:#$0328; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH OGONEK
    (Unicode:#$0173; Attr:daNone; Ch1:#$0075; Ch2:#$0328; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH OGONEK
    (Unicode:#$0174; Attr:daNone; Ch1:#$0057; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0175; Attr:daNone; Ch1:#$0077; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH CIRCUMFLEX
    (Unicode:#$0176; Attr:daNone; Ch1:#$0059; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0177; Attr:daNone; Ch1:#$0079; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH CIRCUMFLEX
    (Unicode:#$0178; Attr:daNone; Ch1:#$0059; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH DIAERESIS
    (Unicode:#$0179; Attr:daNone; Ch1:#$005A; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Z WITH ACUTE
    (Unicode:#$017A; Attr:daNone; Ch1:#$007A; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER Z WITH ACUTE
    (Unicode:#$017B; Attr:daNone; Ch1:#$005A; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Z WITH DOT ABOVE
    (Unicode:#$017C; Attr:daNone; Ch1:#$007A; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER Z WITH DOT ABOVE
    (Unicode:#$017D; Attr:daNone; Ch1:#$005A; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Z WITH CARON
    (Unicode:#$017E; Attr:daNone; Ch1:#$007A; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER Z WITH CARON
    (Unicode:#$017F; Attr:daCompat; Ch1:#$0073; Ch2:#$FFFF),              // LATIN SMALL LETTER LONG S
    (Unicode:#$01A0; Attr:daNone; Ch1:#$004F; Ch2:#$031B; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HORN
    (Unicode:#$01A1; Attr:daNone; Ch1:#$006F; Ch2:#$031B; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HORN
    (Unicode:#$01AF; Attr:daNone; Ch1:#$0055; Ch2:#$031B; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HORN
    (Unicode:#$01B0; Attr:daNone; Ch1:#$0075; Ch2:#$031B; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HORN
    (Unicode:#$01C4; Attr:daCompat; Ch1:#$0044; Ch2:#$017D; Ch3:#$FFFF),  // LATIN CAPITAL LETTER DZ WITH CARON
    (Unicode:#$01C5; Attr:daCompat; Ch1:#$0044; Ch2:#$017E; Ch3:#$FFFF),  // LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON
    (Unicode:#$01C6; Attr:daCompat; Ch1:#$0064; Ch2:#$017E; Ch3:#$FFFF),  // LATIN SMALL LETTER DZ WITH CARON
    (Unicode:#$01C7; Attr:daCompat; Ch1:#$004C; Ch2:#$004A; Ch3:#$FFFF),  // LATIN CAPITAL LETTER LJ
    (Unicode:#$01C8; Attr:daCompat; Ch1:#$004C; Ch2:#$006A; Ch3:#$FFFF),  // LATIN CAPITAL LETTER L WITH SMALL LETTER J
    (Unicode:#$01C9; Attr:daCompat; Ch1:#$006C; Ch2:#$006A; Ch3:#$FFFF),  // LATIN SMALL LETTER LJ
    (Unicode:#$01CA; Attr:daCompat; Ch1:#$004E; Ch2:#$004A; Ch3:#$FFFF),  // LATIN CAPITAL LETTER NJ
    (Unicode:#$01CB; Attr:daCompat; Ch1:#$004E; Ch2:#$006A; Ch3:#$FFFF),  // LATIN CAPITAL LETTER N WITH SMALL LETTER J
    (Unicode:#$01CC; Attr:daCompat; Ch1:#$006E; Ch2:#$006A; Ch3:#$FFFF),  // LATIN SMALL LETTER NJ
    (Unicode:#$01CD; Attr:daNone; Ch1:#$0041; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CARON
    (Unicode:#$01CE; Attr:daNone; Ch1:#$0061; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CARON
    (Unicode:#$01CF; Attr:daNone; Ch1:#$0049; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH CARON
    (Unicode:#$01D0; Attr:daNone; Ch1:#$0069; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH CARON
    (Unicode:#$01D1; Attr:daNone; Ch1:#$004F; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CARON
    (Unicode:#$01D2; Attr:daNone; Ch1:#$006F; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CARON
    (Unicode:#$01D3; Attr:daNone; Ch1:#$0055; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH CARON
    (Unicode:#$01D4; Attr:daNone; Ch1:#$0075; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH CARON
    (Unicode:#$01D5; Attr:daNone; Ch1:#$00DC; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D6; Attr:daNone; Ch1:#$00FC; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DIAERESIS AND MACRON
    (Unicode:#$01D7; Attr:daNone; Ch1:#$00DC; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D8; Attr:daNone; Ch1:#$00FC; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DIAERESIS AND ACUTE
    (Unicode:#$01D9; Attr:daNone; Ch1:#$00DC; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DA; Attr:daNone; Ch1:#$00FC; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DIAERESIS AND CARON
    (Unicode:#$01DB; Attr:daNone; Ch1:#$00DC; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DC; Attr:daNone; Ch1:#$00FC; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DIAERESIS AND GRAVE
    (Unicode:#$01DE; Attr:daNone; Ch1:#$00C4; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01DF; Attr:daNone; Ch1:#$00E4; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH DIAERESIS AND MACRON
    (Unicode:#$01E0; Attr:daNone; Ch1:#$0226; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E1; Attr:daNone; Ch1:#$0227; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH DOT ABOVE AND MACRON
    (Unicode:#$01E2; Attr:daNone; Ch1:#$00C6; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER AE WITH MACRON
    (Unicode:#$01E3; Attr:daNone; Ch1:#$00E6; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER AE WITH MACRON
    (Unicode:#$01E6; Attr:daNone; Ch1:#$0047; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH CARON
    (Unicode:#$01E7; Attr:daNone; Ch1:#$0067; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH CARON
    (Unicode:#$01E8; Attr:daNone; Ch1:#$004B; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER K WITH CARON
    (Unicode:#$01E9; Attr:daNone; Ch1:#$006B; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER K WITH CARON
    (Unicode:#$01EA; Attr:daNone; Ch1:#$004F; Ch2:#$0328; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH OGONEK
    (Unicode:#$01EB; Attr:daNone; Ch1:#$006F; Ch2:#$0328; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH OGONEK
    (Unicode:#$01EC; Attr:daNone; Ch1:#$01EA; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01ED; Attr:daNone; Ch1:#$01EB; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH OGONEK AND MACRON
    (Unicode:#$01EE; Attr:daNone; Ch1:#$01B7; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER EZH WITH CARON
    (Unicode:#$01EF; Attr:daNone; Ch1:#$0292; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER EZH WITH CARON
    (Unicode:#$01F0; Attr:daNone; Ch1:#$006A; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER J WITH CARON
    (Unicode:#$01F1; Attr:daCompat; Ch1:#$0044; Ch2:#$005A; Ch3:#$FFFF),  // LATIN CAPITAL LETTER DZ
    (Unicode:#$01F2; Attr:daCompat; Ch1:#$0044; Ch2:#$007A; Ch3:#$FFFF),  // LATIN CAPITAL LETTER D WITH SMALL LETTER Z
    (Unicode:#$01F3; Attr:daCompat; Ch1:#$0064; Ch2:#$007A; Ch3:#$FFFF),  // LATIN SMALL LETTER DZ
    (Unicode:#$01F4; Attr:daNone; Ch1:#$0047; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH ACUTE
    (Unicode:#$01F5; Attr:daNone; Ch1:#$0067; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH ACUTE
    (Unicode:#$01F8; Attr:daNone; Ch1:#$004E; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH GRAVE
    (Unicode:#$01F9; Attr:daNone; Ch1:#$006E; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH GRAVE
    (Unicode:#$01FA; Attr:daNone; Ch1:#$00C5; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FB; Attr:daNone; Ch1:#$00E5; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH RING ABOVE AND ACUTE
    (Unicode:#$01FC; Attr:daNone; Ch1:#$00C6; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER AE WITH ACUTE
    (Unicode:#$01FD; Attr:daNone; Ch1:#$00E6; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER AE WITH ACUTE
    (Unicode:#$01FE; Attr:daNone; Ch1:#$00D8; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$01FF; Attr:daNone; Ch1:#$00F8; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH STROKE AND ACUTE
    (Unicode:#$0200; Attr:daNone; Ch1:#$0041; Ch2:#$030F; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0201; Attr:daNone; Ch1:#$0061; Ch2:#$030F; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH DOUBLE GRAVE
    (Unicode:#$0202; Attr:daNone; Ch1:#$0041; Ch2:#$0311; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH INVERTED BREVE
    (Unicode:#$0203; Attr:daNone; Ch1:#$0061; Ch2:#$0311; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH INVERTED BREVE
    (Unicode:#$0204; Attr:daNone; Ch1:#$0045; Ch2:#$030F; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0205; Attr:daNone; Ch1:#$0065; Ch2:#$030F; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH DOUBLE GRAVE
    (Unicode:#$0206; Attr:daNone; Ch1:#$0045; Ch2:#$0311; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH INVERTED BREVE
    (Unicode:#$0207; Attr:daNone; Ch1:#$0065; Ch2:#$0311; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH INVERTED BREVE
    (Unicode:#$0208; Attr:daNone; Ch1:#$0049; Ch2:#$030F; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$0209; Attr:daNone; Ch1:#$0069; Ch2:#$030F; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH DOUBLE GRAVE
    (Unicode:#$020A; Attr:daNone; Ch1:#$0049; Ch2:#$0311; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH INVERTED BREVE
    (Unicode:#$020B; Attr:daNone; Ch1:#$0069; Ch2:#$0311; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH INVERTED BREVE
    (Unicode:#$020C; Attr:daNone; Ch1:#$004F; Ch2:#$030F; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020D; Attr:daNone; Ch1:#$006F; Ch2:#$030F; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DOUBLE GRAVE
    (Unicode:#$020E; Attr:daNone; Ch1:#$004F; Ch2:#$0311; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH INVERTED BREVE
    (Unicode:#$020F; Attr:daNone; Ch1:#$006F; Ch2:#$0311; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH INVERTED BREVE
    (Unicode:#$0210; Attr:daNone; Ch1:#$0052; Ch2:#$030F; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0211; Attr:daNone; Ch1:#$0072; Ch2:#$030F; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH DOUBLE GRAVE
    (Unicode:#$0212; Attr:daNone; Ch1:#$0052; Ch2:#$0311; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH INVERTED BREVE
    (Unicode:#$0213; Attr:daNone; Ch1:#$0072; Ch2:#$0311; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH INVERTED BREVE
    (Unicode:#$0214; Attr:daNone; Ch1:#$0055; Ch2:#$030F; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0215; Attr:daNone; Ch1:#$0075; Ch2:#$030F; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DOUBLE GRAVE
    (Unicode:#$0216; Attr:daNone; Ch1:#$0055; Ch2:#$0311; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH INVERTED BREVE
    (Unicode:#$0217; Attr:daNone; Ch1:#$0075; Ch2:#$0311; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH INVERTED BREVE
    (Unicode:#$0218; Attr:daNone; Ch1:#$0053; Ch2:#$0326; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH COMMA BELOW
    (Unicode:#$0219; Attr:daNone; Ch1:#$0073; Ch2:#$0326; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH COMMA BELOW
    (Unicode:#$021A; Attr:daNone; Ch1:#$0054; Ch2:#$0326; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH COMMA BELOW
    (Unicode:#$021B; Attr:daNone; Ch1:#$0074; Ch2:#$0326; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH COMMA BELOW
    (Unicode:#$021E; Attr:daNone; Ch1:#$0048; Ch2:#$030C; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH CARON
    (Unicode:#$021F; Attr:daNone; Ch1:#$0068; Ch2:#$030C; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH CARON
    (Unicode:#$0226; Attr:daNone; Ch1:#$0041; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH DOT ABOVE
    (Unicode:#$0227; Attr:daNone; Ch1:#$0061; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH DOT ABOVE
    (Unicode:#$0228; Attr:daNone; Ch1:#$0045; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CEDILLA
    (Unicode:#$0229; Attr:daNone; Ch1:#$0065; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CEDILLA
    (Unicode:#$022A; Attr:daNone; Ch1:#$00D6; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022B; Attr:daNone; Ch1:#$00F6; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DIAERESIS AND MACRON
    (Unicode:#$022C; Attr:daNone; Ch1:#$00D5; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022D; Attr:daNone; Ch1:#$00F5; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH TILDE AND MACRON
    (Unicode:#$022E; Attr:daNone; Ch1:#$004F; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DOT ABOVE
    (Unicode:#$022F; Attr:daNone; Ch1:#$006F; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DOT ABOVE
    (Unicode:#$0230; Attr:daNone; Ch1:#$022E; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0231; Attr:daNone; Ch1:#$022F; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DOT ABOVE AND MACRON
    (Unicode:#$0232; Attr:daNone; Ch1:#$0059; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH MACRON
    (Unicode:#$0233; Attr:daNone; Ch1:#$0079; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH MACRON
    (Unicode:#$02B0; Attr:daSuper; Ch1:#$0068; Ch2:#$FFFF),               // MODIFIER LETTER SMALL H
    (Unicode:#$02B1; Attr:daSuper; Ch1:#$0266; Ch2:#$FFFF),               // MODIFIER LETTER SMALL H WITH HOOK
    (Unicode:#$02B2; Attr:daSuper; Ch1:#$006A; Ch2:#$FFFF),               // MODIFIER LETTER SMALL J
    (Unicode:#$02B3; Attr:daSuper; Ch1:#$0072; Ch2:#$FFFF),               // MODIFIER LETTER SMALL R
    (Unicode:#$02B4; Attr:daSuper; Ch1:#$0279; Ch2:#$FFFF),               // MODIFIER LETTER SMALL TURNED R
    (Unicode:#$02B5; Attr:daSuper; Ch1:#$027B; Ch2:#$FFFF),               // MODIFIER LETTER SMALL TURNED R WITH HOOK
    (Unicode:#$02B6; Attr:daSuper; Ch1:#$0281; Ch2:#$FFFF),               // MODIFIER LETTER SMALL CAPITAL INVERTED R
    (Unicode:#$02B7; Attr:daSuper; Ch1:#$0077; Ch2:#$FFFF),               // MODIFIER LETTER SMALL W
    (Unicode:#$02B8; Attr:daSuper; Ch1:#$0079; Ch2:#$FFFF),               // MODIFIER LETTER SMALL Y
    (Unicode:#$02D8; Attr:daCompat; Ch1:#$0020; Ch2:#$0306; Ch3:#$FFFF),  // BREVE
    (Unicode:#$02D9; Attr:daCompat; Ch1:#$0020; Ch2:#$0307; Ch3:#$FFFF),  // DOT ABOVE
    (Unicode:#$02DA; Attr:daCompat; Ch1:#$0020; Ch2:#$030A; Ch3:#$FFFF),  // RING ABOVE
    (Unicode:#$02DB; Attr:daCompat; Ch1:#$0020; Ch2:#$0328; Ch3:#$FFFF),  // OGONEK
    (Unicode:#$02DC; Attr:daCompat; Ch1:#$0020; Ch2:#$0303; Ch3:#$FFFF),  // SMALL TILDE
    (Unicode:#$02DD; Attr:daCompat; Ch1:#$0020; Ch2:#$030B; Ch3:#$FFFF),  // DOUBLE ACUTE ACCENT
    (Unicode:#$02E0; Attr:daSuper; Ch1:#$0263; Ch2:#$FFFF),               // MODIFIER LETTER SMALL GAMMA
    (Unicode:#$02E1; Attr:daSuper; Ch1:#$006C; Ch2:#$FFFF),               // MODIFIER LETTER SMALL L
    (Unicode:#$02E2; Attr:daSuper; Ch1:#$0073; Ch2:#$FFFF),               // MODIFIER LETTER SMALL S
    (Unicode:#$02E3; Attr:daSuper; Ch1:#$0078; Ch2:#$FFFF),               // MODIFIER LETTER SMALL X
    (Unicode:#$02E4; Attr:daSuper; Ch1:#$0295; Ch2:#$FFFF),               // MODIFIER LETTER SMALL REVERSED GLOTTAL STOP
    (Unicode:#$0340; Attr:daNone; Ch1:#$0300; Ch2:#$FFFF),                // COMBINING GRAVE TONE MARK
    (Unicode:#$0341; Attr:daNone; Ch1:#$0301; Ch2:#$FFFF),                // COMBINING ACUTE TONE MARK
    (Unicode:#$0343; Attr:daNone; Ch1:#$0313; Ch2:#$FFFF),                // COMBINING GREEK KORONIS
    (Unicode:#$0344; Attr:daNone; Ch1:#$0308; Ch2:#$0301; Ch3:#$FFFF),    // COMBINING GREEK DIALYTIKA TONOS
    (Unicode:#$0374; Attr:daNone; Ch1:#$02B9; Ch2:#$FFFF),                // GREEK NUMERAL SIGN
    (Unicode:#$037A; Attr:daCompat; Ch1:#$0020; Ch2:#$0345; Ch3:#$FFFF),  // GREEK YPOGEGRAMMENI
    (Unicode:#$037E; Attr:daNone; Ch1:#$003B; Ch2:#$FFFF),                // GREEK QUESTION MARK
    (Unicode:#$0384; Attr:daCompat; Ch1:#$0020; Ch2:#$0301; Ch3:#$FFFF),  // GREEK TONOS
    (Unicode:#$0385; Attr:daNone; Ch1:#$00A8; Ch2:#$0301; Ch3:#$FFFF),    // GREEK DIALYTIKA TONOS
    (Unicode:#$0386; Attr:daNone; Ch1:#$0391; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH TONOS
    (Unicode:#$0387; Attr:daNone; Ch1:#$00B7; Ch2:#$FFFF),                // GREEK ANO TELEIA
    (Unicode:#$0388; Attr:daNone; Ch1:#$0395; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH TONOS
    (Unicode:#$0389; Attr:daNone; Ch1:#$0397; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH TONOS
    (Unicode:#$038A; Attr:daNone; Ch1:#$0399; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH TONOS
    (Unicode:#$038C; Attr:daNone; Ch1:#$039F; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH TONOS
    (Unicode:#$038E; Attr:daNone; Ch1:#$03A5; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH TONOS
    (Unicode:#$038F; Attr:daNone; Ch1:#$03A9; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH TONOS
    (Unicode:#$0390; Attr:daNone; Ch1:#$03CA; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND TONOS
    (Unicode:#$03AA; Attr:daNone; Ch1:#$0399; Ch2:#$0308; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03AB; Attr:daNone; Ch1:#$03A5; Ch2:#$0308; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03AC; Attr:daNone; Ch1:#$03B1; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH TONOS
    (Unicode:#$03AD; Attr:daNone; Ch1:#$03B5; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH TONOS
    (Unicode:#$03AE; Attr:daNone; Ch1:#$03B7; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH TONOS
    (Unicode:#$03AF; Attr:daNone; Ch1:#$03B9; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH TONOS
    (Unicode:#$03B0; Attr:daNone; Ch1:#$03CB; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND TONOS
    (Unicode:#$03CA; Attr:daNone; Ch1:#$03B9; Ch2:#$0308; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA
    (Unicode:#$03CB; Attr:daNone; Ch1:#$03C5; Ch2:#$0308; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA
    (Unicode:#$03CC; Attr:daNone; Ch1:#$03BF; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH TONOS
    (Unicode:#$03CD; Attr:daNone; Ch1:#$03C5; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH TONOS
    (Unicode:#$03CE; Attr:daNone; Ch1:#$03C9; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH TONOS
    (Unicode:#$03D0; Attr:daCompat; Ch1:#$03B2; Ch2:#$FFFF),              // GREEK BETA SYMBOL
    (Unicode:#$03D1; Attr:daCompat; Ch1:#$03B8; Ch2:#$FFFF),              // GREEK THETA SYMBOL
    (Unicode:#$03D2; Attr:daCompat; Ch1:#$03A5; Ch2:#$FFFF),              // GREEK UPSILON WITH HOOK SYMBOL
    (Unicode:#$03D3; Attr:daNone; Ch1:#$03D2; Ch2:#$0301; Ch3:#$FFFF),    // GREEK UPSILON WITH ACUTE AND HOOK SYMBOL
    (Unicode:#$03D4; Attr:daNone; Ch1:#$03D2; Ch2:#$0308; Ch3:#$FFFF),    // GREEK UPSILON WITH DIAERESIS AND HOOK SYMBOL
    (Unicode:#$03D5; Attr:daCompat; Ch1:#$03C6; Ch2:#$FFFF),              // GREEK PHI SYMBOL
    (Unicode:#$03D6; Attr:daCompat; Ch1:#$03C0; Ch2:#$FFFF),              // GREEK PI SYMBOL
    (Unicode:#$03F0; Attr:daCompat; Ch1:#$03BA; Ch2:#$FFFF),              // GREEK KAPPA SYMBOL
    (Unicode:#$03F1; Attr:daCompat; Ch1:#$03C1; Ch2:#$FFFF),              // GREEK RHO SYMBOL
    (Unicode:#$03F2; Attr:daCompat; Ch1:#$03C2; Ch2:#$FFFF),              // GREEK LUNATE SIGMA SYMBOL
    (Unicode:#$03F4; Attr:daCompat; Ch1:#$0398; Ch2:#$FFFF),              // GREEK CAPITAL THETA SYMBOL
    (Unicode:#$03F5; Attr:daCompat; Ch1:#$03B5; Ch2:#$FFFF),              // GREEK LUNATE EPSILON SYMBOL
    (Unicode:#$0400; Attr:daNone; Ch1:#$0415; Ch2:#$0300; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER IE WITH GRAVE
    (Unicode:#$0401; Attr:daNone; Ch1:#$0415; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER IO
    (Unicode:#$0403; Attr:daNone; Ch1:#$0413; Ch2:#$0301; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER GJE
    (Unicode:#$0407; Attr:daNone; Ch1:#$0406; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER YI
    (Unicode:#$040C; Attr:daNone; Ch1:#$041A; Ch2:#$0301; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER KJE
    (Unicode:#$040D; Attr:daNone; Ch1:#$0418; Ch2:#$0300; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER I WITH GRAVE
    (Unicode:#$040E; Attr:daNone; Ch1:#$0423; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER SHORT U
    (Unicode:#$0419; Attr:daNone; Ch1:#$0418; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER SHORT I
    (Unicode:#$0439; Attr:daNone; Ch1:#$0438; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER SHORT I
    (Unicode:#$0450; Attr:daNone; Ch1:#$0435; Ch2:#$0300; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER IE WITH GRAVE
    (Unicode:#$0451; Attr:daNone; Ch1:#$0435; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER IO
    (Unicode:#$0453; Attr:daNone; Ch1:#$0433; Ch2:#$0301; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER GJE
    (Unicode:#$0457; Attr:daNone; Ch1:#$0456; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER YI
    (Unicode:#$045C; Attr:daNone; Ch1:#$043A; Ch2:#$0301; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER KJE
    (Unicode:#$045D; Attr:daNone; Ch1:#$0438; Ch2:#$0300; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER I WITH GRAVE
    (Unicode:#$045E; Attr:daNone; Ch1:#$0443; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER SHORT U
    (Unicode:#$0476; Attr:daNone; Ch1:#$0474; Ch2:#$030F; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$0477; Attr:daNone; Ch1:#$0475; Ch2:#$030F; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER IZHITSA WITH DOUBLE GRAVE ACCENT
    (Unicode:#$04C1; Attr:daNone; Ch1:#$0416; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER ZHE WITH BREVE
    (Unicode:#$04C2; Attr:daNone; Ch1:#$0436; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER ZHE WITH BREVE
    (Unicode:#$04D0; Attr:daNone; Ch1:#$0410; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER A WITH BREVE
    (Unicode:#$04D1; Attr:daNone; Ch1:#$0430; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER A WITH BREVE
    (Unicode:#$04D2; Attr:daNone; Ch1:#$0410; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER A WITH DIAERESIS
    (Unicode:#$04D3; Attr:daNone; Ch1:#$0430; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER A WITH DIAERESIS
    (Unicode:#$04D6; Attr:daNone; Ch1:#$0415; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER IE WITH BREVE
    (Unicode:#$04D7; Attr:daNone; Ch1:#$0435; Ch2:#$0306; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER IE WITH BREVE
    (Unicode:#$04DA; Attr:daNone; Ch1:#$04D8; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DB; Attr:daNone; Ch1:#$04D9; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER SCHWA WITH DIAERESIS
    (Unicode:#$04DC; Attr:daNone; Ch1:#$0416; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DD; Attr:daNone; Ch1:#$0436; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER ZHE WITH DIAERESIS
    (Unicode:#$04DE; Attr:daNone; Ch1:#$0417; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER ZE WITH DIAERESIS
    (Unicode:#$04DF; Attr:daNone; Ch1:#$0437; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER ZE WITH DIAERESIS
    (Unicode:#$04E2; Attr:daNone; Ch1:#$0418; Ch2:#$0304; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER I WITH MACRON
    (Unicode:#$04E3; Attr:daNone; Ch1:#$0438; Ch2:#$0304; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER I WITH MACRON
    (Unicode:#$04E4; Attr:daNone; Ch1:#$0418; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER I WITH DIAERESIS
    (Unicode:#$04E5; Attr:daNone; Ch1:#$0438; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER I WITH DIAERESIS
    (Unicode:#$04E6; Attr:daNone; Ch1:#$041E; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER O WITH DIAERESIS
    (Unicode:#$04E7; Attr:daNone; Ch1:#$043E; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER O WITH DIAERESIS
    (Unicode:#$04EA; Attr:daNone; Ch1:#$04E8; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EB; Attr:daNone; Ch1:#$04E9; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER BARRED O WITH DIAERESIS
    (Unicode:#$04EC; Attr:daNone; Ch1:#$042D; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER E WITH DIAERESIS
    (Unicode:#$04ED; Attr:daNone; Ch1:#$044D; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER E WITH DIAERESIS
    (Unicode:#$04EE; Attr:daNone; Ch1:#$0423; Ch2:#$0304; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER U WITH MACRON
    (Unicode:#$04EF; Attr:daNone; Ch1:#$0443; Ch2:#$0304; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER U WITH MACRON
    (Unicode:#$04F0; Attr:daNone; Ch1:#$0423; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER U WITH DIAERESIS
    (Unicode:#$04F1; Attr:daNone; Ch1:#$0443; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER U WITH DIAERESIS
    (Unicode:#$04F2; Attr:daNone; Ch1:#$0423; Ch2:#$030B; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F3; Attr:daNone; Ch1:#$0443; Ch2:#$030B; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER U WITH DOUBLE ACUTE
    (Unicode:#$04F4; Attr:daNone; Ch1:#$0427; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F5; Attr:daNone; Ch1:#$0447; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER CHE WITH DIAERESIS
    (Unicode:#$04F8; Attr:daNone; Ch1:#$042B; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC CAPITAL LETTER YERU WITH DIAERESIS
    (Unicode:#$04F9; Attr:daNone; Ch1:#$044B; Ch2:#$0308; Ch3:#$FFFF),    // CYRILLIC SMALL LETTER YERU WITH DIAERESIS
    (Unicode:#$0587; Attr:daCompat; Ch1:#$0565; Ch2:#$0582; Ch3:#$FFFF),  // ARMENIAN SMALL LIGATURE ECH YIWN
    (Unicode:#$0622; Attr:daNone; Ch1:#$0627; Ch2:#$0653; Ch3:#$FFFF),    // ARABIC LETTER ALEF WITH MADDA ABOVE
    (Unicode:#$0623; Attr:daNone; Ch1:#$0627; Ch2:#$0654; Ch3:#$FFFF),    // ARABIC LETTER ALEF WITH HAMZA ABOVE
    (Unicode:#$0624; Attr:daNone; Ch1:#$0648; Ch2:#$0654; Ch3:#$FFFF),    // ARABIC LETTER WAW WITH HAMZA ABOVE
    (Unicode:#$0625; Attr:daNone; Ch1:#$0627; Ch2:#$0655; Ch3:#$FFFF),    // ARABIC LETTER ALEF WITH HAMZA BELOW
    (Unicode:#$0626; Attr:daNone; Ch1:#$064A; Ch2:#$0654; Ch3:#$FFFF),    // ARABIC LETTER YEH WITH HAMZA ABOVE
    (Unicode:#$0675; Attr:daCompat; Ch1:#$0627; Ch2:#$0674; Ch3:#$FFFF),  // ARABIC LETTER HIGH HAMZA ALEF
    (Unicode:#$0676; Attr:daCompat; Ch1:#$0648; Ch2:#$0674; Ch3:#$FFFF),  // ARABIC LETTER HIGH HAMZA WAW
    (Unicode:#$0677; Attr:daCompat; Ch1:#$06C7; Ch2:#$0674; Ch3:#$FFFF),  // ARABIC LETTER U WITH HAMZA ABOVE
    (Unicode:#$0678; Attr:daCompat; Ch1:#$064A; Ch2:#$0674; Ch3:#$FFFF),  // ARABIC LETTER HIGH HAMZA YEH
    (Unicode:#$06C0; Attr:daNone; Ch1:#$06D5; Ch2:#$0654; Ch3:#$FFFF),    // ARABIC LETTER HEH WITH YEH ABOVE
    (Unicode:#$06C2; Attr:daNone; Ch1:#$06C1; Ch2:#$0654; Ch3:#$FFFF),    // ARABIC LETTER HEH GOAL WITH HAMZA ABOVE
    (Unicode:#$06D3; Attr:daNone; Ch1:#$06D2; Ch2:#$0654; Ch3:#$FFFF),    // ARABIC LETTER YEH BARREE WITH HAMZA ABOVE
    (Unicode:#$0929; Attr:daNone; Ch1:#$0928; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER NNNA
    (Unicode:#$0931; Attr:daNone; Ch1:#$0930; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER RRA
    (Unicode:#$0934; Attr:daNone; Ch1:#$0933; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER LLLA
    (Unicode:#$0958; Attr:daNone; Ch1:#$0915; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER QA
    (Unicode:#$0959; Attr:daNone; Ch1:#$0916; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER KHHA
    (Unicode:#$095A; Attr:daNone; Ch1:#$0917; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER GHHA
    (Unicode:#$095B; Attr:daNone; Ch1:#$091C; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER ZA
    (Unicode:#$095C; Attr:daNone; Ch1:#$0921; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER DDDHA
    (Unicode:#$095D; Attr:daNone; Ch1:#$0922; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER RHA
    (Unicode:#$095E; Attr:daNone; Ch1:#$092B; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER FA
    (Unicode:#$095F; Attr:daNone; Ch1:#$092F; Ch2:#$093C; Ch3:#$FFFF),    // DEVANAGARI LETTER YYA
    (Unicode:#$09CB; Attr:daNone; Ch1:#$09C7; Ch2:#$09BE; Ch3:#$FFFF),    // BENGALI VOWEL SIGN O
    (Unicode:#$09CC; Attr:daNone; Ch1:#$09C7; Ch2:#$09D7; Ch3:#$FFFF),    // BENGALI VOWEL SIGN AU
    (Unicode:#$09DC; Attr:daNone; Ch1:#$09A1; Ch2:#$09BC; Ch3:#$FFFF),    // BENGALI LETTER RRA
    (Unicode:#$09DD; Attr:daNone; Ch1:#$09A2; Ch2:#$09BC; Ch3:#$FFFF),    // BENGALI LETTER RHA
    (Unicode:#$09DF; Attr:daNone; Ch1:#$09AF; Ch2:#$09BC; Ch3:#$FFFF),    // BENGALI LETTER YYA
    (Unicode:#$0A33; Attr:daNone; Ch1:#$0A32; Ch2:#$0A3C; Ch3:#$FFFF),    // GURMUKHI LETTER LLA
    (Unicode:#$0A36; Attr:daNone; Ch1:#$0A38; Ch2:#$0A3C; Ch3:#$FFFF),    // GURMUKHI LETTER SHA
    (Unicode:#$0A59; Attr:daNone; Ch1:#$0A16; Ch2:#$0A3C; Ch3:#$FFFF),    // GURMUKHI LETTER KHHA
    (Unicode:#$0A5A; Attr:daNone; Ch1:#$0A17; Ch2:#$0A3C; Ch3:#$FFFF),    // GURMUKHI LETTER GHHA
    (Unicode:#$0A5B; Attr:daNone; Ch1:#$0A1C; Ch2:#$0A3C; Ch3:#$FFFF),    // GURMUKHI LETTER ZA
    (Unicode:#$0A5E; Attr:daNone; Ch1:#$0A2B; Ch2:#$0A3C; Ch3:#$FFFF),    // GURMUKHI LETTER FA
    (Unicode:#$0B48; Attr:daNone; Ch1:#$0B47; Ch2:#$0B56; Ch3:#$FFFF),    // ORIYA VOWEL SIGN AI
    (Unicode:#$0B4B; Attr:daNone; Ch1:#$0B47; Ch2:#$0B3E; Ch3:#$FFFF),    // ORIYA VOWEL SIGN O
    (Unicode:#$0B4C; Attr:daNone; Ch1:#$0B47; Ch2:#$0B57; Ch3:#$FFFF),    // ORIYA VOWEL SIGN AU
    (Unicode:#$0B5C; Attr:daNone; Ch1:#$0B21; Ch2:#$0B3C; Ch3:#$FFFF),    // ORIYA LETTER RRA
    (Unicode:#$0B5D; Attr:daNone; Ch1:#$0B22; Ch2:#$0B3C; Ch3:#$FFFF),    // ORIYA LETTER RHA
    (Unicode:#$0B94; Attr:daNone; Ch1:#$0B92; Ch2:#$0BD7; Ch3:#$FFFF),    // TAMIL LETTER AU
    (Unicode:#$0BCA; Attr:daNone; Ch1:#$0BC6; Ch2:#$0BBE; Ch3:#$FFFF),    // TAMIL VOWEL SIGN O
    (Unicode:#$0BCB; Attr:daNone; Ch1:#$0BC7; Ch2:#$0BBE; Ch3:#$FFFF),    // TAMIL VOWEL SIGN OO
    (Unicode:#$0BCC; Attr:daNone; Ch1:#$0BC6; Ch2:#$0BD7; Ch3:#$FFFF),    // TAMIL VOWEL SIGN AU
    (Unicode:#$0C48; Attr:daNone; Ch1:#$0C46; Ch2:#$0C56; Ch3:#$FFFF),    // TELUGU VOWEL SIGN AI
    (Unicode:#$0CC0; Attr:daNone; Ch1:#$0CBF; Ch2:#$0CD5; Ch3:#$FFFF),    // KANNADA VOWEL SIGN II
    (Unicode:#$0CC7; Attr:daNone; Ch1:#$0CC6; Ch2:#$0CD5; Ch3:#$FFFF),    // KANNADA VOWEL SIGN EE
    (Unicode:#$0CC8; Attr:daNone; Ch1:#$0CC6; Ch2:#$0CD6; Ch3:#$FFFF),    // KANNADA VOWEL SIGN AI
    (Unicode:#$0CCA; Attr:daNone; Ch1:#$0CC6; Ch2:#$0CC2; Ch3:#$FFFF),    // KANNADA VOWEL SIGN O
    (Unicode:#$0CCB; Attr:daNone; Ch1:#$0CCA; Ch2:#$0CD5; Ch3:#$FFFF),    // KANNADA VOWEL SIGN OO
    (Unicode:#$0D4A; Attr:daNone; Ch1:#$0D46; Ch2:#$0D3E; Ch3:#$FFFF),    // MALAYALAM VOWEL SIGN O
    (Unicode:#$0D4B; Attr:daNone; Ch1:#$0D47; Ch2:#$0D3E; Ch3:#$FFFF),    // MALAYALAM VOWEL SIGN OO
    (Unicode:#$0D4C; Attr:daNone; Ch1:#$0D46; Ch2:#$0D57; Ch3:#$FFFF),    // MALAYALAM VOWEL SIGN AU
    (Unicode:#$0DDA; Attr:daNone; Ch1:#$0DD9; Ch2:#$0DCA; Ch3:#$FFFF),    // SINHALA VOWEL SIGN DIGA KOMBUVA
    (Unicode:#$0DDC; Attr:daNone; Ch1:#$0DD9; Ch2:#$0DCF; Ch3:#$FFFF),    // SINHALA VOWEL SIGN KOMBUVA HAA AELA-PILLA
    (Unicode:#$0DDD; Attr:daNone; Ch1:#$0DDC; Ch2:#$0DCA; Ch3:#$FFFF),    // SINHALA VOWEL SIGN KOMBUVA HAA DIGA AELA-PILLA
    (Unicode:#$0DDE; Attr:daNone; Ch1:#$0DD9; Ch2:#$0DDF; Ch3:#$FFFF),    // SINHALA VOWEL SIGN KOMBUVA HAA GAYANUKITTA
    (Unicode:#$0E33; Attr:daCompat; Ch1:#$0E4D; Ch2:#$0E32; Ch3:#$FFFF),  // THAI CHARACTER SARA AM
    (Unicode:#$0EB3; Attr:daCompat; Ch1:#$0ECD; Ch2:#$0EB2; Ch3:#$FFFF),  // LAO VOWEL SIGN AM
    (Unicode:#$0EDC; Attr:daCompat; Ch1:#$0EAB; Ch2:#$0E99; Ch3:#$FFFF),  // LAO HO NO
    (Unicode:#$0EDD; Attr:daCompat; Ch1:#$0EAB; Ch2:#$0EA1; Ch3:#$FFFF),  // LAO HO MO
    (Unicode:#$0F0C; Attr:daNoBreak; Ch1:#$0F0B; Ch2:#$FFFF),             // TIBETAN MARK DELIMITER TSHEG BSTAR
    (Unicode:#$0F43; Attr:daNone; Ch1:#$0F42; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN LETTER GHA
    (Unicode:#$0F4D; Attr:daNone; Ch1:#$0F4C; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN LETTER DDHA
    (Unicode:#$0F52; Attr:daNone; Ch1:#$0F51; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN LETTER DHA
    (Unicode:#$0F57; Attr:daNone; Ch1:#$0F56; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN LETTER BHA
    (Unicode:#$0F5C; Attr:daNone; Ch1:#$0F5B; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN LETTER DZHA
    (Unicode:#$0F69; Attr:daNone; Ch1:#$0F40; Ch2:#$0FB5; Ch3:#$FFFF),    // TIBETAN LETTER KSSA
    (Unicode:#$0F73; Attr:daNone; Ch1:#$0F71; Ch2:#$0F72; Ch3:#$FFFF),    // TIBETAN VOWEL SIGN II
    (Unicode:#$0F75; Attr:daNone; Ch1:#$0F71; Ch2:#$0F74; Ch3:#$FFFF),    // TIBETAN VOWEL SIGN UU
    (Unicode:#$0F76; Attr:daNone; Ch1:#$0FB2; Ch2:#$0F80; Ch3:#$FFFF),    // TIBETAN VOWEL SIGN VOCALIC R
    (Unicode:#$0F77; Attr:daCompat; Ch1:#$0FB2; Ch2:#$0F81; Ch3:#$FFFF),  // TIBETAN VOWEL SIGN VOCALIC RR
    (Unicode:#$0F78; Attr:daNone; Ch1:#$0FB3; Ch2:#$0F80; Ch3:#$FFFF),    // TIBETAN VOWEL SIGN VOCALIC L
    (Unicode:#$0F79; Attr:daCompat; Ch1:#$0FB3; Ch2:#$0F81; Ch3:#$FFFF),  // TIBETAN VOWEL SIGN VOCALIC LL
    (Unicode:#$0F81; Attr:daNone; Ch1:#$0F71; Ch2:#$0F80; Ch3:#$FFFF),    // TIBETAN VOWEL SIGN REVERSED II
    (Unicode:#$0F93; Attr:daNone; Ch1:#$0F92; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN SUBJOINED LETTER GHA
    (Unicode:#$0F9D; Attr:daNone; Ch1:#$0F9C; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN SUBJOINED LETTER DDHA
    (Unicode:#$0FA2; Attr:daNone; Ch1:#$0FA1; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN SUBJOINED LETTER DHA
    (Unicode:#$0FA7; Attr:daNone; Ch1:#$0FA6; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN SUBJOINED LETTER BHA
    (Unicode:#$0FAC; Attr:daNone; Ch1:#$0FAB; Ch2:#$0FB7; Ch3:#$FFFF),    // TIBETAN SUBJOINED LETTER DZHA
    (Unicode:#$0FB9; Attr:daNone; Ch1:#$0F90; Ch2:#$0FB5; Ch3:#$FFFF),    // TIBETAN SUBJOINED LETTER KSSA
    (Unicode:#$1026; Attr:daNone; Ch1:#$1025; Ch2:#$102E; Ch3:#$FFFF),    // MYANMAR LETTER UU
    (Unicode:#$1E00; Attr:daNone; Ch1:#$0041; Ch2:#$0325; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH RING BELOW
    (Unicode:#$1E01; Attr:daNone; Ch1:#$0061; Ch2:#$0325; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH RING BELOW
    (Unicode:#$1E02; Attr:daNone; Ch1:#$0042; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER B WITH DOT ABOVE
    (Unicode:#$1E03; Attr:daNone; Ch1:#$0062; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER B WITH DOT ABOVE
    (Unicode:#$1E04; Attr:daNone; Ch1:#$0042; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER B WITH DOT BELOW
    (Unicode:#$1E05; Attr:daNone; Ch1:#$0062; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER B WITH DOT BELOW
    (Unicode:#$1E06; Attr:daNone; Ch1:#$0042; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER B WITH LINE BELOW
    (Unicode:#$1E07; Attr:daNone; Ch1:#$0062; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER B WITH LINE BELOW
    (Unicode:#$1E08; Attr:daNone; Ch1:#$00C7; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E09; Attr:daNone; Ch1:#$00E7; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER C WITH CEDILLA AND ACUTE
    (Unicode:#$1E0A; Attr:daNone; Ch1:#$0044; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0B; Attr:daNone; Ch1:#$0064; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER D WITH DOT ABOVE
    (Unicode:#$1E0C; Attr:daNone; Ch1:#$0044; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER D WITH DOT BELOW
    (Unicode:#$1E0D; Attr:daNone; Ch1:#$0064; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER D WITH DOT BELOW
    (Unicode:#$1E0E; Attr:daNone; Ch1:#$0044; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER D WITH LINE BELOW
    (Unicode:#$1E0F; Attr:daNone; Ch1:#$0064; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER D WITH LINE BELOW
    (Unicode:#$1E10; Attr:daNone; Ch1:#$0044; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER D WITH CEDILLA
    (Unicode:#$1E11; Attr:daNone; Ch1:#$0064; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER D WITH CEDILLA
    (Unicode:#$1E12; Attr:daNone; Ch1:#$0044; Ch2:#$032D; Ch3:#$FFFF),    // LATIN CAPITAL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E13; Attr:daNone; Ch1:#$0064; Ch2:#$032D; Ch3:#$FFFF),    // LATIN SMALL LETTER D WITH CIRCUMFLEX BELOW
    (Unicode:#$1E14; Attr:daNone; Ch1:#$0112; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E15; Attr:daNone; Ch1:#$0113; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH MACRON AND GRAVE
    (Unicode:#$1E16; Attr:daNone; Ch1:#$0112; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E17; Attr:daNone; Ch1:#$0113; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH MACRON AND ACUTE
    (Unicode:#$1E18; Attr:daNone; Ch1:#$0045; Ch2:#$032D; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E19; Attr:daNone; Ch1:#$0065; Ch2:#$032D; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX BELOW
    (Unicode:#$1E1A; Attr:daNone; Ch1:#$0045; Ch2:#$0330; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1B; Attr:daNone; Ch1:#$0065; Ch2:#$0330; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH TILDE BELOW
    (Unicode:#$1E1C; Attr:daNone; Ch1:#$0228; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1D; Attr:daNone; Ch1:#$0229; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CEDILLA AND BREVE
    (Unicode:#$1E1E; Attr:daNone; Ch1:#$0046; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER F WITH DOT ABOVE
    (Unicode:#$1E1F; Attr:daNone; Ch1:#$0066; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER F WITH DOT ABOVE
    (Unicode:#$1E20; Attr:daNone; Ch1:#$0047; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER G WITH MACRON
    (Unicode:#$1E21; Attr:daNone; Ch1:#$0067; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER G WITH MACRON
    (Unicode:#$1E22; Attr:daNone; Ch1:#$0048; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH DOT ABOVE
    (Unicode:#$1E23; Attr:daNone; Ch1:#$0068; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH DOT ABOVE
    (Unicode:#$1E24; Attr:daNone; Ch1:#$0048; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH DOT BELOW
    (Unicode:#$1E25; Attr:daNone; Ch1:#$0068; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH DOT BELOW
    (Unicode:#$1E26; Attr:daNone; Ch1:#$0048; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH DIAERESIS
    (Unicode:#$1E27; Attr:daNone; Ch1:#$0068; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH DIAERESIS
    (Unicode:#$1E28; Attr:daNone; Ch1:#$0048; Ch2:#$0327; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH CEDILLA
    (Unicode:#$1E29; Attr:daNone; Ch1:#$0068; Ch2:#$0327; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH CEDILLA
    (Unicode:#$1E2A; Attr:daNone; Ch1:#$0048; Ch2:#$032E; Ch3:#$FFFF),    // LATIN CAPITAL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2B; Attr:daNone; Ch1:#$0068; Ch2:#$032E; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH BREVE BELOW
    (Unicode:#$1E2C; Attr:daNone; Ch1:#$0049; Ch2:#$0330; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2D; Attr:daNone; Ch1:#$0069; Ch2:#$0330; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH TILDE BELOW
    (Unicode:#$1E2E; Attr:daNone; Ch1:#$00CF; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E2F; Attr:daNone; Ch1:#$00EF; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH DIAERESIS AND ACUTE
    (Unicode:#$1E30; Attr:daNone; Ch1:#$004B; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER K WITH ACUTE
    (Unicode:#$1E31; Attr:daNone; Ch1:#$006B; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER K WITH ACUTE
    (Unicode:#$1E32; Attr:daNone; Ch1:#$004B; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER K WITH DOT BELOW
    (Unicode:#$1E33; Attr:daNone; Ch1:#$006B; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER K WITH DOT BELOW
    (Unicode:#$1E34; Attr:daNone; Ch1:#$004B; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER K WITH LINE BELOW
    (Unicode:#$1E35; Attr:daNone; Ch1:#$006B; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER K WITH LINE BELOW
    (Unicode:#$1E36; Attr:daNone; Ch1:#$004C; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH DOT BELOW
    (Unicode:#$1E37; Attr:daNone; Ch1:#$006C; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH DOT BELOW
    (Unicode:#$1E38; Attr:daNone; Ch1:#$1E36; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E39; Attr:daNone; Ch1:#$1E37; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH DOT BELOW AND MACRON
    (Unicode:#$1E3A; Attr:daNone; Ch1:#$004C; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH LINE BELOW
    (Unicode:#$1E3B; Attr:daNone; Ch1:#$006C; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH LINE BELOW
    (Unicode:#$1E3C; Attr:daNone; Ch1:#$004C; Ch2:#$032D; Ch3:#$FFFF),    // LATIN CAPITAL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3D; Attr:daNone; Ch1:#$006C; Ch2:#$032D; Ch3:#$FFFF),    // LATIN SMALL LETTER L WITH CIRCUMFLEX BELOW
    (Unicode:#$1E3E; Attr:daNone; Ch1:#$004D; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER M WITH ACUTE
    (Unicode:#$1E3F; Attr:daNone; Ch1:#$006D; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER M WITH ACUTE
    (Unicode:#$1E40; Attr:daNone; Ch1:#$004D; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER M WITH DOT ABOVE
    (Unicode:#$1E41; Attr:daNone; Ch1:#$006D; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER M WITH DOT ABOVE
    (Unicode:#$1E42; Attr:daNone; Ch1:#$004D; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER M WITH DOT BELOW
    (Unicode:#$1E43; Attr:daNone; Ch1:#$006D; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER M WITH DOT BELOW
    (Unicode:#$1E44; Attr:daNone; Ch1:#$004E; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH DOT ABOVE
    (Unicode:#$1E45; Attr:daNone; Ch1:#$006E; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH DOT ABOVE
    (Unicode:#$1E46; Attr:daNone; Ch1:#$004E; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH DOT BELOW
    (Unicode:#$1E47; Attr:daNone; Ch1:#$006E; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH DOT BELOW
    (Unicode:#$1E48; Attr:daNone; Ch1:#$004E; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH LINE BELOW
    (Unicode:#$1E49; Attr:daNone; Ch1:#$006E; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH LINE BELOW
    (Unicode:#$1E4A; Attr:daNone; Ch1:#$004E; Ch2:#$032D; Ch3:#$FFFF),    // LATIN CAPITAL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4B; Attr:daNone; Ch1:#$006E; Ch2:#$032D; Ch3:#$FFFF),    // LATIN SMALL LETTER N WITH CIRCUMFLEX BELOW
    (Unicode:#$1E4C; Attr:daNone; Ch1:#$00D5; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4D; Attr:daNone; Ch1:#$00F5; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH TILDE AND ACUTE
    (Unicode:#$1E4E; Attr:daNone; Ch1:#$00D5; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E4F; Attr:daNone; Ch1:#$00F5; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH TILDE AND DIAERESIS
    (Unicode:#$1E50; Attr:daNone; Ch1:#$014C; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E51; Attr:daNone; Ch1:#$014D; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH MACRON AND GRAVE
    (Unicode:#$1E52; Attr:daNone; Ch1:#$014C; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E53; Attr:daNone; Ch1:#$014D; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH MACRON AND ACUTE
    (Unicode:#$1E54; Attr:daNone; Ch1:#$0050; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER P WITH ACUTE
    (Unicode:#$1E55; Attr:daNone; Ch1:#$0070; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER P WITH ACUTE
    (Unicode:#$1E56; Attr:daNone; Ch1:#$0050; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER P WITH DOT ABOVE
    (Unicode:#$1E57; Attr:daNone; Ch1:#$0070; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER P WITH DOT ABOVE
    (Unicode:#$1E58; Attr:daNone; Ch1:#$0052; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH DOT ABOVE
    (Unicode:#$1E59; Attr:daNone; Ch1:#$0072; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH DOT ABOVE
    (Unicode:#$1E5A; Attr:daNone; Ch1:#$0052; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH DOT BELOW
    (Unicode:#$1E5B; Attr:daNone; Ch1:#$0072; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH DOT BELOW
    (Unicode:#$1E5C; Attr:daNone; Ch1:#$1E5A; Ch2:#$0304; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5D; Attr:daNone; Ch1:#$1E5B; Ch2:#$0304; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH DOT BELOW AND MACRON
    (Unicode:#$1E5E; Attr:daNone; Ch1:#$0052; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER R WITH LINE BELOW
    (Unicode:#$1E5F; Attr:daNone; Ch1:#$0072; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER R WITH LINE BELOW
    (Unicode:#$1E60; Attr:daNone; Ch1:#$0053; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH DOT ABOVE
    (Unicode:#$1E61; Attr:daNone; Ch1:#$0073; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH DOT ABOVE
    (Unicode:#$1E62; Attr:daNone; Ch1:#$0053; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH DOT BELOW
    (Unicode:#$1E63; Attr:daNone; Ch1:#$0073; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH DOT BELOW
    (Unicode:#$1E64; Attr:daNone; Ch1:#$015A; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E65; Attr:daNone; Ch1:#$015B; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH ACUTE AND DOT ABOVE
    (Unicode:#$1E66; Attr:daNone; Ch1:#$0160; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E67; Attr:daNone; Ch1:#$0161; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH CARON AND DOT ABOVE
    (Unicode:#$1E68; Attr:daNone; Ch1:#$1E62; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E69; Attr:daNone; Ch1:#$1E63; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER S WITH DOT BELOW AND DOT ABOVE
    (Unicode:#$1E6A; Attr:daNone; Ch1:#$0054; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6B; Attr:daNone; Ch1:#$0074; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH DOT ABOVE
    (Unicode:#$1E6C; Attr:daNone; Ch1:#$0054; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH DOT BELOW
    (Unicode:#$1E6D; Attr:daNone; Ch1:#$0074; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH DOT BELOW
    (Unicode:#$1E6E; Attr:daNone; Ch1:#$0054; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH LINE BELOW
    (Unicode:#$1E6F; Attr:daNone; Ch1:#$0074; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH LINE BELOW
    (Unicode:#$1E70; Attr:daNone; Ch1:#$0054; Ch2:#$032D; Ch3:#$FFFF),    // LATIN CAPITAL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E71; Attr:daNone; Ch1:#$0074; Ch2:#$032D; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH CIRCUMFLEX BELOW
    (Unicode:#$1E72; Attr:daNone; Ch1:#$0055; Ch2:#$0324; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E73; Attr:daNone; Ch1:#$0075; Ch2:#$0324; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DIAERESIS BELOW
    (Unicode:#$1E74; Attr:daNone; Ch1:#$0055; Ch2:#$0330; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH TILDE BELOW
    (Unicode:#$1E75; Attr:daNone; Ch1:#$0075; Ch2:#$0330; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH TILDE BELOW
    (Unicode:#$1E76; Attr:daNone; Ch1:#$0055; Ch2:#$032D; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E77; Attr:daNone; Ch1:#$0075; Ch2:#$032D; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH CIRCUMFLEX BELOW
    (Unicode:#$1E78; Attr:daNone; Ch1:#$0168; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E79; Attr:daNone; Ch1:#$0169; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH TILDE AND ACUTE
    (Unicode:#$1E7A; Attr:daNone; Ch1:#$016A; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7B; Attr:daNone; Ch1:#$016B; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH MACRON AND DIAERESIS
    (Unicode:#$1E7C; Attr:daNone; Ch1:#$0056; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER V WITH TILDE
    (Unicode:#$1E7D; Attr:daNone; Ch1:#$0076; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER V WITH TILDE
    (Unicode:#$1E7E; Attr:daNone; Ch1:#$0056; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER V WITH DOT BELOW
    (Unicode:#$1E7F; Attr:daNone; Ch1:#$0076; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER V WITH DOT BELOW
    (Unicode:#$1E80; Attr:daNone; Ch1:#$0057; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER W WITH GRAVE
    (Unicode:#$1E81; Attr:daNone; Ch1:#$0077; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH GRAVE
    (Unicode:#$1E82; Attr:daNone; Ch1:#$0057; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER W WITH ACUTE
    (Unicode:#$1E83; Attr:daNone; Ch1:#$0077; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH ACUTE
    (Unicode:#$1E84; Attr:daNone; Ch1:#$0057; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER W WITH DIAERESIS
    (Unicode:#$1E85; Attr:daNone; Ch1:#$0077; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH DIAERESIS
    (Unicode:#$1E86; Attr:daNone; Ch1:#$0057; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER W WITH DOT ABOVE
    (Unicode:#$1E87; Attr:daNone; Ch1:#$0077; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH DOT ABOVE
    (Unicode:#$1E88; Attr:daNone; Ch1:#$0057; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER W WITH DOT BELOW
    (Unicode:#$1E89; Attr:daNone; Ch1:#$0077; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH DOT BELOW
    (Unicode:#$1E8A; Attr:daNone; Ch1:#$0058; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8B; Attr:daNone; Ch1:#$0078; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER X WITH DOT ABOVE
    (Unicode:#$1E8C; Attr:daNone; Ch1:#$0058; Ch2:#$0308; Ch3:#$FFFF),    // LATIN CAPITAL LETTER X WITH DIAERESIS
    (Unicode:#$1E8D; Attr:daNone; Ch1:#$0078; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER X WITH DIAERESIS
    (Unicode:#$1E8E; Attr:daNone; Ch1:#$0059; Ch2:#$0307; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E8F; Attr:daNone; Ch1:#$0079; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH DOT ABOVE
    (Unicode:#$1E90; Attr:daNone; Ch1:#$005A; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E91; Attr:daNone; Ch1:#$007A; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER Z WITH CIRCUMFLEX
    (Unicode:#$1E92; Attr:daNone; Ch1:#$005A; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Z WITH DOT BELOW
    (Unicode:#$1E93; Attr:daNone; Ch1:#$007A; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER Z WITH DOT BELOW
    (Unicode:#$1E94; Attr:daNone; Ch1:#$005A; Ch2:#$0331; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Z WITH LINE BELOW
    (Unicode:#$1E95; Attr:daNone; Ch1:#$007A; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER Z WITH LINE BELOW
    (Unicode:#$1E96; Attr:daNone; Ch1:#$0068; Ch2:#$0331; Ch3:#$FFFF),    // LATIN SMALL LETTER H WITH LINE BELOW
    (Unicode:#$1E97; Attr:daNone; Ch1:#$0074; Ch2:#$0308; Ch3:#$FFFF),    // LATIN SMALL LETTER T WITH DIAERESIS
    (Unicode:#$1E98; Attr:daNone; Ch1:#$0077; Ch2:#$030A; Ch3:#$FFFF),    // LATIN SMALL LETTER W WITH RING ABOVE
    (Unicode:#$1E99; Attr:daNone; Ch1:#$0079; Ch2:#$030A; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH RING ABOVE
    (Unicode:#$1E9A; Attr:daCompat; Ch1:#$0061; Ch2:#$02BE; Ch3:#$FFFF),  // LATIN SMALL LETTER A WITH RIGHT HALF RING
    (Unicode:#$1E9B; Attr:daNone; Ch1:#$017F; Ch2:#$0307; Ch3:#$FFFF),    // LATIN SMALL LETTER LONG S WITH DOT ABOVE
    (Unicode:#$1EA0; Attr:daNone; Ch1:#$0041; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH DOT BELOW
    (Unicode:#$1EA1; Attr:daNone; Ch1:#$0061; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH DOT BELOW
    (Unicode:#$1EA2; Attr:daNone; Ch1:#$0041; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA3; Attr:daNone; Ch1:#$0061; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH HOOK ABOVE
    (Unicode:#$1EA4; Attr:daNone; Ch1:#$00C2; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA5; Attr:daNone; Ch1:#$00E2; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EA6; Attr:daNone; Ch1:#$00C2; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA7; Attr:daNone; Ch1:#$00E2; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EA8; Attr:daNone; Ch1:#$00C2; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EA9; Attr:daNone; Ch1:#$00E2; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EAA; Attr:daNone; Ch1:#$00C2; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAB; Attr:daNone; Ch1:#$00E2; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EAC; Attr:daNone; Ch1:#$1EA0; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAD; Attr:daNone; Ch1:#$1EA1; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EAE; Attr:daNone; Ch1:#$0102; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EAF; Attr:daNone; Ch1:#$0103; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH BREVE AND ACUTE
    (Unicode:#$1EB0; Attr:daNone; Ch1:#$0102; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB1; Attr:daNone; Ch1:#$0103; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH BREVE AND GRAVE
    (Unicode:#$1EB2; Attr:daNone; Ch1:#$0102; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB3; Attr:daNone; Ch1:#$0103; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH BREVE AND HOOK ABOVE
    (Unicode:#$1EB4; Attr:daNone; Ch1:#$0102; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB5; Attr:daNone; Ch1:#$0103; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH BREVE AND TILDE
    (Unicode:#$1EB6; Attr:daNone; Ch1:#$1EA0; Ch2:#$0306; Ch3:#$FFFF),    // LATIN CAPITAL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB7; Attr:daNone; Ch1:#$1EA1; Ch2:#$0306; Ch3:#$FFFF),    // LATIN SMALL LETTER A WITH BREVE AND DOT BELOW
    (Unicode:#$1EB8; Attr:daNone; Ch1:#$0045; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH DOT BELOW
    (Unicode:#$1EB9; Attr:daNone; Ch1:#$0065; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH DOT BELOW
    (Unicode:#$1EBA; Attr:daNone; Ch1:#$0045; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBB; Attr:daNone; Ch1:#$0065; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH HOOK ABOVE
    (Unicode:#$1EBC; Attr:daNone; Ch1:#$0045; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH TILDE
    (Unicode:#$1EBD; Attr:daNone; Ch1:#$0065; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH TILDE
    (Unicode:#$1EBE; Attr:daNone; Ch1:#$00CA; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EBF; Attr:daNone; Ch1:#$00EA; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1EC0; Attr:daNone; Ch1:#$00CA; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC1; Attr:daNone; Ch1:#$00EA; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1EC2; Attr:daNone; Ch1:#$00CA; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC3; Attr:daNone; Ch1:#$00EA; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1EC4; Attr:daNone; Ch1:#$00CA; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC5; Attr:daNone; Ch1:#$00EA; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1EC6; Attr:daNone; Ch1:#$1EB8; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC7; Attr:daNone; Ch1:#$1EB9; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER E WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EC8; Attr:daNone; Ch1:#$0049; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH HOOK ABOVE
    (Unicode:#$1EC9; Attr:daNone; Ch1:#$0069; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH HOOK ABOVE
    (Unicode:#$1ECA; Attr:daNone; Ch1:#$0049; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER I WITH DOT BELOW
    (Unicode:#$1ECB; Attr:daNone; Ch1:#$0069; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER I WITH DOT BELOW
    (Unicode:#$1ECC; Attr:daNone; Ch1:#$004F; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH DOT BELOW
    (Unicode:#$1ECD; Attr:daNone; Ch1:#$006F; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH DOT BELOW
    (Unicode:#$1ECE; Attr:daNone; Ch1:#$004F; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ECF; Attr:daNone; Ch1:#$006F; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HOOK ABOVE
    (Unicode:#$1ED0; Attr:daNone; Ch1:#$00D4; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED1; Attr:daNone; Ch1:#$00F4; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND ACUTE
    (Unicode:#$1ED2; Attr:daNone; Ch1:#$00D4; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED3; Attr:daNone; Ch1:#$00F4; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND GRAVE
    (Unicode:#$1ED4; Attr:daNone; Ch1:#$00D4; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED5; Attr:daNone; Ch1:#$00F4; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND HOOK ABOVE
    (Unicode:#$1ED6; Attr:daNone; Ch1:#$00D4; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED7; Attr:daNone; Ch1:#$00F4; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND TILDE
    (Unicode:#$1ED8; Attr:daNone; Ch1:#$1ECC; Ch2:#$0302; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1ED9; Attr:daNone; Ch1:#$1ECD; Ch2:#$0302; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH CIRCUMFLEX AND DOT BELOW
    (Unicode:#$1EDA; Attr:daNone; Ch1:#$01A0; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDB; Attr:daNone; Ch1:#$01A1; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HORN AND ACUTE
    (Unicode:#$1EDC; Attr:daNone; Ch1:#$01A0; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDD; Attr:daNone; Ch1:#$01A1; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HORN AND GRAVE
    (Unicode:#$1EDE; Attr:daNone; Ch1:#$01A0; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EDF; Attr:daNone; Ch1:#$01A1; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HORN AND HOOK ABOVE
    (Unicode:#$1EE0; Attr:daNone; Ch1:#$01A0; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE1; Attr:daNone; Ch1:#$01A1; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HORN AND TILDE
    (Unicode:#$1EE2; Attr:daNone; Ch1:#$01A0; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE3; Attr:daNone; Ch1:#$01A1; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER O WITH HORN AND DOT BELOW
    (Unicode:#$1EE4; Attr:daNone; Ch1:#$0055; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH DOT BELOW
    (Unicode:#$1EE5; Attr:daNone; Ch1:#$0075; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH DOT BELOW
    (Unicode:#$1EE6; Attr:daNone; Ch1:#$0055; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE7; Attr:daNone; Ch1:#$0075; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HOOK ABOVE
    (Unicode:#$1EE8; Attr:daNone; Ch1:#$01AF; Ch2:#$0301; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EE9; Attr:daNone; Ch1:#$01B0; Ch2:#$0301; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HORN AND ACUTE
    (Unicode:#$1EEA; Attr:daNone; Ch1:#$01AF; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEB; Attr:daNone; Ch1:#$01B0; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HORN AND GRAVE
    (Unicode:#$1EEC; Attr:daNone; Ch1:#$01AF; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EED; Attr:daNone; Ch1:#$01B0; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HORN AND HOOK ABOVE
    (Unicode:#$1EEE; Attr:daNone; Ch1:#$01AF; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EEF; Attr:daNone; Ch1:#$01B0; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HORN AND TILDE
    (Unicode:#$1EF0; Attr:daNone; Ch1:#$01AF; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF1; Attr:daNone; Ch1:#$01B0; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER U WITH HORN AND DOT BELOW
    (Unicode:#$1EF2; Attr:daNone; Ch1:#$0059; Ch2:#$0300; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH GRAVE
    (Unicode:#$1EF3; Attr:daNone; Ch1:#$0079; Ch2:#$0300; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH GRAVE
    (Unicode:#$1EF4; Attr:daNone; Ch1:#$0059; Ch2:#$0323; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF5; Attr:daNone; Ch1:#$0079; Ch2:#$0323; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH DOT BELOW
    (Unicode:#$1EF6; Attr:daNone; Ch1:#$0059; Ch2:#$0309; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF7; Attr:daNone; Ch1:#$0079; Ch2:#$0309; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH HOOK ABOVE
    (Unicode:#$1EF8; Attr:daNone; Ch1:#$0059; Ch2:#$0303; Ch3:#$FFFF),    // LATIN CAPITAL LETTER Y WITH TILDE
    (Unicode:#$1EF9; Attr:daNone; Ch1:#$0079; Ch2:#$0303; Ch3:#$FFFF),    // LATIN SMALL LETTER Y WITH TILDE
    (Unicode:#$1F00; Attr:daNone; Ch1:#$03B1; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI
    (Unicode:#$1F01; Attr:daNone; Ch1:#$03B1; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA
    (Unicode:#$1F02; Attr:daNone; Ch1:#$1F00; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F03; Attr:daNone; Ch1:#$1F01; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F04; Attr:daNone; Ch1:#$1F00; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F05; Attr:daNone; Ch1:#$1F01; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F06; Attr:daNone; Ch1:#$1F00; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F07; Attr:daNone; Ch1:#$1F01; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F08; Attr:daNone; Ch1:#$0391; Ch2:#$0313; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI
    (Unicode:#$1F09; Attr:daNone; Ch1:#$0391; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA
    (Unicode:#$1F0A; Attr:daNone; Ch1:#$1F08; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA
    (Unicode:#$1F0B; Attr:daNone; Ch1:#$1F09; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA
    (Unicode:#$1F0C; Attr:daNone; Ch1:#$1F08; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA
    (Unicode:#$1F0D; Attr:daNone; Ch1:#$1F09; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA
    (Unicode:#$1F0E; Attr:daNone; Ch1:#$1F08; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F0F; Attr:daNone; Ch1:#$1F09; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F10; Attr:daNone; Ch1:#$03B5; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH PSILI
    (Unicode:#$1F11; Attr:daNone; Ch1:#$03B5; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH DASIA
    (Unicode:#$1F12; Attr:daNone; Ch1:#$1F10; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F13; Attr:daNone; Ch1:#$1F11; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F14; Attr:daNone; Ch1:#$1F10; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F15; Attr:daNone; Ch1:#$1F11; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F18; Attr:daNone; Ch1:#$0395; Ch2:#$0313; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH PSILI
    (Unicode:#$1F19; Attr:daNone; Ch1:#$0395; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH DASIA
    (Unicode:#$1F1A; Attr:daNone; Ch1:#$1F18; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH PSILI AND VARIA
    (Unicode:#$1F1B; Attr:daNone; Ch1:#$1F19; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH DASIA AND VARIA
    (Unicode:#$1F1C; Attr:daNone; Ch1:#$1F18; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH PSILI AND OXIA
    (Unicode:#$1F1D; Attr:daNone; Ch1:#$1F19; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH DASIA AND OXIA
    (Unicode:#$1F20; Attr:daNone; Ch1:#$03B7; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI
    (Unicode:#$1F21; Attr:daNone; Ch1:#$03B7; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA
    (Unicode:#$1F22; Attr:daNone; Ch1:#$1F20; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F23; Attr:daNone; Ch1:#$1F21; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F24; Attr:daNone; Ch1:#$1F20; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F25; Attr:daNone; Ch1:#$1F21; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F26; Attr:daNone; Ch1:#$1F20; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F27; Attr:daNone; Ch1:#$1F21; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F28; Attr:daNone; Ch1:#$0397; Ch2:#$0313; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI
    (Unicode:#$1F29; Attr:daNone; Ch1:#$0397; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA
    (Unicode:#$1F2A; Attr:daNone; Ch1:#$1F28; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA
    (Unicode:#$1F2B; Attr:daNone; Ch1:#$1F29; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA
    (Unicode:#$1F2C; Attr:daNone; Ch1:#$1F28; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA
    (Unicode:#$1F2D; Attr:daNone; Ch1:#$1F29; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA
    (Unicode:#$1F2E; Attr:daNone; Ch1:#$1F28; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F2F; Attr:daNone; Ch1:#$1F29; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F30; Attr:daNone; Ch1:#$03B9; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH PSILI
    (Unicode:#$1F31; Attr:daNone; Ch1:#$03B9; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DASIA
    (Unicode:#$1F32; Attr:daNone; Ch1:#$1F30; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F33; Attr:daNone; Ch1:#$1F31; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F34; Attr:daNone; Ch1:#$1F30; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F35; Attr:daNone; Ch1:#$1F31; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F36; Attr:daNone; Ch1:#$1F30; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F37; Attr:daNone; Ch1:#$1F31; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F38; Attr:daNone; Ch1:#$0399; Ch2:#$0313; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH PSILI
    (Unicode:#$1F39; Attr:daNone; Ch1:#$0399; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH DASIA
    (Unicode:#$1F3A; Attr:daNone; Ch1:#$1F38; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH PSILI AND VARIA
    (Unicode:#$1F3B; Attr:daNone; Ch1:#$1F39; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH DASIA AND VARIA
    (Unicode:#$1F3C; Attr:daNone; Ch1:#$1F38; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH PSILI AND OXIA
    (Unicode:#$1F3D; Attr:daNone; Ch1:#$1F39; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH DASIA AND OXIA
    (Unicode:#$1F3E; Attr:daNone; Ch1:#$1F38; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F3F; Attr:daNone; Ch1:#$1F39; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F40; Attr:daNone; Ch1:#$03BF; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH PSILI
    (Unicode:#$1F41; Attr:daNone; Ch1:#$03BF; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH DASIA
    (Unicode:#$1F42; Attr:daNone; Ch1:#$1F40; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F43; Attr:daNone; Ch1:#$1F41; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F44; Attr:daNone; Ch1:#$1F40; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F45; Attr:daNone; Ch1:#$1F41; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F48; Attr:daNone; Ch1:#$039F; Ch2:#$0313; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH PSILI
    (Unicode:#$1F49; Attr:daNone; Ch1:#$039F; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH DASIA
    (Unicode:#$1F4A; Attr:daNone; Ch1:#$1F48; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH PSILI AND VARIA
    (Unicode:#$1F4B; Attr:daNone; Ch1:#$1F49; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH DASIA AND VARIA
    (Unicode:#$1F4C; Attr:daNone; Ch1:#$1F48; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH PSILI AND OXIA
    (Unicode:#$1F4D; Attr:daNone; Ch1:#$1F49; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH DASIA AND OXIA
    (Unicode:#$1F50; Attr:daNone; Ch1:#$03C5; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH PSILI
    (Unicode:#$1F51; Attr:daNone; Ch1:#$03C5; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DASIA
    (Unicode:#$1F52; Attr:daNone; Ch1:#$1F50; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH PSILI AND VARIA
    (Unicode:#$1F53; Attr:daNone; Ch1:#$1F51; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F54; Attr:daNone; Ch1:#$1F50; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH PSILI AND OXIA
    (Unicode:#$1F55; Attr:daNone; Ch1:#$1F51; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F56; Attr:daNone; Ch1:#$1F50; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH PSILI AND PERISPOMENI
    (Unicode:#$1F57; Attr:daNone; Ch1:#$1F51; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F59; Attr:daNone; Ch1:#$03A5; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH DASIA
    (Unicode:#$1F5B; Attr:daNone; Ch1:#$1F59; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND VARIA
    (Unicode:#$1F5D; Attr:daNone; Ch1:#$1F59; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND OXIA
    (Unicode:#$1F5F; Attr:daNone; Ch1:#$1F59; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH DASIA AND PERISPOMENI
    (Unicode:#$1F60; Attr:daNone; Ch1:#$03C9; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI
    (Unicode:#$1F61; Attr:daNone; Ch1:#$03C9; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA
    (Unicode:#$1F62; Attr:daNone; Ch1:#$1F60; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F63; Attr:daNone; Ch1:#$1F61; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F64; Attr:daNone; Ch1:#$1F60; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F65; Attr:daNone; Ch1:#$1F61; Ch2:#$0301; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F66; Attr:daNone; Ch1:#$1F60; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F67; Attr:daNone; Ch1:#$1F61; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F68; Attr:daNone; Ch1:#$03A9; Ch2:#$0313; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI
    (Unicode:#$1F69; Attr:daNone; Ch1:#$03A9; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA
    (Unicode:#$1F6A; Attr:daNone; Ch1:#$1F68; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA
    (Unicode:#$1F6B; Attr:daNone; Ch1:#$1F69; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA
    (Unicode:#$1F6C; Attr:daNone; Ch1:#$1F68; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA
    (Unicode:#$1F6D; Attr:daNone; Ch1:#$1F69; Ch2:#$0301; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA
    (Unicode:#$1F6E; Attr:daNone; Ch1:#$1F68; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI
    (Unicode:#$1F6F; Attr:daNone; Ch1:#$1F69; Ch2:#$0342; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI
    (Unicode:#$1F70; Attr:daNone; Ch1:#$03B1; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH VARIA
    (Unicode:#$1F71; Attr:daNone; Ch1:#$03AC; Ch2:#$FFFF),                // GREEK SMALL LETTER ALPHA WITH OXIA
    (Unicode:#$1F72; Attr:daNone; Ch1:#$03B5; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER EPSILON WITH VARIA
    (Unicode:#$1F73; Attr:daNone; Ch1:#$03AD; Ch2:#$FFFF),                // GREEK SMALL LETTER EPSILON WITH OXIA
    (Unicode:#$1F74; Attr:daNone; Ch1:#$03B7; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH VARIA
    (Unicode:#$1F75; Attr:daNone; Ch1:#$03AE; Ch2:#$FFFF),                // GREEK SMALL LETTER ETA WITH OXIA
    (Unicode:#$1F76; Attr:daNone; Ch1:#$03B9; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH VARIA
    (Unicode:#$1F77; Attr:daNone; Ch1:#$03AF; Ch2:#$FFFF),                // GREEK SMALL LETTER IOTA WITH OXIA
    (Unicode:#$1F78; Attr:daNone; Ch1:#$03BF; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER OMICRON WITH VARIA
    (Unicode:#$1F79; Attr:daNone; Ch1:#$03CC; Ch2:#$FFFF),                // GREEK SMALL LETTER OMICRON WITH OXIA
    (Unicode:#$1F7A; Attr:daNone; Ch1:#$03C5; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH VARIA
    (Unicode:#$1F7B; Attr:daNone; Ch1:#$03CD; Ch2:#$FFFF),                // GREEK SMALL LETTER UPSILON WITH OXIA
    (Unicode:#$1F7C; Attr:daNone; Ch1:#$03C9; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH VARIA
    (Unicode:#$1F7D; Attr:daNone; Ch1:#$03CE; Ch2:#$FFFF),                // GREEK SMALL LETTER OMEGA WITH OXIA
    (Unicode:#$1F80; Attr:daNone; Ch1:#$1F00; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F81; Attr:daNone; Ch1:#$1F01; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F82; Attr:daNone; Ch1:#$1F02; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F83; Attr:daNone; Ch1:#$1F03; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F84; Attr:daNone; Ch1:#$1F04; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F85; Attr:daNone; Ch1:#$1F05; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F86; Attr:daNone; Ch1:#$1F06; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F87; Attr:daNone; Ch1:#$1F07; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F88; Attr:daNone; Ch1:#$1F08; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F89; Attr:daNone; Ch1:#$1F09; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F8A; Attr:daNone; Ch1:#$1F0A; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8B; Attr:daNone; Ch1:#$1F0B; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F8C; Attr:daNone; Ch1:#$1F0C; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8D; Attr:daNone; Ch1:#$1F0D; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F8E; Attr:daNone; Ch1:#$1F0E; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F8F; Attr:daNone; Ch1:#$1F0F; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F90; Attr:daNone; Ch1:#$1F20; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1F91; Attr:daNone; Ch1:#$1F21; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1F92; Attr:daNone; Ch1:#$1F22; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F93; Attr:daNone; Ch1:#$1F23; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1F94; Attr:daNone; Ch1:#$1F24; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F95; Attr:daNone; Ch1:#$1F25; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1F96; Attr:daNone; Ch1:#$1F26; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F97; Attr:daNone; Ch1:#$1F27; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1F98; Attr:daNone; Ch1:#$1F28; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1F99; Attr:daNone; Ch1:#$1F29; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1F9A; Attr:daNone; Ch1:#$1F2A; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9B; Attr:daNone; Ch1:#$1F2B; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1F9C; Attr:daNone; Ch1:#$1F2C; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9D; Attr:daNone; Ch1:#$1F2D; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1F9E; Attr:daNone; Ch1:#$1F2E; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1F9F; Attr:daNone; Ch1:#$1F2F; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FA0; Attr:daNone; Ch1:#$1F60; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND YPOGEGRAMMENI
    (Unicode:#$1FA1; Attr:daNone; Ch1:#$1F61; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND YPOGEGRAMMENI
    (Unicode:#$1FA2; Attr:daNone; Ch1:#$1F62; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA3; Attr:daNone; Ch1:#$1F63; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FA4; Attr:daNone; Ch1:#$1F64; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA5; Attr:daNone; Ch1:#$1F65; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FA6; Attr:daNone; Ch1:#$1F66; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PSILI AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA7; Attr:daNone; Ch1:#$1F67; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH DASIA AND PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FA8; Attr:daNone; Ch1:#$1F68; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PROSGEGRAMMENI
    (Unicode:#$1FA9; Attr:daNone; Ch1:#$1F69; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PROSGEGRAMMENI
    (Unicode:#$1FAA; Attr:daNone; Ch1:#$1F6A; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAB; Attr:daNone; Ch1:#$1F6B; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND VARIA AND PROSGEGRAMMENI
    (Unicode:#$1FAC; Attr:daNone; Ch1:#$1F6C; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAD; Attr:daNone; Ch1:#$1F6D; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND OXIA AND PROSGEGRAMMENI
    (Unicode:#$1FAE; Attr:daNone; Ch1:#$1F6E; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PSILI AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FAF; Attr:daNone; Ch1:#$1F6F; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH DASIA AND PERISPOMENI AND PROSGEGRAMMENI
    (Unicode:#$1FB0; Attr:daNone; Ch1:#$03B1; Ch2:#$0306; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB1; Attr:daNone; Ch1:#$03B1; Ch2:#$0304; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH MACRON
    (Unicode:#$1FB2; Attr:daNone; Ch1:#$1F70; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FB3; Attr:daNone; Ch1:#$03B1; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH YPOGEGRAMMENI
    (Unicode:#$1FB4; Attr:daNone; Ch1:#$03AC; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FB6; Attr:daNone; Ch1:#$03B1; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI
    (Unicode:#$1FB7; Attr:daNone; Ch1:#$1FB6; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ALPHA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FB8; Attr:daNone; Ch1:#$0391; Ch2:#$0306; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH VRACHY
    (Unicode:#$1FB9; Attr:daNone; Ch1:#$0391; Ch2:#$0304; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH MACRON
    (Unicode:#$1FBA; Attr:daNone; Ch1:#$0391; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH VARIA
    (Unicode:#$1FBB; Attr:daNone; Ch1:#$0386; Ch2:#$FFFF),                // GREEK CAPITAL LETTER ALPHA WITH OXIA
    (Unicode:#$1FBC; Attr:daNone; Ch1:#$0391; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ALPHA WITH PROSGEGRAMMENI
    (Unicode:#$1FBD; Attr:daCompat; Ch1:#$0020; Ch2:#$0313; Ch3:#$FFFF),  // GREEK KORONIS
    (Unicode:#$1FBE; Attr:daNone; Ch1:#$03B9; Ch2:#$FFFF),                // GREEK PROSGEGRAMMENI
    (Unicode:#$1FBF; Attr:daCompat; Ch1:#$0020; Ch2:#$0313; Ch3:#$FFFF),  // GREEK PSILI
    (Unicode:#$1FC0; Attr:daCompat; Ch1:#$0020; Ch2:#$0342; Ch3:#$FFFF),  // GREEK PERISPOMENI
    (Unicode:#$1FC1; Attr:daNone; Ch1:#$00A8; Ch2:#$0342; Ch3:#$FFFF),    // GREEK DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FC2; Attr:daNone; Ch1:#$1F74; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FC3; Attr:daNone; Ch1:#$03B7; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH YPOGEGRAMMENI
    (Unicode:#$1FC4; Attr:daNone; Ch1:#$03AE; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FC6; Attr:daNone; Ch1:#$03B7; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PERISPOMENI
    (Unicode:#$1FC7; Attr:daNone; Ch1:#$1FC6; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER ETA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FC8; Attr:daNone; Ch1:#$0395; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER EPSILON WITH VARIA
    (Unicode:#$1FC9; Attr:daNone; Ch1:#$0388; Ch2:#$FFFF),                // GREEK CAPITAL LETTER EPSILON WITH OXIA
    (Unicode:#$1FCA; Attr:daNone; Ch1:#$0397; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH VARIA
    (Unicode:#$1FCB; Attr:daNone; Ch1:#$0389; Ch2:#$FFFF),                // GREEK CAPITAL LETTER ETA WITH OXIA
    (Unicode:#$1FCC; Attr:daNone; Ch1:#$0397; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER ETA WITH PROSGEGRAMMENI
    (Unicode:#$1FCD; Attr:daNone; Ch1:#$1FBF; Ch2:#$0300; Ch3:#$FFFF),    // GREEK PSILI AND VARIA
    (Unicode:#$1FCE; Attr:daNone; Ch1:#$1FBF; Ch2:#$0301; Ch3:#$FFFF),    // GREEK PSILI AND OXIA
    (Unicode:#$1FCF; Attr:daNone; Ch1:#$1FBF; Ch2:#$0342; Ch3:#$FFFF),    // GREEK PSILI AND PERISPOMENI
    (Unicode:#$1FD0; Attr:daNone; Ch1:#$03B9; Ch2:#$0306; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD1; Attr:daNone; Ch1:#$03B9; Ch2:#$0304; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH MACRON
    (Unicode:#$1FD2; Attr:daNone; Ch1:#$03CA; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND VARIA
    (Unicode:#$1FD3; Attr:daNone; Ch1:#$0390; Ch2:#$FFFF),                // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND OXIA
    (Unicode:#$1FD6; Attr:daNone; Ch1:#$03B9; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH PERISPOMENI
    (Unicode:#$1FD7; Attr:daNone; Ch1:#$03CA; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER IOTA WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FD8; Attr:daNone; Ch1:#$0399; Ch2:#$0306; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH VRACHY
    (Unicode:#$1FD9; Attr:daNone; Ch1:#$0399; Ch2:#$0304; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH MACRON
    (Unicode:#$1FDA; Attr:daNone; Ch1:#$0399; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER IOTA WITH VARIA
    (Unicode:#$1FDB; Attr:daNone; Ch1:#$038A; Ch2:#$FFFF),                // GREEK CAPITAL LETTER IOTA WITH OXIA
    (Unicode:#$1FDD; Attr:daNone; Ch1:#$1FFE; Ch2:#$0300; Ch3:#$FFFF),    // GREEK DASIA AND VARIA
    (Unicode:#$1FDE; Attr:daNone; Ch1:#$1FFE; Ch2:#$0301; Ch3:#$FFFF),    // GREEK DASIA AND OXIA
    (Unicode:#$1FDF; Attr:daNone; Ch1:#$1FFE; Ch2:#$0342; Ch3:#$FFFF),    // GREEK DASIA AND PERISPOMENI
    (Unicode:#$1FE0; Attr:daNone; Ch1:#$03C5; Ch2:#$0306; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE1; Attr:daNone; Ch1:#$03C5; Ch2:#$0304; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH MACRON
    (Unicode:#$1FE2; Attr:daNone; Ch1:#$03CB; Ch2:#$0300; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND VARIA
    (Unicode:#$1FE3; Attr:daNone; Ch1:#$03B0; Ch2:#$FFFF),                // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND OXIA
    (Unicode:#$1FE4; Attr:daNone; Ch1:#$03C1; Ch2:#$0313; Ch3:#$FFFF),    // GREEK SMALL LETTER RHO WITH PSILI
    (Unicode:#$1FE5; Attr:daNone; Ch1:#$03C1; Ch2:#$0314; Ch3:#$FFFF),    // GREEK SMALL LETTER RHO WITH DASIA
    (Unicode:#$1FE6; Attr:daNone; Ch1:#$03C5; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH PERISPOMENI
    (Unicode:#$1FE7; Attr:daNone; Ch1:#$03CB; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER UPSILON WITH DIALYTIKA AND PERISPOMENI
    (Unicode:#$1FE8; Attr:daNone; Ch1:#$03A5; Ch2:#$0306; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH VRACHY
    (Unicode:#$1FE9; Attr:daNone; Ch1:#$03A5; Ch2:#$0304; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH MACRON
    (Unicode:#$1FEA; Attr:daNone; Ch1:#$03A5; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER UPSILON WITH VARIA
    (Unicode:#$1FEB; Attr:daNone; Ch1:#$038E; Ch2:#$FFFF),                // GREEK CAPITAL LETTER UPSILON WITH OXIA
    (Unicode:#$1FEC; Attr:daNone; Ch1:#$03A1; Ch2:#$0314; Ch3:#$FFFF),    // GREEK CAPITAL LETTER RHO WITH DASIA
    (Unicode:#$1FED; Attr:daNone; Ch1:#$00A8; Ch2:#$0300; Ch3:#$FFFF),    // GREEK DIALYTIKA AND VARIA
    (Unicode:#$1FEE; Attr:daNone; Ch1:#$0385; Ch2:#$FFFF),                // GREEK DIALYTIKA AND OXIA
    (Unicode:#$1FEF; Attr:daNone; Ch1:#$0060; Ch2:#$FFFF),                // GREEK VARIA
    (Unicode:#$1FF2; Attr:daNone; Ch1:#$1F7C; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH VARIA AND YPOGEGRAMMENI
    (Unicode:#$1FF3; Attr:daNone; Ch1:#$03C9; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH YPOGEGRAMMENI
    (Unicode:#$1FF4; Attr:daNone; Ch1:#$03CE; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH OXIA AND YPOGEGRAMMENI
    (Unicode:#$1FF6; Attr:daNone; Ch1:#$03C9; Ch2:#$0342; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI
    (Unicode:#$1FF7; Attr:daNone; Ch1:#$1FF6; Ch2:#$0345; Ch3:#$FFFF),    // GREEK SMALL LETTER OMEGA WITH PERISPOMENI AND YPOGEGRAMMENI
    (Unicode:#$1FF8; Attr:daNone; Ch1:#$039F; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMICRON WITH VARIA
    (Unicode:#$1FF9; Attr:daNone; Ch1:#$038C; Ch2:#$FFFF),                // GREEK CAPITAL LETTER OMICRON WITH OXIA
    (Unicode:#$1FFA; Attr:daNone; Ch1:#$03A9; Ch2:#$0300; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH VARIA
    (Unicode:#$1FFB; Attr:daNone; Ch1:#$038F; Ch2:#$FFFF),                // GREEK CAPITAL LETTER OMEGA WITH OXIA
    (Unicode:#$1FFC; Attr:daNone; Ch1:#$03A9; Ch2:#$0345; Ch3:#$FFFF),    // GREEK CAPITAL LETTER OMEGA WITH PROSGEGRAMMENI
    (Unicode:#$1FFD; Attr:daNone; Ch1:#$00B4; Ch2:#$FFFF),                // GREEK OXIA
    (Unicode:#$1FFE; Attr:daCompat; Ch1:#$0020; Ch2:#$0314; Ch3:#$FFFF),  // GREEK DASIA
    (Unicode:#$2000; Attr:daNone; Ch1:#$2002; Ch2:#$FFFF),                             // EN QUAD
    (Unicode:#$2001; Attr:daNone; Ch1:#$2003; Ch2:#$FFFF),                             // EM QUAD
    (Unicode:#$2002; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // EN SPACE
    (Unicode:#$2003; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // EM SPACE
    (Unicode:#$2004; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // THREE-PER-EM SPACE
    (Unicode:#$2005; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // FOUR-PER-EM SPACE
    (Unicode:#$2006; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // SIX-PER-EM SPACE
    (Unicode:#$2007; Attr:daNoBreak; Ch1:#$0020; Ch2:#$FFFF),                          // FIGURE SPACE
    (Unicode:#$2008; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // PUNCTUATION SPACE
    (Unicode:#$2009; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // THIN SPACE
    (Unicode:#$200A; Attr:daCompat; Ch1:#$0020; Ch2:#$FFFF),                           // HAIR SPACE
    (Unicode:#$2011; Attr:daNoBreak; Ch1:#$2010; Ch2:#$FFFF),                          // NON-BREAKING HYPHEN
    (Unicode:#$2017; Attr:daCompat; Ch1:#$0020; Ch2:#$0333; Ch3:#$FFFF),               // DOUBLE LOW LINE
    (Unicode:#$2024; Attr:daCompat; Ch1:#$002E; Ch2:#$FFFF),                           // ONE DOT LEADER
    (Unicode:#$2025; Attr:daCompat; Ch1:#$002E; Ch2:#$002E; Ch3:#$FFFF),               // TWO DOT LEADER
    (Unicode:#$2026; Attr:daCompat; Ch1:#$002E; Ch2:#$002E; Ch3:#$002E; Ch4:#$FFFF),   // HORIZONTAL ELLIPSIS
    (Unicode:#$202F; Attr:daNoBreak; Ch1:#$0020; Ch2:#$FFFF),                          // NARROW NO-BREAK SPACE
    (Unicode:#$2033; Attr:daCompat; Ch1:#$2032; Ch2:#$2032; Ch3:#$FFFF),               // DOUBLE PRIME
    (Unicode:#$2034; Attr:daCompat; Ch1:#$2032; Ch2:#$2032; Ch3:#$2032; Ch4:#$FFFF),   // TRIPLE PRIME
    (Unicode:#$2036; Attr:daCompat; Ch1:#$2035; Ch2:#$2035; Ch3:#$FFFF),               // REVERSED DOUBLE PRIME
    (Unicode:#$2037; Attr:daCompat; Ch1:#$2035; Ch2:#$2035; Ch3:#$2035; Ch4:#$FFFF),   // REVERSED TRIPLE PRIME
    (Unicode:#$203C; Attr:daCompat; Ch1:#$0021; Ch2:#$0021; Ch3:#$FFFF),               // DOUBLE EXCLAMATION MARK
    (Unicode:#$203E; Attr:daCompat; Ch1:#$0020; Ch2:#$0305; Ch3:#$FFFF),               // OVERLINE
    (Unicode:#$2048; Attr:daCompat; Ch1:#$003F; Ch2:#$0021; Ch3:#$FFFF),               // QUESTION EXCLAMATION MARK
    (Unicode:#$2049; Attr:daCompat; Ch1:#$0021; Ch2:#$003F; Ch3:#$FFFF),               // EXCLAMATION QUESTION MARK
    (Unicode:#$2070; Attr:daSuper; Ch1:#$0030; Ch2:#$FFFF),               // SUPERSCRIPT ZERO
    (Unicode:#$2074; Attr:daSuper; Ch1:#$0034; Ch2:#$FFFF),               // SUPERSCRIPT FOUR
    (Unicode:#$2075; Attr:daSuper; Ch1:#$0035; Ch2:#$FFFF),               // SUPERSCRIPT FIVE
    (Unicode:#$2076; Attr:daSuper; Ch1:#$0036; Ch2:#$FFFF),               // SUPERSCRIPT SIX
    (Unicode:#$2077; Attr:daSuper; Ch1:#$0037; Ch2:#$FFFF),               // SUPERSCRIPT SEVEN
    (Unicode:#$2078; Attr:daSuper; Ch1:#$0038; Ch2:#$FFFF),               // SUPERSCRIPT EIGHT
    (Unicode:#$2079; Attr:daSuper; Ch1:#$0039; Ch2:#$FFFF),               // SUPERSCRIPT NINE
    (Unicode:#$207A; Attr:daSuper; Ch1:#$002B; Ch2:#$FFFF),               // SUPERSCRIPT PLUS SIGN
    (Unicode:#$207B; Attr:daSuper; Ch1:#$2212; Ch2:#$FFFF),               // SUPERSCRIPT MINUS
    (Unicode:#$207C; Attr:daSuper; Ch1:#$003D; Ch2:#$FFFF),               // SUPERSCRIPT EQUALS SIGN
    (Unicode:#$207D; Attr:daSuper; Ch1:#$0028; Ch2:#$FFFF),               // SUPERSCRIPT LEFT PARENTHESIS
    (Unicode:#$207E; Attr:daSuper; Ch1:#$0029; Ch2:#$FFFF),               // SUPERSCRIPT RIGHT PARENTHESIS
    (Unicode:#$207F; Attr:daSuper; Ch1:#$006E; Ch2:#$FFFF),               // SUPERSCRIPT LATIN SMALL LETTER N
    (Unicode:#$2080; Attr:daSub; Ch1:#$0030; Ch2:#$FFFF),                 // SUBSCRIPT ZERO
    (Unicode:#$2081; Attr:daSub; Ch1:#$0031; Ch2:#$FFFF),                 // SUBSCRIPT ONE
    (Unicode:#$2082; Attr:daSub; Ch1:#$0032; Ch2:#$FFFF),                 // SUBSCRIPT TWO
    (Unicode:#$2083; Attr:daSub; Ch1:#$0033; Ch2:#$FFFF),                 // SUBSCRIPT THREE
    (Unicode:#$2084; Attr:daSub; Ch1:#$0034; Ch2:#$FFFF),                 // SUBSCRIPT FOUR
    (Unicode:#$2085; Attr:daSub; Ch1:#$0035; Ch2:#$FFFF),                 // SUBSCRIPT FIVE
    (Unicode:#$2086; Attr:daSub; Ch1:#$0036; Ch2:#$FFFF),                 // SUBSCRIPT SIX
    (Unicode:#$2087; Attr:daSub; Ch1:#$0037; Ch2:#$FFFF),                 // SUBSCRIPT SEVEN
    (Unicode:#$2088; Attr:daSub; Ch1:#$0038; Ch2:#$FFFF),                 // SUBSCRIPT EIGHT
    (Unicode:#$2089; Attr:daSub; Ch1:#$0039; Ch2:#$FFFF),                 // SUBSCRIPT NINE
    (Unicode:#$208A; Attr:daSub; Ch1:#$002B; Ch2:#$FFFF),                 // SUBSCRIPT PLUS SIGN
    (Unicode:#$208B; Attr:daSub; Ch1:#$2212; Ch2:#$FFFF),                 // SUBSCRIPT MINUS
    (Unicode:#$208C; Attr:daSub; Ch1:#$003D; Ch2:#$FFFF),                 // SUBSCRIPT EQUALS SIGN
    (Unicode:#$208D; Attr:daSub; Ch1:#$0028; Ch2:#$FFFF),                 // SUBSCRIPT LEFT PARENTHESIS
    (Unicode:#$208E; Attr:daSub; Ch1:#$0029; Ch2:#$FFFF),                 // SUBSCRIPT RIGHT PARENTHESIS
    (Unicode:#$20A8; Attr:daCompat; Ch1:#$0052; Ch2:#$0073; Ch3:#$FFFF),               // RUPEE SIGN
    (Unicode:#$2100; Attr:daCompat; Ch1:#$0061; Ch2:#$002F; Ch3:#$0063; Ch4:#$FFFF),   // ACCOUNT OF
    (Unicode:#$2101; Attr:daCompat; Ch1:#$0061; Ch2:#$002F; Ch3:#$0073; Ch4:#$FFFF),   // ADDRESSED TO THE SUBJECT
    (Unicode:#$2102; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL C
    (Unicode:#$2103; Attr:daCompat; Ch1:#$00B0; Ch2:#$0043; Ch3:#$FFFF),               // DEGREE CELSIUS
    (Unicode:#$2105; Attr:daCompat; Ch1:#$0063; Ch2:#$002F; Ch3:#$006F; Ch4:#$FFFF),   // CARE OF
    (Unicode:#$2106; Attr:daCompat; Ch1:#$0063; Ch2:#$002F; Ch3:#$0075; Ch4:#$FFFF),   // CADA UNA
    (Unicode:#$2107; Attr:daCompat; Ch1:#$0190; Ch2:#$FFFF),                           // EULER CONSTANT
    (Unicode:#$2109; Attr:daCompat; Ch1:#$00B0; Ch2:#$0046; Ch3:#$FFFF),               // DEGREE FAHRENHEIT
    (Unicode:#$210A; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),                             // SCRIPT SMALL G
    (Unicode:#$210B; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),                             // SCRIPT CAPITAL H
    (Unicode:#$210C; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),                             // BLACK-LETTER CAPITAL H
    (Unicode:#$210D; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL H
    (Unicode:#$210E; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),                             // PLANCK CONSTANT
    (Unicode:#$210F; Attr:daFont; Ch1:#$0127; Ch2:#$FFFF),                             // PLANCK CONSTANT OVER TWO PI
    (Unicode:#$2110; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),                             // SCRIPT CAPITAL I
    (Unicode:#$2111; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),                             // BLACK-LETTER CAPITAL I
    (Unicode:#$2112; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),                             // SCRIPT CAPITAL L
    (Unicode:#$2113; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),                             // SCRIPT SMALL L
    (Unicode:#$2115; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL N
    (Unicode:#$2116; Attr:daCompat; Ch1:#$004E; Ch2:#$006F; Ch3:#$FFFF),               // NUMERO SIGN
    (Unicode:#$2119; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL P
    (Unicode:#$211A; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL Q
    (Unicode:#$211B; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),                             // SCRIPT CAPITAL R
    (Unicode:#$211C; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),                             // BLACK-LETTER CAPITAL R
    (Unicode:#$211D; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL R
    (Unicode:#$2120; Attr:daSuper; Ch1:#$0053; Ch2:#$004D; Ch3:#$FFFF),                // SERVICE MARK
    (Unicode:#$2121; Attr:daCompat; Ch1:#$0054; Ch2:#$0045; Ch3:#$004C; Ch4:#$FFFF),   // TELEPHONE SIGN
    (Unicode:#$2122; Attr:daSuper; Ch1:#$0054; Ch2:#$004D; Ch3:#$FFFF),                // TRADE MARK SIGN
    (Unicode:#$2124; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),                             // DOUBLE-STRUCK CAPITAL Z
    (Unicode:#$2126; Attr:daNone; Ch1:#$03A9; Ch2:#$FFFF),                             // OHM SIGN
    (Unicode:#$2128; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),                             // BLACK-LETTER CAPITAL Z
    (Unicode:#$212A; Attr:daNone; Ch1:#$004B; Ch2:#$FFFF),                             // KELVIN SIGN
    (Unicode:#$212B; Attr:daNone; Ch1:#$00C5; Ch2:#$FFFF),                             // ANGSTROM SIGN
    (Unicode:#$212C; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),                             // SCRIPT CAPITAL B
    (Unicode:#$212D; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),                             // BLACK-LETTER CAPITAL C
    (Unicode:#$212F; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),                             // SCRIPT SMALL E
    (Unicode:#$2130; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),                             // SCRIPT CAPITAL E
    (Unicode:#$2131; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),                             // SCRIPT CAPITAL F
    (Unicode:#$2133; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),                             // SCRIPT CAPITAL M
    (Unicode:#$2134; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),                             // SCRIPT SMALL O
    (Unicode:#$2135; Attr:daCompat; Ch1:#$05D0; Ch2:#$FFFF),                           // ALEF SYMBOL
    (Unicode:#$2136; Attr:daCompat; Ch1:#$05D1; Ch2:#$FFFF),                           // BET SYMBOL
    (Unicode:#$2137; Attr:daCompat; Ch1:#$05D2; Ch2:#$FFFF),                           // GIMEL SYMBOL
    (Unicode:#$2138; Attr:daCompat; Ch1:#$05D3; Ch2:#$FFFF),                           // DALET SYMBOL
    (Unicode:#$2139; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),                             // INFORMATION SOURCE
    (Unicode:#$2153; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$0033; Ch4:#$FFFF), // VULGAR FRACTION ONE THIRD
    (Unicode:#$2154; Attr:daFraction; Ch1:#$0032; Ch2:#$2044; Ch3:#$0033; Ch4:#$FFFF), // VULGAR FRACTION TWO THIRDS
    (Unicode:#$2155; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$0035; Ch4:#$FFFF), // VULGAR FRACTION ONE FIFTH
    (Unicode:#$2156; Attr:daFraction; Ch1:#$0032; Ch2:#$2044; Ch3:#$0035; Ch4:#$FFFF), // VULGAR FRACTION TWO FIFTHS
    (Unicode:#$2157; Attr:daFraction; Ch1:#$0033; Ch2:#$2044; Ch3:#$0035; Ch4:#$FFFF), // VULGAR FRACTION THREE FIFTHS
    (Unicode:#$2158; Attr:daFraction; Ch1:#$0034; Ch2:#$2044; Ch3:#$0035; Ch4:#$FFFF), // VULGAR FRACTION FOUR FIFTHS
    (Unicode:#$2159; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$0036; Ch4:#$FFFF), // VULGAR FRACTION ONE SIXTH
    (Unicode:#$215A; Attr:daFraction; Ch1:#$0035; Ch2:#$2044; Ch3:#$0036; Ch4:#$FFFF), // VULGAR FRACTION FIVE SIXTHS
    (Unicode:#$215B; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$0038; Ch4:#$FFFF), // VULGAR FRACTION ONE EIGHTH
    (Unicode:#$215C; Attr:daFraction; Ch1:#$0033; Ch2:#$2044; Ch3:#$0038; Ch4:#$FFFF), // VULGAR FRACTION THREE EIGHTHS
    (Unicode:#$215D; Attr:daFraction; Ch1:#$0035; Ch2:#$2044; Ch3:#$0038; Ch4:#$FFFF), // VULGAR FRACTION FIVE EIGHTHS
    (Unicode:#$215E; Attr:daFraction; Ch1:#$0037; Ch2:#$2044; Ch3:#$0038; Ch4:#$FFFF), // VULGAR FRACTION SEVEN EIGHTHS
    (Unicode:#$215F; Attr:daFraction; Ch1:#$0031; Ch2:#$2044; Ch3:#$FFFF),             // FRACTION NUMERATOR ONE
    (Unicode:#$2160; Attr:daCompat; Ch1:#$0049; Ch2:#$FFFF),                           // ROMAN NUMERAL ONE
    (Unicode:#$2161; Attr:daCompat; Ch1:#$0049; Ch2:#$0049; Ch3:#$FFFF),               // ROMAN NUMERAL TWO
    (Unicode:#$2162; Attr:daCompat; Ch1:#$0049; Ch2:#$0049; Ch3:#$0049; Ch4:#$FFFF),   // ROMAN NUMERAL THREE
    (Unicode:#$2163; Attr:daCompat; Ch1:#$0049; Ch2:#$0056; Ch3:#$FFFF),               // ROMAN NUMERAL FOUR
    (Unicode:#$2164; Attr:daCompat; Ch1:#$0056; Ch2:#$FFFF),                           // ROMAN NUMERAL FIVE
    (Unicode:#$2165; Attr:daCompat; Ch1:#$0056; Ch2:#$0049; Ch3:#$FFFF),               // ROMAN NUMERAL SIX
    (Unicode:#$2166; Attr:daCompat; Ch1:#$0056; Ch2:#$0049; Ch3:#$0049; Ch4:#$FFFF),   // ROMAN NUMERAL SEVEN
    (Unicode:#$2167; Attr:daCompat; Ch1:#$0056; Ch2:#$0049; Ch3:#$0049; Ch4:#$0049; Ch5:#$FFFF),  // ROMAN NUMERAL EIGHT
    (Unicode:#$2168; Attr:daCompat; Ch1:#$0049; Ch2:#$0058; Ch3:#$FFFF),               // ROMAN NUMERAL NINE
    (Unicode:#$2169; Attr:daCompat; Ch1:#$0058; Ch2:#$FFFF),                           // ROMAN NUMERAL TEN
    (Unicode:#$216A; Attr:daCompat; Ch1:#$0058; Ch2:#$0049; Ch3:#$FFFF),               // ROMAN NUMERAL ELEVEN
    (Unicode:#$216B; Attr:daCompat; Ch1:#$0058; Ch2:#$0049; Ch3:#$0049; Ch4:#$FFFF),   // ROMAN NUMERAL TWELVE
    (Unicode:#$216C; Attr:daCompat; Ch1:#$004C; Ch2:#$FFFF),                           // ROMAN NUMERAL FIFTY
    (Unicode:#$216D; Attr:daCompat; Ch1:#$0043; Ch2:#$FFFF),                           // ROMAN NUMERAL ONE HUNDRED
    (Unicode:#$216E; Attr:daCompat; Ch1:#$0044; Ch2:#$FFFF),                           // ROMAN NUMERAL FIVE HUNDRED
    (Unicode:#$216F; Attr:daCompat; Ch1:#$004D; Ch2:#$FFFF),                           // ROMAN NUMERAL ONE THOUSAND
    (Unicode:#$2170; Attr:daCompat; Ch1:#$0069; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL ONE
    (Unicode:#$2171; Attr:daCompat; Ch1:#$0069; Ch2:#$0069; Ch3:#$FFFF),               // SMALL ROMAN NUMERAL TWO
    (Unicode:#$2172; Attr:daCompat; Ch1:#$0069; Ch2:#$0069; Ch3:#$0069; Ch4:#$FFFF),   // SMALL ROMAN NUMERAL THREE
    (Unicode:#$2173; Attr:daCompat; Ch1:#$0069; Ch2:#$0076; Ch3:#$FFFF),               // SMALL ROMAN NUMERAL FOUR
    (Unicode:#$2174; Attr:daCompat; Ch1:#$0076; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL FIVE
    (Unicode:#$2175; Attr:daCompat; Ch1:#$0076; Ch2:#$0069; Ch3:#$FFFF),               // SMALL ROMAN NUMERAL SIX
    (Unicode:#$2176; Attr:daCompat; Ch1:#$0076; Ch2:#$0069; Ch3:#$0069; Ch4:#$FFFF),   // SMALL ROMAN NUMERAL SEVEN
    (Unicode:#$2177; Attr:daCompat; Ch1:#$0076; Ch2:#$0069; Ch3:#$0069; Ch4:#$0069; Ch5:#$FFFF),  // SMALL ROMAN NUMERAL EIGHT
    (Unicode:#$2178; Attr:daCompat; Ch1:#$0069; Ch2:#$0078; Ch3:#$FFFF),               // SMALL ROMAN NUMERAL NINE
    (Unicode:#$2179; Attr:daCompat; Ch1:#$0078; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL TEN
    (Unicode:#$217A; Attr:daCompat; Ch1:#$0078; Ch2:#$0069; Ch3:#$FFFF),               // SMALL ROMAN NUMERAL ELEVEN
    (Unicode:#$217B; Attr:daCompat; Ch1:#$0078; Ch2:#$0069; Ch3:#$0069; Ch4:#$FFFF),   // SMALL ROMAN NUMERAL TWELVE
    (Unicode:#$217C; Attr:daCompat; Ch1:#$006C; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL FIFTY
    (Unicode:#$217D; Attr:daCompat; Ch1:#$0063; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL ONE HUNDRED
    (Unicode:#$217E; Attr:daCompat; Ch1:#$0064; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL FIVE HUNDRED
    (Unicode:#$217F; Attr:daCompat; Ch1:#$006D; Ch2:#$FFFF),                           // SMALL ROMAN NUMERAL ONE THOUSAND
    (Unicode:#$219A; Attr:daNone; Ch1:#$2190; Ch2:#$0338; Ch3:#$FFFF),                 // LEFTWARDS ARROW WITH STROKE
    (Unicode:#$219B; Attr:daNone; Ch1:#$2192; Ch2:#$0338; Ch3:#$FFFF),                 // RIGHTWARDS ARROW WITH STROKE
    (Unicode:#$21AE; Attr:daNone; Ch1:#$2194; Ch2:#$0338; Ch3:#$FFFF),                 // LEFT RIGHT ARROW WITH STROKE
    (Unicode:#$21CD; Attr:daNone; Ch1:#$21D0; Ch2:#$0338; Ch3:#$FFFF),                 // LEFTWARDS DOUBLE ARROW WITH STROKE
    (Unicode:#$21CE; Attr:daNone; Ch1:#$21D4; Ch2:#$0338; Ch3:#$FFFF),                 // LEFT RIGHT DOUBLE ARROW WITH STROKE
    (Unicode:#$21CF; Attr:daNone; Ch1:#$21D2; Ch2:#$0338; Ch3:#$FFFF),                 // RIGHTWARDS DOUBLE ARROW WITH STROKE
    (Unicode:#$2204; Attr:daNone; Ch1:#$2203; Ch2:#$0338; Ch3:#$FFFF),                 // THERE DOES NOT EXIST
    (Unicode:#$2209; Attr:daNone; Ch1:#$2208; Ch2:#$0338; Ch3:#$FFFF),                 // NOT AN ELEMENT OF
    (Unicode:#$220C; Attr:daNone; Ch1:#$220B; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT CONTAIN AS MEMBER
    (Unicode:#$2224; Attr:daNone; Ch1:#$2223; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT DIVIDE
    (Unicode:#$2226; Attr:daNone; Ch1:#$2225; Ch2:#$0338; Ch3:#$FFFF),                 // NOT PARALLEL TO
    (Unicode:#$222C; Attr:daCompat; Ch1:#$222B; Ch2:#$222B; Ch3:#$FFFF),               // DOUBLE INTEGRAL
    (Unicode:#$222D; Attr:daCompat; Ch1:#$222B; Ch2:#$222B; Ch3:#$222B; Ch4:#$FFFF),   // TRIPLE INTEGRAL
    (Unicode:#$222F; Attr:daCompat; Ch1:#$222E; Ch2:#$222E; Ch3:#$FFFF),               // SURFACE INTEGRAL
    (Unicode:#$2230; Attr:daCompat; Ch1:#$222E; Ch2:#$222E; Ch3:#$222E; Ch4:#$FFFF),   // VOLUME INTEGRAL
    (Unicode:#$2241; Attr:daNone; Ch1:#$223C; Ch2:#$0338; Ch3:#$FFFF),                 // NOT TILDE
    (Unicode:#$2244; Attr:daNone; Ch1:#$2243; Ch2:#$0338; Ch3:#$FFFF),                 // NOT ASYMPTOTICALLY EQUAL TO
    (Unicode:#$2247; Attr:daNone; Ch1:#$2245; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER APPROXIMATELY NOR ACTUALLY EQUAL TO
    (Unicode:#$2249; Attr:daNone; Ch1:#$2248; Ch2:#$0338; Ch3:#$FFFF),                 // NOT ALMOST EQUAL TO
    (Unicode:#$2260; Attr:daNone; Ch1:#$003D; Ch2:#$0338; Ch3:#$FFFF),                 // NOT EQUAL TO
    (Unicode:#$2262; Attr:daNone; Ch1:#$2261; Ch2:#$0338; Ch3:#$FFFF),                 // NOT IDENTICAL TO
    (Unicode:#$226D; Attr:daNone; Ch1:#$224D; Ch2:#$0338; Ch3:#$FFFF),                 // NOT EQUIVALENT TO
    (Unicode:#$226E; Attr:daNone; Ch1:#$003C; Ch2:#$0338; Ch3:#$FFFF),                 // NOT LESS-THAN
    (Unicode:#$226F; Attr:daNone; Ch1:#$003E; Ch2:#$0338; Ch3:#$FFFF),                 // NOT GREATER-THAN
    (Unicode:#$2270; Attr:daNone; Ch1:#$2264; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER LESS-THAN NOR EQUAL TO
    (Unicode:#$2271; Attr:daNone; Ch1:#$2265; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER GREATER-THAN NOR EQUAL TO
    (Unicode:#$2274; Attr:daNone; Ch1:#$2272; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER LESS-THAN NOR EQUIVALENT TO
    (Unicode:#$2275; Attr:daNone; Ch1:#$2273; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER GREATER-THAN NOR EQUIVALENT TO
    (Unicode:#$2278; Attr:daNone; Ch1:#$2276; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER LESS-THAN NOR GREATER-THAN
    (Unicode:#$2279; Attr:daNone; Ch1:#$2277; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER GREATER-THAN NOR LESS-THAN
    (Unicode:#$2280; Attr:daNone; Ch1:#$227A; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT PRECEDE
    (Unicode:#$2281; Attr:daNone; Ch1:#$227B; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT SUCCEED
    (Unicode:#$2284; Attr:daNone; Ch1:#$2282; Ch2:#$0338; Ch3:#$FFFF),                 // NOT A SUBSET OF
    (Unicode:#$2285; Attr:daNone; Ch1:#$2283; Ch2:#$0338; Ch3:#$FFFF),                 // NOT A SUPERSET OF
    (Unicode:#$2288; Attr:daNone; Ch1:#$2286; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER A SUBSET OF NOR EQUAL TO
    (Unicode:#$2289; Attr:daNone; Ch1:#$2287; Ch2:#$0338; Ch3:#$FFFF),                 // NEITHER A SUPERSET OF NOR EQUAL TO
    (Unicode:#$22AC; Attr:daNone; Ch1:#$22A2; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT PROVE
    (Unicode:#$22AD; Attr:daNone; Ch1:#$22A8; Ch2:#$0338; Ch3:#$FFFF),                 // NOT TRUE
    (Unicode:#$22AE; Attr:daNone; Ch1:#$22A9; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT FORCE
    (Unicode:#$22AF; Attr:daNone; Ch1:#$22AB; Ch2:#$0338; Ch3:#$FFFF),                 // NEGATED DOUBLE VERTICAL BAR DOUBLE RIGHT TURNSTILE
    (Unicode:#$22E0; Attr:daNone; Ch1:#$227C; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT PRECEDE OR EQUAL
    (Unicode:#$22E1; Attr:daNone; Ch1:#$227D; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT SUCCEED OR EQUAL
    (Unicode:#$22E2; Attr:daNone; Ch1:#$2291; Ch2:#$0338; Ch3:#$FFFF),                 // NOT SQUARE IMAGE OF OR EQUAL TO
    (Unicode:#$22E3; Attr:daNone; Ch1:#$2292; Ch2:#$0338; Ch3:#$FFFF),                 // NOT SQUARE ORIGINAL OF OR EQUAL TO
    (Unicode:#$22EA; Attr:daNone; Ch1:#$22B2; Ch2:#$0338; Ch3:#$FFFF),                 // NOT NORMAL SUBGROUP OF
    (Unicode:#$22EB; Attr:daNone; Ch1:#$22B3; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT CONTAIN AS NORMAL SUBGROUP
    (Unicode:#$22EC; Attr:daNone; Ch1:#$22B4; Ch2:#$0338; Ch3:#$FFFF),                 // NOT NORMAL SUBGROUP OF OR EQUAL TO
    (Unicode:#$22ED; Attr:daNone; Ch1:#$22B5; Ch2:#$0338; Ch3:#$FFFF),                 // DOES NOT CONTAIN AS NORMAL SUBGROUP OR EQUAL
    (Unicode:#$2329; Attr:daNone; Ch1:#$3008; Ch2:#$FFFF),                             // LEFT-POINTING ANGLE BRACKET
    (Unicode:#$232A; Attr:daNone; Ch1:#$3009; Ch2:#$FFFF),                             // RIGHT-POINTING ANGLE BRACKET
    (Unicode:#$2460; Attr:daCircle; Ch1:#$0031; Ch2:#$FFFF),                           // CIRCLED DIGIT ONE
    (Unicode:#$2461; Attr:daCircle; Ch1:#$0032; Ch2:#$FFFF),                           // CIRCLED DIGIT TWO
    (Unicode:#$2462; Attr:daCircle; Ch1:#$0033; Ch2:#$FFFF),                           // CIRCLED DIGIT THREE
    (Unicode:#$2463; Attr:daCircle; Ch1:#$0034; Ch2:#$FFFF),                           // CIRCLED DIGIT FOUR
    (Unicode:#$2464; Attr:daCircle; Ch1:#$0035; Ch2:#$FFFF),                           // CIRCLED DIGIT FIVE
    (Unicode:#$2465; Attr:daCircle; Ch1:#$0036; Ch2:#$FFFF),                           // CIRCLED DIGIT SIX
    (Unicode:#$2466; Attr:daCircle; Ch1:#$0037; Ch2:#$FFFF),                           // CIRCLED DIGIT SEVEN
    (Unicode:#$2467; Attr:daCircle; Ch1:#$0038; Ch2:#$FFFF),                           // CIRCLED DIGIT EIGHT
    (Unicode:#$2468; Attr:daCircle; Ch1:#$0039; Ch2:#$FFFF),                           // CIRCLED DIGIT NINE
    (Unicode:#$2469; Attr:daCircle; Ch1:#$0031; Ch2:#$0030; Ch3:#$FFFF),               // CIRCLED NUMBER TEN
    (Unicode:#$246A; Attr:daCircle; Ch1:#$0031; Ch2:#$0031; Ch3:#$FFFF),               // CIRCLED NUMBER ELEVEN
    (Unicode:#$246B; Attr:daCircle; Ch1:#$0031; Ch2:#$0032; Ch3:#$FFFF),               // CIRCLED NUMBER TWELVE
    (Unicode:#$246C; Attr:daCircle; Ch1:#$0031; Ch2:#$0033; Ch3:#$FFFF),               // CIRCLED NUMBER THIRTEEN
    (Unicode:#$246D; Attr:daCircle; Ch1:#$0031; Ch2:#$0034; Ch3:#$FFFF),               // CIRCLED NUMBER FOURTEEN
    (Unicode:#$246E; Attr:daCircle; Ch1:#$0031; Ch2:#$0035; Ch3:#$FFFF),               // CIRCLED NUMBER FIFTEEN
    (Unicode:#$246F; Attr:daCircle; Ch1:#$0031; Ch2:#$0036; Ch3:#$FFFF),               // CIRCLED NUMBER SIXTEEN
    (Unicode:#$2470; Attr:daCircle; Ch1:#$0031; Ch2:#$0037; Ch3:#$FFFF),               // CIRCLED NUMBER SEVENTEEN
    (Unicode:#$2471; Attr:daCircle; Ch1:#$0031; Ch2:#$0038; Ch3:#$FFFF),               // CIRCLED NUMBER EIGHTEEN
    (Unicode:#$2472; Attr:daCircle; Ch1:#$0031; Ch2:#$0039; Ch3:#$FFFF),               // CIRCLED NUMBER NINETEEN
    (Unicode:#$2473; Attr:daCircle; Ch1:#$0032; Ch2:#$0030; Ch3:#$FFFF),               // CIRCLED NUMBER TWENTY
    (Unicode:#$2474; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT ONE
    (Unicode:#$2475; Attr:daCompat; Ch1:#$0028; Ch2:#$0032; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT TWO
    (Unicode:#$2476; Attr:daCompat; Ch1:#$0028; Ch2:#$0033; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT THREE
    (Unicode:#$2477; Attr:daCompat; Ch1:#$0028; Ch2:#$0034; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT FOUR
    (Unicode:#$2478; Attr:daCompat; Ch1:#$0028; Ch2:#$0035; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT FIVE
    (Unicode:#$2479; Attr:daCompat; Ch1:#$0028; Ch2:#$0036; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT SIX
    (Unicode:#$247A; Attr:daCompat; Ch1:#$0028; Ch2:#$0037; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT SEVEN
    (Unicode:#$247B; Attr:daCompat; Ch1:#$0028; Ch2:#$0038; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT EIGHT
    (Unicode:#$247C; Attr:daCompat; Ch1:#$0028; Ch2:#$0039; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED DIGIT NINE
    (Unicode:#$247D; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0030; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER TEN
    (Unicode:#$247E; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0031; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER ELEVEN
    (Unicode:#$247F; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0032; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER TWELVE
    (Unicode:#$2480; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0033; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER THIRTEEN
    (Unicode:#$2481; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0034; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER FOURTEEN
    (Unicode:#$2482; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0035; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER FIFTEEN
    (Unicode:#$2483; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0036; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER SIXTEEN
    (Unicode:#$2484; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0037; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER SEVENTEEN
    (Unicode:#$2485; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0038; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER EIGHTEEN
    (Unicode:#$2486; Attr:daCompat; Ch1:#$0028; Ch2:#$0031; Ch3:#$0039; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER NINETEEN
    (Unicode:#$2487; Attr:daCompat; Ch1:#$0028; Ch2:#$0032; Ch3:#$0030; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED NUMBER TWENTY
    (Unicode:#$2488; Attr:daCompat; Ch1:#$0031; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT ONE FULL STOP
    (Unicode:#$2489; Attr:daCompat; Ch1:#$0032; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT TWO FULL STOP
    (Unicode:#$248A; Attr:daCompat; Ch1:#$0033; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT THREE FULL STOP
    (Unicode:#$248B; Attr:daCompat; Ch1:#$0034; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT FOUR FULL STOP
    (Unicode:#$248C; Attr:daCompat; Ch1:#$0035; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT FIVE FULL STOP
    (Unicode:#$248D; Attr:daCompat; Ch1:#$0036; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT SIX FULL STOP
    (Unicode:#$248E; Attr:daCompat; Ch1:#$0037; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT SEVEN FULL STOP
    (Unicode:#$248F; Attr:daCompat; Ch1:#$0038; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT EIGHT FULL STOP
    (Unicode:#$2490; Attr:daCompat; Ch1:#$0039; Ch2:#$002E; Ch3:#$FFFF),  // DIGIT NINE FULL STOP
    (Unicode:#$2491; Attr:daCompat; Ch1:#$0031; Ch2:#$0030; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER TEN FULL STOP
    (Unicode:#$2492; Attr:daCompat; Ch1:#$0031; Ch2:#$0031; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER ELEVEN FULL STOP
    (Unicode:#$2493; Attr:daCompat; Ch1:#$0031; Ch2:#$0032; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER TWELVE FULL STOP
    (Unicode:#$2494; Attr:daCompat; Ch1:#$0031; Ch2:#$0033; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER THIRTEEN FULL STOP
    (Unicode:#$2495; Attr:daCompat; Ch1:#$0031; Ch2:#$0034; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER FOURTEEN FULL STOP
    (Unicode:#$2496; Attr:daCompat; Ch1:#$0031; Ch2:#$0035; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER FIFTEEN FULL STOP
    (Unicode:#$2497; Attr:daCompat; Ch1:#$0031; Ch2:#$0036; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER SIXTEEN FULL STOP
    (Unicode:#$2498; Attr:daCompat; Ch1:#$0031; Ch2:#$0037; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER SEVENTEEN FULL STOP
    (Unicode:#$2499; Attr:daCompat; Ch1:#$0031; Ch2:#$0038; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER EIGHTEEN FULL STOP
    (Unicode:#$249A; Attr:daCompat; Ch1:#$0031; Ch2:#$0039; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER NINETEEN FULL STOP
    (Unicode:#$249B; Attr:daCompat; Ch1:#$0032; Ch2:#$0030; Ch3:#$002E; Ch4:#$FFFF),   // NUMBER TWENTY FULL STOP
    (Unicode:#$249C; Attr:daCompat; Ch1:#$0028; Ch2:#$0061; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER A
    (Unicode:#$249D; Attr:daCompat; Ch1:#$0028; Ch2:#$0062; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER B
    (Unicode:#$249E; Attr:daCompat; Ch1:#$0028; Ch2:#$0063; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER C
    (Unicode:#$249F; Attr:daCompat; Ch1:#$0028; Ch2:#$0064; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER D
    (Unicode:#$24A0; Attr:daCompat; Ch1:#$0028; Ch2:#$0065; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER E
    (Unicode:#$24A1; Attr:daCompat; Ch1:#$0028; Ch2:#$0066; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER F
    (Unicode:#$24A2; Attr:daCompat; Ch1:#$0028; Ch2:#$0067; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER G
    (Unicode:#$24A3; Attr:daCompat; Ch1:#$0028; Ch2:#$0068; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER H
    (Unicode:#$24A4; Attr:daCompat; Ch1:#$0028; Ch2:#$0069; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER I
    (Unicode:#$24A5; Attr:daCompat; Ch1:#$0028; Ch2:#$006A; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER J
    (Unicode:#$24A6; Attr:daCompat; Ch1:#$0028; Ch2:#$006B; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER K
    (Unicode:#$24A7; Attr:daCompat; Ch1:#$0028; Ch2:#$006C; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER L
    (Unicode:#$24A8; Attr:daCompat; Ch1:#$0028; Ch2:#$006D; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER M
    (Unicode:#$24A9; Attr:daCompat; Ch1:#$0028; Ch2:#$006E; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER N
    (Unicode:#$24AA; Attr:daCompat; Ch1:#$0028; Ch2:#$006F; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER O
    (Unicode:#$24AB; Attr:daCompat; Ch1:#$0028; Ch2:#$0070; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER P
    (Unicode:#$24AC; Attr:daCompat; Ch1:#$0028; Ch2:#$0071; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER Q
    (Unicode:#$24AD; Attr:daCompat; Ch1:#$0028; Ch2:#$0072; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER R
    (Unicode:#$24AE; Attr:daCompat; Ch1:#$0028; Ch2:#$0073; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER S
    (Unicode:#$24AF; Attr:daCompat; Ch1:#$0028; Ch2:#$0074; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER T
    (Unicode:#$24B0; Attr:daCompat; Ch1:#$0028; Ch2:#$0075; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER U
    (Unicode:#$24B1; Attr:daCompat; Ch1:#$0028; Ch2:#$0076; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER V
    (Unicode:#$24B2; Attr:daCompat; Ch1:#$0028; Ch2:#$0077; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER W
    (Unicode:#$24B3; Attr:daCompat; Ch1:#$0028; Ch2:#$0078; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER X
    (Unicode:#$24B4; Attr:daCompat; Ch1:#$0028; Ch2:#$0079; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER Y
    (Unicode:#$24B5; Attr:daCompat; Ch1:#$0028; Ch2:#$007A; Ch3:#$0029; Ch4:#$FFFF),   // PARENTHESIZED LATIN SMALL LETTER Z
    (Unicode:#$24B6; Attr:daCircle; Ch1:#$0041; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER A
    (Unicode:#$24B7; Attr:daCircle; Ch1:#$0042; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER B
    (Unicode:#$24B8; Attr:daCircle; Ch1:#$0043; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER C
    (Unicode:#$24B9; Attr:daCircle; Ch1:#$0044; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER D
    (Unicode:#$24BA; Attr:daCircle; Ch1:#$0045; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER E
    (Unicode:#$24BB; Attr:daCircle; Ch1:#$0046; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER F
    (Unicode:#$24BC; Attr:daCircle; Ch1:#$0047; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER G
    (Unicode:#$24BD; Attr:daCircle; Ch1:#$0048; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER H
    (Unicode:#$24BE; Attr:daCircle; Ch1:#$0049; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER I
    (Unicode:#$24BF; Attr:daCircle; Ch1:#$004A; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER J
    (Unicode:#$24C0; Attr:daCircle; Ch1:#$004B; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER K
    (Unicode:#$24C1; Attr:daCircle; Ch1:#$004C; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER L
    (Unicode:#$24C2; Attr:daCircle; Ch1:#$004D; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER M
    (Unicode:#$24C3; Attr:daCircle; Ch1:#$004E; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER N
    (Unicode:#$24C4; Attr:daCircle; Ch1:#$004F; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER O
    (Unicode:#$24C5; Attr:daCircle; Ch1:#$0050; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER P
    (Unicode:#$24C6; Attr:daCircle; Ch1:#$0051; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER Q
    (Unicode:#$24C7; Attr:daCircle; Ch1:#$0052; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER R
    (Unicode:#$24C8; Attr:daCircle; Ch1:#$0053; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER S
    (Unicode:#$24C9; Attr:daCircle; Ch1:#$0054; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER T
    (Unicode:#$24CA; Attr:daCircle; Ch1:#$0055; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER U
    (Unicode:#$24CB; Attr:daCircle; Ch1:#$0056; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER V
    (Unicode:#$24CC; Attr:daCircle; Ch1:#$0057; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER W
    (Unicode:#$24CD; Attr:daCircle; Ch1:#$0058; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER X
    (Unicode:#$24CE; Attr:daCircle; Ch1:#$0059; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER Y
    (Unicode:#$24CF; Attr:daCircle; Ch1:#$005A; Ch2:#$FFFF),              // CIRCLED LATIN CAPITAL LETTER Z
    (Unicode:#$24D0; Attr:daCircle; Ch1:#$0061; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER A
    (Unicode:#$24D1; Attr:daCircle; Ch1:#$0062; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER B
    (Unicode:#$24D2; Attr:daCircle; Ch1:#$0063; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER C
    (Unicode:#$24D3; Attr:daCircle; Ch1:#$0064; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER D
    (Unicode:#$24D4; Attr:daCircle; Ch1:#$0065; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER E
    (Unicode:#$24D5; Attr:daCircle; Ch1:#$0066; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER F
    (Unicode:#$24D6; Attr:daCircle; Ch1:#$0067; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER G
    (Unicode:#$24D7; Attr:daCircle; Ch1:#$0068; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER H
    (Unicode:#$24D8; Attr:daCircle; Ch1:#$0069; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER I
    (Unicode:#$24D9; Attr:daCircle; Ch1:#$006A; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER J
    (Unicode:#$24DA; Attr:daCircle; Ch1:#$006B; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER K
    (Unicode:#$24DB; Attr:daCircle; Ch1:#$006C; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER L
    (Unicode:#$24DC; Attr:daCircle; Ch1:#$006D; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER M
    (Unicode:#$24DD; Attr:daCircle; Ch1:#$006E; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER N
    (Unicode:#$24DE; Attr:daCircle; Ch1:#$006F; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER O
    (Unicode:#$24DF; Attr:daCircle; Ch1:#$0070; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER P
    (Unicode:#$24E0; Attr:daCircle; Ch1:#$0071; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER Q
    (Unicode:#$24E1; Attr:daCircle; Ch1:#$0072; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER R
    (Unicode:#$24E2; Attr:daCircle; Ch1:#$0073; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER S
    (Unicode:#$24E3; Attr:daCircle; Ch1:#$0074; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER T
    (Unicode:#$24E4; Attr:daCircle; Ch1:#$0075; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER U
    (Unicode:#$24E5; Attr:daCircle; Ch1:#$0076; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER V
    (Unicode:#$24E6; Attr:daCircle; Ch1:#$0077; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER W
    (Unicode:#$24E7; Attr:daCircle; Ch1:#$0078; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER X
    (Unicode:#$24E8; Attr:daCircle; Ch1:#$0079; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER Y
    (Unicode:#$24E9; Attr:daCircle; Ch1:#$007A; Ch2:#$FFFF),              // CIRCLED LATIN SMALL LETTER Z
    (Unicode:#$24EA; Attr:daCircle; Ch1:#$0030; Ch2:#$FFFF),              // CIRCLED DIGIT ZERO
    (Unicode:#$2E9F; Attr:daCompat; Ch1:#$6BCD; Ch2:#$FFFF),              // CJK RADICAL MOTHER
    (Unicode:#$2EF3; Attr:daCompat; Ch1:#$9F9F; Ch2:#$FFFF),              // CJK RADICAL C-SIMPLIFIED TURTLE
    (Unicode:#$2F00; Attr:daCompat; Ch1:#$4E00; Ch2:#$FFFF),              // KANGXI RADICAL ONE
    (Unicode:#$2F01; Attr:daCompat; Ch1:#$4E28; Ch2:#$FFFF),              // KANGXI RADICAL LINE
    (Unicode:#$2F02; Attr:daCompat; Ch1:#$4E36; Ch2:#$FFFF),              // KANGXI RADICAL DOT
    (Unicode:#$2F03; Attr:daCompat; Ch1:#$4E3F; Ch2:#$FFFF),              // KANGXI RADICAL SLASH
    (Unicode:#$2F04; Attr:daCompat; Ch1:#$4E59; Ch2:#$FFFF),              // KANGXI RADICAL SECOND
    (Unicode:#$2F05; Attr:daCompat; Ch1:#$4E85; Ch2:#$FFFF),              // KANGXI RADICAL HOOK
    (Unicode:#$2F06; Attr:daCompat; Ch1:#$4E8C; Ch2:#$FFFF),              // KANGXI RADICAL TWO
    (Unicode:#$2F07; Attr:daCompat; Ch1:#$4EA0; Ch2:#$FFFF),              // KANGXI RADICAL LID
    (Unicode:#$2F08; Attr:daCompat; Ch1:#$4EBA; Ch2:#$FFFF),              // KANGXI RADICAL MAN
    (Unicode:#$2F09; Attr:daCompat; Ch1:#$513F; Ch2:#$FFFF),              // KANGXI RADICAL LEGS
    (Unicode:#$2F0A; Attr:daCompat; Ch1:#$5165; Ch2:#$FFFF),              // KANGXI RADICAL ENTER
    (Unicode:#$2F0B; Attr:daCompat; Ch1:#$516B; Ch2:#$FFFF),              // KANGXI RADICAL EIGHT
    (Unicode:#$2F0C; Attr:daCompat; Ch1:#$5182; Ch2:#$FFFF),              // KANGXI RADICAL DOWN BOX
    (Unicode:#$2F0D; Attr:daCompat; Ch1:#$5196; Ch2:#$FFFF),              // KANGXI RADICAL COVER
    (Unicode:#$2F0E; Attr:daCompat; Ch1:#$51AB; Ch2:#$FFFF),              // KANGXI RADICAL ICE
    (Unicode:#$2F0F; Attr:daCompat; Ch1:#$51E0; Ch2:#$FFFF),              // KANGXI RADICAL TABLE
    (Unicode:#$2F10; Attr:daCompat; Ch1:#$51F5; Ch2:#$FFFF),              // KANGXI RADICAL OPEN BOX
    (Unicode:#$2F11; Attr:daCompat; Ch1:#$5200; Ch2:#$FFFF),              // KANGXI RADICAL KNIFE
    (Unicode:#$2F12; Attr:daCompat; Ch1:#$529B; Ch2:#$FFFF),              // KANGXI RADICAL POWER
    (Unicode:#$2F13; Attr:daCompat; Ch1:#$52F9; Ch2:#$FFFF),              // KANGXI RADICAL WRAP
    (Unicode:#$2F14; Attr:daCompat; Ch1:#$5315; Ch2:#$FFFF),              // KANGXI RADICAL SPOON
    (Unicode:#$2F15; Attr:daCompat; Ch1:#$531A; Ch2:#$FFFF),              // KANGXI RADICAL RIGHT OPEN BOX
    (Unicode:#$2F16; Attr:daCompat; Ch1:#$5338; Ch2:#$FFFF),              // KANGXI RADICAL HIDING ENCLOSURE
    (Unicode:#$2F17; Attr:daCompat; Ch1:#$5341; Ch2:#$FFFF),              // KANGXI RADICAL TEN
    (Unicode:#$2F18; Attr:daCompat; Ch1:#$535C; Ch2:#$FFFF),              // KANGXI RADICAL DIVINATION
    (Unicode:#$2F19; Attr:daCompat; Ch1:#$5369; Ch2:#$FFFF),              // KANGXI RADICAL SEAL
    (Unicode:#$2F1A; Attr:daCompat; Ch1:#$5382; Ch2:#$FFFF),              // KANGXI RADICAL CLIFF
    (Unicode:#$2F1B; Attr:daCompat; Ch1:#$53B6; Ch2:#$FFFF),              // KANGXI RADICAL PRIVATE
    (Unicode:#$2F1C; Attr:daCompat; Ch1:#$53C8; Ch2:#$FFFF),              // KANGXI RADICAL AGAIN
    (Unicode:#$2F1D; Attr:daCompat; Ch1:#$53E3; Ch2:#$FFFF),              // KANGXI RADICAL MOUTH
    (Unicode:#$2F1E; Attr:daCompat; Ch1:#$56D7; Ch2:#$FFFF),              // KANGXI RADICAL ENCLOSURE
    (Unicode:#$2F1F; Attr:daCompat; Ch1:#$571F; Ch2:#$FFFF),              // KANGXI RADICAL EARTH
    (Unicode:#$2F20; Attr:daCompat; Ch1:#$58EB; Ch2:#$FFFF),              // KANGXI RADICAL SCHOLAR
    (Unicode:#$2F21; Attr:daCompat; Ch1:#$5902; Ch2:#$FFFF),              // KANGXI RADICAL GO
    (Unicode:#$2F22; Attr:daCompat; Ch1:#$590A; Ch2:#$FFFF),              // KANGXI RADICAL GO SLOWLY
    (Unicode:#$2F23; Attr:daCompat; Ch1:#$5915; Ch2:#$FFFF),              // KANGXI RADICAL EVENING
    (Unicode:#$2F24; Attr:daCompat; Ch1:#$5927; Ch2:#$FFFF),              // KANGXI RADICAL BIG
    (Unicode:#$2F25; Attr:daCompat; Ch1:#$5973; Ch2:#$FFFF),              // KANGXI RADICAL WOMAN
    (Unicode:#$2F26; Attr:daCompat; Ch1:#$5B50; Ch2:#$FFFF),              // KANGXI RADICAL CHILD
    (Unicode:#$2F27; Attr:daCompat; Ch1:#$5B80; Ch2:#$FFFF),              // KANGXI RADICAL ROOF
    (Unicode:#$2F28; Attr:daCompat; Ch1:#$5BF8; Ch2:#$FFFF),              // KANGXI RADICAL INCH
    (Unicode:#$2F29; Attr:daCompat; Ch1:#$5C0F; Ch2:#$FFFF),              // KANGXI RADICAL SMALL
    (Unicode:#$2F2A; Attr:daCompat; Ch1:#$5C22; Ch2:#$FFFF),              // KANGXI RADICAL LAME
    (Unicode:#$2F2B; Attr:daCompat; Ch1:#$5C38; Ch2:#$FFFF),              // KANGXI RADICAL CORPSE
    (Unicode:#$2F2C; Attr:daCompat; Ch1:#$5C6E; Ch2:#$FFFF),              // KANGXI RADICAL SPROUT
    (Unicode:#$2F2D; Attr:daCompat; Ch1:#$5C71; Ch2:#$FFFF),              // KANGXI RADICAL MOUNTAIN
    (Unicode:#$2F2E; Attr:daCompat; Ch1:#$5DDB; Ch2:#$FFFF),              // KANGXI RADICAL RIVER
    (Unicode:#$2F2F; Attr:daCompat; Ch1:#$5DE5; Ch2:#$FFFF),              // KANGXI RADICAL WORK
    (Unicode:#$2F30; Attr:daCompat; Ch1:#$5DF1; Ch2:#$FFFF),              // KANGXI RADICAL ONESELF
    (Unicode:#$2F31; Attr:daCompat; Ch1:#$5DFE; Ch2:#$FFFF),              // KANGXI RADICAL TURBAN
    (Unicode:#$2F32; Attr:daCompat; Ch1:#$5E72; Ch2:#$FFFF),              // KANGXI RADICAL DRY
    (Unicode:#$2F33; Attr:daCompat; Ch1:#$5E7A; Ch2:#$FFFF),              // KANGXI RADICAL SHORT THREAD
    (Unicode:#$2F34; Attr:daCompat; Ch1:#$5E7F; Ch2:#$FFFF),              // KANGXI RADICAL DOTTED CLIFF
    (Unicode:#$2F35; Attr:daCompat; Ch1:#$5EF4; Ch2:#$FFFF),              // KANGXI RADICAL LONG STRIDE
    (Unicode:#$2F36; Attr:daCompat; Ch1:#$5EFE; Ch2:#$FFFF),              // KANGXI RADICAL TWO HANDS
    (Unicode:#$2F37; Attr:daCompat; Ch1:#$5F0B; Ch2:#$FFFF),              // KANGXI RADICAL SHOOT
    (Unicode:#$2F38; Attr:daCompat; Ch1:#$5F13; Ch2:#$FFFF),              // KANGXI RADICAL BOW
    (Unicode:#$2F39; Attr:daCompat; Ch1:#$5F50; Ch2:#$FFFF),              // KANGXI RADICAL SNOUT
    (Unicode:#$2F3A; Attr:daCompat; Ch1:#$5F61; Ch2:#$FFFF),              // KANGXI RADICAL BRISTLE
    (Unicode:#$2F3B; Attr:daCompat; Ch1:#$5F73; Ch2:#$FFFF),              // KANGXI RADICAL STEP
    (Unicode:#$2F3C; Attr:daCompat; Ch1:#$5FC3; Ch2:#$FFFF),              // KANGXI RADICAL HEART
    (Unicode:#$2F3D; Attr:daCompat; Ch1:#$6208; Ch2:#$FFFF),              // KANGXI RADICAL HALBERD
    (Unicode:#$2F3E; Attr:daCompat; Ch1:#$6236; Ch2:#$FFFF),              // KANGXI RADICAL DOOR
    (Unicode:#$2F3F; Attr:daCompat; Ch1:#$624B; Ch2:#$FFFF),              // KANGXI RADICAL HAND
    (Unicode:#$2F40; Attr:daCompat; Ch1:#$652F; Ch2:#$FFFF),              // KANGXI RADICAL BRANCH
    (Unicode:#$2F41; Attr:daCompat; Ch1:#$6534; Ch2:#$FFFF),              // KANGXI RADICAL RAP
    (Unicode:#$2F42; Attr:daCompat; Ch1:#$6587; Ch2:#$FFFF),              // KANGXI RADICAL SCRIPT
    (Unicode:#$2F43; Attr:daCompat; Ch1:#$6597; Ch2:#$FFFF),              // KANGXI RADICAL DIPPER
    (Unicode:#$2F44; Attr:daCompat; Ch1:#$65A4; Ch2:#$FFFF),              // KANGXI RADICAL AXE
    (Unicode:#$2F45; Attr:daCompat; Ch1:#$65B9; Ch2:#$FFFF),              // KANGXI RADICAL SQUARE
    (Unicode:#$2F46; Attr:daCompat; Ch1:#$65E0; Ch2:#$FFFF),              // KANGXI RADICAL NOT
    (Unicode:#$2F47; Attr:daCompat; Ch1:#$65E5; Ch2:#$FFFF),              // KANGXI RADICAL SUN
    (Unicode:#$2F48; Attr:daCompat; Ch1:#$66F0; Ch2:#$FFFF),              // KANGXI RADICAL SAY
    (Unicode:#$2F49; Attr:daCompat; Ch1:#$6708; Ch2:#$FFFF),              // KANGXI RADICAL MOON
    (Unicode:#$2F4A; Attr:daCompat; Ch1:#$6728; Ch2:#$FFFF),              // KANGXI RADICAL TREE
    (Unicode:#$2F4B; Attr:daCompat; Ch1:#$6B20; Ch2:#$FFFF),              // KANGXI RADICAL LACK
    (Unicode:#$2F4C; Attr:daCompat; Ch1:#$6B62; Ch2:#$FFFF),              // KANGXI RADICAL STOP
    (Unicode:#$2F4D; Attr:daCompat; Ch1:#$6B79; Ch2:#$FFFF),              // KANGXI RADICAL DEATH
    (Unicode:#$2F4E; Attr:daCompat; Ch1:#$6BB3; Ch2:#$FFFF),              // KANGXI RADICAL WEAPON
    (Unicode:#$2F4F; Attr:daCompat; Ch1:#$6BCB; Ch2:#$FFFF),              // KANGXI RADICAL DO NOT
    (Unicode:#$2F50; Attr:daCompat; Ch1:#$6BD4; Ch2:#$FFFF),              // KANGXI RADICAL COMPARE
    (Unicode:#$2F51; Attr:daCompat; Ch1:#$6BDB; Ch2:#$FFFF),              // KANGXI RADICAL FUR
    (Unicode:#$2F52; Attr:daCompat; Ch1:#$6C0F; Ch2:#$FFFF),              // KANGXI RADICAL CLAN
    (Unicode:#$2F53; Attr:daCompat; Ch1:#$6C14; Ch2:#$FFFF),              // KANGXI RADICAL STEAM
    (Unicode:#$2F54; Attr:daCompat; Ch1:#$6C34; Ch2:#$FFFF),              // KANGXI RADICAL WATER
    (Unicode:#$2F55; Attr:daCompat; Ch1:#$706B; Ch2:#$FFFF),              // KANGXI RADICAL FIRE
    (Unicode:#$2F56; Attr:daCompat; Ch1:#$722A; Ch2:#$FFFF),              // KANGXI RADICAL CLAW
    (Unicode:#$2F57; Attr:daCompat; Ch1:#$7236; Ch2:#$FFFF),              // KANGXI RADICAL FATHER
    (Unicode:#$2F58; Attr:daCompat; Ch1:#$723B; Ch2:#$FFFF),              // KANGXI RADICAL DOUBLE X
    (Unicode:#$2F59; Attr:daCompat; Ch1:#$723F; Ch2:#$FFFF),              // KANGXI RADICAL HALF TREE TRUNK
    (Unicode:#$2F5A; Attr:daCompat; Ch1:#$7247; Ch2:#$FFFF),              // KANGXI RADICAL SLICE
    (Unicode:#$2F5B; Attr:daCompat; Ch1:#$7259; Ch2:#$FFFF),              // KANGXI RADICAL FANG
    (Unicode:#$2F5C; Attr:daCompat; Ch1:#$725B; Ch2:#$FFFF),              // KANGXI RADICAL COW
    (Unicode:#$2F5D; Attr:daCompat; Ch1:#$72AC; Ch2:#$FFFF),              // KANGXI RADICAL DOG
    (Unicode:#$2F5E; Attr:daCompat; Ch1:#$7384; Ch2:#$FFFF),              // KANGXI RADICAL PROFOUND
    (Unicode:#$2F5F; Attr:daCompat; Ch1:#$7389; Ch2:#$FFFF),              // KANGXI RADICAL JADE
    (Unicode:#$2F60; Attr:daCompat; Ch1:#$74DC; Ch2:#$FFFF),              // KANGXI RADICAL MELON
    (Unicode:#$2F61; Attr:daCompat; Ch1:#$74E6; Ch2:#$FFFF),              // KANGXI RADICAL TILE
    (Unicode:#$2F62; Attr:daCompat; Ch1:#$7518; Ch2:#$FFFF),              // KANGXI RADICAL SWEET
    (Unicode:#$2F63; Attr:daCompat; Ch1:#$751F; Ch2:#$FFFF),              // KANGXI RADICAL LIFE
    (Unicode:#$2F64; Attr:daCompat; Ch1:#$7528; Ch2:#$FFFF),              // KANGXI RADICAL USE
    (Unicode:#$2F65; Attr:daCompat; Ch1:#$7530; Ch2:#$FFFF),              // KANGXI RADICAL FIELD
    (Unicode:#$2F66; Attr:daCompat; Ch1:#$758B; Ch2:#$FFFF),              // KANGXI RADICAL BOLT OF CLOTH
    (Unicode:#$2F67; Attr:daCompat; Ch1:#$7592; Ch2:#$FFFF),              // KANGXI RADICAL SICKNESS
    (Unicode:#$2F68; Attr:daCompat; Ch1:#$7676; Ch2:#$FFFF),              // KANGXI RADICAL DOTTED TENT
    (Unicode:#$2F69; Attr:daCompat; Ch1:#$767D; Ch2:#$FFFF),              // KANGXI RADICAL WHITE
    (Unicode:#$2F6A; Attr:daCompat; Ch1:#$76AE; Ch2:#$FFFF),              // KANGXI RADICAL SKIN
    (Unicode:#$2F6B; Attr:daCompat; Ch1:#$76BF; Ch2:#$FFFF),              // KANGXI RADICAL DISH
    (Unicode:#$2F6C; Attr:daCompat; Ch1:#$76EE; Ch2:#$FFFF),              // KANGXI RADICAL EYE
    (Unicode:#$2F6D; Attr:daCompat; Ch1:#$77DB; Ch2:#$FFFF),              // KANGXI RADICAL SPEAR
    (Unicode:#$2F6E; Attr:daCompat; Ch1:#$77E2; Ch2:#$FFFF),              // KANGXI RADICAL ARROW
    (Unicode:#$2F6F; Attr:daCompat; Ch1:#$77F3; Ch2:#$FFFF),              // KANGXI RADICAL STONE
    (Unicode:#$2F70; Attr:daCompat; Ch1:#$793A; Ch2:#$FFFF),              // KANGXI RADICAL SPIRIT
    (Unicode:#$2F71; Attr:daCompat; Ch1:#$79B8; Ch2:#$FFFF),              // KANGXI RADICAL TRACK
    (Unicode:#$2F72; Attr:daCompat; Ch1:#$79BE; Ch2:#$FFFF),              // KANGXI RADICAL GRAIN
    (Unicode:#$2F73; Attr:daCompat; Ch1:#$7A74; Ch2:#$FFFF),              // KANGXI RADICAL CAVE
    (Unicode:#$2F74; Attr:daCompat; Ch1:#$7ACB; Ch2:#$FFFF),              // KANGXI RADICAL STAND
    (Unicode:#$2F75; Attr:daCompat; Ch1:#$7AF9; Ch2:#$FFFF),              // KANGXI RADICAL BAMBOO
    (Unicode:#$2F76; Attr:daCompat; Ch1:#$7C73; Ch2:#$FFFF),              // KANGXI RADICAL RICE
    (Unicode:#$2F77; Attr:daCompat; Ch1:#$7CF8; Ch2:#$FFFF),              // KANGXI RADICAL SILK
    (Unicode:#$2F78; Attr:daCompat; Ch1:#$7F36; Ch2:#$FFFF),              // KANGXI RADICAL JAR
    (Unicode:#$2F79; Attr:daCompat; Ch1:#$7F51; Ch2:#$FFFF),              // KANGXI RADICAL NET
    (Unicode:#$2F7A; Attr:daCompat; Ch1:#$7F8A; Ch2:#$FFFF),              // KANGXI RADICAL SHEEP
    (Unicode:#$2F7B; Attr:daCompat; Ch1:#$7FBD; Ch2:#$FFFF),              // KANGXI RADICAL FEATHER
    (Unicode:#$2F7C; Attr:daCompat; Ch1:#$8001; Ch2:#$FFFF),              // KANGXI RADICAL OLD
    (Unicode:#$2F7D; Attr:daCompat; Ch1:#$800C; Ch2:#$FFFF),              // KANGXI RADICAL AND
    (Unicode:#$2F7E; Attr:daCompat; Ch1:#$8012; Ch2:#$FFFF),              // KANGXI RADICAL PLOW
    (Unicode:#$2F7F; Attr:daCompat; Ch1:#$8033; Ch2:#$FFFF),              // KANGXI RADICAL EAR
    (Unicode:#$2F80; Attr:daCompat; Ch1:#$807F; Ch2:#$FFFF),              // KANGXI RADICAL BRUSH
    (Unicode:#$2F81; Attr:daCompat; Ch1:#$8089; Ch2:#$FFFF),              // KANGXI RADICAL MEAT
    (Unicode:#$2F82; Attr:daCompat; Ch1:#$81E3; Ch2:#$FFFF),              // KANGXI RADICAL MINISTER
    (Unicode:#$2F83; Attr:daCompat; Ch1:#$81EA; Ch2:#$FFFF),              // KANGXI RADICAL SELF
    (Unicode:#$2F84; Attr:daCompat; Ch1:#$81F3; Ch2:#$FFFF),              // KANGXI RADICAL ARRIVE
    (Unicode:#$2F85; Attr:daCompat; Ch1:#$81FC; Ch2:#$FFFF),              // KANGXI RADICAL MORTAR
    (Unicode:#$2F86; Attr:daCompat; Ch1:#$820C; Ch2:#$FFFF),              // KANGXI RADICAL TONGUE
    (Unicode:#$2F87; Attr:daCompat; Ch1:#$821B; Ch2:#$FFFF),              // KANGXI RADICAL OPPOSE
    (Unicode:#$2F88; Attr:daCompat; Ch1:#$821F; Ch2:#$FFFF),              // KANGXI RADICAL BOAT
    (Unicode:#$2F89; Attr:daCompat; Ch1:#$826E; Ch2:#$FFFF),              // KANGXI RADICAL STOPPING
    (Unicode:#$2F8A; Attr:daCompat; Ch1:#$8272; Ch2:#$FFFF),              // KANGXI RADICAL COLOR
    (Unicode:#$2F8B; Attr:daCompat; Ch1:#$8278; Ch2:#$FFFF),              // KANGXI RADICAL GRASS
    (Unicode:#$2F8C; Attr:daCompat; Ch1:#$864D; Ch2:#$FFFF),              // KANGXI RADICAL TIGER
    (Unicode:#$2F8D; Attr:daCompat; Ch1:#$866B; Ch2:#$FFFF),              // KANGXI RADICAL INSECT
    (Unicode:#$2F8E; Attr:daCompat; Ch1:#$8840; Ch2:#$FFFF),              // KANGXI RADICAL BLOOD
    (Unicode:#$2F8F; Attr:daCompat; Ch1:#$884C; Ch2:#$FFFF),              // KANGXI RADICAL WALK ENCLOSURE
    (Unicode:#$2F90; Attr:daCompat; Ch1:#$8863; Ch2:#$FFFF),              // KANGXI RADICAL CLOTHES
    (Unicode:#$2F91; Attr:daCompat; Ch1:#$897E; Ch2:#$FFFF),              // KANGXI RADICAL WEST
    (Unicode:#$2F92; Attr:daCompat; Ch1:#$898B; Ch2:#$FFFF),              // KANGXI RADICAL SEE
    (Unicode:#$2F93; Attr:daCompat; Ch1:#$89D2; Ch2:#$FFFF),              // KANGXI RADICAL HORN
    (Unicode:#$2F94; Attr:daCompat; Ch1:#$8A00; Ch2:#$FFFF),              // KANGXI RADICAL SPEECH
    (Unicode:#$2F95; Attr:daCompat; Ch1:#$8C37; Ch2:#$FFFF),              // KANGXI RADICAL VALLEY
    (Unicode:#$2F96; Attr:daCompat; Ch1:#$8C46; Ch2:#$FFFF),              // KANGXI RADICAL BEAN
    (Unicode:#$2F97; Attr:daCompat; Ch1:#$8C55; Ch2:#$FFFF),              // KANGXI RADICAL PIG
    (Unicode:#$2F98; Attr:daCompat; Ch1:#$8C78; Ch2:#$FFFF),              // KANGXI RADICAL BADGER
    (Unicode:#$2F99; Attr:daCompat; Ch1:#$8C9D; Ch2:#$FFFF),              // KANGXI RADICAL SHELL
    (Unicode:#$2F9A; Attr:daCompat; Ch1:#$8D64; Ch2:#$FFFF),              // KANGXI RADICAL RED
    (Unicode:#$2F9B; Attr:daCompat; Ch1:#$8D70; Ch2:#$FFFF),              // KANGXI RADICAL RUN
    (Unicode:#$2F9C; Attr:daCompat; Ch1:#$8DB3; Ch2:#$FFFF),              // KANGXI RADICAL FOOT
    (Unicode:#$2F9D; Attr:daCompat; Ch1:#$8EAB; Ch2:#$FFFF),              // KANGXI RADICAL BODY
    (Unicode:#$2F9E; Attr:daCompat; Ch1:#$8ECA; Ch2:#$FFFF),              // KANGXI RADICAL CART
    (Unicode:#$2F9F; Attr:daCompat; Ch1:#$8F9B; Ch2:#$FFFF),              // KANGXI RADICAL BITTER
    (Unicode:#$2FA0; Attr:daCompat; Ch1:#$8FB0; Ch2:#$FFFF),              // KANGXI RADICAL MORNING
    (Unicode:#$2FA1; Attr:daCompat; Ch1:#$8FB5; Ch2:#$FFFF),              // KANGXI RADICAL WALK
    (Unicode:#$2FA2; Attr:daCompat; Ch1:#$9091; Ch2:#$FFFF),              // KANGXI RADICAL CITY
    (Unicode:#$2FA3; Attr:daCompat; Ch1:#$9149; Ch2:#$FFFF),              // KANGXI RADICAL WINE
    (Unicode:#$2FA4; Attr:daCompat; Ch1:#$91C6; Ch2:#$FFFF),              // KANGXI RADICAL DISTINGUISH
    (Unicode:#$2FA5; Attr:daCompat; Ch1:#$91CC; Ch2:#$FFFF),              // KANGXI RADICAL VILLAGE
    (Unicode:#$2FA6; Attr:daCompat; Ch1:#$91D1; Ch2:#$FFFF),              // KANGXI RADICAL GOLD
    (Unicode:#$2FA7; Attr:daCompat; Ch1:#$9577; Ch2:#$FFFF),              // KANGXI RADICAL LONG
    (Unicode:#$2FA8; Attr:daCompat; Ch1:#$9580; Ch2:#$FFFF),              // KANGXI RADICAL GATE
    (Unicode:#$2FA9; Attr:daCompat; Ch1:#$961C; Ch2:#$FFFF),              // KANGXI RADICAL MOUND
    (Unicode:#$2FAA; Attr:daCompat; Ch1:#$96B6; Ch2:#$FFFF),              // KANGXI RADICAL SLAVE
    (Unicode:#$2FAB; Attr:daCompat; Ch1:#$96B9; Ch2:#$FFFF),              // KANGXI RADICAL SHORT TAILED BIRD
    (Unicode:#$2FAC; Attr:daCompat; Ch1:#$96E8; Ch2:#$FFFF),              // KANGXI RADICAL RAIN
    (Unicode:#$2FAD; Attr:daCompat; Ch1:#$9751; Ch2:#$FFFF),              // KANGXI RADICAL BLUE
    (Unicode:#$2FAE; Attr:daCompat; Ch1:#$975E; Ch2:#$FFFF),              // KANGXI RADICAL WRONG
    (Unicode:#$2FAF; Attr:daCompat; Ch1:#$9762; Ch2:#$FFFF),              // KANGXI RADICAL FACE
    (Unicode:#$2FB0; Attr:daCompat; Ch1:#$9769; Ch2:#$FFFF),              // KANGXI RADICAL LEATHER
    (Unicode:#$2FB1; Attr:daCompat; Ch1:#$97CB; Ch2:#$FFFF),              // KANGXI RADICAL TANNED LEATHER
    (Unicode:#$2FB2; Attr:daCompat; Ch1:#$97ED; Ch2:#$FFFF),              // KANGXI RADICAL LEEK
    (Unicode:#$2FB3; Attr:daCompat; Ch1:#$97F3; Ch2:#$FFFF),              // KANGXI RADICAL SOUND
    (Unicode:#$2FB4; Attr:daCompat; Ch1:#$9801; Ch2:#$FFFF),              // KANGXI RADICAL LEAF
    (Unicode:#$2FB5; Attr:daCompat; Ch1:#$98A8; Ch2:#$FFFF),              // KANGXI RADICAL WIND
    (Unicode:#$2FB6; Attr:daCompat; Ch1:#$98DB; Ch2:#$FFFF),              // KANGXI RADICAL FLY
    (Unicode:#$2FB7; Attr:daCompat; Ch1:#$98DF; Ch2:#$FFFF),              // KANGXI RADICAL EAT
    (Unicode:#$2FB8; Attr:daCompat; Ch1:#$9996; Ch2:#$FFFF),              // KANGXI RADICAL HEAD
    (Unicode:#$2FB9; Attr:daCompat; Ch1:#$9999; Ch2:#$FFFF),              // KANGXI RADICAL FRAGRANT
    (Unicode:#$2FBA; Attr:daCompat; Ch1:#$99AC; Ch2:#$FFFF),              // KANGXI RADICAL HORSE
    (Unicode:#$2FBB; Attr:daCompat; Ch1:#$9AA8; Ch2:#$FFFF),              // KANGXI RADICAL BONE
    (Unicode:#$2FBC; Attr:daCompat; Ch1:#$9AD8; Ch2:#$FFFF),              // KANGXI RADICAL TALL
    (Unicode:#$2FBD; Attr:daCompat; Ch1:#$9ADF; Ch2:#$FFFF),              // KANGXI RADICAL HAIR
    (Unicode:#$2FBE; Attr:daCompat; Ch1:#$9B25; Ch2:#$FFFF),              // KANGXI RADICAL FIGHT
    (Unicode:#$2FBF; Attr:daCompat; Ch1:#$9B2F; Ch2:#$FFFF),              // KANGXI RADICAL SACRIFICIAL WINE
    (Unicode:#$2FC0; Attr:daCompat; Ch1:#$9B32; Ch2:#$FFFF),              // KANGXI RADICAL CAULDRON
    (Unicode:#$2FC1; Attr:daCompat; Ch1:#$9B3C; Ch2:#$FFFF),              // KANGXI RADICAL GHOST
    (Unicode:#$2FC2; Attr:daCompat; Ch1:#$9B5A; Ch2:#$FFFF),              // KANGXI RADICAL FISH
    (Unicode:#$2FC3; Attr:daCompat; Ch1:#$9CE5; Ch2:#$FFFF),              // KANGXI RADICAL BIRD
    (Unicode:#$2FC4; Attr:daCompat; Ch1:#$9E75; Ch2:#$FFFF),              // KANGXI RADICAL SALT
    (Unicode:#$2FC5; Attr:daCompat; Ch1:#$9E7F; Ch2:#$FFFF),              // KANGXI RADICAL DEER
    (Unicode:#$2FC6; Attr:daCompat; Ch1:#$9EA5; Ch2:#$FFFF),              // KANGXI RADICAL WHEAT
    (Unicode:#$2FC7; Attr:daCompat; Ch1:#$9EBB; Ch2:#$FFFF),              // KANGXI RADICAL HEMP
    (Unicode:#$2FC8; Attr:daCompat; Ch1:#$9EC3; Ch2:#$FFFF),              // KANGXI RADICAL YELLOW
    (Unicode:#$2FC9; Attr:daCompat; Ch1:#$9ECD; Ch2:#$FFFF),              // KANGXI RADICAL MILLET
    (Unicode:#$2FCA; Attr:daCompat; Ch1:#$9ED1; Ch2:#$FFFF),              // KANGXI RADICAL BLACK
    (Unicode:#$2FCB; Attr:daCompat; Ch1:#$9EF9; Ch2:#$FFFF),              // KANGXI RADICAL EMBROIDERY
    (Unicode:#$2FCC; Attr:daCompat; Ch1:#$9EFD; Ch2:#$FFFF),              // KANGXI RADICAL FROG
    (Unicode:#$2FCD; Attr:daCompat; Ch1:#$9F0E; Ch2:#$FFFF),              // KANGXI RADICAL TRIPOD
    (Unicode:#$2FCE; Attr:daCompat; Ch1:#$9F13; Ch2:#$FFFF),              // KANGXI RADICAL DRUM
    (Unicode:#$2FCF; Attr:daCompat; Ch1:#$9F20; Ch2:#$FFFF),              // KANGXI RADICAL RAT
    (Unicode:#$2FD0; Attr:daCompat; Ch1:#$9F3B; Ch2:#$FFFF),              // KANGXI RADICAL NOSE
    (Unicode:#$2FD1; Attr:daCompat; Ch1:#$9F4A; Ch2:#$FFFF),              // KANGXI RADICAL EVEN
    (Unicode:#$2FD2; Attr:daCompat; Ch1:#$9F52; Ch2:#$FFFF),              // KANGXI RADICAL TOOTH
    (Unicode:#$2FD3; Attr:daCompat; Ch1:#$9F8D; Ch2:#$FFFF),              // KANGXI RADICAL DRAGON
    (Unicode:#$2FD4; Attr:daCompat; Ch1:#$9F9C; Ch2:#$FFFF),              // KANGXI RADICAL TURTLE
    (Unicode:#$2FD5; Attr:daCompat; Ch1:#$9FA0; Ch2:#$FFFF),              // KANGXI RADICAL FLUTE
    (Unicode:#$3000; Attr:daWide; Ch1:#$0020; Ch2:#$FFFF),                // IDEOGRAPHIC SPACE
    (Unicode:#$3036; Attr:daCompat; Ch1:#$3012; Ch2:#$FFFF),              // CIRCLED POSTAL MARK
    (Unicode:#$3038; Attr:daCompat; Ch1:#$5341; Ch2:#$FFFF),              // HANGZHOU NUMERAL TEN
    (Unicode:#$3039; Attr:daCompat; Ch1:#$5344; Ch2:#$FFFF),              // HANGZHOU NUMERAL TWENTY
    (Unicode:#$303A; Attr:daCompat; Ch1:#$5345; Ch2:#$FFFF),              // HANGZHOU NUMERAL THIRTY
    (Unicode:#$304C; Attr:daNone; Ch1:#$304B; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER GA
    (Unicode:#$304E; Attr:daNone; Ch1:#$304D; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER GI
    (Unicode:#$3050; Attr:daNone; Ch1:#$304F; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER GU
    (Unicode:#$3052; Attr:daNone; Ch1:#$3051; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER GE
    (Unicode:#$3054; Attr:daNone; Ch1:#$3053; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER GO
    (Unicode:#$3056; Attr:daNone; Ch1:#$3055; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER ZA
    (Unicode:#$3058; Attr:daNone; Ch1:#$3057; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER ZI
    (Unicode:#$305A; Attr:daNone; Ch1:#$3059; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER ZU
    (Unicode:#$305C; Attr:daNone; Ch1:#$305B; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER ZE
    (Unicode:#$305E; Attr:daNone; Ch1:#$305D; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER ZO
    (Unicode:#$3060; Attr:daNone; Ch1:#$305F; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER DA
    (Unicode:#$3062; Attr:daNone; Ch1:#$3061; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER DI
    (Unicode:#$3065; Attr:daNone; Ch1:#$3064; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER DU
    (Unicode:#$3067; Attr:daNone; Ch1:#$3066; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER DE
    (Unicode:#$3069; Attr:daNone; Ch1:#$3068; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER DO
    (Unicode:#$3070; Attr:daNone; Ch1:#$306F; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER BA
    (Unicode:#$3071; Attr:daNone; Ch1:#$306F; Ch2:#$309A; Ch3:#$FFFF),    // HIRAGANA LETTER PA
    (Unicode:#$3073; Attr:daNone; Ch1:#$3072; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER BI
    (Unicode:#$3074; Attr:daNone; Ch1:#$3072; Ch2:#$309A; Ch3:#$FFFF),    // HIRAGANA LETTER PI
    (Unicode:#$3076; Attr:daNone; Ch1:#$3075; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER BU
    (Unicode:#$3077; Attr:daNone; Ch1:#$3075; Ch2:#$309A; Ch3:#$FFFF),    // HIRAGANA LETTER PU
    (Unicode:#$3079; Attr:daNone; Ch1:#$3078; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER BE
    (Unicode:#$307A; Attr:daNone; Ch1:#$3078; Ch2:#$309A; Ch3:#$FFFF),    // HIRAGANA LETTER PE
    (Unicode:#$307C; Attr:daNone; Ch1:#$307B; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER BO
    (Unicode:#$307D; Attr:daNone; Ch1:#$307B; Ch2:#$309A; Ch3:#$FFFF),    // HIRAGANA LETTER PO
    (Unicode:#$3094; Attr:daNone; Ch1:#$3046; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA LETTER VU
    (Unicode:#$309B; Attr:daCompat; Ch1:#$0020; Ch2:#$3099; Ch3:#$FFFF),  // KATAKANA-HIRAGANA VOICED SOUND MARK
    (Unicode:#$309C; Attr:daCompat; Ch1:#$0020; Ch2:#$309A; Ch3:#$FFFF),  // KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
    (Unicode:#$309E; Attr:daNone; Ch1:#$309D; Ch2:#$3099; Ch3:#$FFFF),    // HIRAGANA VOICED ITERATION MARK
    (Unicode:#$30AC; Attr:daNone; Ch1:#$30AB; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER GA
    (Unicode:#$30AE; Attr:daNone; Ch1:#$30AD; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER GI
    (Unicode:#$30B0; Attr:daNone; Ch1:#$30AF; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER GU
    (Unicode:#$30B2; Attr:daNone; Ch1:#$30B1; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER GE
    (Unicode:#$30B4; Attr:daNone; Ch1:#$30B3; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER GO
    (Unicode:#$30B6; Attr:daNone; Ch1:#$30B5; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER ZA
    (Unicode:#$30B8; Attr:daNone; Ch1:#$30B7; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER ZI
    (Unicode:#$30BA; Attr:daNone; Ch1:#$30B9; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER ZU
    (Unicode:#$30BC; Attr:daNone; Ch1:#$30BB; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER ZE
    (Unicode:#$30BE; Attr:daNone; Ch1:#$30BD; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER ZO
    (Unicode:#$30C0; Attr:daNone; Ch1:#$30BF; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER DA
    (Unicode:#$30C2; Attr:daNone; Ch1:#$30C1; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER DI
    (Unicode:#$30C5; Attr:daNone; Ch1:#$30C4; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER DU
    (Unicode:#$30C7; Attr:daNone; Ch1:#$30C6; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER DE
    (Unicode:#$30C9; Attr:daNone; Ch1:#$30C8; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER DO
    (Unicode:#$30D0; Attr:daNone; Ch1:#$30CF; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER BA
    (Unicode:#$30D1; Attr:daNone; Ch1:#$30CF; Ch2:#$309A; Ch3:#$FFFF),    // KATAKANA LETTER PA
    (Unicode:#$30D3; Attr:daNone; Ch1:#$30D2; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER BI
    (Unicode:#$30D4; Attr:daNone; Ch1:#$30D2; Ch2:#$309A; Ch3:#$FFFF),    // KATAKANA LETTER PI
    (Unicode:#$30D6; Attr:daNone; Ch1:#$30D5; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER BU
    (Unicode:#$30D7; Attr:daNone; Ch1:#$30D5; Ch2:#$309A; Ch3:#$FFFF),    // KATAKANA LETTER PU
    (Unicode:#$30D9; Attr:daNone; Ch1:#$30D8; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER BE
    (Unicode:#$30DA; Attr:daNone; Ch1:#$30D8; Ch2:#$309A; Ch3:#$FFFF),    // KATAKANA LETTER PE
    (Unicode:#$30DC; Attr:daNone; Ch1:#$30DB; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER BO
    (Unicode:#$30DD; Attr:daNone; Ch1:#$30DB; Ch2:#$309A; Ch3:#$FFFF),    // KATAKANA LETTER PO
    (Unicode:#$30F4; Attr:daNone; Ch1:#$30A6; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER VU
    (Unicode:#$30F7; Attr:daNone; Ch1:#$30EF; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER VA
    (Unicode:#$30F8; Attr:daNone; Ch1:#$30F0; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER VI
    (Unicode:#$30F9; Attr:daNone; Ch1:#$30F1; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER VE
    (Unicode:#$30FA; Attr:daNone; Ch1:#$30F2; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA LETTER VO
    (Unicode:#$30FE; Attr:daNone; Ch1:#$30FD; Ch2:#$3099; Ch3:#$FFFF),    // KATAKANA VOICED ITERATION MARK
    (Unicode:#$3131; Attr:daCompat; Ch1:#$1100; Ch2:#$FFFF),              // HANGUL LETTER KIYEOK
    (Unicode:#$3132; Attr:daCompat; Ch1:#$1101; Ch2:#$FFFF),              // HANGUL LETTER SSANGKIYEOK
    (Unicode:#$3133; Attr:daCompat; Ch1:#$11AA; Ch2:#$FFFF),              // HANGUL LETTER KIYEOK-SIOS
    (Unicode:#$3134; Attr:daCompat; Ch1:#$1102; Ch2:#$FFFF),              // HANGUL LETTER NIEUN
    (Unicode:#$3135; Attr:daCompat; Ch1:#$11AC; Ch2:#$FFFF),              // HANGUL LETTER NIEUN-CIEUC
    (Unicode:#$3136; Attr:daCompat; Ch1:#$11AD; Ch2:#$FFFF),              // HANGUL LETTER NIEUN-HIEUH
    (Unicode:#$3137; Attr:daCompat; Ch1:#$1103; Ch2:#$FFFF),              // HANGUL LETTER TIKEUT
    (Unicode:#$3138; Attr:daCompat; Ch1:#$1104; Ch2:#$FFFF),              // HANGUL LETTER SSANGTIKEUT
    (Unicode:#$3139; Attr:daCompat; Ch1:#$1105; Ch2:#$FFFF),              // HANGUL LETTER RIEUL
    (Unicode:#$313A; Attr:daCompat; Ch1:#$11B0; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-KIYEOK
    (Unicode:#$313B; Attr:daCompat; Ch1:#$11B1; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-MIEUM
    (Unicode:#$313C; Attr:daCompat; Ch1:#$11B2; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-PIEUP
    (Unicode:#$313D; Attr:daCompat; Ch1:#$11B3; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-SIOS
    (Unicode:#$313E; Attr:daCompat; Ch1:#$11B4; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-THIEUTH
    (Unicode:#$313F; Attr:daCompat; Ch1:#$11B5; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-PHIEUPH
    (Unicode:#$3140; Attr:daCompat; Ch1:#$111A; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-HIEUH
    (Unicode:#$3141; Attr:daCompat; Ch1:#$1106; Ch2:#$FFFF),              // HANGUL LETTER MIEUM
    (Unicode:#$3142; Attr:daCompat; Ch1:#$1107; Ch2:#$FFFF),              // HANGUL LETTER PIEUP
    (Unicode:#$3143; Attr:daCompat; Ch1:#$1108; Ch2:#$FFFF),              // HANGUL LETTER SSANGPIEUP
    (Unicode:#$3144; Attr:daCompat; Ch1:#$1121; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-SIOS
    (Unicode:#$3145; Attr:daCompat; Ch1:#$1109; Ch2:#$FFFF),              // HANGUL LETTER SIOS
    (Unicode:#$3146; Attr:daCompat; Ch1:#$110A; Ch2:#$FFFF),              // HANGUL LETTER SSANGSIOS
    (Unicode:#$3147; Attr:daCompat; Ch1:#$110B; Ch2:#$FFFF),              // HANGUL LETTER IEUNG
    (Unicode:#$3148; Attr:daCompat; Ch1:#$110C; Ch2:#$FFFF),              // HANGUL LETTER CIEUC
    (Unicode:#$3149; Attr:daCompat; Ch1:#$110D; Ch2:#$FFFF),              // HANGUL LETTER SSANGCIEUC
    (Unicode:#$314A; Attr:daCompat; Ch1:#$110E; Ch2:#$FFFF),              // HANGUL LETTER CHIEUCH
    (Unicode:#$314B; Attr:daCompat; Ch1:#$110F; Ch2:#$FFFF),              // HANGUL LETTER KHIEUKH
    (Unicode:#$314C; Attr:daCompat; Ch1:#$1110; Ch2:#$FFFF),              // HANGUL LETTER THIEUTH
    (Unicode:#$314D; Attr:daCompat; Ch1:#$1111; Ch2:#$FFFF),              // HANGUL LETTER PHIEUPH
    (Unicode:#$314E; Attr:daCompat; Ch1:#$1112; Ch2:#$FFFF),              // HANGUL LETTER HIEUH
    (Unicode:#$314F; Attr:daCompat; Ch1:#$1161; Ch2:#$FFFF),              // HANGUL LETTER A
    (Unicode:#$3150; Attr:daCompat; Ch1:#$1162; Ch2:#$FFFF),              // HANGUL LETTER AE
    (Unicode:#$3151; Attr:daCompat; Ch1:#$1163; Ch2:#$FFFF),              // HANGUL LETTER YA
    (Unicode:#$3152; Attr:daCompat; Ch1:#$1164; Ch2:#$FFFF),              // HANGUL LETTER YAE
    (Unicode:#$3153; Attr:daCompat; Ch1:#$1165; Ch2:#$FFFF),              // HANGUL LETTER EO
    (Unicode:#$3154; Attr:daCompat; Ch1:#$1166; Ch2:#$FFFF),              // HANGUL LETTER E
    (Unicode:#$3155; Attr:daCompat; Ch1:#$1167; Ch2:#$FFFF),              // HANGUL LETTER YEO
    (Unicode:#$3156; Attr:daCompat; Ch1:#$1168; Ch2:#$FFFF),              // HANGUL LETTER YE
    (Unicode:#$3157; Attr:daCompat; Ch1:#$1169; Ch2:#$FFFF),              // HANGUL LETTER O
    (Unicode:#$3158; Attr:daCompat; Ch1:#$116A; Ch2:#$FFFF),              // HANGUL LETTER WA
    (Unicode:#$3159; Attr:daCompat; Ch1:#$116B; Ch2:#$FFFF),              // HANGUL LETTER WAE
    (Unicode:#$315A; Attr:daCompat; Ch1:#$116C; Ch2:#$FFFF),              // HANGUL LETTER OE
    (Unicode:#$315B; Attr:daCompat; Ch1:#$116D; Ch2:#$FFFF),              // HANGUL LETTER YO
    (Unicode:#$315C; Attr:daCompat; Ch1:#$116E; Ch2:#$FFFF),              // HANGUL LETTER U
    (Unicode:#$315D; Attr:daCompat; Ch1:#$116F; Ch2:#$FFFF),              // HANGUL LETTER WEO
    (Unicode:#$315E; Attr:daCompat; Ch1:#$1170; Ch2:#$FFFF),              // HANGUL LETTER WE
    (Unicode:#$315F; Attr:daCompat; Ch1:#$1171; Ch2:#$FFFF),              // HANGUL LETTER WI
    (Unicode:#$3160; Attr:daCompat; Ch1:#$1172; Ch2:#$FFFF),              // HANGUL LETTER YU
    (Unicode:#$3161; Attr:daCompat; Ch1:#$1173; Ch2:#$FFFF),              // HANGUL LETTER EU
    (Unicode:#$3162; Attr:daCompat; Ch1:#$1174; Ch2:#$FFFF),              // HANGUL LETTER YI
    (Unicode:#$3163; Attr:daCompat; Ch1:#$1175; Ch2:#$FFFF),              // HANGUL LETTER I
    (Unicode:#$3164; Attr:daCompat; Ch1:#$1160; Ch2:#$FFFF),              // HANGUL FILLER
    (Unicode:#$3165; Attr:daCompat; Ch1:#$1114; Ch2:#$FFFF),              // HANGUL LETTER SSANGNIEUN
    (Unicode:#$3166; Attr:daCompat; Ch1:#$1115; Ch2:#$FFFF),              // HANGUL LETTER NIEUN-TIKEUT
    (Unicode:#$3167; Attr:daCompat; Ch1:#$11C7; Ch2:#$FFFF),              // HANGUL LETTER NIEUN-SIOS
    (Unicode:#$3168; Attr:daCompat; Ch1:#$11C8; Ch2:#$FFFF),              // HANGUL LETTER NIEUN-PANSIOS
    (Unicode:#$3169; Attr:daCompat; Ch1:#$11CC; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-KIYEOK-SIOS
    (Unicode:#$316A; Attr:daCompat; Ch1:#$11CE; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-TIKEUT
    (Unicode:#$316B; Attr:daCompat; Ch1:#$11D3; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-PIEUP-SIOS
    (Unicode:#$316C; Attr:daCompat; Ch1:#$11D7; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-PANSIOS
    (Unicode:#$316D; Attr:daCompat; Ch1:#$11D9; Ch2:#$FFFF),              // HANGUL LETTER RIEUL-YEORINHIEUH
    (Unicode:#$316E; Attr:daCompat; Ch1:#$111C; Ch2:#$FFFF),              // HANGUL LETTER MIEUM-PIEUP
    (Unicode:#$316F; Attr:daCompat; Ch1:#$11DD; Ch2:#$FFFF),              // HANGUL LETTER MIEUM-SIOS
    (Unicode:#$3170; Attr:daCompat; Ch1:#$11DF; Ch2:#$FFFF),              // HANGUL LETTER MIEUM-PANSIOS
    (Unicode:#$3171; Attr:daCompat; Ch1:#$111D; Ch2:#$FFFF),              // HANGUL LETTER KAPYEOUNMIEUM
    (Unicode:#$3172; Attr:daCompat; Ch1:#$111E; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-KIYEOK
    (Unicode:#$3173; Attr:daCompat; Ch1:#$1120; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-TIKEUT
    (Unicode:#$3174; Attr:daCompat; Ch1:#$1122; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-SIOS-KIYEOK
    (Unicode:#$3175; Attr:daCompat; Ch1:#$1123; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-SIOS-TIKEUT
    (Unicode:#$3176; Attr:daCompat; Ch1:#$1127; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-CIEUC
    (Unicode:#$3177; Attr:daCompat; Ch1:#$1129; Ch2:#$FFFF),              // HANGUL LETTER PIEUP-THIEUTH
    (Unicode:#$3178; Attr:daCompat; Ch1:#$112B; Ch2:#$FFFF),              // HANGUL LETTER KAPYEOUNPIEUP
    (Unicode:#$3179; Attr:daCompat; Ch1:#$112C; Ch2:#$FFFF),              // HANGUL LETTER KAPYEOUNSSANGPIEUP
    (Unicode:#$317A; Attr:daCompat; Ch1:#$112D; Ch2:#$FFFF),              // HANGUL LETTER SIOS-KIYEOK
    (Unicode:#$317B; Attr:daCompat; Ch1:#$112E; Ch2:#$FFFF),              // HANGUL LETTER SIOS-NIEUN
    (Unicode:#$317C; Attr:daCompat; Ch1:#$112F; Ch2:#$FFFF),              // HANGUL LETTER SIOS-TIKEUT
    (Unicode:#$317D; Attr:daCompat; Ch1:#$1132; Ch2:#$FFFF),              // HANGUL LETTER SIOS-PIEUP
    (Unicode:#$317E; Attr:daCompat; Ch1:#$1136; Ch2:#$FFFF),              // HANGUL LETTER SIOS-CIEUC
    (Unicode:#$317F; Attr:daCompat; Ch1:#$1140; Ch2:#$FFFF),              // HANGUL LETTER PANSIOS
    (Unicode:#$3180; Attr:daCompat; Ch1:#$1147; Ch2:#$FFFF),              // HANGUL LETTER SSANGIEUNG
    (Unicode:#$3181; Attr:daCompat; Ch1:#$114C; Ch2:#$FFFF),              // HANGUL LETTER YESIEUNG
    (Unicode:#$3182; Attr:daCompat; Ch1:#$11F1; Ch2:#$FFFF),              // HANGUL LETTER YESIEUNG-SIOS
    (Unicode:#$3183; Attr:daCompat; Ch1:#$11F2; Ch2:#$FFFF),              // HANGUL LETTER YESIEUNG-PANSIOS
    (Unicode:#$3184; Attr:daCompat; Ch1:#$1157; Ch2:#$FFFF),              // HANGUL LETTER KAPYEOUNPHIEUPH
    (Unicode:#$3185; Attr:daCompat; Ch1:#$1158; Ch2:#$FFFF),              // HANGUL LETTER SSANGHIEUH
    (Unicode:#$3186; Attr:daCompat; Ch1:#$1159; Ch2:#$FFFF),              // HANGUL LETTER YEORINHIEUH
    (Unicode:#$3187; Attr:daCompat; Ch1:#$1184; Ch2:#$FFFF),              // HANGUL LETTER YO-YA
    (Unicode:#$3188; Attr:daCompat; Ch1:#$1185; Ch2:#$FFFF),              // HANGUL LETTER YO-YAE
    (Unicode:#$3189; Attr:daCompat; Ch1:#$1188; Ch2:#$FFFF),              // HANGUL LETTER YO-I
    (Unicode:#$318A; Attr:daCompat; Ch1:#$1191; Ch2:#$FFFF),              // HANGUL LETTER YU-YEO
    (Unicode:#$318B; Attr:daCompat; Ch1:#$1192; Ch2:#$FFFF),              // HANGUL LETTER YU-YE
    (Unicode:#$318C; Attr:daCompat; Ch1:#$1194; Ch2:#$FFFF),              // HANGUL LETTER YU-I
    (Unicode:#$318D; Attr:daCompat; Ch1:#$119E; Ch2:#$FFFF),              // HANGUL LETTER ARAEA
    (Unicode:#$318E; Attr:daCompat; Ch1:#$11A1; Ch2:#$FFFF),              // HANGUL LETTER ARAEAE
    (Unicode:#$3192; Attr:daSuper; Ch1:#$4E00; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION ONE MARK
    (Unicode:#$3193; Attr:daSuper; Ch1:#$4E8C; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION TWO MARK
    (Unicode:#$3194; Attr:daSuper; Ch1:#$4E09; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION THREE MARK
    (Unicode:#$3195; Attr:daSuper; Ch1:#$56DB; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION FOUR MARK
    (Unicode:#$3196; Attr:daSuper; Ch1:#$4E0A; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION TOP MARK
    (Unicode:#$3197; Attr:daSuper; Ch1:#$4E2D; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION MIDDLE MARK
    (Unicode:#$3198; Attr:daSuper; Ch1:#$4E0B; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION BOTTOM MARK
    (Unicode:#$3199; Attr:daSuper; Ch1:#$7532; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION FIRST MARK
    (Unicode:#$319A; Attr:daSuper; Ch1:#$4E59; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION SECOND MARK
    (Unicode:#$319B; Attr:daSuper; Ch1:#$4E19; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION THIRD MARK
    (Unicode:#$319C; Attr:daSuper; Ch1:#$4E01; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION FOURTH MARK
    (Unicode:#$319D; Attr:daSuper; Ch1:#$5929; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION HEAVEN MARK
    (Unicode:#$319E; Attr:daSuper; Ch1:#$5730; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION EARTH MARK
    (Unicode:#$319F; Attr:daSuper; Ch1:#$4EBA; Ch2:#$FFFF),               // IDEOGRAPHIC ANNOTATION MAN MARK
    (Unicode:#$3200; Attr:daCompat; Ch1:#$0028; Ch2:#$1100; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL KIYEOK
    (Unicode:#$3201; Attr:daCompat; Ch1:#$0028; Ch2:#$1102; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL NIEUN
    (Unicode:#$3202; Attr:daCompat; Ch1:#$0028; Ch2:#$1103; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL TIKEUT
    (Unicode:#$3203; Attr:daCompat; Ch1:#$0028; Ch2:#$1105; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL RIEUL
    (Unicode:#$3204; Attr:daCompat; Ch1:#$0028; Ch2:#$1106; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL MIEUM
    (Unicode:#$3205; Attr:daCompat; Ch1:#$0028; Ch2:#$1107; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL PIEUP
    (Unicode:#$3206; Attr:daCompat; Ch1:#$0028; Ch2:#$1109; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL SIOS
    (Unicode:#$3207; Attr:daCompat; Ch1:#$0028; Ch2:#$110B; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL IEUNG
    (Unicode:#$3208; Attr:daCompat; Ch1:#$0028; Ch2:#$110C; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL CIEUC
    (Unicode:#$3209; Attr:daCompat; Ch1:#$0028; Ch2:#$110E; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL CHIEUCH
    (Unicode:#$320A; Attr:daCompat; Ch1:#$0028; Ch2:#$110F; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL KHIEUKH
    (Unicode:#$320B; Attr:daCompat; Ch1:#$0028; Ch2:#$1110; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL THIEUTH
    (Unicode:#$320C; Attr:daCompat; Ch1:#$0028; Ch2:#$1111; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL PHIEUPH
    (Unicode:#$320D; Attr:daCompat; Ch1:#$0028; Ch2:#$1112; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED HANGUL HIEUH
    (Unicode:#$320E; Attr:daCompat; Ch1:#$0028; Ch2:#$1100; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL KIYEOK A
    (Unicode:#$320F; Attr:daCompat; Ch1:#$0028; Ch2:#$1102; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL NIEUN A
    (Unicode:#$3210; Attr:daCompat; Ch1:#$0028; Ch2:#$1103; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL TIKEUT A
    (Unicode:#$3211; Attr:daCompat; Ch1:#$0028; Ch2:#$1105; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL RIEUL A
    (Unicode:#$3212; Attr:daCompat; Ch1:#$0028; Ch2:#$1106; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL MIEUM A
    (Unicode:#$3213; Attr:daCompat; Ch1:#$0028; Ch2:#$1107; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL PIEUP A
    (Unicode:#$3214; Attr:daCompat; Ch1:#$0028; Ch2:#$1109; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL SIOS A
    (Unicode:#$3215; Attr:daCompat; Ch1:#$0028; Ch2:#$110B; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL IEUNG A
    (Unicode:#$3216; Attr:daCompat; Ch1:#$0028; Ch2:#$110C; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL CIEUC A
    (Unicode:#$3217; Attr:daCompat; Ch1:#$0028; Ch2:#$110E; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL CHIEUCH A
    (Unicode:#$3218; Attr:daCompat; Ch1:#$0028; Ch2:#$110F; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL KHIEUKH A
    (Unicode:#$3219; Attr:daCompat; Ch1:#$0028; Ch2:#$1110; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL THIEUTH A
    (Unicode:#$321A; Attr:daCompat; Ch1:#$0028; Ch2:#$1111; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL PHIEUPH A
    (Unicode:#$321B; Attr:daCompat; Ch1:#$0028; Ch2:#$1112; Ch3:#$1161; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL HIEUH A
    (Unicode:#$321C; Attr:daCompat; Ch1:#$0028; Ch2:#$110C; Ch3:#$116E; Ch4:#$0029; Ch5:#$FFFF),  // PARENTHESIZED HANGUL CIEUC U
    (Unicode:#$3220; Attr:daCompat; Ch1:#$0028; Ch2:#$4E00; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH ONE
    (Unicode:#$3221; Attr:daCompat; Ch1:#$0028; Ch2:#$4E8C; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH TWO
    (Unicode:#$3222; Attr:daCompat; Ch1:#$0028; Ch2:#$4E09; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH THREE
    (Unicode:#$3223; Attr:daCompat; Ch1:#$0028; Ch2:#$56DB; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH FOUR
    (Unicode:#$3224; Attr:daCompat; Ch1:#$0028; Ch2:#$4E94; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH FIVE
    (Unicode:#$3225; Attr:daCompat; Ch1:#$0028; Ch2:#$516D; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SIX
    (Unicode:#$3226; Attr:daCompat; Ch1:#$0028; Ch2:#$4E03; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SEVEN
    (Unicode:#$3227; Attr:daCompat; Ch1:#$0028; Ch2:#$516B; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH EIGHT
    (Unicode:#$3228; Attr:daCompat; Ch1:#$0028; Ch2:#$4E5D; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH NINE
    (Unicode:#$3229; Attr:daCompat; Ch1:#$0028; Ch2:#$5341; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH TEN
    (Unicode:#$322A; Attr:daCompat; Ch1:#$0028; Ch2:#$6708; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH MOON
    (Unicode:#$322B; Attr:daCompat; Ch1:#$0028; Ch2:#$706B; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH FIRE
    (Unicode:#$322C; Attr:daCompat; Ch1:#$0028; Ch2:#$6C34; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH WATER
    (Unicode:#$322D; Attr:daCompat; Ch1:#$0028; Ch2:#$6728; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH WOOD
    (Unicode:#$322E; Attr:daCompat; Ch1:#$0028; Ch2:#$91D1; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH METAL
    (Unicode:#$322F; Attr:daCompat; Ch1:#$0028; Ch2:#$571F; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH EARTH
    (Unicode:#$3230; Attr:daCompat; Ch1:#$0028; Ch2:#$65E5; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SUN
    (Unicode:#$3231; Attr:daCompat; Ch1:#$0028; Ch2:#$682A; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH STOCK
    (Unicode:#$3232; Attr:daCompat; Ch1:#$0028; Ch2:#$6709; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH HAVE
    (Unicode:#$3233; Attr:daCompat; Ch1:#$0028; Ch2:#$793E; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SOCIETY
    (Unicode:#$3234; Attr:daCompat; Ch1:#$0028; Ch2:#$540D; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH NAME
    (Unicode:#$3235; Attr:daCompat; Ch1:#$0028; Ch2:#$7279; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SPECIAL
    (Unicode:#$3236; Attr:daCompat; Ch1:#$0028; Ch2:#$8CA1; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH FINANCIAL
    (Unicode:#$3237; Attr:daCompat; Ch1:#$0028; Ch2:#$795D; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH CONGRATULATION
    (Unicode:#$3238; Attr:daCompat; Ch1:#$0028; Ch2:#$52B4; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH LABOR
    (Unicode:#$3239; Attr:daCompat; Ch1:#$0028; Ch2:#$4EE3; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH REPRESENT
    (Unicode:#$323A; Attr:daCompat; Ch1:#$0028; Ch2:#$547C; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH CALL
    (Unicode:#$323B; Attr:daCompat; Ch1:#$0028; Ch2:#$5B66; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH STUDY
    (Unicode:#$323C; Attr:daCompat; Ch1:#$0028; Ch2:#$76E3; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SUPERVISE
    (Unicode:#$323D; Attr:daCompat; Ch1:#$0028; Ch2:#$4F01; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH ENTERPRISE
    (Unicode:#$323E; Attr:daCompat; Ch1:#$0028; Ch2:#$8CC7; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH RESOURCE
    (Unicode:#$323F; Attr:daCompat; Ch1:#$0028; Ch2:#$5354; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH ALLIANCE
    (Unicode:#$3240; Attr:daCompat; Ch1:#$0028; Ch2:#$796D; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH FESTIVAL
    (Unicode:#$3241; Attr:daCompat; Ch1:#$0028; Ch2:#$4F11; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH REST
    (Unicode:#$3242; Attr:daCompat; Ch1:#$0028; Ch2:#$81EA; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH SELF
    (Unicode:#$3243; Attr:daCompat; Ch1:#$0028; Ch2:#$81F3; Ch3:#$0029; Ch4:#$FFFF),              // PARENTHESIZED IDEOGRAPH REACH
    (Unicode:#$3260; Attr:daCircle; Ch1:#$1100; Ch2:#$FFFF),              // CIRCLED HANGUL KIYEOK
    (Unicode:#$3261; Attr:daCircle; Ch1:#$1102; Ch2:#$FFFF),              // CIRCLED HANGUL NIEUN
    (Unicode:#$3262; Attr:daCircle; Ch1:#$1103; Ch2:#$FFFF),              // CIRCLED HANGUL TIKEUT
    (Unicode:#$3263; Attr:daCircle; Ch1:#$1105; Ch2:#$FFFF),              // CIRCLED HANGUL RIEUL
    (Unicode:#$3264; Attr:daCircle; Ch1:#$1106; Ch2:#$FFFF),              // CIRCLED HANGUL MIEUM
    (Unicode:#$3265; Attr:daCircle; Ch1:#$1107; Ch2:#$FFFF),              // CIRCLED HANGUL PIEUP
    (Unicode:#$3266; Attr:daCircle; Ch1:#$1109; Ch2:#$FFFF),              // CIRCLED HANGUL SIOS
    (Unicode:#$3267; Attr:daCircle; Ch1:#$110B; Ch2:#$FFFF),              // CIRCLED HANGUL IEUNG
    (Unicode:#$3268; Attr:daCircle; Ch1:#$110C; Ch2:#$FFFF),              // CIRCLED HANGUL CIEUC
    (Unicode:#$3269; Attr:daCircle; Ch1:#$110E; Ch2:#$FFFF),              // CIRCLED HANGUL CHIEUCH
    (Unicode:#$326A; Attr:daCircle; Ch1:#$110F; Ch2:#$FFFF),              // CIRCLED HANGUL KHIEUKH
    (Unicode:#$326B; Attr:daCircle; Ch1:#$1110; Ch2:#$FFFF),              // CIRCLED HANGUL THIEUTH
    (Unicode:#$326C; Attr:daCircle; Ch1:#$1111; Ch2:#$FFFF),              // CIRCLED HANGUL PHIEUPH
    (Unicode:#$326D; Attr:daCircle; Ch1:#$1112; Ch2:#$FFFF),              // CIRCLED HANGUL HIEUH
    (Unicode:#$326E; Attr:daCircle; Ch1:#$1100; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL KIYEOK A
    (Unicode:#$326F; Attr:daCircle; Ch1:#$1102; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL NIEUN A
    (Unicode:#$3270; Attr:daCircle; Ch1:#$1103; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL TIKEUT A
    (Unicode:#$3271; Attr:daCircle; Ch1:#$1105; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL RIEUL A
    (Unicode:#$3272; Attr:daCircle; Ch1:#$1106; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL MIEUM A
    (Unicode:#$3273; Attr:daCircle; Ch1:#$1107; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL PIEUP A
    (Unicode:#$3274; Attr:daCircle; Ch1:#$1109; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL SIOS A
    (Unicode:#$3275; Attr:daCircle; Ch1:#$110B; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL IEUNG A
    (Unicode:#$3276; Attr:daCircle; Ch1:#$110C; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL CIEUC A
    (Unicode:#$3277; Attr:daCircle; Ch1:#$110E; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL CHIEUCH A
    (Unicode:#$3278; Attr:daCircle; Ch1:#$110F; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL KHIEUKH A
    (Unicode:#$3279; Attr:daCircle; Ch1:#$1110; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL THIEUTH A
    (Unicode:#$327A; Attr:daCircle; Ch1:#$1111; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL PHIEUPH A
    (Unicode:#$327B; Attr:daCircle; Ch1:#$1112; Ch2:#$1161; Ch3:#$FFFF),  // CIRCLED HANGUL HIEUH A
    (Unicode:#$3280; Attr:daCircle; Ch1:#$4E00; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH ONE
    (Unicode:#$3281; Attr:daCircle; Ch1:#$4E8C; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH TWO
    (Unicode:#$3282; Attr:daCircle; Ch1:#$4E09; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH THREE
    (Unicode:#$3283; Attr:daCircle; Ch1:#$56DB; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH FOUR
    (Unicode:#$3284; Attr:daCircle; Ch1:#$4E94; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH FIVE
    (Unicode:#$3285; Attr:daCircle; Ch1:#$516D; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SIX
    (Unicode:#$3286; Attr:daCircle; Ch1:#$4E03; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SEVEN
    (Unicode:#$3287; Attr:daCircle; Ch1:#$516B; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH EIGHT
    (Unicode:#$3288; Attr:daCircle; Ch1:#$4E5D; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH NINE
    (Unicode:#$3289; Attr:daCircle; Ch1:#$5341; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH TEN
    (Unicode:#$328A; Attr:daCircle; Ch1:#$6708; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH MOON
    (Unicode:#$328B; Attr:daCircle; Ch1:#$706B; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH FIRE
    (Unicode:#$328C; Attr:daCircle; Ch1:#$6C34; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH WATER
    (Unicode:#$328D; Attr:daCircle; Ch1:#$6728; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH WOOD
    (Unicode:#$328E; Attr:daCircle; Ch1:#$91D1; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH METAL
    (Unicode:#$328F; Attr:daCircle; Ch1:#$571F; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH EARTH
    (Unicode:#$3290; Attr:daCircle; Ch1:#$65E5; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SUN
    (Unicode:#$3291; Attr:daCircle; Ch1:#$682A; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH STOCK
    (Unicode:#$3292; Attr:daCircle; Ch1:#$6709; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH HAVE
    (Unicode:#$3293; Attr:daCircle; Ch1:#$793E; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SOCIETY
    (Unicode:#$3294; Attr:daCircle; Ch1:#$540D; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH NAME
    (Unicode:#$3295; Attr:daCircle; Ch1:#$7279; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SPECIAL
    (Unicode:#$3296; Attr:daCircle; Ch1:#$8CA1; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH FINANCIAL
    (Unicode:#$3297; Attr:daCircle; Ch1:#$795D; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH CONGRATULATION
    (Unicode:#$3298; Attr:daCircle; Ch1:#$52B4; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH LABOR
    (Unicode:#$3299; Attr:daCircle; Ch1:#$79D8; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SECRET
    (Unicode:#$329A; Attr:daCircle; Ch1:#$7537; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH MALE
    (Unicode:#$329B; Attr:daCircle; Ch1:#$5973; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH FEMALE
    (Unicode:#$329C; Attr:daCircle; Ch1:#$9069; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SUITABLE
    (Unicode:#$329D; Attr:daCircle; Ch1:#$512A; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH EXCELLENT
    (Unicode:#$329E; Attr:daCircle; Ch1:#$5370; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH PRINT
    (Unicode:#$329F; Attr:daCircle; Ch1:#$6CE8; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH ATTENTION
    (Unicode:#$32A0; Attr:daCircle; Ch1:#$9805; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH ITEM
    (Unicode:#$32A1; Attr:daCircle; Ch1:#$4F11; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH REST
    (Unicode:#$32A2; Attr:daCircle; Ch1:#$5199; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH COPY
    (Unicode:#$32A3; Attr:daCircle; Ch1:#$6B63; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH CORRECT
    (Unicode:#$32A4; Attr:daCircle; Ch1:#$4E0A; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH HIGH
    (Unicode:#$32A5; Attr:daCircle; Ch1:#$4E2D; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH CENTRE
    (Unicode:#$32A6; Attr:daCircle; Ch1:#$4E0B; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH LOW
    (Unicode:#$32A7; Attr:daCircle; Ch1:#$5DE6; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH LEFT
    (Unicode:#$32A8; Attr:daCircle; Ch1:#$53F3; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH RIGHT
    (Unicode:#$32A9; Attr:daCircle; Ch1:#$533B; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH MEDICINE
    (Unicode:#$32AA; Attr:daCircle; Ch1:#$5B97; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH RELIGION
    (Unicode:#$32AB; Attr:daCircle; Ch1:#$5B66; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH STUDY
    (Unicode:#$32AC; Attr:daCircle; Ch1:#$76E3; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH SUPERVISE
    (Unicode:#$32AD; Attr:daCircle; Ch1:#$4F01; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH ENTERPRISE
    (Unicode:#$32AE; Attr:daCircle; Ch1:#$8CC7; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH RESOURCE
    (Unicode:#$32AF; Attr:daCircle; Ch1:#$5354; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH ALLIANCE
    (Unicode:#$32B0; Attr:daCircle; Ch1:#$591C; Ch2:#$FFFF),              // CIRCLED IDEOGRAPH NIGHT
    (Unicode:#$32C0; Attr:daCompat; Ch1:#$0031; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR JANUARY
    (Unicode:#$32C1; Attr:daCompat; Ch1:#$0032; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR FEBRUARY
    (Unicode:#$32C2; Attr:daCompat; Ch1:#$0033; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR MARCH
    (Unicode:#$32C3; Attr:daCompat; Ch1:#$0034; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR APRIL
    (Unicode:#$32C4; Attr:daCompat; Ch1:#$0035; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR MAY
    (Unicode:#$32C5; Attr:daCompat; Ch1:#$0036; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR JUNE
    (Unicode:#$32C6; Attr:daCompat; Ch1:#$0037; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR JULY
    (Unicode:#$32C7; Attr:daCompat; Ch1:#$0038; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR AUGUST
    (Unicode:#$32C8; Attr:daCompat; Ch1:#$0039; Ch2:#$6708; Ch3:#$FFFF),               // IDEOGRAPHIC TELEGRAPH SYMBOL FOR SEPTEMBER
    (Unicode:#$32C9; Attr:daCompat; Ch1:#$0031; Ch2:#$0030; Ch3:#$6708; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR OCTOBER
    (Unicode:#$32CA; Attr:daCompat; Ch1:#$0031; Ch2:#$0031; Ch3:#$6708; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR NOVEMBER
    (Unicode:#$32CB; Attr:daCompat; Ch1:#$0031; Ch2:#$0032; Ch3:#$6708; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DECEMBER
    (Unicode:#$32D0; Attr:daCircle; Ch1:#$30A2; Ch2:#$FFFF),              // CIRCLED KATAKANA A
    (Unicode:#$32D1; Attr:daCircle; Ch1:#$30A4; Ch2:#$FFFF),              // CIRCLED KATAKANA I
    (Unicode:#$32D2; Attr:daCircle; Ch1:#$30A6; Ch2:#$FFFF),              // CIRCLED KATAKANA U
    (Unicode:#$32D3; Attr:daCircle; Ch1:#$30A8; Ch2:#$FFFF),              // CIRCLED KATAKANA E
    (Unicode:#$32D4; Attr:daCircle; Ch1:#$30AA; Ch2:#$FFFF),              // CIRCLED KATAKANA O
    (Unicode:#$32D5; Attr:daCircle; Ch1:#$30AB; Ch2:#$FFFF),              // CIRCLED KATAKANA KA
    (Unicode:#$32D6; Attr:daCircle; Ch1:#$30AD; Ch2:#$FFFF),              // CIRCLED KATAKANA KI
    (Unicode:#$32D7; Attr:daCircle; Ch1:#$30AF; Ch2:#$FFFF),              // CIRCLED KATAKANA KU
    (Unicode:#$32D8; Attr:daCircle; Ch1:#$30B1; Ch2:#$FFFF),              // CIRCLED KATAKANA KE
    (Unicode:#$32D9; Attr:daCircle; Ch1:#$30B3; Ch2:#$FFFF),              // CIRCLED KATAKANA KO
    (Unicode:#$32DA; Attr:daCircle; Ch1:#$30B5; Ch2:#$FFFF),              // CIRCLED KATAKANA SA
    (Unicode:#$32DB; Attr:daCircle; Ch1:#$30B7; Ch2:#$FFFF),              // CIRCLED KATAKANA SI
    (Unicode:#$32DC; Attr:daCircle; Ch1:#$30B9; Ch2:#$FFFF),              // CIRCLED KATAKANA SU
    (Unicode:#$32DD; Attr:daCircle; Ch1:#$30BB; Ch2:#$FFFF),              // CIRCLED KATAKANA SE
    (Unicode:#$32DE; Attr:daCircle; Ch1:#$30BD; Ch2:#$FFFF),              // CIRCLED KATAKANA SO
    (Unicode:#$32DF; Attr:daCircle; Ch1:#$30BF; Ch2:#$FFFF),              // CIRCLED KATAKANA TA
    (Unicode:#$32E0; Attr:daCircle; Ch1:#$30C1; Ch2:#$FFFF),              // CIRCLED KATAKANA TI
    (Unicode:#$32E1; Attr:daCircle; Ch1:#$30C4; Ch2:#$FFFF),              // CIRCLED KATAKANA TU
    (Unicode:#$32E2; Attr:daCircle; Ch1:#$30C6; Ch2:#$FFFF),              // CIRCLED KATAKANA TE
    (Unicode:#$32E3; Attr:daCircle; Ch1:#$30C8; Ch2:#$FFFF),              // CIRCLED KATAKANA TO
    (Unicode:#$32E4; Attr:daCircle; Ch1:#$30CA; Ch2:#$FFFF),              // CIRCLED KATAKANA NA
    (Unicode:#$32E5; Attr:daCircle; Ch1:#$30CB; Ch2:#$FFFF),              // CIRCLED KATAKANA NI
    (Unicode:#$32E6; Attr:daCircle; Ch1:#$30CC; Ch2:#$FFFF),              // CIRCLED KATAKANA NU
    (Unicode:#$32E7; Attr:daCircle; Ch1:#$30CD; Ch2:#$FFFF),              // CIRCLED KATAKANA NE
    (Unicode:#$32E8; Attr:daCircle; Ch1:#$30CE; Ch2:#$FFFF),              // CIRCLED KATAKANA NO
    (Unicode:#$32E9; Attr:daCircle; Ch1:#$30CF; Ch2:#$FFFF),              // CIRCLED KATAKANA HA
    (Unicode:#$32EA; Attr:daCircle; Ch1:#$30D2; Ch2:#$FFFF),              // CIRCLED KATAKANA HI
    (Unicode:#$32EB; Attr:daCircle; Ch1:#$30D5; Ch2:#$FFFF),              // CIRCLED KATAKANA HU
    (Unicode:#$32EC; Attr:daCircle; Ch1:#$30D8; Ch2:#$FFFF),              // CIRCLED KATAKANA HE
    (Unicode:#$32ED; Attr:daCircle; Ch1:#$30DB; Ch2:#$FFFF),              // CIRCLED KATAKANA HO
    (Unicode:#$32EE; Attr:daCircle; Ch1:#$30DE; Ch2:#$FFFF),              // CIRCLED KATAKANA MA
    (Unicode:#$32EF; Attr:daCircle; Ch1:#$30DF; Ch2:#$FFFF),              // CIRCLED KATAKANA MI
    (Unicode:#$32F0; Attr:daCircle; Ch1:#$30E0; Ch2:#$FFFF),              // CIRCLED KATAKANA MU
    (Unicode:#$32F1; Attr:daCircle; Ch1:#$30E1; Ch2:#$FFFF),              // CIRCLED KATAKANA ME
    (Unicode:#$32F2; Attr:daCircle; Ch1:#$30E2; Ch2:#$FFFF),              // CIRCLED KATAKANA MO
    (Unicode:#$32F3; Attr:daCircle; Ch1:#$30E4; Ch2:#$FFFF),              // CIRCLED KATAKANA YA
    (Unicode:#$32F4; Attr:daCircle; Ch1:#$30E6; Ch2:#$FFFF),              // CIRCLED KATAKANA YU
    (Unicode:#$32F5; Attr:daCircle; Ch1:#$30E8; Ch2:#$FFFF),              // CIRCLED KATAKANA YO
    (Unicode:#$32F6; Attr:daCircle; Ch1:#$30E9; Ch2:#$FFFF),              // CIRCLED KATAKANA RA
    (Unicode:#$32F7; Attr:daCircle; Ch1:#$30EA; Ch2:#$FFFF),              // CIRCLED KATAKANA RI
    (Unicode:#$32F8; Attr:daCircle; Ch1:#$30EB; Ch2:#$FFFF),              // CIRCLED KATAKANA RU
    (Unicode:#$32F9; Attr:daCircle; Ch1:#$30EC; Ch2:#$FFFF),              // CIRCLED KATAKANA RE
    (Unicode:#$32FA; Attr:daCircle; Ch1:#$30ED; Ch2:#$FFFF),              // CIRCLED KATAKANA RO
    (Unicode:#$32FB; Attr:daCircle; Ch1:#$30EF; Ch2:#$FFFF),              // CIRCLED KATAKANA WA
    (Unicode:#$32FC; Attr:daCircle; Ch1:#$30F0; Ch2:#$FFFF),              // CIRCLED KATAKANA WI
    (Unicode:#$32FD; Attr:daCircle; Ch1:#$30F1; Ch2:#$FFFF),              // CIRCLED KATAKANA WE
    (Unicode:#$32FE; Attr:daCircle; Ch1:#$30F2; Ch2:#$FFFF),              // CIRCLED KATAKANA WO
    (Unicode:#$3300; Attr:daSquare; Ch1:#$30A2; Ch2:#$30D1; Ch3:#$30FC; Ch4:#$30C8; Ch5:#$FFFF),   // SQUARE APAATO
    (Unicode:#$3301; Attr:daSquare; Ch1:#$30A2; Ch2:#$30EB; Ch3:#$30D5; Ch4:#$30A1; Ch5:#$FFFF),   // SQUARE ARUHUA
    (Unicode:#$3302; Attr:daSquare; Ch1:#$30A2; Ch2:#$30F3; Ch3:#$30DA; Ch4:#$30A2; Ch5:#$FFFF),   // SQUARE ANPEA
    (Unicode:#$3303; Attr:daSquare; Ch1:#$30A2; Ch2:#$30FC; Ch3:#$30EB; Ch4:#$FFFF),               // SQUARE AARU
    (Unicode:#$3304; Attr:daSquare; Ch1:#$30A4; Ch2:#$30CB; Ch3:#$30F3; Ch4:#$30B0; Ch5:#$FFFF),   // SQUARE ININGU
    (Unicode:#$3305; Attr:daSquare; Ch1:#$30A4; Ch2:#$30F3; Ch3:#$30C1; Ch4:#$FFFF),               // SQUARE INTI
    (Unicode:#$3306; Attr:daSquare; Ch1:#$30A6; Ch2:#$30A9; Ch3:#$30F3; Ch4:#$FFFF),               // SQUARE UON
    (Unicode:#$3307; Attr:daSquare; Ch1:#$30A8; Ch2:#$30B9; Ch3:#$30AF; Ch4:#$30FC; Ch5:#$30C9),   // SQUARE ESUKUUDO
    (Unicode:#$3308; Attr:daSquare; Ch1:#$30A8; Ch2:#$30FC; Ch3:#$30AB; Ch4:#$30FC; Ch5:#$FFFF),   // SQUARE EEKAA
    (Unicode:#$3309; Attr:daSquare; Ch1:#$30AA; Ch2:#$30F3; Ch3:#$30B9; Ch4:#$FFFF),               // SQUARE ONSU
    (Unicode:#$330A; Attr:daSquare; Ch1:#$30AA; Ch2:#$30FC; Ch3:#$30E0; Ch4:#$FFFF),               // SQUARE OOMU
    (Unicode:#$330B; Attr:daSquare; Ch1:#$30AB; Ch2:#$30A4; Ch3:#$30EA; Ch4:#$FFFF),               // SQUARE KAIRI
    (Unicode:#$330C; Attr:daSquare; Ch1:#$30AB; Ch2:#$30E9; Ch3:#$30C3; Ch4:#$30C8; Ch5:#$FFFF),   // SQUARE KARATTO
    (Unicode:#$330D; Attr:daSquare; Ch1:#$30AB; Ch2:#$30ED; Ch3:#$30EA; Ch4:#$30FC; Ch5:#$FFFF),   // SQUARE KARORII
    (Unicode:#$330E; Attr:daSquare; Ch1:#$30AC; Ch2:#$30ED; Ch3:#$30F3; Ch4:#$FFFF),               // SQUARE GARON
    (Unicode:#$330F; Attr:daSquare; Ch1:#$30AC; Ch2:#$30F3; Ch3:#$30DE; Ch4:#$FFFF),               // SQUARE GANMA
    (Unicode:#$3310; Attr:daSquare; Ch1:#$30AE; Ch2:#$30AC; Ch3:#$FFFF),                           // SQUARE GIGA
    (Unicode:#$3311; Attr:daSquare; Ch1:#$30AE; Ch2:#$30CB; Ch3:#$30FC; Ch4:#$FFFF),               // SQUARE GINII
    (Unicode:#$3312; Attr:daSquare; Ch1:#$30AD; Ch2:#$30E5; Ch3:#$30EA; Ch4:#$30FC; Ch5:#$FFFF),   // SQUARE KYURII
    (Unicode:#$3313; Attr:daSquare; Ch1:#$30AE; Ch2:#$30EB; Ch3:#$30C0; Ch4:#$30FC; Ch5:#$FFFF),   // SQUARE GIRUDAA
    (Unicode:#$3314; Attr:daSquare; Ch1:#$30AD; Ch2:#$30ED; Ch3:#$FFFF),                           // SQUARE KIRO
    (Unicode:#$3315; Attr:daSquare; Ch1:#$30AD; Ch2:#$30ED; Ch3:#$30B0; Ch4:#$30E9; Ch5:#$30E0),   // SQUARE KIROGURAMU
    (Unicode:#$3317; Attr:daSquare; Ch1:#$30AD; Ch2:#$30ED; Ch3:#$30EF; Ch4:#$30C3; Ch5:#$30C8),   // SQUARE KIROWATTO
    (Unicode:#$3318; Attr:daSquare; Ch1:#$30B0; Ch2:#$30E9; Ch3:#$30E0; Ch4:#$FFFF),               // SQUARE GURAMU
    (Unicode:#$3319; Attr:daSquare; Ch1:#$30B0; Ch2:#$30E9; Ch3:#$30E0; Ch4:#$30C8; Ch5:#$30F3),   // SQUARE GURAMUTON
    (Unicode:#$331A; Attr:daSquare; Ch1:#$30AF; Ch2:#$30EB; Ch3:#$30BC; Ch4:#$30A4; Ch5:#$30ED),   // SQUARE KURUZEIRO
    (Unicode:#$331B; Attr:daSquare; Ch1:#$30AF; Ch2:#$30ED; Ch3:#$30FC; Ch4:#$30CD; Ch5:#$FFFF),   // SQUARE KUROONE
    (Unicode:#$331C; Attr:daSquare; Ch1:#$30B1; Ch2:#$30FC; Ch3:#$30B9; Ch4:#$FFFF),               // SQUARE KEESU
    (Unicode:#$331D; Attr:daSquare; Ch1:#$30B3; Ch2:#$30EB; Ch3:#$30CA; Ch4:#$FFFF),               // SQUARE KORUNA
    (Unicode:#$331E; Attr:daSquare; Ch1:#$30B3; Ch2:#$30FC; Ch3:#$30DD; Ch4:#$FFFF),               // SQUARE KOOPO
    (Unicode:#$331F; Attr:daSquare; Ch1:#$30B5; Ch2:#$30A4; Ch3:#$30AF; Ch4:#$30EB; Ch5:#$FFFF),   // SQUARE SAIKURU
    (Unicode:#$3320; Attr:daSquare; Ch1:#$30B5; Ch2:#$30F3; Ch3:#$30C1; Ch4:#$30FC; Ch5:#$30E0),   // SQUARE SANTIIMU
    (Unicode:#$3321; Attr:daSquare; Ch1:#$30B7; Ch2:#$30EA; Ch3:#$30F3; Ch4:#$30B0; Ch5:#$FFFF),   // SQUARE SIRINGU
    (Unicode:#$3322; Attr:daSquare; Ch1:#$30BB; Ch2:#$30F3; Ch3:#$30C1; Ch4:#$FFFF),               // SQUARE SENTI
    (Unicode:#$3323; Attr:daSquare; Ch1:#$30BB; Ch2:#$30F3; Ch3:#$30C8; Ch4:#$FFFF),               // SQUARE SENTO
    (Unicode:#$3324; Attr:daSquare; Ch1:#$30C0; Ch2:#$30FC; Ch3:#$30B9; Ch4:#$FFFF),               // SQUARE DAASU
    (Unicode:#$3325; Attr:daSquare; Ch1:#$30C7; Ch2:#$30B7; Ch3:#$FFFF),                           // SQUARE DESI
    (Unicode:#$3326; Attr:daSquare; Ch1:#$30C9; Ch2:#$30EB; Ch3:#$FFFF),                           // SQUARE DORU
    (Unicode:#$3327; Attr:daSquare; Ch1:#$30C8; Ch2:#$30F3; Ch3:#$FFFF),                           // SQUARE TON
    (Unicode:#$3328; Attr:daSquare; Ch1:#$30CA; Ch2:#$30CE; Ch3:#$FFFF),                           // SQUARE NANO
    (Unicode:#$3329; Attr:daSquare; Ch1:#$30CE; Ch2:#$30C3; Ch3:#$30C8; Ch4:#$FFFF),               // SQUARE NOTTO
    (Unicode:#$332A; Attr:daSquare; Ch1:#$30CF; Ch2:#$30A4; Ch3:#$30C4; Ch4:#$FFFF),               // SQUARE HAITU
    (Unicode:#$332B; Attr:daSquare; Ch1:#$30D1; Ch2:#$30FC; Ch3:#$30BB; Ch4:#$30F3; Ch5:#$30C8),   // SQUARE PAASENTO
    (Unicode:#$332C; Attr:daSquare; Ch1:#$30D1; Ch2:#$30FC; Ch3:#$30C4; Ch4:#$FFFF),               // SQUARE PAATU
    (Unicode:#$332D; Attr:daSquare; Ch1:#$30D0; Ch2:#$30FC; Ch3:#$30EC; Ch4:#$30EB; Ch5:#$FFFF),   // SQUARE BAARERU
    (Unicode:#$332E; Attr:daSquare; Ch1:#$30D4; Ch2:#$30A2; Ch3:#$30B9; Ch4:#$30C8; Ch5:#$30EB),   // SQUARE PIASUTORU
    (Unicode:#$332F; Attr:daSquare; Ch1:#$30D4; Ch2:#$30AF; Ch3:#$30EB; Ch4:#$FFFF),               // SQUARE PIKURU
    (Unicode:#$3330; Attr:daSquare; Ch1:#$30D4; Ch2:#$30B3; Ch3:#$FFFF),                           // SQUARE PIKO
    (Unicode:#$3331; Attr:daSquare; Ch1:#$30D3; Ch2:#$30EB; Ch3:#$FFFF),                           // SQUARE BIRU
    (Unicode:#$3332; Attr:daSquare; Ch1:#$30D5; Ch2:#$30A1; Ch3:#$30E9; Ch4:#$30C3; Ch5:#$30C9),   // SQUARE HUARADDO
    (Unicode:#$3333; Attr:daSquare; Ch1:#$30D5; Ch2:#$30A3; Ch3:#$30FC; Ch4:#$30C8; Ch5:#$FFFF),   // SQUARE HUIITO
    (Unicode:#$3334; Attr:daSquare; Ch1:#$30D6; Ch2:#$30C3; Ch3:#$30B7; Ch4:#$30A7; Ch5:#$30EB),   // SQUARE BUSSYERU
    (Unicode:#$3335; Attr:daSquare; Ch1:#$30D5; Ch2:#$30E9; Ch3:#$30F3; Ch4:#$FFFF),               // SQUARE HURAN
    (Unicode:#$3336; Attr:daSquare; Ch1:#$30D8; Ch2:#$30AF; Ch3:#$30BF; Ch4:#$30FC; Ch5:#$30EB),   // SQUARE HEKUTAARU
    (Unicode:#$3337; Attr:daSquare; Ch1:#$30DA; Ch2:#$30BD; Ch3:#$FFFF),                           // SQUARE PESO
    (Unicode:#$3338; Attr:daSquare; Ch1:#$30DA; Ch2:#$30CB; Ch3:#$30D2; Ch4:#$FFFF),               // SQUARE PENIHI
    (Unicode:#$3339; Attr:daSquare; Ch1:#$30D8; Ch2:#$30EB; Ch3:#$30C4; Ch4:#$FFFF),               // SQUARE HERUTU
    (Unicode:#$333A; Attr:daSquare; Ch1:#$30DA; Ch2:#$30F3; Ch3:#$30B9; Ch4:#$FFFF),               // SQUARE PENSU
    (Unicode:#$333B; Attr:daSquare; Ch1:#$30DA; Ch2:#$30FC; Ch3:#$30B8; Ch4:#$FFFF),               // SQUARE PEEZI
    (Unicode:#$333C; Attr:daSquare; Ch1:#$30D9; Ch2:#$30FC; Ch3:#$30BF; Ch4:#$FFFF),               // SQUARE BEETA
    (Unicode:#$333D; Attr:daSquare; Ch1:#$30DD; Ch2:#$30A4; Ch3:#$30F3; Ch4:#$30C8; Ch5:#$FFFF),   // SQUARE POINTO
    (Unicode:#$333E; Attr:daSquare; Ch1:#$30DC; Ch2:#$30EB; Ch3:#$30C8; Ch4:#$FFFF),               // SQUARE BORUTO
    (Unicode:#$333F; Attr:daSquare; Ch1:#$30DB; Ch2:#$30F3; Ch3:#$FFFF),                           // SQUARE HON
    (Unicode:#$3340; Attr:daSquare; Ch1:#$30DD; Ch2:#$30F3; Ch3:#$30C9; Ch4:#$FFFF),               // SQUARE PONDO
    (Unicode:#$3341; Attr:daSquare; Ch1:#$30DB; Ch2:#$30FC; Ch3:#$30EB; Ch4:#$FFFF),               // SQUARE HOORU
    (Unicode:#$3342; Attr:daSquare; Ch1:#$30DB; Ch2:#$30FC; Ch3:#$30F3; Ch4:#$FFFF),               // SQUARE HOON
    (Unicode:#$3343; Attr:daSquare; Ch1:#$30DE; Ch2:#$30A4; Ch3:#$30AF; Ch4:#$30ED; Ch5:#$FFFF),   // SQUARE MAIKURO
    (Unicode:#$3344; Attr:daSquare; Ch1:#$30DE; Ch2:#$30A4; Ch3:#$30EB; Ch4:#$FFFF),               // SQUARE MAIRU
    (Unicode:#$3345; Attr:daSquare; Ch1:#$30DE; Ch2:#$30C3; Ch3:#$30CF; Ch4:#$FFFF),               // SQUARE MAHHA
    (Unicode:#$3346; Attr:daSquare; Ch1:#$30DE; Ch2:#$30EB; Ch3:#$30AF; Ch4:#$FFFF),               // SQUARE MARUKU
    (Unicode:#$3347; Attr:daSquare; Ch1:#$30DE; Ch2:#$30F3; Ch3:#$30B7; Ch4:#$30E7; Ch5:#$30F3),   // SQUARE MANSYON
    (Unicode:#$3348; Attr:daSquare; Ch1:#$30DF; Ch2:#$30AF; Ch3:#$30ED; Ch4:#$30F3; Ch5:#$FFFF),   // SQUARE MIKURON
    (Unicode:#$3349; Attr:daSquare; Ch1:#$30DF; Ch2:#$30EA; Ch3:#$FFFF),                           // SQUARE MIRI
    (Unicode:#$334A; Attr:daSquare; Ch1:#$30DF; Ch2:#$30EA; Ch3:#$30D0; Ch4:#$30FC; Ch5:#$30EB),   // SQUARE MIRIBAARU
    (Unicode:#$334B; Attr:daSquare; Ch1:#$30E1; Ch2:#$30AC; Ch3:#$FFFF),                           // SQUARE MEGA
    (Unicode:#$334C; Attr:daSquare; Ch1:#$30E1; Ch2:#$30AC; Ch3:#$30C8; Ch4:#$30F3; Ch5:#$FFFF),   // SQUARE MEGATON
    (Unicode:#$334D; Attr:daSquare; Ch1:#$30E1; Ch2:#$30FC; Ch3:#$30C8; Ch4:#$30EB; Ch5:#$FFFF),   // SQUARE MEETORU
    (Unicode:#$334E; Attr:daSquare; Ch1:#$30E4; Ch2:#$30FC; Ch3:#$30C9; Ch4:#$FFFF),               // SQUARE YAADO
    (Unicode:#$334F; Attr:daSquare; Ch1:#$30E4; Ch2:#$30FC; Ch3:#$30EB; Ch4:#$FFFF),               // SQUARE YAARU
    (Unicode:#$3350; Attr:daSquare; Ch1:#$30E6; Ch2:#$30A2; Ch3:#$30F3; Ch4:#$FFFF),               // SQUARE YUAN
    (Unicode:#$3351; Attr:daSquare; Ch1:#$30EA; Ch2:#$30C3; Ch3:#$30C8; Ch4:#$30EB; Ch5:#$FFFF),   // SQUARE RITTORU
    (Unicode:#$3352; Attr:daSquare; Ch1:#$30EA; Ch2:#$30E9; Ch3:#$FFFF),                           // SQUARE RIRA
    (Unicode:#$3353; Attr:daSquare; Ch1:#$30EB; Ch2:#$30D4; Ch3:#$30FC; Ch4:#$FFFF),               // SQUARE RUPII
    (Unicode:#$3354; Attr:daSquare; Ch1:#$30EB; Ch2:#$30FC; Ch3:#$30D6; Ch4:#$30EB; Ch5:#$FFFF),   // SQUARE RUUBURU
    (Unicode:#$3355; Attr:daSquare; Ch1:#$30EC; Ch2:#$30E0; Ch3:#$FFFF),                           // SQUARE REMU
    (Unicode:#$3356; Attr:daSquare; Ch1:#$30EC; Ch2:#$30F3; Ch3:#$30C8; Ch4:#$30B2; Ch5:#$30F3),   // SQUARE RENTOGEN
    (Unicode:#$3357; Attr:daSquare; Ch1:#$30EF; Ch2:#$30C3; Ch3:#$30C8; Ch4:#$FFFF),               // SQUARE WATTO
    (Unicode:#$3358; Attr:daCompat; Ch1:#$0030; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ZERO
    (Unicode:#$3359; Attr:daCompat; Ch1:#$0031; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ONE
    (Unicode:#$335A; Attr:daCompat; Ch1:#$0032; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWO
    (Unicode:#$335B; Attr:daCompat; Ch1:#$0033; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR THREE
    (Unicode:#$335C; Attr:daCompat; Ch1:#$0034; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FOUR
    (Unicode:#$335D; Attr:daCompat; Ch1:#$0035; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FIVE
    (Unicode:#$335E; Attr:daCompat; Ch1:#$0036; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SIX
    (Unicode:#$335F; Attr:daCompat; Ch1:#$0037; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SEVEN
    (Unicode:#$3360; Attr:daCompat; Ch1:#$0038; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR EIGHT
    (Unicode:#$3361; Attr:daCompat; Ch1:#$0039; Ch2:#$70B9; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR NINE
    (Unicode:#$3362; Attr:daCompat; Ch1:#$0031; Ch2:#$0030; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TEN
    (Unicode:#$3363; Attr:daCompat; Ch1:#$0031; Ch2:#$0031; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR ELEVEN
    (Unicode:#$3364; Attr:daCompat; Ch1:#$0031; Ch2:#$0032; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWELVE
    (Unicode:#$3365; Attr:daCompat; Ch1:#$0031; Ch2:#$0033; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR THIRTEEN
    (Unicode:#$3366; Attr:daCompat; Ch1:#$0031; Ch2:#$0034; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FOURTEEN
    (Unicode:#$3367; Attr:daCompat; Ch1:#$0031; Ch2:#$0035; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR FIFTEEN
    (Unicode:#$3368; Attr:daCompat; Ch1:#$0031; Ch2:#$0036; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SIXTEEN
    (Unicode:#$3369; Attr:daCompat; Ch1:#$0031; Ch2:#$0037; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR SEVENTEEN
    (Unicode:#$336A; Attr:daCompat; Ch1:#$0031; Ch2:#$0038; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR EIGHTEEN
    (Unicode:#$336B; Attr:daCompat; Ch1:#$0031; Ch2:#$0039; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR NINETEEN
    (Unicode:#$336C; Attr:daCompat; Ch1:#$0032; Ch2:#$0030; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY
    (Unicode:#$336D; Attr:daCompat; Ch1:#$0032; Ch2:#$0031; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-ONE
    (Unicode:#$336E; Attr:daCompat; Ch1:#$0032; Ch2:#$0032; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-TWO
    (Unicode:#$336F; Attr:daCompat; Ch1:#$0032; Ch2:#$0033; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-THREE
    (Unicode:#$3370; Attr:daCompat; Ch1:#$0032; Ch2:#$0034; Ch3:#$70B9; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR HOUR TWENTY-FOUR
    (Unicode:#$3371; Attr:daSquare; Ch1:#$0068; Ch2:#$0050; Ch3:#$0061; Ch4:#$FFFF),   // SQUARE HPA
    (Unicode:#$3372; Attr:daSquare; Ch1:#$0064; Ch2:#$0061; Ch3:#$FFFF),  // SQUARE DA
    (Unicode:#$3373; Attr:daSquare; Ch1:#$0041; Ch2:#$0055; Ch3:#$FFFF),  // SQUARE AU
    (Unicode:#$3374; Attr:daSquare; Ch1:#$0062; Ch2:#$0061; Ch3:#$0072; Ch4:#$FFFF),   // SQUARE BAR
    (Unicode:#$3375; Attr:daSquare; Ch1:#$006F; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE OV
    (Unicode:#$3376; Attr:daSquare; Ch1:#$0070; Ch2:#$0063; Ch3:#$FFFF),  // SQUARE PC
    (Unicode:#$337B; Attr:daSquare; Ch1:#$5E73; Ch2:#$6210; Ch3:#$FFFF),  // SQUARE ERA NAME HEISEI
    (Unicode:#$337C; Attr:daSquare; Ch1:#$662D; Ch2:#$548C; Ch3:#$FFFF),  // SQUARE ERA NAME SYOUWA
    (Unicode:#$337D; Attr:daSquare; Ch1:#$5927; Ch2:#$6B63; Ch3:#$FFFF),  // SQUARE ERA NAME TAISYOU
    (Unicode:#$337E; Attr:daSquare; Ch1:#$660E; Ch2:#$6CBB; Ch3:#$FFFF),  // SQUARE ERA NAME MEIZI
    (Unicode:#$337F; Attr:daSquare; Ch1:#$682A; Ch2:#$5F0F; Ch3:#$4F1A; Ch4:#$793E; Ch5:#$FFFF),  // SQUARE CORPORATION
    (Unicode:#$3380; Attr:daSquare; Ch1:#$0070; Ch2:#$0041; Ch3:#$FFFF),  // SQUARE PA AMPS
    (Unicode:#$3381; Attr:daSquare; Ch1:#$006E; Ch2:#$0041; Ch3:#$FFFF),  // SQUARE NA
    (Unicode:#$3382; Attr:daSquare; Ch1:#$03BC; Ch2:#$0041; Ch3:#$FFFF),  // SQUARE MU A
    (Unicode:#$3383; Attr:daSquare; Ch1:#$006D; Ch2:#$0041; Ch3:#$FFFF),  // SQUARE MA
    (Unicode:#$3384; Attr:daSquare; Ch1:#$006B; Ch2:#$0041; Ch3:#$FFFF),  // SQUARE KA
    (Unicode:#$3385; Attr:daSquare; Ch1:#$004B; Ch2:#$0042; Ch3:#$FFFF),  // SQUARE KB
    (Unicode:#$3386; Attr:daSquare; Ch1:#$004D; Ch2:#$0042; Ch3:#$FFFF),  // SQUARE MB
    (Unicode:#$3387; Attr:daSquare; Ch1:#$0047; Ch2:#$0042; Ch3:#$FFFF),  // SQUARE GB
    (Unicode:#$3388; Attr:daSquare; Ch1:#$0063; Ch2:#$0061; Ch3:#$006C; Ch4:#$FFFF),   // SQUARE CAL
    (Unicode:#$3389; Attr:daSquare; Ch1:#$006B; Ch2:#$0063; Ch3:#$0061; Ch4:#$006C; Ch5:#$FFFF),  // SQUARE KCAL
    (Unicode:#$338A; Attr:daSquare; Ch1:#$0070; Ch2:#$0046; Ch3:#$FFFF),  // SQUARE PF
    (Unicode:#$338B; Attr:daSquare; Ch1:#$006E; Ch2:#$0046; Ch3:#$FFFF),  // SQUARE NF
    (Unicode:#$338C; Attr:daSquare; Ch1:#$03BC; Ch2:#$0046; Ch3:#$FFFF),  // SQUARE MU F
    (Unicode:#$338D; Attr:daSquare; Ch1:#$03BC; Ch2:#$0067; Ch3:#$FFFF),  // SQUARE MU G
    (Unicode:#$338E; Attr:daSquare; Ch1:#$006D; Ch2:#$0067; Ch3:#$FFFF),  // SQUARE MG
    (Unicode:#$338F; Attr:daSquare; Ch1:#$006B; Ch2:#$0067; Ch3:#$FFFF),  // SQUARE KG
    (Unicode:#$3390; Attr:daSquare; Ch1:#$0048; Ch2:#$007A; Ch3:#$FFFF),  // SQUARE HZ
    (Unicode:#$3391; Attr:daSquare; Ch1:#$006B; Ch2:#$0048; Ch3:#$007A; Ch4:#$FFFF),   // SQUARE KHZ
    (Unicode:#$3392; Attr:daSquare; Ch1:#$004D; Ch2:#$0048; Ch3:#$007A; Ch4:#$FFFF),   // SQUARE MHZ
    (Unicode:#$3393; Attr:daSquare; Ch1:#$0047; Ch2:#$0048; Ch3:#$007A; Ch4:#$FFFF),   // SQUARE GHZ
    (Unicode:#$3394; Attr:daSquare; Ch1:#$0054; Ch2:#$0048; Ch3:#$007A; Ch4:#$FFFF),   // SQUARE THZ
    (Unicode:#$3395; Attr:daSquare; Ch1:#$03BC; Ch2:#$2113; Ch3:#$FFFF),  // SQUARE MU L
    (Unicode:#$3396; Attr:daSquare; Ch1:#$006D; Ch2:#$2113; Ch3:#$FFFF),  // SQUARE ML
    (Unicode:#$3397; Attr:daSquare; Ch1:#$0064; Ch2:#$2113; Ch3:#$FFFF),  // SQUARE DL
    (Unicode:#$3398; Attr:daSquare; Ch1:#$006B; Ch2:#$2113; Ch3:#$FFFF),  // SQUARE KL
    (Unicode:#$3399; Attr:daSquare; Ch1:#$0066; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE FM
    (Unicode:#$339A; Attr:daSquare; Ch1:#$006E; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE NM
    (Unicode:#$339B; Attr:daSquare; Ch1:#$03BC; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE MU M
    (Unicode:#$339C; Attr:daSquare; Ch1:#$006D; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE MM
    (Unicode:#$339D; Attr:daSquare; Ch1:#$0063; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE CM
    (Unicode:#$339E; Attr:daSquare; Ch1:#$006B; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE KM
    (Unicode:#$339F; Attr:daSquare; Ch1:#$006D; Ch2:#$006D; Ch3:#$00B2; Ch4:#$FFFF),   // SQUARE MM SQUARED
    (Unicode:#$33A0; Attr:daSquare; Ch1:#$0063; Ch2:#$006D; Ch3:#$00B2; Ch4:#$FFFF),   // SQUARE CM SQUARED
    (Unicode:#$33A1; Attr:daSquare; Ch1:#$006D; Ch2:#$00B2; Ch3:#$FFFF),  // SQUARE M SQUARED
    (Unicode:#$33A2; Attr:daSquare; Ch1:#$006B; Ch2:#$006D; Ch3:#$00B2; Ch4:#$FFFF),   // SQUARE KM SQUARED
    (Unicode:#$33A3; Attr:daSquare; Ch1:#$006D; Ch2:#$006D; Ch3:#$00B3; Ch4:#$FFFF),   // SQUARE MM CUBED
    (Unicode:#$33A4; Attr:daSquare; Ch1:#$0063; Ch2:#$006D; Ch3:#$00B3; Ch4:#$FFFF),   // SQUARE CM CUBED
    (Unicode:#$33A5; Attr:daSquare; Ch1:#$006D; Ch2:#$00B3; Ch3:#$FFFF),  // SQUARE M CUBED
    (Unicode:#$33A6; Attr:daSquare; Ch1:#$006B; Ch2:#$006D; Ch3:#$00B3; Ch4:#$FFFF),   // SQUARE KM CUBED
    (Unicode:#$33A7; Attr:daSquare; Ch1:#$006D; Ch2:#$2215; Ch3:#$0073; Ch4:#$FFFF),   // SQUARE M OVER S
    (Unicode:#$33A8; Attr:daSquare; Ch1:#$006D; Ch2:#$2215; Ch3:#$0073; Ch4:#$00B2; Ch5:#$FFFF),  // SQUARE M OVER S SQUARED
    (Unicode:#$33A9; Attr:daSquare; Ch1:#$0050; Ch2:#$0061; Ch3:#$FFFF),  // SQUARE PA
    (Unicode:#$33AA; Attr:daSquare; Ch1:#$006B; Ch2:#$0050; Ch3:#$0061; Ch4:#$FFFF),   // SQUARE KPA
    (Unicode:#$33AB; Attr:daSquare; Ch1:#$004D; Ch2:#$0050; Ch3:#$0061; Ch4:#$FFFF),   // SQUARE MPA
    (Unicode:#$33AC; Attr:daSquare; Ch1:#$0047; Ch2:#$0050; Ch3:#$0061; Ch4:#$FFFF),   // SQUARE GPA
    (Unicode:#$33AD; Attr:daSquare; Ch1:#$0072; Ch2:#$0061; Ch3:#$0064; Ch4:#$FFFF),   // SQUARE RAD
    (Unicode:#$33AE; Attr:daSquare; Ch1:#$0072; Ch2:#$0061; Ch3:#$0064; Ch4:#$2215; Ch5:#$0073),   // SQUARE RAD OVER S
    (Unicode:#$33B0; Attr:daSquare; Ch1:#$0070; Ch2:#$0073; Ch3:#$FFFF),  // SQUARE PS
    (Unicode:#$33B1; Attr:daSquare; Ch1:#$006E; Ch2:#$0073; Ch3:#$FFFF),  // SQUARE NS
    (Unicode:#$33B2; Attr:daSquare; Ch1:#$03BC; Ch2:#$0073; Ch3:#$FFFF),  // SQUARE MU S
    (Unicode:#$33B3; Attr:daSquare; Ch1:#$006D; Ch2:#$0073; Ch3:#$FFFF),  // SQUARE MS
    (Unicode:#$33B4; Attr:daSquare; Ch1:#$0070; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE PV
    (Unicode:#$33B5; Attr:daSquare; Ch1:#$006E; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE NV
    (Unicode:#$33B6; Attr:daSquare; Ch1:#$03BC; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE MU V
    (Unicode:#$33B7; Attr:daSquare; Ch1:#$006D; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE MV
    (Unicode:#$33B8; Attr:daSquare; Ch1:#$006B; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE KV
    (Unicode:#$33B9; Attr:daSquare; Ch1:#$004D; Ch2:#$0056; Ch3:#$FFFF),  // SQUARE MV MEGA
    (Unicode:#$33BA; Attr:daSquare; Ch1:#$0070; Ch2:#$0057; Ch3:#$FFFF),  // SQUARE PW
    (Unicode:#$33BB; Attr:daSquare; Ch1:#$006E; Ch2:#$0057; Ch3:#$FFFF),  // SQUARE NW
    (Unicode:#$33BC; Attr:daSquare; Ch1:#$03BC; Ch2:#$0057; Ch3:#$FFFF),  // SQUARE MU W
    (Unicode:#$33BD; Attr:daSquare; Ch1:#$006D; Ch2:#$0057; Ch3:#$FFFF),  // SQUARE MW
    (Unicode:#$33BE; Attr:daSquare; Ch1:#$006B; Ch2:#$0057; Ch3:#$FFFF),  // SQUARE KW
    (Unicode:#$33BF; Attr:daSquare; Ch1:#$004D; Ch2:#$0057; Ch3:#$FFFF),  // SQUARE MW MEGA
    (Unicode:#$33C0; Attr:daSquare; Ch1:#$006B; Ch2:#$03A9; Ch3:#$FFFF),  // SQUARE K OHM
    (Unicode:#$33C1; Attr:daSquare; Ch1:#$004D; Ch2:#$03A9; Ch3:#$FFFF),  // SQUARE M OHM
    (Unicode:#$33C2; Attr:daSquare; Ch1:#$0061; Ch2:#$002E; Ch3:#$006D; Ch4:#$002E; Ch5:#$FFFF),  // SQUARE AM
    (Unicode:#$33C3; Attr:daSquare; Ch1:#$0042; Ch2:#$0071; Ch3:#$FFFF),  // SQUARE BQ
    (Unicode:#$33C4; Attr:daSquare; Ch1:#$0063; Ch2:#$0063; Ch3:#$FFFF),  // SQUARE CC
    (Unicode:#$33C5; Attr:daSquare; Ch1:#$0063; Ch2:#$0064; Ch3:#$FFFF),  // SQUARE CD
    (Unicode:#$33C6; Attr:daSquare; Ch1:#$0043; Ch2:#$2215; Ch3:#$006B; Ch4:#$0067; Ch5:#$FFFF),  // SQUARE C OVER KG
    (Unicode:#$33C7; Attr:daSquare; Ch1:#$0043; Ch2:#$006F; Ch3:#$002E; Ch4:#$FFFF),   // SQUARE CO
    (Unicode:#$33C8; Attr:daSquare; Ch1:#$0064; Ch2:#$0042; Ch3:#$FFFF),  // SQUARE DB
    (Unicode:#$33C9; Attr:daSquare; Ch1:#$0047; Ch2:#$0079; Ch3:#$FFFF),  // SQUARE GY
    (Unicode:#$33CA; Attr:daSquare; Ch1:#$0068; Ch2:#$0061; Ch3:#$FFFF),  // SQUARE HA
    (Unicode:#$33CB; Attr:daSquare; Ch1:#$0048; Ch2:#$0050; Ch3:#$FFFF),  // SQUARE HP
    (Unicode:#$33CC; Attr:daSquare; Ch1:#$0069; Ch2:#$006E; Ch3:#$FFFF),  // SQUARE IN
    (Unicode:#$33CD; Attr:daSquare; Ch1:#$004B; Ch2:#$004B; Ch3:#$FFFF),  // SQUARE KK
    (Unicode:#$33CE; Attr:daSquare; Ch1:#$004B; Ch2:#$004D; Ch3:#$FFFF),  // SQUARE KM CAPITAL
    (Unicode:#$33CF; Attr:daSquare; Ch1:#$006B; Ch2:#$0074; Ch3:#$FFFF),  // SQUARE KT
    (Unicode:#$33D0; Attr:daSquare; Ch1:#$006C; Ch2:#$006D; Ch3:#$FFFF),  // SQUARE LM
    (Unicode:#$33D1; Attr:daSquare; Ch1:#$006C; Ch2:#$006E; Ch3:#$FFFF),  // SQUARE LN
    (Unicode:#$33D2; Attr:daSquare; Ch1:#$006C; Ch2:#$006F; Ch3:#$0067; Ch4:#$FFFF),   // SQUARE LOG
    (Unicode:#$33D3; Attr:daSquare; Ch1:#$006C; Ch2:#$0078; Ch3:#$FFFF),  // SQUARE LX
    (Unicode:#$33D4; Attr:daSquare; Ch1:#$006D; Ch2:#$0062; Ch3:#$FFFF),  // SQUARE MB SMALL
    (Unicode:#$33D5; Attr:daSquare; Ch1:#$006D; Ch2:#$0069; Ch3:#$006C; Ch4:#$FFFF),   // SQUARE MIL
    (Unicode:#$33D6; Attr:daSquare; Ch1:#$006D; Ch2:#$006F; Ch3:#$006C; Ch4:#$FFFF),   // SQUARE MOL
    (Unicode:#$33D7; Attr:daSquare; Ch1:#$0050; Ch2:#$0048; Ch3:#$FFFF),  // SQUARE PH
    (Unicode:#$33D8; Attr:daSquare; Ch1:#$0070; Ch2:#$002E; Ch3:#$006D; Ch4:#$002E; Ch5:#$FFFF),  // SQUARE PM
    (Unicode:#$33D9; Attr:daSquare; Ch1:#$0050; Ch2:#$0050; Ch3:#$004D; Ch4:#$FFFF),   // SQUARE PPM
    (Unicode:#$33DA; Attr:daSquare; Ch1:#$0050; Ch2:#$0052; Ch3:#$FFFF),  // SQUARE PR
    (Unicode:#$33DB; Attr:daSquare; Ch1:#$0073; Ch2:#$0072; Ch3:#$FFFF),  // SQUARE SR
    (Unicode:#$33DC; Attr:daSquare; Ch1:#$0053; Ch2:#$0076; Ch3:#$FFFF),  // SQUARE SV
    (Unicode:#$33DD; Attr:daSquare; Ch1:#$0057; Ch2:#$0062; Ch3:#$FFFF),  // SQUARE WB
    (Unicode:#$33E0; Attr:daCompat; Ch1:#$0031; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ONE
    (Unicode:#$33E1; Attr:daCompat; Ch1:#$0032; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWO
    (Unicode:#$33E2; Attr:daCompat; Ch1:#$0033; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THREE
    (Unicode:#$33E3; Attr:daCompat; Ch1:#$0034; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FOUR
    (Unicode:#$33E4; Attr:daCompat; Ch1:#$0035; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FIVE
    (Unicode:#$33E5; Attr:daCompat; Ch1:#$0036; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SIX
    (Unicode:#$33E6; Attr:daCompat; Ch1:#$0037; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SEVEN
    (Unicode:#$33E7; Attr:daCompat; Ch1:#$0038; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY EIGHT
    (Unicode:#$33E8; Attr:daCompat; Ch1:#$0039; Ch2:#$65E5; Ch3:#$FFFF),  // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY NINE
    (Unicode:#$33E9; Attr:daCompat; Ch1:#$0031; Ch2:#$0030; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TEN
    (Unicode:#$33EA; Attr:daCompat; Ch1:#$0031; Ch2:#$0031; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY ELEVEN
    (Unicode:#$33EB; Attr:daCompat; Ch1:#$0031; Ch2:#$0032; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWELVE
    (Unicode:#$33EC; Attr:daCompat; Ch1:#$0031; Ch2:#$0033; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTEEN
    (Unicode:#$33ED; Attr:daCompat; Ch1:#$0031; Ch2:#$0034; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FOURTEEN
    (Unicode:#$33EE; Attr:daCompat; Ch1:#$0031; Ch2:#$0035; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY FIFTEEN
    (Unicode:#$33EF; Attr:daCompat; Ch1:#$0031; Ch2:#$0036; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SIXTEEN
    (Unicode:#$33F0; Attr:daCompat; Ch1:#$0031; Ch2:#$0037; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY SEVENTEEN
    (Unicode:#$33F1; Attr:daCompat; Ch1:#$0031; Ch2:#$0038; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY EIGHTEEN
    (Unicode:#$33F2; Attr:daCompat; Ch1:#$0031; Ch2:#$0039; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY NINETEEN
    (Unicode:#$33F3; Attr:daCompat; Ch1:#$0032; Ch2:#$0030; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY
    (Unicode:#$33F4; Attr:daCompat; Ch1:#$0032; Ch2:#$0031; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-ONE
    (Unicode:#$33F5; Attr:daCompat; Ch1:#$0032; Ch2:#$0032; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-TWO
    (Unicode:#$33F6; Attr:daCompat; Ch1:#$0032; Ch2:#$0033; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-THREE
    (Unicode:#$33F7; Attr:daCompat; Ch1:#$0032; Ch2:#$0034; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-FOUR
    (Unicode:#$33F8; Attr:daCompat; Ch1:#$0032; Ch2:#$0035; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-FIVE
    (Unicode:#$33F9; Attr:daCompat; Ch1:#$0032; Ch2:#$0036; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-SIX
    (Unicode:#$33FA; Attr:daCompat; Ch1:#$0032; Ch2:#$0037; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-SEVEN
    (Unicode:#$33FB; Attr:daCompat; Ch1:#$0032; Ch2:#$0038; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-EIGHT
    (Unicode:#$33FC; Attr:daCompat; Ch1:#$0032; Ch2:#$0039; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY TWENTY-NINE
    (Unicode:#$33FD; Attr:daCompat; Ch1:#$0033; Ch2:#$0030; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY
    (Unicode:#$33FE; Attr:daCompat; Ch1:#$0033; Ch2:#$0031; Ch3:#$65E5; Ch4:#$FFFF),   // IDEOGRAPHIC TELEGRAPH SYMBOL FOR DAY THIRTY-ONE
    (Unicode:#$F900; Attr:daNone; Ch1:#$8C48; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F900
    (Unicode:#$F901; Attr:daNone; Ch1:#$66F4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F901
    (Unicode:#$F902; Attr:daNone; Ch1:#$8ECA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F902
    (Unicode:#$F903; Attr:daNone; Ch1:#$8CC8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F903
    (Unicode:#$F904; Attr:daNone; Ch1:#$6ED1; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F904
    (Unicode:#$F905; Attr:daNone; Ch1:#$4E32; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F905
    (Unicode:#$F906; Attr:daNone; Ch1:#$53E5; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F906
    (Unicode:#$F907; Attr:daNone; Ch1:#$9F9C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F907
    (Unicode:#$F908; Attr:daNone; Ch1:#$9F9C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F908
    (Unicode:#$F909; Attr:daNone; Ch1:#$5951; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F909
    (Unicode:#$F90A; Attr:daNone; Ch1:#$91D1; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F90A
    (Unicode:#$F90B; Attr:daNone; Ch1:#$5587; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F90B
    (Unicode:#$F90C; Attr:daNone; Ch1:#$5948; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F90C
    (Unicode:#$F90D; Attr:daNone; Ch1:#$61F6; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F90D
    (Unicode:#$F90E; Attr:daNone; Ch1:#$7669; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F90E
    (Unicode:#$F90F; Attr:daNone; Ch1:#$7F85; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F90F
    (Unicode:#$F910; Attr:daNone; Ch1:#$863F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F910
    (Unicode:#$F911; Attr:daNone; Ch1:#$87BA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F911
    (Unicode:#$F912; Attr:daNone; Ch1:#$88F8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F912
    (Unicode:#$F913; Attr:daNone; Ch1:#$908F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F913
    (Unicode:#$F914; Attr:daNone; Ch1:#$6A02; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F914
    (Unicode:#$F915; Attr:daNone; Ch1:#$6D1B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F915
    (Unicode:#$F916; Attr:daNone; Ch1:#$70D9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F916
    (Unicode:#$F917; Attr:daNone; Ch1:#$73DE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F917
    (Unicode:#$F918; Attr:daNone; Ch1:#$843D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F918
    (Unicode:#$F919; Attr:daNone; Ch1:#$916A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F919
    (Unicode:#$F91A; Attr:daNone; Ch1:#$99F1; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F91A
    (Unicode:#$F91B; Attr:daNone; Ch1:#$4E82; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F91B
    (Unicode:#$F91C; Attr:daNone; Ch1:#$5375; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F91C
    (Unicode:#$F91D; Attr:daNone; Ch1:#$6B04; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F91D
    (Unicode:#$F91E; Attr:daNone; Ch1:#$721B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F91E
    (Unicode:#$F91F; Attr:daNone; Ch1:#$862D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F91F
    (Unicode:#$F920; Attr:daNone; Ch1:#$9E1E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F920
    (Unicode:#$F921; Attr:daNone; Ch1:#$5D50; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F921
    (Unicode:#$F922; Attr:daNone; Ch1:#$6FEB; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F922
    (Unicode:#$F923; Attr:daNone; Ch1:#$85CD; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F923
    (Unicode:#$F924; Attr:daNone; Ch1:#$8964; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F924
    (Unicode:#$F925; Attr:daNone; Ch1:#$62C9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F925
    (Unicode:#$F926; Attr:daNone; Ch1:#$81D8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F926
    (Unicode:#$F927; Attr:daNone; Ch1:#$881F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F927
    (Unicode:#$F928; Attr:daNone; Ch1:#$5ECA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F928
    (Unicode:#$F929; Attr:daNone; Ch1:#$6717; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F929
    (Unicode:#$F92A; Attr:daNone; Ch1:#$6D6A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F92A
    (Unicode:#$F92B; Attr:daNone; Ch1:#$72FC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F92B
    (Unicode:#$F92C; Attr:daNone; Ch1:#$90CE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F92C
    (Unicode:#$F92D; Attr:daNone; Ch1:#$4F86; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F92D
    (Unicode:#$F92E; Attr:daNone; Ch1:#$51B7; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F92E
    (Unicode:#$F92F; Attr:daNone; Ch1:#$52DE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F92F
    (Unicode:#$F930; Attr:daNone; Ch1:#$64C4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F930
    (Unicode:#$F931; Attr:daNone; Ch1:#$6AD3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F931
    (Unicode:#$F932; Attr:daNone; Ch1:#$7210; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F932
    (Unicode:#$F933; Attr:daNone; Ch1:#$76E7; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F933
    (Unicode:#$F934; Attr:daNone; Ch1:#$8001; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F934
    (Unicode:#$F935; Attr:daNone; Ch1:#$8606; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F935
    (Unicode:#$F936; Attr:daNone; Ch1:#$865C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F936
    (Unicode:#$F937; Attr:daNone; Ch1:#$8DEF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F937
    (Unicode:#$F938; Attr:daNone; Ch1:#$9732; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F938
    (Unicode:#$F939; Attr:daNone; Ch1:#$9B6F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F939
    (Unicode:#$F93A; Attr:daNone; Ch1:#$9DFA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F93A
    (Unicode:#$F93B; Attr:daNone; Ch1:#$788C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F93B
    (Unicode:#$F93C; Attr:daNone; Ch1:#$797F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F93C
    (Unicode:#$F93D; Attr:daNone; Ch1:#$7DA0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F93D
    (Unicode:#$F93E; Attr:daNone; Ch1:#$83C9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F93E
    (Unicode:#$F93F; Attr:daNone; Ch1:#$9304; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F93F
    (Unicode:#$F940; Attr:daNone; Ch1:#$9E7F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F940
    (Unicode:#$F941; Attr:daNone; Ch1:#$8AD6; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F941
    (Unicode:#$F942; Attr:daNone; Ch1:#$58DF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F942
    (Unicode:#$F943; Attr:daNone; Ch1:#$5F04; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F943
    (Unicode:#$F944; Attr:daNone; Ch1:#$7C60; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F944
    (Unicode:#$F945; Attr:daNone; Ch1:#$807E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F945
    (Unicode:#$F946; Attr:daNone; Ch1:#$7262; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F946
    (Unicode:#$F947; Attr:daNone; Ch1:#$78CA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F947
    (Unicode:#$F948; Attr:daNone; Ch1:#$8CC2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F948
    (Unicode:#$F949; Attr:daNone; Ch1:#$96F7; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F949
    (Unicode:#$F94A; Attr:daNone; Ch1:#$58D8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F94A
    (Unicode:#$F94B; Attr:daNone; Ch1:#$5C62; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F94B
    (Unicode:#$F94C; Attr:daNone; Ch1:#$6A13; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F94C
    (Unicode:#$F94D; Attr:daNone; Ch1:#$6DDA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F94D
    (Unicode:#$F94E; Attr:daNone; Ch1:#$6F0F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F94E
    (Unicode:#$F94F; Attr:daNone; Ch1:#$7D2F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F94F
    (Unicode:#$F950; Attr:daNone; Ch1:#$7E37; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F950
    (Unicode:#$F951; Attr:daNone; Ch1:#$96FB; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F951
    (Unicode:#$F952; Attr:daNone; Ch1:#$52D2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F952
    (Unicode:#$F953; Attr:daNone; Ch1:#$808B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F953
    (Unicode:#$F954; Attr:daNone; Ch1:#$51DC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F954
    (Unicode:#$F955; Attr:daNone; Ch1:#$51CC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F955
    (Unicode:#$F956; Attr:daNone; Ch1:#$7A1C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F956
    (Unicode:#$F957; Attr:daNone; Ch1:#$7DBE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F957
    (Unicode:#$F958; Attr:daNone; Ch1:#$83F1; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F958
    (Unicode:#$F959; Attr:daNone; Ch1:#$9675; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F959
    (Unicode:#$F95A; Attr:daNone; Ch1:#$8B80; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F95A
    (Unicode:#$F95B; Attr:daNone; Ch1:#$62CF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F95B
    (Unicode:#$F95C; Attr:daNone; Ch1:#$6A02; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F95C
    (Unicode:#$F95D; Attr:daNone; Ch1:#$8AFE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F95D
    (Unicode:#$F95E; Attr:daNone; Ch1:#$4E39; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F95E
    (Unicode:#$F95F; Attr:daNone; Ch1:#$5BE7; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F95F
    (Unicode:#$F960; Attr:daNone; Ch1:#$6012; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F960
    (Unicode:#$F961; Attr:daNone; Ch1:#$7387; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F961
    (Unicode:#$F962; Attr:daNone; Ch1:#$7570; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F962
    (Unicode:#$F963; Attr:daNone; Ch1:#$5317; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F963
    (Unicode:#$F964; Attr:daNone; Ch1:#$78FB; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F964
    (Unicode:#$F965; Attr:daNone; Ch1:#$4FBF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F965
    (Unicode:#$F966; Attr:daNone; Ch1:#$5FA9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F966
    (Unicode:#$F967; Attr:daNone; Ch1:#$4E0D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F967
    (Unicode:#$F968; Attr:daNone; Ch1:#$6CCC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F968
    (Unicode:#$F969; Attr:daNone; Ch1:#$6578; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F969
    (Unicode:#$F96A; Attr:daNone; Ch1:#$7D22; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F96A
    (Unicode:#$F96B; Attr:daNone; Ch1:#$53C3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F96B
    (Unicode:#$F96C; Attr:daNone; Ch1:#$585E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F96C
    (Unicode:#$F96D; Attr:daNone; Ch1:#$7701; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F96D
    (Unicode:#$F96E; Attr:daNone; Ch1:#$8449; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F96E
    (Unicode:#$F96F; Attr:daNone; Ch1:#$8AAA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F96F
    (Unicode:#$F970; Attr:daNone; Ch1:#$6BBA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F970
    (Unicode:#$F971; Attr:daNone; Ch1:#$8FB0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F971
    (Unicode:#$F972; Attr:daNone; Ch1:#$6C88; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F972
    (Unicode:#$F973; Attr:daNone; Ch1:#$62FE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F973
    (Unicode:#$F974; Attr:daNone; Ch1:#$82E5; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F974
    (Unicode:#$F975; Attr:daNone; Ch1:#$63A0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F975
    (Unicode:#$F976; Attr:daNone; Ch1:#$7565; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F976
    (Unicode:#$F977; Attr:daNone; Ch1:#$4EAE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F977
    (Unicode:#$F978; Attr:daNone; Ch1:#$5169; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F978
    (Unicode:#$F979; Attr:daNone; Ch1:#$51C9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F979
    (Unicode:#$F97A; Attr:daNone; Ch1:#$6881; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F97A
    (Unicode:#$F97B; Attr:daNone; Ch1:#$7CE7; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F97B
    (Unicode:#$F97C; Attr:daNone; Ch1:#$826F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F97C
    (Unicode:#$F97D; Attr:daNone; Ch1:#$8AD2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F97D
    (Unicode:#$F97E; Attr:daNone; Ch1:#$91CF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F97E
    (Unicode:#$F97F; Attr:daNone; Ch1:#$52F5; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F97F
    (Unicode:#$F980; Attr:daNone; Ch1:#$5442; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F980
    (Unicode:#$F981; Attr:daNone; Ch1:#$5973; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F981
    (Unicode:#$F982; Attr:daNone; Ch1:#$5EEC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F982
    (Unicode:#$F983; Attr:daNone; Ch1:#$65C5; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F983
    (Unicode:#$F984; Attr:daNone; Ch1:#$6FFE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F984
    (Unicode:#$F985; Attr:daNone; Ch1:#$792A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F985
    (Unicode:#$F986; Attr:daNone; Ch1:#$95AD; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F986
    (Unicode:#$F987; Attr:daNone; Ch1:#$9A6A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F987
    (Unicode:#$F988; Attr:daNone; Ch1:#$9E97; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F988
    (Unicode:#$F989; Attr:daNone; Ch1:#$9ECE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F989
    (Unicode:#$F98A; Attr:daNone; Ch1:#$529B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F98A
    (Unicode:#$F98B; Attr:daNone; Ch1:#$66C6; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F98B
    (Unicode:#$F98C; Attr:daNone; Ch1:#$6B77; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F98C
    (Unicode:#$F98D; Attr:daNone; Ch1:#$8F62; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F98D
    (Unicode:#$F98E; Attr:daNone; Ch1:#$5E74; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F98E
    (Unicode:#$F98F; Attr:daNone; Ch1:#$6190; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F98F
    (Unicode:#$F990; Attr:daNone; Ch1:#$6200; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F990
    (Unicode:#$F991; Attr:daNone; Ch1:#$649A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F991
    (Unicode:#$F992; Attr:daNone; Ch1:#$6F23; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F992
    (Unicode:#$F993; Attr:daNone; Ch1:#$7149; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F993
    (Unicode:#$F994; Attr:daNone; Ch1:#$7489; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F994
    (Unicode:#$F995; Attr:daNone; Ch1:#$79CA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F995
    (Unicode:#$F996; Attr:daNone; Ch1:#$7DF4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F996
    (Unicode:#$F997; Attr:daNone; Ch1:#$806F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F997
    (Unicode:#$F998; Attr:daNone; Ch1:#$8F26; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F998
    (Unicode:#$F999; Attr:daNone; Ch1:#$84EE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F999
    (Unicode:#$F99A; Attr:daNone; Ch1:#$9023; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F99A
    (Unicode:#$F99B; Attr:daNone; Ch1:#$934A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F99B
    (Unicode:#$F99C; Attr:daNone; Ch1:#$5217; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F99C
    (Unicode:#$F99D; Attr:daNone; Ch1:#$52A3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F99D
    (Unicode:#$F99E; Attr:daNone; Ch1:#$54BD; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F99E
    (Unicode:#$F99F; Attr:daNone; Ch1:#$70C8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F99F
    (Unicode:#$F9A0; Attr:daNone; Ch1:#$88C2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A0
    (Unicode:#$F9A1; Attr:daNone; Ch1:#$8AAA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A1
    (Unicode:#$F9A2; Attr:daNone; Ch1:#$5EC9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A2
    (Unicode:#$F9A3; Attr:daNone; Ch1:#$5FF5; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A3
    (Unicode:#$F9A4; Attr:daNone; Ch1:#$637B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A4
    (Unicode:#$F9A5; Attr:daNone; Ch1:#$6BAE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A5
    (Unicode:#$F9A6; Attr:daNone; Ch1:#$7C3E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A6
    (Unicode:#$F9A7; Attr:daNone; Ch1:#$7375; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A7
    (Unicode:#$F9A8; Attr:daNone; Ch1:#$4EE4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A8
    (Unicode:#$F9A9; Attr:daNone; Ch1:#$56F9; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9A9
    (Unicode:#$F9AA; Attr:daNone; Ch1:#$5BE7; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9AA
    (Unicode:#$F9AB; Attr:daNone; Ch1:#$5DBA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9AB
    (Unicode:#$F9AC; Attr:daNone; Ch1:#$601C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9AC
    (Unicode:#$F9AD; Attr:daNone; Ch1:#$73B2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9AD
    (Unicode:#$F9AE; Attr:daNone; Ch1:#$7469; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9AE
    (Unicode:#$F9AF; Attr:daNone; Ch1:#$7F9A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9AF
    (Unicode:#$F9B0; Attr:daNone; Ch1:#$8046; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B0
    (Unicode:#$F9B1; Attr:daNone; Ch1:#$9234; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B1
    (Unicode:#$F9B2; Attr:daNone; Ch1:#$96F6; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B2
    (Unicode:#$F9B3; Attr:daNone; Ch1:#$9748; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B3
    (Unicode:#$F9B4; Attr:daNone; Ch1:#$9818; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B4
    (Unicode:#$F9B5; Attr:daNone; Ch1:#$4F8B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B5
    (Unicode:#$F9B6; Attr:daNone; Ch1:#$79AE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B6
    (Unicode:#$F9B7; Attr:daNone; Ch1:#$91B4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B7
    (Unicode:#$F9B8; Attr:daNone; Ch1:#$96B8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B8
    (Unicode:#$F9B9; Attr:daNone; Ch1:#$60E1; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9B9
    (Unicode:#$F9BA; Attr:daNone; Ch1:#$4E86; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9BA
    (Unicode:#$F9BB; Attr:daNone; Ch1:#$50DA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9BB
    (Unicode:#$F9BC; Attr:daNone; Ch1:#$5BEE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9BC
    (Unicode:#$F9BD; Attr:daNone; Ch1:#$5C3F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9BD
    (Unicode:#$F9BE; Attr:daNone; Ch1:#$6599; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9BE
    (Unicode:#$F9BF; Attr:daNone; Ch1:#$6A02; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9BF
    (Unicode:#$F9C0; Attr:daNone; Ch1:#$71CE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C0
    (Unicode:#$F9C1; Attr:daNone; Ch1:#$7642; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C1
    (Unicode:#$F9C2; Attr:daNone; Ch1:#$84FC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C2
    (Unicode:#$F9C3; Attr:daNone; Ch1:#$907C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C3
    (Unicode:#$F9C4; Attr:daNone; Ch1:#$9F8D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C4
    (Unicode:#$F9C5; Attr:daNone; Ch1:#$6688; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C5
    (Unicode:#$F9C6; Attr:daNone; Ch1:#$962E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C6
    (Unicode:#$F9C7; Attr:daNone; Ch1:#$5289; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C7
    (Unicode:#$F9C8; Attr:daNone; Ch1:#$677B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C8
    (Unicode:#$F9C9; Attr:daNone; Ch1:#$67F3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9C9
    (Unicode:#$F9CA; Attr:daNone; Ch1:#$6D41; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9CA
    (Unicode:#$F9CB; Attr:daNone; Ch1:#$6E9C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9CB
    (Unicode:#$F9CC; Attr:daNone; Ch1:#$7409; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9CC
    (Unicode:#$F9CD; Attr:daNone; Ch1:#$7559; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9CD
    (Unicode:#$F9CE; Attr:daNone; Ch1:#$786B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9CE
    (Unicode:#$F9CF; Attr:daNone; Ch1:#$7D10; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9CF
    (Unicode:#$F9D0; Attr:daNone; Ch1:#$985E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D0
    (Unicode:#$F9D1; Attr:daNone; Ch1:#$516D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D1
    (Unicode:#$F9D2; Attr:daNone; Ch1:#$622E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D2
    (Unicode:#$F9D3; Attr:daNone; Ch1:#$9678; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D3
    (Unicode:#$F9D4; Attr:daNone; Ch1:#$502B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D4
    (Unicode:#$F9D5; Attr:daNone; Ch1:#$5D19; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D5
    (Unicode:#$F9D6; Attr:daNone; Ch1:#$6DEA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D6
    (Unicode:#$F9D7; Attr:daNone; Ch1:#$8F2A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D7
    (Unicode:#$F9D8; Attr:daNone; Ch1:#$5F8B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D8
    (Unicode:#$F9D9; Attr:daNone; Ch1:#$6144; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9D9
    (Unicode:#$F9DA; Attr:daNone; Ch1:#$6817; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9DA
    (Unicode:#$F9DB; Attr:daNone; Ch1:#$7387; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9DB
    (Unicode:#$F9DC; Attr:daNone; Ch1:#$9686; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9DC
    (Unicode:#$F9DD; Attr:daNone; Ch1:#$5229; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9DD
    (Unicode:#$F9DE; Attr:daNone; Ch1:#$540F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9DE
    (Unicode:#$F9DF; Attr:daNone; Ch1:#$5C65; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9DF
    (Unicode:#$F9E0; Attr:daNone; Ch1:#$6613; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E0
    (Unicode:#$F9E1; Attr:daNone; Ch1:#$674E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E1
    (Unicode:#$F9E2; Attr:daNone; Ch1:#$68A8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E2
    (Unicode:#$F9E3; Attr:daNone; Ch1:#$6CE5; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E3
    (Unicode:#$F9E4; Attr:daNone; Ch1:#$7406; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E4
    (Unicode:#$F9E5; Attr:daNone; Ch1:#$75E2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E5
    (Unicode:#$F9E6; Attr:daNone; Ch1:#$7F79; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E6
    (Unicode:#$F9E7; Attr:daNone; Ch1:#$88CF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E7
    (Unicode:#$F9E8; Attr:daNone; Ch1:#$88E1; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E8
    (Unicode:#$F9E9; Attr:daNone; Ch1:#$91CC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9E9
    (Unicode:#$F9EA; Attr:daNone; Ch1:#$96E2; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9EA
    (Unicode:#$F9EB; Attr:daNone; Ch1:#$533F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9EB
    (Unicode:#$F9EC; Attr:daNone; Ch1:#$6EBA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9EC
    (Unicode:#$F9ED; Attr:daNone; Ch1:#$541D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9ED
    (Unicode:#$F9EE; Attr:daNone; Ch1:#$71D0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9EE
    (Unicode:#$F9EF; Attr:daNone; Ch1:#$7498; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9EF
    (Unicode:#$F9F0; Attr:daNone; Ch1:#$85FA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F0
    (Unicode:#$F9F1; Attr:daNone; Ch1:#$96A3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F1
    (Unicode:#$F9F2; Attr:daNone; Ch1:#$9C57; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F2
    (Unicode:#$F9F3; Attr:daNone; Ch1:#$9E9F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F3
    (Unicode:#$F9F4; Attr:daNone; Ch1:#$6797; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F4
    (Unicode:#$F9F5; Attr:daNone; Ch1:#$6DCB; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F5
    (Unicode:#$F9F6; Attr:daNone; Ch1:#$81E8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F6
    (Unicode:#$F9F7; Attr:daNone; Ch1:#$7ACB; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F7
    (Unicode:#$F9F8; Attr:daNone; Ch1:#$7B20; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F8
    (Unicode:#$F9F9; Attr:daNone; Ch1:#$7C92; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9F9
    (Unicode:#$F9FA; Attr:daNone; Ch1:#$72C0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9FA
    (Unicode:#$F9FB; Attr:daNone; Ch1:#$7099; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9FB
    (Unicode:#$F9FC; Attr:daNone; Ch1:#$8B58; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9FC
    (Unicode:#$F9FD; Attr:daNone; Ch1:#$4EC0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9FD
    (Unicode:#$F9FE; Attr:daNone; Ch1:#$8336; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9FE
    (Unicode:#$F9FF; Attr:daNone; Ch1:#$523A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-F9FF
    (Unicode:#$FA00; Attr:daNone; Ch1:#$5207; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA00
    (Unicode:#$FA01; Attr:daNone; Ch1:#$5EA6; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA01
    (Unicode:#$FA02; Attr:daNone; Ch1:#$62D3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA02
    (Unicode:#$FA03; Attr:daNone; Ch1:#$7CD6; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA03
    (Unicode:#$FA04; Attr:daNone; Ch1:#$5B85; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA04
    (Unicode:#$FA05; Attr:daNone; Ch1:#$6D1E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA05
    (Unicode:#$FA06; Attr:daNone; Ch1:#$66B4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA06
    (Unicode:#$FA07; Attr:daNone; Ch1:#$8F3B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA07
    (Unicode:#$FA08; Attr:daNone; Ch1:#$884C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA08
    (Unicode:#$FA09; Attr:daNone; Ch1:#$964D; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA09
    (Unicode:#$FA0A; Attr:daNone; Ch1:#$898B; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA0A
    (Unicode:#$FA0B; Attr:daNone; Ch1:#$5ED3; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA0B
    (Unicode:#$FA0C; Attr:daNone; Ch1:#$5140; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA0C
    (Unicode:#$FA0D; Attr:daNone; Ch1:#$55C0; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA0D
    (Unicode:#$FA10; Attr:daNone; Ch1:#$585A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA10
    (Unicode:#$FA12; Attr:daNone; Ch1:#$6674; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA12
    (Unicode:#$FA15; Attr:daNone; Ch1:#$51DE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA15
    (Unicode:#$FA16; Attr:daNone; Ch1:#$732A; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA16
    (Unicode:#$FA17; Attr:daNone; Ch1:#$76CA; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA17
    (Unicode:#$FA18; Attr:daNone; Ch1:#$793C; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA18
    (Unicode:#$FA19; Attr:daNone; Ch1:#$795E; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA19
    (Unicode:#$FA1A; Attr:daNone; Ch1:#$7965; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA1A
    (Unicode:#$FA1B; Attr:daNone; Ch1:#$798F; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA1B
    (Unicode:#$FA1C; Attr:daNone; Ch1:#$9756; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA1C
    (Unicode:#$FA1D; Attr:daNone; Ch1:#$7CBE; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA1D
    (Unicode:#$FA1E; Attr:daNone; Ch1:#$7FBD; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA1E
    (Unicode:#$FA20; Attr:daNone; Ch1:#$8612; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA20
    (Unicode:#$FA22; Attr:daNone; Ch1:#$8AF8; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA22
    (Unicode:#$FA25; Attr:daNone; Ch1:#$9038; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA25
    (Unicode:#$FA26; Attr:daNone; Ch1:#$90FD; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA26
    (Unicode:#$FA2A; Attr:daNone; Ch1:#$98EF; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA2A
    (Unicode:#$FA2B; Attr:daNone; Ch1:#$98FC; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA2B
    (Unicode:#$FA2C; Attr:daNone; Ch1:#$9928; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA2C
    (Unicode:#$FA2D; Attr:daNone; Ch1:#$9DB4; Ch2:#$FFFF),                // CJK COMPATIBILITY IDEOGRAPH-FA2D
    (Unicode:#$FB00; Attr:daCompat; Ch1:#$0066; Ch2:#$0066; Ch3:#$FFFF),               // LATIN SMALL LIGATURE FF
    (Unicode:#$FB01; Attr:daCompat; Ch1:#$0066; Ch2:#$0069; Ch3:#$FFFF),               // LATIN SMALL LIGATURE FI
    (Unicode:#$FB02; Attr:daCompat; Ch1:#$0066; Ch2:#$006C; Ch3:#$FFFF),               // LATIN SMALL LIGATURE FL
    (Unicode:#$FB03; Attr:daCompat; Ch1:#$0066; Ch2:#$0066; Ch3:#$0069; Ch4:#$FFFF),   // LATIN SMALL LIGATURE FFI
    (Unicode:#$FB04; Attr:daCompat; Ch1:#$0066; Ch2:#$0066; Ch3:#$006C; Ch4:#$FFFF),   // LATIN SMALL LIGATURE FFL
    (Unicode:#$FB05; Attr:daCompat; Ch1:#$017F; Ch2:#$0074; Ch3:#$FFFF),  // LATIN SMALL LIGATURE LONG S T
    (Unicode:#$FB06; Attr:daCompat; Ch1:#$0073; Ch2:#$0074; Ch3:#$FFFF),  // LATIN SMALL LIGATURE ST
    (Unicode:#$FB13; Attr:daCompat; Ch1:#$0574; Ch2:#$0576; Ch3:#$FFFF),  // ARMENIAN SMALL LIGATURE MEN NOW
    (Unicode:#$FB14; Attr:daCompat; Ch1:#$0574; Ch2:#$0565; Ch3:#$FFFF),  // ARMENIAN SMALL LIGATURE MEN ECH
    (Unicode:#$FB15; Attr:daCompat; Ch1:#$0574; Ch2:#$056B; Ch3:#$FFFF),  // ARMENIAN SMALL LIGATURE MEN INI
    (Unicode:#$FB16; Attr:daCompat; Ch1:#$057E; Ch2:#$0576; Ch3:#$FFFF),  // ARMENIAN SMALL LIGATURE VEW NOW
    (Unicode:#$FB17; Attr:daCompat; Ch1:#$0574; Ch2:#$056D; Ch3:#$FFFF),  // ARMENIAN SMALL LIGATURE MEN XEH
    (Unicode:#$FB1D; Attr:daNone; Ch1:#$05D9; Ch2:#$05B4; Ch3:#$FFFF),    // HEBREW LETTER YOD WITH HIRIQ
    (Unicode:#$FB1F; Attr:daNone; Ch1:#$05F2; Ch2:#$05B7; Ch3:#$FFFF),    // HEBREW LIGATURE YIDDISH YOD YOD PATAH
    (Unicode:#$FB20; Attr:daFont; Ch1:#$05E2; Ch2:#$FFFF),                // HEBREW LETTER ALTERNATIVE AYIN
    (Unicode:#$FB21; Attr:daFont; Ch1:#$05D0; Ch2:#$FFFF),                // HEBREW LETTER WIDE ALEF
    (Unicode:#$FB22; Attr:daFont; Ch1:#$05D3; Ch2:#$FFFF),                // HEBREW LETTER WIDE DALET
    (Unicode:#$FB23; Attr:daFont; Ch1:#$05D4; Ch2:#$FFFF),                // HEBREW LETTER WIDE HE
    (Unicode:#$FB24; Attr:daFont; Ch1:#$05DB; Ch2:#$FFFF),                // HEBREW LETTER WIDE KAF
    (Unicode:#$FB25; Attr:daFont; Ch1:#$05DC; Ch2:#$FFFF),                // HEBREW LETTER WIDE LAMED
    (Unicode:#$FB26; Attr:daFont; Ch1:#$05DD; Ch2:#$FFFF),                // HEBREW LETTER WIDE FINAL MEM
    (Unicode:#$FB27; Attr:daFont; Ch1:#$05E8; Ch2:#$FFFF),                // HEBREW LETTER WIDE RESH
    (Unicode:#$FB28; Attr:daFont; Ch1:#$05EA; Ch2:#$FFFF),                // HEBREW LETTER WIDE TAV
    (Unicode:#$FB29; Attr:daFont; Ch1:#$002B; Ch2:#$FFFF),                // HEBREW LETTER ALTERNATIVE PLUS SIGN
    (Unicode:#$FB2A; Attr:daNone; Ch1:#$05E9; Ch2:#$05C1; Ch3:#$FFFF),    // HEBREW LETTER SHIN WITH SHIN DOT
    (Unicode:#$FB2B; Attr:daNone; Ch1:#$05E9; Ch2:#$05C2; Ch3:#$FFFF),    // HEBREW LETTER SHIN WITH SIN DOT
    (Unicode:#$FB2C; Attr:daNone; Ch1:#$FB49; Ch2:#$05C1; Ch3:#$FFFF),    // HEBREW LETTER SHIN WITH DAGESH AND SHIN DOT
    (Unicode:#$FB2D; Attr:daNone; Ch1:#$FB49; Ch2:#$05C2; Ch3:#$FFFF),    // HEBREW LETTER SHIN WITH DAGESH AND SIN DOT
    (Unicode:#$FB2E; Attr:daNone; Ch1:#$05D0; Ch2:#$05B7; Ch3:#$FFFF),    // HEBREW LETTER ALEF WITH PATAH
    (Unicode:#$FB2F; Attr:daNone; Ch1:#$05D0; Ch2:#$05B8; Ch3:#$FFFF),    // HEBREW LETTER ALEF WITH QAMATS
    (Unicode:#$FB30; Attr:daNone; Ch1:#$05D0; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER ALEF WITH MAPIQ
    (Unicode:#$FB31; Attr:daNone; Ch1:#$05D1; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER BET WITH DAGESH
    (Unicode:#$FB32; Attr:daNone; Ch1:#$05D2; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER GIMEL WITH DAGESH
    (Unicode:#$FB33; Attr:daNone; Ch1:#$05D3; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER DALET WITH DAGESH
    (Unicode:#$FB34; Attr:daNone; Ch1:#$05D4; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER HE WITH MAPIQ
    (Unicode:#$FB35; Attr:daNone; Ch1:#$05D5; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER VAV WITH DAGESH
    (Unicode:#$FB36; Attr:daNone; Ch1:#$05D6; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER ZAYIN WITH DAGESH
    (Unicode:#$FB38; Attr:daNone; Ch1:#$05D8; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER TET WITH DAGESH
    (Unicode:#$FB39; Attr:daNone; Ch1:#$05D9; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER YOD WITH DAGESH
    (Unicode:#$FB3A; Attr:daNone; Ch1:#$05DA; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER FINAL KAF WITH DAGESH
    (Unicode:#$FB3B; Attr:daNone; Ch1:#$05DB; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER KAF WITH DAGESH
    (Unicode:#$FB3C; Attr:daNone; Ch1:#$05DC; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER LAMED WITH DAGESH
    (Unicode:#$FB3E; Attr:daNone; Ch1:#$05DE; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER MEM WITH DAGESH
    (Unicode:#$FB40; Attr:daNone; Ch1:#$05E0; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER NUN WITH DAGESH
    (Unicode:#$FB41; Attr:daNone; Ch1:#$05E1; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER SAMEKH WITH DAGESH
    (Unicode:#$FB43; Attr:daNone; Ch1:#$05E3; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER FINAL PE WITH DAGESH
    (Unicode:#$FB44; Attr:daNone; Ch1:#$05E4; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER PE WITH DAGESH
    (Unicode:#$FB46; Attr:daNone; Ch1:#$05E6; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER TSADI WITH DAGESH
    (Unicode:#$FB47; Attr:daNone; Ch1:#$05E7; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER QOF WITH DAGESH
    (Unicode:#$FB48; Attr:daNone; Ch1:#$05E8; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER RESH WITH DAGESH
    (Unicode:#$FB49; Attr:daNone; Ch1:#$05E9; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER SHIN WITH DAGESH
    (Unicode:#$FB4A; Attr:daNone; Ch1:#$05EA; Ch2:#$05BC; Ch3:#$FFFF),    // HEBREW LETTER TAV WITH DAGESH
    (Unicode:#$FB4B; Attr:daNone; Ch1:#$05D5; Ch2:#$05B9; Ch3:#$FFFF),    // HEBREW LETTER VAV WITH HOLAM
    (Unicode:#$FB4C; Attr:daNone; Ch1:#$05D1; Ch2:#$05BF; Ch3:#$FFFF),    // HEBREW LETTER BET WITH RAFE
    (Unicode:#$FB4D; Attr:daNone; Ch1:#$05DB; Ch2:#$05BF; Ch3:#$FFFF),    // HEBREW LETTER KAF WITH RAFE
    (Unicode:#$FB4E; Attr:daNone; Ch1:#$05E4; Ch2:#$05BF; Ch3:#$FFFF),    // HEBREW LETTER PE WITH RAFE
    (Unicode:#$FB4F; Attr:daCompat; Ch1:#$05D0; Ch2:#$05DC; Ch3:#$FFFF),  // HEBREW LIGATURE ALEF LAMED
    (Unicode:#$FB50; Attr:daIsolated; Ch1:#$0671; Ch2:#$FFFF),            // ARABIC LETTER ALEF WASLA ISOLATED FORM
    (Unicode:#$FB51; Attr:daFinal; Ch1:#$0671; Ch2:#$FFFF),               // ARABIC LETTER ALEF WASLA FINAL FORM
    (Unicode:#$FB52; Attr:daIsolated; Ch1:#$067B; Ch2:#$FFFF),            // ARABIC LETTER BEEH ISOLATED FORM
    (Unicode:#$FB53; Attr:daFinal; Ch1:#$067B; Ch2:#$FFFF),               // ARABIC LETTER BEEH FINAL FORM
    (Unicode:#$FB54; Attr:daInitial; Ch1:#$067B; Ch2:#$FFFF),             // ARABIC LETTER BEEH INITIAL FORM
    (Unicode:#$FB55; Attr:daMedial; Ch1:#$067B; Ch2:#$FFFF),              // ARABIC LETTER BEEH MEDIAL FORM
    (Unicode:#$FB56; Attr:daIsolated; Ch1:#$067E; Ch2:#$FFFF),            // ARABIC LETTER PEH ISOLATED FORM
    (Unicode:#$FB57; Attr:daFinal; Ch1:#$067E; Ch2:#$FFFF),               // ARABIC LETTER PEH FINAL FORM
    (Unicode:#$FB58; Attr:daInitial; Ch1:#$067E; Ch2:#$FFFF),             // ARABIC LETTER PEH INITIAL FORM
    (Unicode:#$FB59; Attr:daMedial; Ch1:#$067E; Ch2:#$FFFF),              // ARABIC LETTER PEH MEDIAL FORM
    (Unicode:#$FB5A; Attr:daIsolated; Ch1:#$0680; Ch2:#$FFFF),            // ARABIC LETTER BEHEH ISOLATED FORM
    (Unicode:#$FB5B; Attr:daFinal; Ch1:#$0680; Ch2:#$FFFF),               // ARABIC LETTER BEHEH FINAL FORM
    (Unicode:#$FB5C; Attr:daInitial; Ch1:#$0680; Ch2:#$FFFF),             // ARABIC LETTER BEHEH INITIAL FORM
    (Unicode:#$FB5D; Attr:daMedial; Ch1:#$0680; Ch2:#$FFFF),              // ARABIC LETTER BEHEH MEDIAL FORM
    (Unicode:#$FB5E; Attr:daIsolated; Ch1:#$067A; Ch2:#$FFFF),            // ARABIC LETTER TTEHEH ISOLATED FORM
    (Unicode:#$FB5F; Attr:daFinal; Ch1:#$067A; Ch2:#$FFFF),               // ARABIC LETTER TTEHEH FINAL FORM
    (Unicode:#$FB60; Attr:daInitial; Ch1:#$067A; Ch2:#$FFFF),             // ARABIC LETTER TTEHEH INITIAL FORM
    (Unicode:#$FB61; Attr:daMedial; Ch1:#$067A; Ch2:#$FFFF),              // ARABIC LETTER TTEHEH MEDIAL FORM
    (Unicode:#$FB62; Attr:daIsolated; Ch1:#$067F; Ch2:#$FFFF),            // ARABIC LETTER TEHEH ISOLATED FORM
    (Unicode:#$FB63; Attr:daFinal; Ch1:#$067F; Ch2:#$FFFF),               // ARABIC LETTER TEHEH FINAL FORM
    (Unicode:#$FB64; Attr:daInitial; Ch1:#$067F; Ch2:#$FFFF),             // ARABIC LETTER TEHEH INITIAL FORM
    (Unicode:#$FB65; Attr:daMedial; Ch1:#$067F; Ch2:#$FFFF),              // ARABIC LETTER TEHEH MEDIAL FORM
    (Unicode:#$FB66; Attr:daIsolated; Ch1:#$0679; Ch2:#$FFFF),            // ARABIC LETTER TTEH ISOLATED FORM
    (Unicode:#$FB67; Attr:daFinal; Ch1:#$0679; Ch2:#$FFFF),               // ARABIC LETTER TTEH FINAL FORM
    (Unicode:#$FB68; Attr:daInitial; Ch1:#$0679; Ch2:#$FFFF),             // ARABIC LETTER TTEH INITIAL FORM
    (Unicode:#$FB69; Attr:daMedial; Ch1:#$0679; Ch2:#$FFFF),              // ARABIC LETTER TTEH MEDIAL FORM
    (Unicode:#$FB6A; Attr:daIsolated; Ch1:#$06A4; Ch2:#$FFFF),            // ARABIC LETTER VEH ISOLATED FORM
    (Unicode:#$FB6B; Attr:daFinal; Ch1:#$06A4; Ch2:#$FFFF),               // ARABIC LETTER VEH FINAL FORM
    (Unicode:#$FB6C; Attr:daInitial; Ch1:#$06A4; Ch2:#$FFFF),             // ARABIC LETTER VEH INITIAL FORM
    (Unicode:#$FB6D; Attr:daMedial; Ch1:#$06A4; Ch2:#$FFFF),              // ARABIC LETTER VEH MEDIAL FORM
    (Unicode:#$FB6E; Attr:daIsolated; Ch1:#$06A6; Ch2:#$FFFF),            // ARABIC LETTER PEHEH ISOLATED FORM
    (Unicode:#$FB6F; Attr:daFinal; Ch1:#$06A6; Ch2:#$FFFF),               // ARABIC LETTER PEHEH FINAL FORM
    (Unicode:#$FB70; Attr:daInitial; Ch1:#$06A6; Ch2:#$FFFF),             // ARABIC LETTER PEHEH INITIAL FORM
    (Unicode:#$FB71; Attr:daMedial; Ch1:#$06A6; Ch2:#$FFFF),              // ARABIC LETTER PEHEH MEDIAL FORM
    (Unicode:#$FB72; Attr:daIsolated; Ch1:#$0684; Ch2:#$FFFF),            // ARABIC LETTER DYEH ISOLATED FORM
    (Unicode:#$FB73; Attr:daFinal; Ch1:#$0684; Ch2:#$FFFF),               // ARABIC LETTER DYEH FINAL FORM
    (Unicode:#$FB74; Attr:daInitial; Ch1:#$0684; Ch2:#$FFFF),             // ARABIC LETTER DYEH INITIAL FORM
    (Unicode:#$FB75; Attr:daMedial; Ch1:#$0684; Ch2:#$FFFF),              // ARABIC LETTER DYEH MEDIAL FORM
    (Unicode:#$FB76; Attr:daIsolated; Ch1:#$0683; Ch2:#$FFFF),            // ARABIC LETTER NYEH ISOLATED FORM
    (Unicode:#$FB77; Attr:daFinal; Ch1:#$0683; Ch2:#$FFFF),               // ARABIC LETTER NYEH FINAL FORM
    (Unicode:#$FB78; Attr:daInitial; Ch1:#$0683; Ch2:#$FFFF),             // ARABIC LETTER NYEH INITIAL FORM
    (Unicode:#$FB79; Attr:daMedial; Ch1:#$0683; Ch2:#$FFFF),              // ARABIC LETTER NYEH MEDIAL FORM
    (Unicode:#$FB7A; Attr:daIsolated; Ch1:#$0686; Ch2:#$FFFF),            // ARABIC LETTER TCHEH ISOLATED FORM
    (Unicode:#$FB7B; Attr:daFinal; Ch1:#$0686; Ch2:#$FFFF),               // ARABIC LETTER TCHEH FINAL FORM
    (Unicode:#$FB7C; Attr:daInitial; Ch1:#$0686; Ch2:#$FFFF),             // ARABIC LETTER TCHEH INITIAL FORM
    (Unicode:#$FB7D; Attr:daMedial; Ch1:#$0686; Ch2:#$FFFF),              // ARABIC LETTER TCHEH MEDIAL FORM
    (Unicode:#$FB7E; Attr:daIsolated; Ch1:#$0687; Ch2:#$FFFF),            // ARABIC LETTER TCHEHEH ISOLATED FORM
    (Unicode:#$FB7F; Attr:daFinal; Ch1:#$0687; Ch2:#$FFFF),               // ARABIC LETTER TCHEHEH FINAL FORM
    (Unicode:#$FB80; Attr:daInitial; Ch1:#$0687; Ch2:#$FFFF),             // ARABIC LETTER TCHEHEH INITIAL FORM
    (Unicode:#$FB81; Attr:daMedial; Ch1:#$0687; Ch2:#$FFFF),              // ARABIC LETTER TCHEHEH MEDIAL FORM
    (Unicode:#$FB82; Attr:daIsolated; Ch1:#$068D; Ch2:#$FFFF),            // ARABIC LETTER DDAHAL ISOLATED FORM
    (Unicode:#$FB83; Attr:daFinal; Ch1:#$068D; Ch2:#$FFFF),               // ARABIC LETTER DDAHAL FINAL FORM
    (Unicode:#$FB84; Attr:daIsolated; Ch1:#$068C; Ch2:#$FFFF),            // ARABIC LETTER DAHAL ISOLATED FORM
    (Unicode:#$FB85; Attr:daFinal; Ch1:#$068C; Ch2:#$FFFF),               // ARABIC LETTER DAHAL FINAL FORM
    (Unicode:#$FB86; Attr:daIsolated; Ch1:#$068E; Ch2:#$FFFF),            // ARABIC LETTER DUL ISOLATED FORM
    (Unicode:#$FB87; Attr:daFinal; Ch1:#$068E; Ch2:#$FFFF),               // ARABIC LETTER DUL FINAL FORM
    (Unicode:#$FB88; Attr:daIsolated; Ch1:#$0688; Ch2:#$FFFF),            // ARABIC LETTER DDAL ISOLATED FORM
    (Unicode:#$FB89; Attr:daFinal; Ch1:#$0688; Ch2:#$FFFF),               // ARABIC LETTER DDAL FINAL FORM
    (Unicode:#$FB8A; Attr:daIsolated; Ch1:#$0698; Ch2:#$FFFF),            // ARABIC LETTER JEH ISOLATED FORM
    (Unicode:#$FB8B; Attr:daFinal; Ch1:#$0698; Ch2:#$FFFF),               // ARABIC LETTER JEH FINAL FORM
    (Unicode:#$FB8C; Attr:daIsolated; Ch1:#$0691; Ch2:#$FFFF),            // ARABIC LETTER RREH ISOLATED FORM
    (Unicode:#$FB8D; Attr:daFinal; Ch1:#$0691; Ch2:#$FFFF),               // ARABIC LETTER RREH FINAL FORM
    (Unicode:#$FB8E; Attr:daIsolated; Ch1:#$06A9; Ch2:#$FFFF),            // ARABIC LETTER KEHEH ISOLATED FORM
    (Unicode:#$FB8F; Attr:daFinal; Ch1:#$06A9; Ch2:#$FFFF),               // ARABIC LETTER KEHEH FINAL FORM
    (Unicode:#$FB90; Attr:daInitial; Ch1:#$06A9; Ch2:#$FFFF),             // ARABIC LETTER KEHEH INITIAL FORM
    (Unicode:#$FB91; Attr:daMedial; Ch1:#$06A9; Ch2:#$FFFF),              // ARABIC LETTER KEHEH MEDIAL FORM
    (Unicode:#$FB92; Attr:daIsolated; Ch1:#$06AF; Ch2:#$FFFF),            // ARABIC LETTER GAF ISOLATED FORM
    (Unicode:#$FB93; Attr:daFinal; Ch1:#$06AF; Ch2:#$FFFF),               // ARABIC LETTER GAF FINAL FORM
    (Unicode:#$FB94; Attr:daInitial; Ch1:#$06AF; Ch2:#$FFFF),             // ARABIC LETTER GAF INITIAL FORM
    (Unicode:#$FB95; Attr:daMedial; Ch1:#$06AF; Ch2:#$FFFF),              // ARABIC LETTER GAF MEDIAL FORM
    (Unicode:#$FB96; Attr:daIsolated; Ch1:#$06B3; Ch2:#$FFFF),            // ARABIC LETTER GUEH ISOLATED FORM
    (Unicode:#$FB97; Attr:daFinal; Ch1:#$06B3; Ch2:#$FFFF),               // ARABIC LETTER GUEH FINAL FORM
    (Unicode:#$FB98; Attr:daInitial; Ch1:#$06B3; Ch2:#$FFFF),             // ARABIC LETTER GUEH INITIAL FORM
    (Unicode:#$FB99; Attr:daMedial; Ch1:#$06B3; Ch2:#$FFFF),              // ARABIC LETTER GUEH MEDIAL FORM
    (Unicode:#$FB9A; Attr:daIsolated; Ch1:#$06B1; Ch2:#$FFFF),            // ARABIC LETTER NGOEH ISOLATED FORM
    (Unicode:#$FB9B; Attr:daFinal; Ch1:#$06B1; Ch2:#$FFFF),               // ARABIC LETTER NGOEH FINAL FORM
    (Unicode:#$FB9C; Attr:daInitial; Ch1:#$06B1; Ch2:#$FFFF),             // ARABIC LETTER NGOEH INITIAL FORM
    (Unicode:#$FB9D; Attr:daMedial; Ch1:#$06B1; Ch2:#$FFFF),              // ARABIC LETTER NGOEH MEDIAL FORM
    (Unicode:#$FB9E; Attr:daIsolated; Ch1:#$06BA; Ch2:#$FFFF),            // ARABIC LETTER NOON GHUNNA ISOLATED FORM
    (Unicode:#$FB9F; Attr:daFinal; Ch1:#$06BA; Ch2:#$FFFF),               // ARABIC LETTER NOON GHUNNA FINAL FORM
    (Unicode:#$FBA0; Attr:daIsolated; Ch1:#$06BB; Ch2:#$FFFF),            // ARABIC LETTER RNOON ISOLATED FORM
    (Unicode:#$FBA1; Attr:daFinal; Ch1:#$06BB; Ch2:#$FFFF),               // ARABIC LETTER RNOON FINAL FORM
    (Unicode:#$FBA2; Attr:daInitial; Ch1:#$06BB; Ch2:#$FFFF),             // ARABIC LETTER RNOON INITIAL FORM
    (Unicode:#$FBA3; Attr:daMedial; Ch1:#$06BB; Ch2:#$FFFF),              // ARABIC LETTER RNOON MEDIAL FORM
    (Unicode:#$FBA4; Attr:daIsolated; Ch1:#$06C0; Ch2:#$FFFF),            // ARABIC LETTER HEH WITH YEH ABOVE ISOLATED FORM
    (Unicode:#$FBA5; Attr:daFinal; Ch1:#$06C0; Ch2:#$FFFF),               // ARABIC LETTER HEH WITH YEH ABOVE FINAL FORM
    (Unicode:#$FBA6; Attr:daIsolated; Ch1:#$06C1; Ch2:#$FFFF),            // ARABIC LETTER HEH GOAL ISOLATED FORM
    (Unicode:#$FBA7; Attr:daFinal; Ch1:#$06C1; Ch2:#$FFFF),               // ARABIC LETTER HEH GOAL FINAL FORM
    (Unicode:#$FBA8; Attr:daInitial; Ch1:#$06C1; Ch2:#$FFFF),             // ARABIC LETTER HEH GOAL INITIAL FORM
    (Unicode:#$FBA9; Attr:daMedial; Ch1:#$06C1; Ch2:#$FFFF),              // ARABIC LETTER HEH GOAL MEDIAL FORM
    (Unicode:#$FBAA; Attr:daIsolated; Ch1:#$06BE; Ch2:#$FFFF),            // ARABIC LETTER HEH DOACHASHMEE ISOLATED FORM
    (Unicode:#$FBAB; Attr:daFinal; Ch1:#$06BE; Ch2:#$FFFF),               // ARABIC LETTER HEH DOACHASHMEE FINAL FORM
    (Unicode:#$FBAC; Attr:daInitial; Ch1:#$06BE; Ch2:#$FFFF),             // ARABIC LETTER HEH DOACHASHMEE INITIAL FORM
    (Unicode:#$FBAD; Attr:daMedial; Ch1:#$06BE; Ch2:#$FFFF),              // ARABIC LETTER HEH DOACHASHMEE MEDIAL FORM
    (Unicode:#$FBAE; Attr:daIsolated; Ch1:#$06D2; Ch2:#$FFFF),            // ARABIC LETTER YEH BARREE ISOLATED FORM
    (Unicode:#$FBAF; Attr:daFinal; Ch1:#$06D2; Ch2:#$FFFF),               // ARABIC LETTER YEH BARREE FINAL FORM
    (Unicode:#$FBB0; Attr:daIsolated; Ch1:#$06D3; Ch2:#$FFFF),            // ARABIC LETTER YEH BARREE WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FBB1; Attr:daFinal; Ch1:#$06D3; Ch2:#$FFFF),               // ARABIC LETTER YEH BARREE WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FBD3; Attr:daIsolated; Ch1:#$06AD; Ch2:#$FFFF),            // ARABIC LETTER NG ISOLATED FORM
    (Unicode:#$FBD4; Attr:daFinal; Ch1:#$06AD; Ch2:#$FFFF),               // ARABIC LETTER NG FINAL FORM
    (Unicode:#$FBD5; Attr:daInitial; Ch1:#$06AD; Ch2:#$FFFF),             // ARABIC LETTER NG INITIAL FORM
    (Unicode:#$FBD6; Attr:daMedial; Ch1:#$06AD; Ch2:#$FFFF),              // ARABIC LETTER NG MEDIAL FORM
    (Unicode:#$FBD7; Attr:daIsolated; Ch1:#$06C7; Ch2:#$FFFF),            // ARABIC LETTER U ISOLATED FORM
    (Unicode:#$FBD8; Attr:daFinal; Ch1:#$06C7; Ch2:#$FFFF),               // ARABIC LETTER U FINAL FORM
    (Unicode:#$FBD9; Attr:daIsolated; Ch1:#$06C6; Ch2:#$FFFF),            // ARABIC LETTER OE ISOLATED FORM
    (Unicode:#$FBDA; Attr:daFinal; Ch1:#$06C6; Ch2:#$FFFF),               // ARABIC LETTER OE FINAL FORM
    (Unicode:#$FBDB; Attr:daIsolated; Ch1:#$06C8; Ch2:#$FFFF),            // ARABIC LETTER YU ISOLATED FORM
    (Unicode:#$FBDC; Attr:daFinal; Ch1:#$06C8; Ch2:#$FFFF),               // ARABIC LETTER YU FINAL FORM
    (Unicode:#$FBDD; Attr:daIsolated; Ch1:#$0677; Ch2:#$FFFF),            // ARABIC LETTER U WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FBDE; Attr:daIsolated; Ch1:#$06CB; Ch2:#$FFFF),            // ARABIC LETTER VE ISOLATED FORM
    (Unicode:#$FBDF; Attr:daFinal; Ch1:#$06CB; Ch2:#$FFFF),               // ARABIC LETTER VE FINAL FORM
    (Unicode:#$FBE0; Attr:daIsolated; Ch1:#$06C5; Ch2:#$FFFF),            // ARABIC LETTER KIRGHIZ OE ISOLATED FORM
    (Unicode:#$FBE1; Attr:daFinal; Ch1:#$06C5; Ch2:#$FFFF),               // ARABIC LETTER KIRGHIZ OE FINAL FORM
    (Unicode:#$FBE2; Attr:daIsolated; Ch1:#$06C9; Ch2:#$FFFF),            // ARABIC LETTER KIRGHIZ YU ISOLATED FORM
    (Unicode:#$FBE3; Attr:daFinal; Ch1:#$06C9; Ch2:#$FFFF),               // ARABIC LETTER KIRGHIZ YU FINAL FORM
    (Unicode:#$FBE4; Attr:daIsolated; Ch1:#$06D0; Ch2:#$FFFF),            // ARABIC LETTER E ISOLATED FORM
    (Unicode:#$FBE5; Attr:daFinal; Ch1:#$06D0; Ch2:#$FFFF),               // ARABIC LETTER E FINAL FORM
    (Unicode:#$FBE6; Attr:daInitial; Ch1:#$06D0; Ch2:#$FFFF),             // ARABIC LETTER E INITIAL FORM
    (Unicode:#$FBE7; Attr:daMedial; Ch1:#$06D0; Ch2:#$FFFF),              // ARABIC LETTER E MEDIAL FORM
    (Unicode:#$FBE8; Attr:daInitial; Ch1:#$0649; Ch2:#$FFFF),             // ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA INITIAL FORM
    (Unicode:#$FBE9; Attr:daMedial; Ch1:#$0649; Ch2:#$FFFF),              // ARABIC LETTER UIGHUR KAZAKH KIRGHIZ ALEF MAKSURA MEDIAL FORM
    (Unicode:#$FBEA; Attr:daIsolated; Ch1:#$0626; Ch2:#$0627; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF ISOLATED FORM
    (Unicode:#$FBEB; Attr:daFinal; Ch1:#$0626; Ch2:#$0627; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF FINAL FORM
    (Unicode:#$FBEC; Attr:daIsolated; Ch1:#$0626; Ch2:#$06D5; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE ISOLATED FORM
    (Unicode:#$FBED; Attr:daFinal; Ch1:#$0626; Ch2:#$06D5; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH AE FINAL FORM
    (Unicode:#$FBEE; Attr:daIsolated; Ch1:#$0626; Ch2:#$0648; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW ISOLATED FORM
    (Unicode:#$FBEF; Attr:daFinal; Ch1:#$0626; Ch2:#$0648; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH WAW FINAL FORM
    (Unicode:#$FBF0; Attr:daIsolated; Ch1:#$0626; Ch2:#$06C7; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U ISOLATED FORM
    (Unicode:#$FBF1; Attr:daFinal; Ch1:#$0626; Ch2:#$06C7; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH U FINAL FORM
    (Unicode:#$FBF2; Attr:daIsolated; Ch1:#$0626; Ch2:#$06C6; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE ISOLATED FORM
    (Unicode:#$FBF3; Attr:daFinal; Ch1:#$0626; Ch2:#$06C6; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH OE FINAL FORM
    (Unicode:#$FBF4; Attr:daIsolated; Ch1:#$0626; Ch2:#$06C8; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU ISOLATED FORM
    (Unicode:#$FBF5; Attr:daFinal; Ch1:#$0626; Ch2:#$06C8; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YU FINAL FORM
    (Unicode:#$FBF6; Attr:daIsolated; Ch1:#$0626; Ch2:#$06D0; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E ISOLATED FORM
    (Unicode:#$FBF7; Attr:daFinal; Ch1:#$0626; Ch2:#$06D0; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E FINAL FORM
    (Unicode:#$FBF8; Attr:daInitial; Ch1:#$0626; Ch2:#$06D0; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH E INITIAL FORM
    (Unicode:#$FBF9; Attr:daIsolated; Ch1:#$0626; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FBFA; Attr:daFinal; Ch1:#$0626; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FBFB; Attr:daInitial; Ch1:#$0626; Ch2:#$0649; Ch3:#$FFFF), // ARABIC LIGATURE UIGHUR KIRGHIZ YEH WITH HAMZA ABOVE WITH ALEF MAKSURA INITIAL FORM
    (Unicode:#$FBFC; Attr:daIsolated; Ch1:#$06CC; Ch2:#$FFFF),            // ARABIC LETTER FARSI YEH ISOLATED FORM
    (Unicode:#$FBFD; Attr:daFinal; Ch1:#$06CC; Ch2:#$FFFF),               // ARABIC LETTER FARSI YEH FINAL FORM
    (Unicode:#$FBFE; Attr:daInitial; Ch1:#$06CC; Ch2:#$FFFF),             // ARABIC LETTER FARSI YEH INITIAL FORM
    (Unicode:#$FBFF; Attr:daMedial; Ch1:#$06CC; Ch2:#$FFFF),              // ARABIC LETTER FARSI YEH MEDIAL FORM
    (Unicode:#$FC00; Attr:daIsolated; Ch1:#$0626; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM ISOLATED FORM
    (Unicode:#$FC01; Attr:daIsolated; Ch1:#$0626; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HAH ISOLATED FORM
    (Unicode:#$FC02; Attr:daIsolated; Ch1:#$0626; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM ISOLATED FORM
    (Unicode:#$FC03; Attr:daIsolated; Ch1:#$0626; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC04; Attr:daIsolated; Ch1:#$0626; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YEH ISOLATED FORM
    (Unicode:#$FC05; Attr:daIsolated; Ch1:#$0628; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE BEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC06; Attr:daIsolated; Ch1:#$0628; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE BEH WITH HAH ISOLATED FORM
    (Unicode:#$FC07; Attr:daIsolated; Ch1:#$0628; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE BEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC08; Attr:daIsolated; Ch1:#$0628; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE BEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC09; Attr:daIsolated; Ch1:#$0628; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE BEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC0A; Attr:daIsolated; Ch1:#$0628; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE BEH WITH YEH ISOLATED FORM
    (Unicode:#$FC0B; Attr:daIsolated; Ch1:#$062A; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE TEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC0C; Attr:daIsolated; Ch1:#$062A; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE TEH WITH HAH ISOLATED FORM
    (Unicode:#$FC0D; Attr:daIsolated; Ch1:#$062A; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE TEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC0E; Attr:daIsolated; Ch1:#$062A; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE TEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC0F; Attr:daIsolated; Ch1:#$062A; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE TEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC10; Attr:daIsolated; Ch1:#$062A; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE TEH WITH YEH ISOLATED FORM
    (Unicode:#$FC11; Attr:daIsolated; Ch1:#$062B; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE THEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC12; Attr:daIsolated; Ch1:#$062B; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE THEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC13; Attr:daIsolated; Ch1:#$062B; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE THEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC14; Attr:daIsolated; Ch1:#$062B; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE THEH WITH YEH ISOLATED FORM
    (Unicode:#$FC15; Attr:daIsolated; Ch1:#$062C; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE JEEM WITH HAH ISOLATED FORM
    (Unicode:#$FC16; Attr:daIsolated; Ch1:#$062C; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE JEEM WITH MEEM ISOLATED FORM
    (Unicode:#$FC17; Attr:daIsolated; Ch1:#$062D; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE HAH WITH JEEM ISOLATED FORM
    (Unicode:#$FC18; Attr:daIsolated; Ch1:#$062D; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE HAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC19; Attr:daIsolated; Ch1:#$062E; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE KHAH WITH JEEM ISOLATED FORM
    (Unicode:#$FC1A; Attr:daIsolated; Ch1:#$062E; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE KHAH WITH HAH ISOLATED FORM
    (Unicode:#$FC1B; Attr:daIsolated; Ch1:#$062E; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE KHAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC1C; Attr:daIsolated; Ch1:#$0633; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH JEEM ISOLATED FORM
    (Unicode:#$FC1D; Attr:daIsolated; Ch1:#$0633; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH HAH ISOLATED FORM
    (Unicode:#$FC1E; Attr:daIsolated; Ch1:#$0633; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH KHAH ISOLATED FORM
    (Unicode:#$FC1F; Attr:daIsolated; Ch1:#$0633; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH MEEM ISOLATED FORM
    (Unicode:#$FC20; Attr:daIsolated; Ch1:#$0635; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE SAD WITH HAH ISOLATED FORM
    (Unicode:#$FC21; Attr:daIsolated; Ch1:#$0635; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE SAD WITH MEEM ISOLATED FORM
    (Unicode:#$FC22; Attr:daIsolated; Ch1:#$0636; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH JEEM ISOLATED FORM
    (Unicode:#$FC23; Attr:daIsolated; Ch1:#$0636; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH HAH ISOLATED FORM
    (Unicode:#$FC24; Attr:daIsolated; Ch1:#$0636; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH KHAH ISOLATED FORM
    (Unicode:#$FC25; Attr:daIsolated; Ch1:#$0636; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH MEEM ISOLATED FORM
    (Unicode:#$FC26; Attr:daIsolated; Ch1:#$0637; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE TAH WITH HAH ISOLATED FORM
    (Unicode:#$FC27; Attr:daIsolated; Ch1:#$0637; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE TAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC28; Attr:daIsolated; Ch1:#$0638; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE ZAH WITH MEEM ISOLATED FORM
    (Unicode:#$FC29; Attr:daIsolated; Ch1:#$0639; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE AIN WITH JEEM ISOLATED FORM
    (Unicode:#$FC2A; Attr:daIsolated; Ch1:#$0639; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE AIN WITH MEEM ISOLATED FORM
    (Unicode:#$FC2B; Attr:daIsolated; Ch1:#$063A; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE GHAIN WITH JEEM ISOLATED FORM
    (Unicode:#$FC2C; Attr:daIsolated; Ch1:#$063A; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE GHAIN WITH MEEM ISOLATED FORM
    (Unicode:#$FC2D; Attr:daIsolated; Ch1:#$0641; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE FEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC2E; Attr:daIsolated; Ch1:#$0641; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE FEH WITH HAH ISOLATED FORM
    (Unicode:#$FC2F; Attr:daIsolated; Ch1:#$0641; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE FEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC30; Attr:daIsolated; Ch1:#$0641; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE FEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC31; Attr:daIsolated; Ch1:#$0641; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE FEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC32; Attr:daIsolated; Ch1:#$0641; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE FEH WITH YEH ISOLATED FORM
    (Unicode:#$FC33; Attr:daIsolated; Ch1:#$0642; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE QAF WITH HAH ISOLATED FORM
    (Unicode:#$FC34; Attr:daIsolated; Ch1:#$0642; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE QAF WITH MEEM ISOLATED FORM
    (Unicode:#$FC35; Attr:daIsolated; Ch1:#$0642; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE QAF WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC36; Attr:daIsolated; Ch1:#$0642; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE QAF WITH YEH ISOLATED FORM
    (Unicode:#$FC37; Attr:daIsolated; Ch1:#$0643; Ch2:#$0627; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH ALEF ISOLATED FORM
    (Unicode:#$FC38; Attr:daIsolated; Ch1:#$0643; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH JEEM ISOLATED FORM
    (Unicode:#$FC39; Attr:daIsolated; Ch1:#$0643; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH HAH ISOLATED FORM
    (Unicode:#$FC3A; Attr:daIsolated; Ch1:#$0643; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH KHAH ISOLATED FORM
    (Unicode:#$FC3B; Attr:daIsolated; Ch1:#$0643; Ch2:#$0644; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH LAM ISOLATED FORM
    (Unicode:#$FC3C; Attr:daIsolated; Ch1:#$0643; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH MEEM ISOLATED FORM
    (Unicode:#$FC3D; Attr:daIsolated; Ch1:#$0643; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC3E; Attr:daIsolated; Ch1:#$0643; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE KAF WITH YEH ISOLATED FORM
    (Unicode:#$FC3F; Attr:daIsolated; Ch1:#$0644; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH JEEM ISOLATED FORM
    (Unicode:#$FC40; Attr:daIsolated; Ch1:#$0644; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH HAH ISOLATED FORM
    (Unicode:#$FC41; Attr:daIsolated; Ch1:#$0644; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH KHAH ISOLATED FORM
    (Unicode:#$FC42; Attr:daIsolated; Ch1:#$0644; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH MEEM ISOLATED FORM
    (Unicode:#$FC43; Attr:daIsolated; Ch1:#$0644; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC44; Attr:daIsolated; Ch1:#$0644; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH YEH ISOLATED FORM
    (Unicode:#$FC45; Attr:daIsolated; Ch1:#$0645; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE MEEM WITH JEEM ISOLATED FORM
    (Unicode:#$FC46; Attr:daIsolated; Ch1:#$0645; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE MEEM WITH HAH ISOLATED FORM
    (Unicode:#$FC47; Attr:daIsolated; Ch1:#$0645; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE MEEM WITH KHAH ISOLATED FORM
    (Unicode:#$FC48; Attr:daIsolated; Ch1:#$0645; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE MEEM WITH MEEM ISOLATED FORM
    (Unicode:#$FC49; Attr:daIsolated; Ch1:#$0645; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE MEEM WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC4A; Attr:daIsolated; Ch1:#$0645; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE MEEM WITH YEH ISOLATED FORM
    (Unicode:#$FC4B; Attr:daIsolated; Ch1:#$0646; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE NOON WITH JEEM ISOLATED FORM
    (Unicode:#$FC4C; Attr:daIsolated; Ch1:#$0646; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE NOON WITH HAH ISOLATED FORM
    (Unicode:#$FC4D; Attr:daIsolated; Ch1:#$0646; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE NOON WITH KHAH ISOLATED FORM
    (Unicode:#$FC4E; Attr:daIsolated; Ch1:#$0646; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE NOON WITH MEEM ISOLATED FORM
    (Unicode:#$FC4F; Attr:daIsolated; Ch1:#$0646; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE NOON WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC50; Attr:daIsolated; Ch1:#$0646; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE NOON WITH YEH ISOLATED FORM
    (Unicode:#$FC51; Attr:daIsolated; Ch1:#$0647; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE HEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC52; Attr:daIsolated; Ch1:#$0647; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE HEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC53; Attr:daIsolated; Ch1:#$0647; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE HEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC54; Attr:daIsolated; Ch1:#$0647; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE HEH WITH YEH ISOLATED FORM
    (Unicode:#$FC55; Attr:daIsolated; Ch1:#$064A; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH JEEM ISOLATED FORM
    (Unicode:#$FC56; Attr:daIsolated; Ch1:#$064A; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH HAH ISOLATED FORM
    (Unicode:#$FC57; Attr:daIsolated; Ch1:#$064A; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH KHAH ISOLATED FORM
    (Unicode:#$FC58; Attr:daIsolated; Ch1:#$064A; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH MEEM ISOLATED FORM
    (Unicode:#$FC59; Attr:daIsolated; Ch1:#$064A; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FC5A; Attr:daIsolated; Ch1:#$064A; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE YEH WITH YEH ISOLATED FORM
    (Unicode:#$FC5B; Attr:daIsolated; Ch1:#$0630; Ch2:#$0670; Ch3:#$FFFF),// ARABIC LIGATURE THAL WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC5C; Attr:daIsolated; Ch1:#$0631; Ch2:#$0670; Ch3:#$FFFF),// ARABIC LIGATURE REH WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC5D; Attr:daIsolated; Ch1:#$0649; Ch2:#$0670; Ch3:#$FFFF),// ARABIC LIGATURE ALEF MAKSURA WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC5E; Attr:daIsolated; Ch1:#$0020; Ch2:#$064C; Ch3:#$0651; Ch4:#$FFFF),  // ARABIC LIGATURE SHADDA WITH DAMMATAN ISOLATED FORM
    (Unicode:#$FC5F; Attr:daIsolated; Ch1:#$0020; Ch2:#$064D; Ch3:#$0651; Ch4:#$FFFF),  // ARABIC LIGATURE SHADDA WITH KASRATAN ISOLATED FORM
    (Unicode:#$FC60; Attr:daIsolated; Ch1:#$0020; Ch2:#$064E; Ch3:#$0651; Ch4:#$FFFF),  // ARABIC LIGATURE SHADDA WITH FATHA ISOLATED FORM
    (Unicode:#$FC61; Attr:daIsolated; Ch1:#$0020; Ch2:#$064F; Ch3:#$0651; Ch4:#$FFFF),  // ARABIC LIGATURE SHADDA WITH DAMMA ISOLATED FORM
    (Unicode:#$FC62; Attr:daIsolated; Ch1:#$0020; Ch2:#$0650; Ch3:#$0651; Ch4:#$FFFF),  // ARABIC LIGATURE SHADDA WITH KASRA ISOLATED FORM
    (Unicode:#$FC63; Attr:daIsolated; Ch1:#$0020; Ch2:#$0651; Ch3:#$0670; Ch4:#$FFFF),  // ARABIC LIGATURE SHADDA WITH SUPERSCRIPT ALEF ISOLATED FORM
    (Unicode:#$FC64; Attr:daFinal; Ch1:#$0626; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH REH FINAL FORM
    (Unicode:#$FC65; Attr:daFinal; Ch1:#$0626; Ch2:#$0632; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ZAIN FINAL FORM
    (Unicode:#$FC66; Attr:daFinal; Ch1:#$0626; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM FINAL FORM
    (Unicode:#$FC67; Attr:daFinal; Ch1:#$0626; Ch2:#$0646; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH NOON FINAL FORM
    (Unicode:#$FC68; Attr:daFinal; Ch1:#$0626; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC69; Attr:daFinal; Ch1:#$0626; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH YEH FINAL FORM
    (Unicode:#$FC6A; Attr:daFinal; Ch1:#$0628; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE BEH WITH REH FINAL FORM
    (Unicode:#$FC6B; Attr:daFinal; Ch1:#$0628; Ch2:#$0632; Ch3:#$FFFF),   // ARABIC LIGATURE BEH WITH ZAIN FINAL FORM
    (Unicode:#$FC6C; Attr:daFinal; Ch1:#$0628; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE BEH WITH MEEM FINAL FORM
    (Unicode:#$FC6D; Attr:daFinal; Ch1:#$0628; Ch2:#$0646; Ch3:#$FFFF),   // ARABIC LIGATURE BEH WITH NOON FINAL FORM
    (Unicode:#$FC6E; Attr:daFinal; Ch1:#$0628; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE BEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC6F; Attr:daFinal; Ch1:#$0628; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE BEH WITH YEH FINAL FORM
    (Unicode:#$FC70; Attr:daFinal; Ch1:#$062A; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE TEH WITH REH FINAL FORM
    (Unicode:#$FC71; Attr:daFinal; Ch1:#$062A; Ch2:#$0632; Ch3:#$FFFF),   // ARABIC LIGATURE TEH WITH ZAIN FINAL FORM
    (Unicode:#$FC72; Attr:daFinal; Ch1:#$062A; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE TEH WITH MEEM FINAL FORM
    (Unicode:#$FC73; Attr:daFinal; Ch1:#$062A; Ch2:#$0646; Ch3:#$FFFF),   // ARABIC LIGATURE TEH WITH NOON FINAL FORM
    (Unicode:#$FC74; Attr:daFinal; Ch1:#$062A; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE TEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC75; Attr:daFinal; Ch1:#$062A; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE TEH WITH YEH FINAL FORM
    (Unicode:#$FC76; Attr:daFinal; Ch1:#$062B; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE THEH WITH REH FINAL FORM
    (Unicode:#$FC77; Attr:daFinal; Ch1:#$062B; Ch2:#$0632; Ch3:#$FFFF),   // ARABIC LIGATURE THEH WITH ZAIN FINAL FORM
    (Unicode:#$FC78; Attr:daFinal; Ch1:#$062B; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE THEH WITH MEEM FINAL FORM
    (Unicode:#$FC79; Attr:daFinal; Ch1:#$062B; Ch2:#$0646; Ch3:#$FFFF),   // ARABIC LIGATURE THEH WITH NOON FINAL FORM
    (Unicode:#$FC7A; Attr:daFinal; Ch1:#$062B; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE THEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC7B; Attr:daFinal; Ch1:#$062B; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE THEH WITH YEH FINAL FORM
    (Unicode:#$FC7C; Attr:daFinal; Ch1:#$0641; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE FEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC7D; Attr:daFinal; Ch1:#$0641; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE FEH WITH YEH FINAL FORM
    (Unicode:#$FC7E; Attr:daFinal; Ch1:#$0642; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE QAF WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC7F; Attr:daFinal; Ch1:#$0642; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE QAF WITH YEH FINAL FORM
    (Unicode:#$FC80; Attr:daFinal; Ch1:#$0643; Ch2:#$0627; Ch3:#$FFFF),   // ARABIC LIGATURE KAF WITH ALEF FINAL FORM
    (Unicode:#$FC81; Attr:daFinal; Ch1:#$0643; Ch2:#$0644; Ch3:#$FFFF),   // ARABIC LIGATURE KAF WITH LAM FINAL FORM
    (Unicode:#$FC82; Attr:daFinal; Ch1:#$0643; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE KAF WITH MEEM FINAL FORM
    (Unicode:#$FC83; Attr:daFinal; Ch1:#$0643; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE KAF WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC84; Attr:daFinal; Ch1:#$0643; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE KAF WITH YEH FINAL FORM
    (Unicode:#$FC85; Attr:daFinal; Ch1:#$0644; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH MEEM FINAL FORM
    (Unicode:#$FC86; Attr:daFinal; Ch1:#$0644; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC87; Attr:daFinal; Ch1:#$0644; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH YEH FINAL FORM
    (Unicode:#$FC88; Attr:daFinal; Ch1:#$0645; Ch2:#$0627; Ch3:#$FFFF),   // ARABIC LIGATURE MEEM WITH ALEF FINAL FORM
    (Unicode:#$FC89; Attr:daFinal; Ch1:#$0645; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE MEEM WITH MEEM FINAL FORM
    (Unicode:#$FC8A; Attr:daFinal; Ch1:#$0646; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE NOON WITH REH FINAL FORM
    (Unicode:#$FC8B; Attr:daFinal; Ch1:#$0646; Ch2:#$0632; Ch3:#$FFFF),   // ARABIC LIGATURE NOON WITH ZAIN FINAL FORM
    (Unicode:#$FC8C; Attr:daFinal; Ch1:#$0646; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE NOON WITH MEEM FINAL FORM
    (Unicode:#$FC8D; Attr:daFinal; Ch1:#$0646; Ch2:#$0646; Ch3:#$FFFF),   // ARABIC LIGATURE NOON WITH NOON FINAL FORM
    (Unicode:#$FC8E; Attr:daFinal; Ch1:#$0646; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE NOON WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC8F; Attr:daFinal; Ch1:#$0646; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE NOON WITH YEH FINAL FORM
    (Unicode:#$FC90; Attr:daFinal; Ch1:#$0649; Ch2:#$0670; Ch3:#$FFFF),   // ARABIC LIGATURE ALEF MAKSURA WITH SUPERSCRIPT ALEF FINAL FORM
    (Unicode:#$FC91; Attr:daFinal; Ch1:#$064A; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH REH FINAL FORM
    (Unicode:#$FC92; Attr:daFinal; Ch1:#$064A; Ch2:#$0632; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH ZAIN FINAL FORM
    (Unicode:#$FC93; Attr:daFinal; Ch1:#$064A; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH MEEM FINAL FORM
    (Unicode:#$FC94; Attr:daFinal; Ch1:#$064A; Ch2:#$0646; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH NOON FINAL FORM
    (Unicode:#$FC95; Attr:daFinal; Ch1:#$064A; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FC96; Attr:daFinal; Ch1:#$064A; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE YEH WITH YEH FINAL FORM
    (Unicode:#$FC97; Attr:daInitial; Ch1:#$0626; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH JEEM INITIAL FORM
    (Unicode:#$FC98; Attr:daInitial; Ch1:#$0626; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HAH INITIAL FORM
    (Unicode:#$FC99; Attr:daInitial; Ch1:#$0626; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH KHAH INITIAL FORM
    (Unicode:#$FC9A; Attr:daInitial; Ch1:#$0626; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM INITIAL FORM
    (Unicode:#$FC9B; Attr:daInitial; Ch1:#$0626; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HEH INITIAL FORM
    (Unicode:#$FC9C; Attr:daInitial; Ch1:#$0628; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE BEH WITH JEEM INITIAL FORM
    (Unicode:#$FC9D; Attr:daInitial; Ch1:#$0628; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE BEH WITH HAH INITIAL FORM
    (Unicode:#$FC9E; Attr:daInitial; Ch1:#$0628; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE BEH WITH KHAH INITIAL FORM
    (Unicode:#$FC9F; Attr:daInitial; Ch1:#$0628; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE BEH WITH MEEM INITIAL FORM
    (Unicode:#$FCA0; Attr:daInitial; Ch1:#$0628; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE BEH WITH HEH INITIAL FORM
    (Unicode:#$FCA1; Attr:daInitial; Ch1:#$062A; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE TEH WITH JEEM INITIAL FORM
    (Unicode:#$FCA2; Attr:daInitial; Ch1:#$062A; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE TEH WITH HAH INITIAL FORM
    (Unicode:#$FCA3; Attr:daInitial; Ch1:#$062A; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE TEH WITH KHAH INITIAL FORM
    (Unicode:#$FCA4; Attr:daInitial; Ch1:#$062A; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE TEH WITH MEEM INITIAL FORM
    (Unicode:#$FCA5; Attr:daInitial; Ch1:#$062A; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE TEH WITH HEH INITIAL FORM
    (Unicode:#$FCA6; Attr:daInitial; Ch1:#$062B; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE THEH WITH MEEM INITIAL FORM
    (Unicode:#$FCA7; Attr:daInitial; Ch1:#$062C; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE JEEM WITH HAH INITIAL FORM
    (Unicode:#$FCA8; Attr:daInitial; Ch1:#$062C; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FCA9; Attr:daInitial; Ch1:#$062D; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE HAH WITH JEEM INITIAL FORM
    (Unicode:#$FCAA; Attr:daInitial; Ch1:#$062D; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE HAH WITH MEEM INITIAL FORM
    (Unicode:#$FCAB; Attr:daInitial; Ch1:#$062E; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE KHAH WITH JEEM INITIAL FORM
    (Unicode:#$FCAC; Attr:daInitial; Ch1:#$062E; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FCAD; Attr:daInitial; Ch1:#$0633; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE SEEN WITH JEEM INITIAL FORM
    (Unicode:#$FCAE; Attr:daInitial; Ch1:#$0633; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE SEEN WITH HAH INITIAL FORM
    (Unicode:#$FCAF; Attr:daInitial; Ch1:#$0633; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE SEEN WITH KHAH INITIAL FORM
    (Unicode:#$FCB0; Attr:daInitial; Ch1:#$0633; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE SEEN WITH MEEM INITIAL FORM
    (Unicode:#$FCB1; Attr:daInitial; Ch1:#$0635; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE SAD WITH HAH INITIAL FORM
    (Unicode:#$FCB2; Attr:daInitial; Ch1:#$0635; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE SAD WITH KHAH INITIAL FORM
    (Unicode:#$FCB3; Attr:daInitial; Ch1:#$0635; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE SAD WITH MEEM INITIAL FORM
    (Unicode:#$FCB4; Attr:daInitial; Ch1:#$0636; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE DAD WITH JEEM INITIAL FORM
    (Unicode:#$FCB5; Attr:daInitial; Ch1:#$0636; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE DAD WITH HAH INITIAL FORM
    (Unicode:#$FCB6; Attr:daInitial; Ch1:#$0636; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE DAD WITH KHAH INITIAL FORM
    (Unicode:#$FCB7; Attr:daInitial; Ch1:#$0636; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE DAD WITH MEEM INITIAL FORM
    (Unicode:#$FCB8; Attr:daInitial; Ch1:#$0637; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE TAH WITH HAH INITIAL FORM
    (Unicode:#$FCB9; Attr:daInitial; Ch1:#$0638; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE ZAH WITH MEEM INITIAL FORM
    (Unicode:#$FCBA; Attr:daInitial; Ch1:#$0639; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE AIN WITH JEEM INITIAL FORM
    (Unicode:#$FCBB; Attr:daInitial; Ch1:#$0639; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE AIN WITH MEEM INITIAL FORM
    (Unicode:#$FCBC; Attr:daInitial; Ch1:#$063A; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE GHAIN WITH JEEM INITIAL FORM
    (Unicode:#$FCBD; Attr:daInitial; Ch1:#$063A; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE GHAIN WITH MEEM INITIAL FORM
    (Unicode:#$FCBE; Attr:daInitial; Ch1:#$0641; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE FEH WITH JEEM INITIAL FORM
    (Unicode:#$FCBF; Attr:daInitial; Ch1:#$0641; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE FEH WITH HAH INITIAL FORM
    (Unicode:#$FCC0; Attr:daInitial; Ch1:#$0641; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE FEH WITH KHAH INITIAL FORM
    (Unicode:#$FCC1; Attr:daInitial; Ch1:#$0641; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE FEH WITH MEEM INITIAL FORM
    (Unicode:#$FCC2; Attr:daInitial; Ch1:#$0642; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE QAF WITH HAH INITIAL FORM
    (Unicode:#$FCC3; Attr:daInitial; Ch1:#$0642; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE QAF WITH MEEM INITIAL FORM
    (Unicode:#$FCC4; Attr:daInitial; Ch1:#$0643; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE KAF WITH JEEM INITIAL FORM
    (Unicode:#$FCC5; Attr:daInitial; Ch1:#$0643; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE KAF WITH HAH INITIAL FORM
    (Unicode:#$FCC6; Attr:daInitial; Ch1:#$0643; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE KAF WITH KHAH INITIAL FORM
    (Unicode:#$FCC7; Attr:daInitial; Ch1:#$0643; Ch2:#$0644; Ch3:#$FFFF), // ARABIC LIGATURE KAF WITH LAM INITIAL FORM
    (Unicode:#$FCC8; Attr:daInitial; Ch1:#$0643; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE KAF WITH MEEM INITIAL FORM
    (Unicode:#$FCC9; Attr:daInitial; Ch1:#$0644; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE LAM WITH JEEM INITIAL FORM
    (Unicode:#$FCCA; Attr:daInitial; Ch1:#$0644; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE LAM WITH HAH INITIAL FORM
    (Unicode:#$FCCB; Attr:daInitial; Ch1:#$0644; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE LAM WITH KHAH INITIAL FORM
    (Unicode:#$FCCC; Attr:daInitial; Ch1:#$0644; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE LAM WITH MEEM INITIAL FORM
    (Unicode:#$FCCD; Attr:daInitial; Ch1:#$0644; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE LAM WITH HEH INITIAL FORM
    (Unicode:#$FCCE; Attr:daInitial; Ch1:#$0645; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FCCF; Attr:daInitial; Ch1:#$0645; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE MEEM WITH HAH INITIAL FORM
    (Unicode:#$FCD0; Attr:daInitial; Ch1:#$0645; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE MEEM WITH KHAH INITIAL FORM
    (Unicode:#$FCD1; Attr:daInitial; Ch1:#$0645; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FCD2; Attr:daInitial; Ch1:#$0646; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE NOON WITH JEEM INITIAL FORM
    (Unicode:#$FCD3; Attr:daInitial; Ch1:#$0646; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE NOON WITH HAH INITIAL FORM
    (Unicode:#$FCD4; Attr:daInitial; Ch1:#$0646; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE NOON WITH KHAH INITIAL FORM
    (Unicode:#$FCD5; Attr:daInitial; Ch1:#$0646; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE NOON WITH MEEM INITIAL FORM
    (Unicode:#$FCD6; Attr:daInitial; Ch1:#$0646; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE NOON WITH HEH INITIAL FORM
    (Unicode:#$FCD7; Attr:daInitial; Ch1:#$0647; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE HEH WITH JEEM INITIAL FORM
    (Unicode:#$FCD8; Attr:daInitial; Ch1:#$0647; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE HEH WITH MEEM INITIAL FORM
    (Unicode:#$FCD9; Attr:daInitial; Ch1:#$0647; Ch2:#$0670; Ch3:#$FFFF), // ARABIC LIGATURE HEH WITH SUPERSCRIPT ALEF INITIAL FORM
    (Unicode:#$FCDA; Attr:daInitial; Ch1:#$064A; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH JEEM INITIAL FORM
    (Unicode:#$FCDB; Attr:daInitial; Ch1:#$064A; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HAH INITIAL FORM
    (Unicode:#$FCDC; Attr:daInitial; Ch1:#$064A; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH KHAH INITIAL FORM
    (Unicode:#$FCDD; Attr:daInitial; Ch1:#$064A; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH MEEM INITIAL FORM
    (Unicode:#$FCDE; Attr:daInitial; Ch1:#$064A; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE YEH WITH HEH INITIAL FORM
    (Unicode:#$FCDF; Attr:daMedial; Ch1:#$0626; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH MEEM MEDIAL FORM
    (Unicode:#$FCE0; Attr:daMedial; Ch1:#$0626; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE YEH WITH HAMZA ABOVE WITH HEH MEDIAL FORM
    (Unicode:#$FCE1; Attr:daMedial; Ch1:#$0628; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE BEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCE2; Attr:daMedial; Ch1:#$0628; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE BEH WITH HEH MEDIAL FORM
    (Unicode:#$FCE3; Attr:daMedial; Ch1:#$062A; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE TEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCE4; Attr:daMedial; Ch1:#$062A; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE TEH WITH HEH MEDIAL FORM
    (Unicode:#$FCE5; Attr:daMedial; Ch1:#$062B; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE THEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCE6; Attr:daMedial; Ch1:#$062B; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE THEH WITH HEH MEDIAL FORM
    (Unicode:#$FCE7; Attr:daMedial; Ch1:#$0633; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE SEEN WITH MEEM MEDIAL FORM
    (Unicode:#$FCE8; Attr:daMedial; Ch1:#$0633; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE SEEN WITH HEH MEDIAL FORM
    (Unicode:#$FCE9; Attr:daMedial; Ch1:#$0634; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE SHEEN WITH MEEM MEDIAL FORM
    (Unicode:#$FCEA; Attr:daMedial; Ch1:#$0634; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE SHEEN WITH HEH MEDIAL FORM
    (Unicode:#$FCEB; Attr:daMedial; Ch1:#$0643; Ch2:#$0644; Ch3:#$FFFF),  // ARABIC LIGATURE KAF WITH LAM MEDIAL FORM
    (Unicode:#$FCEC; Attr:daMedial; Ch1:#$0643; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE KAF WITH MEEM MEDIAL FORM
    (Unicode:#$FCED; Attr:daMedial; Ch1:#$0644; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE LAM WITH MEEM MEDIAL FORM
    (Unicode:#$FCEE; Attr:daMedial; Ch1:#$0646; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE NOON WITH MEEM MEDIAL FORM
    (Unicode:#$FCEF; Attr:daMedial; Ch1:#$0646; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE NOON WITH HEH MEDIAL FORM
    (Unicode:#$FCF0; Attr:daMedial; Ch1:#$064A; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE YEH WITH MEEM MEDIAL FORM
    (Unicode:#$FCF1; Attr:daMedial; Ch1:#$064A; Ch2:#$0647; Ch3:#$FFFF),  // ARABIC LIGATURE YEH WITH HEH MEDIAL FORM
    (Unicode:#$FCF2; Attr:daMedial; Ch1:#$0640; Ch2:#$064E; Ch3:#$0651; Ch4:#$FFFF),   // ARABIC LIGATURE SHADDA WITH FATHA MEDIAL FORM
    (Unicode:#$FCF3; Attr:daMedial; Ch1:#$0640; Ch2:#$064F; Ch3:#$0651; Ch4:#$FFFF),   // ARABIC LIGATURE SHADDA WITH DAMMA MEDIAL FORM
    (Unicode:#$FCF4; Attr:daMedial; Ch1:#$0640; Ch2:#$0650; Ch3:#$0651; Ch4:#$FFFF),   // ARABIC LIGATURE SHADDA WITH KASRA MEDIAL FORM
    (Unicode:#$FCF5; Attr:daIsolated; Ch1:#$0637; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE TAH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCF6; Attr:daIsolated; Ch1:#$0637; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE TAH WITH YEH ISOLATED FORM
    (Unicode:#$FCF7; Attr:daIsolated; Ch1:#$0639; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE AIN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCF8; Attr:daIsolated; Ch1:#$0639; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE AIN WITH YEH ISOLATED FORM
    (Unicode:#$FCF9; Attr:daIsolated; Ch1:#$063A; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE GHAIN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCFA; Attr:daIsolated; Ch1:#$063A; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE GHAIN WITH YEH ISOLATED FORM
    (Unicode:#$FCFB; Attr:daIsolated; Ch1:#$0633; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCFC; Attr:daIsolated; Ch1:#$0633; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH YEH ISOLATED FORM
    (Unicode:#$FCFD; Attr:daIsolated; Ch1:#$0634; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FCFE; Attr:daIsolated; Ch1:#$0634; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH YEH ISOLATED FORM
    (Unicode:#$FCFF; Attr:daIsolated; Ch1:#$062D; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE HAH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD00; Attr:daIsolated; Ch1:#$062D; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE HAH WITH YEH ISOLATED FORM
    (Unicode:#$FD01; Attr:daIsolated; Ch1:#$062C; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE JEEM WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD02; Attr:daIsolated; Ch1:#$062C; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE JEEM WITH YEH ISOLATED FORM
    (Unicode:#$FD03; Attr:daIsolated; Ch1:#$062E; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE KHAH WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD04; Attr:daIsolated; Ch1:#$062E; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE KHAH WITH YEH ISOLATED FORM
    (Unicode:#$FD05; Attr:daIsolated; Ch1:#$0635; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE SAD WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD06; Attr:daIsolated; Ch1:#$0635; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE SAD WITH YEH ISOLATED FORM
    (Unicode:#$FD07; Attr:daIsolated; Ch1:#$0636; Ch2:#$0649; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FD08; Attr:daIsolated; Ch1:#$0636; Ch2:#$064A; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH YEH ISOLATED FORM
    (Unicode:#$FD09; Attr:daIsolated; Ch1:#$0634; Ch2:#$062C; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH JEEM ISOLATED FORM
    (Unicode:#$FD0A; Attr:daIsolated; Ch1:#$0634; Ch2:#$062D; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH HAH ISOLATED FORM
    (Unicode:#$FD0B; Attr:daIsolated; Ch1:#$0634; Ch2:#$062E; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH KHAH ISOLATED FORM
    (Unicode:#$FD0C; Attr:daIsolated; Ch1:#$0634; Ch2:#$0645; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH MEEM ISOLATED FORM
    (Unicode:#$FD0D; Attr:daIsolated; Ch1:#$0634; Ch2:#$0631; Ch3:#$FFFF),// ARABIC LIGATURE SHEEN WITH REH ISOLATED FORM
    (Unicode:#$FD0E; Attr:daIsolated; Ch1:#$0633; Ch2:#$0631; Ch3:#$FFFF),// ARABIC LIGATURE SEEN WITH REH ISOLATED FORM
    (Unicode:#$FD0F; Attr:daIsolated; Ch1:#$0635; Ch2:#$0631; Ch3:#$FFFF),// ARABIC LIGATURE SAD WITH REH ISOLATED FORM
    (Unicode:#$FD10; Attr:daIsolated; Ch1:#$0636; Ch2:#$0631; Ch3:#$FFFF),// ARABIC LIGATURE DAD WITH REH ISOLATED FORM
    (Unicode:#$FD11; Attr:daFinal; Ch1:#$0637; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE TAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD12; Attr:daFinal; Ch1:#$0637; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE TAH WITH YEH FINAL FORM
    (Unicode:#$FD13; Attr:daFinal; Ch1:#$0639; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE AIN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD14; Attr:daFinal; Ch1:#$0639; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE AIN WITH YEH FINAL FORM
    (Unicode:#$FD15; Attr:daFinal; Ch1:#$063A; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE GHAIN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD16; Attr:daFinal; Ch1:#$063A; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE GHAIN WITH YEH FINAL FORM
    (Unicode:#$FD17; Attr:daFinal; Ch1:#$0633; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE SEEN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD18; Attr:daFinal; Ch1:#$0633; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE SEEN WITH YEH FINAL FORM
    (Unicode:#$FD19; Attr:daFinal; Ch1:#$0634; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD1A; Attr:daFinal; Ch1:#$0634; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH YEH FINAL FORM
    (Unicode:#$FD1B; Attr:daFinal; Ch1:#$062D; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD1C; Attr:daFinal; Ch1:#$062D; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE HAH WITH YEH FINAL FORM
    (Unicode:#$FD1D; Attr:daFinal; Ch1:#$062C; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD1E; Attr:daFinal; Ch1:#$062C; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE JEEM WITH YEH FINAL FORM
    (Unicode:#$FD1F; Attr:daFinal; Ch1:#$062E; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE KHAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD20; Attr:daFinal; Ch1:#$062E; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE KHAH WITH YEH FINAL FORM
    (Unicode:#$FD21; Attr:daFinal; Ch1:#$0635; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE SAD WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD22; Attr:daFinal; Ch1:#$0635; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE SAD WITH YEH FINAL FORM
    (Unicode:#$FD23; Attr:daFinal; Ch1:#$0636; Ch2:#$0649; Ch3:#$FFFF),   // ARABIC LIGATURE DAD WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD24; Attr:daFinal; Ch1:#$0636; Ch2:#$064A; Ch3:#$FFFF),   // ARABIC LIGATURE DAD WITH YEH FINAL FORM
    (Unicode:#$FD25; Attr:daFinal; Ch1:#$0634; Ch2:#$062C; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH JEEM FINAL FORM
    (Unicode:#$FD26; Attr:daFinal; Ch1:#$0634; Ch2:#$062D; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH HAH FINAL FORM
    (Unicode:#$FD27; Attr:daFinal; Ch1:#$0634; Ch2:#$062E; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH KHAH FINAL FORM
    (Unicode:#$FD28; Attr:daFinal; Ch1:#$0634; Ch2:#$0645; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH MEEM FINAL FORM
    (Unicode:#$FD29; Attr:daFinal; Ch1:#$0634; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE SHEEN WITH REH FINAL FORM
    (Unicode:#$FD2A; Attr:daFinal; Ch1:#$0633; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE SEEN WITH REH FINAL FORM
    (Unicode:#$FD2B; Attr:daFinal; Ch1:#$0635; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE SAD WITH REH FINAL FORM
    (Unicode:#$FD2C; Attr:daFinal; Ch1:#$0636; Ch2:#$0631; Ch3:#$FFFF),   // ARABIC LIGATURE DAD WITH REH FINAL FORM
    (Unicode:#$FD2D; Attr:daInitial; Ch1:#$0634; Ch2:#$062C; Ch3:#$FFFF), // ARABIC LIGATURE SHEEN WITH JEEM INITIAL FORM
    (Unicode:#$FD2E; Attr:daInitial; Ch1:#$0634; Ch2:#$062D; Ch3:#$FFFF), // ARABIC LIGATURE SHEEN WITH HAH INITIAL FORM
    (Unicode:#$FD2F; Attr:daInitial; Ch1:#$0634; Ch2:#$062E; Ch3:#$FFFF), // ARABIC LIGATURE SHEEN WITH KHAH INITIAL FORM
    (Unicode:#$FD30; Attr:daInitial; Ch1:#$0634; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE SHEEN WITH MEEM INITIAL FORM
    (Unicode:#$FD31; Attr:daInitial; Ch1:#$0633; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE SEEN WITH HEH INITIAL FORM
    (Unicode:#$FD32; Attr:daInitial; Ch1:#$0634; Ch2:#$0647; Ch3:#$FFFF), // ARABIC LIGATURE SHEEN WITH HEH INITIAL FORM
    (Unicode:#$FD33; Attr:daInitial; Ch1:#$0637; Ch2:#$0645; Ch3:#$FFFF), // ARABIC LIGATURE TAH WITH MEEM INITIAL FORM
    (Unicode:#$FD34; Attr:daMedial; Ch1:#$0633; Ch2:#$062C; Ch3:#$FFFF),  // ARABIC LIGATURE SEEN WITH JEEM MEDIAL FORM
    (Unicode:#$FD35; Attr:daMedial; Ch1:#$0633; Ch2:#$062D; Ch3:#$FFFF),  // ARABIC LIGATURE SEEN WITH HAH MEDIAL FORM
    (Unicode:#$FD36; Attr:daMedial; Ch1:#$0633; Ch2:#$062E; Ch3:#$FFFF),  // ARABIC LIGATURE SEEN WITH KHAH MEDIAL FORM
    (Unicode:#$FD37; Attr:daMedial; Ch1:#$0634; Ch2:#$062C; Ch3:#$FFFF),  // ARABIC LIGATURE SHEEN WITH JEEM MEDIAL FORM
    (Unicode:#$FD38; Attr:daMedial; Ch1:#$0634; Ch2:#$062D; Ch3:#$FFFF),  // ARABIC LIGATURE SHEEN WITH HAH MEDIAL FORM
    (Unicode:#$FD39; Attr:daMedial; Ch1:#$0634; Ch2:#$062E; Ch3:#$FFFF),  // ARABIC LIGATURE SHEEN WITH KHAH MEDIAL FORM
    (Unicode:#$FD3A; Attr:daMedial; Ch1:#$0637; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE TAH WITH MEEM MEDIAL FORM
    (Unicode:#$FD3B; Attr:daMedial; Ch1:#$0638; Ch2:#$0645; Ch3:#$FFFF),  // ARABIC LIGATURE ZAH WITH MEEM MEDIAL FORM
    (Unicode:#$FD3C; Attr:daFinal; Ch1:#$0627; Ch2:#$064B; Ch3:#$FFFF),   // ARABIC LIGATURE ALEF WITH FATHATAN FINAL FORM
    (Unicode:#$FD3D; Attr:daIsolated; Ch1:#$0627; Ch2:#$064B; Ch3:#$FFFF),// ARABIC LIGATURE ALEF WITH FATHATAN ISOLATED FORM
    (Unicode:#$FD50; Attr:daInitial; Ch1:#$062A; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD51; Attr:daFinal; Ch1:#$062A; Ch2:#$062D; Ch3:#$062C; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH HAH WITH JEEM FINAL FORM
    (Unicode:#$FD52; Attr:daInitial; Ch1:#$062A; Ch2:#$062D; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH HAH WITH JEEM INITIAL FORM
    (Unicode:#$FD53; Attr:daInitial; Ch1:#$062A; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD54; Attr:daInitial; Ch1:#$062A; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD55; Attr:daInitial; Ch1:#$062A; Ch2:#$0645; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD56; Attr:daInitial; Ch1:#$062A; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD57; Attr:daInitial; Ch1:#$062A; Ch2:#$0645; Ch3:#$062E; Ch4:#$FFFF),  // ARABIC LIGATURE TEH WITH MEEM WITH KHAH INITIAL FORM
    (Unicode:#$FD58; Attr:daFinal; Ch1:#$062C; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE JEEM WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD59; Attr:daInitial; Ch1:#$062C; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE JEEM WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD5A; Attr:daFinal; Ch1:#$062D; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE HAH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD5B; Attr:daFinal; Ch1:#$062D; Ch2:#$0645; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE HAH WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD5C; Attr:daInitial; Ch1:#$0633; Ch2:#$062D; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE SEEN WITH HAH WITH JEEM INITIAL FORM
    (Unicode:#$FD5D; Attr:daInitial; Ch1:#$0633; Ch2:#$062C; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE SEEN WITH JEEM WITH HAH INITIAL FORM
    (Unicode:#$FD5E; Attr:daFinal; Ch1:#$0633; Ch2:#$062C; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE SEEN WITH JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD5F; Attr:daFinal; Ch1:#$0633; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE SEEN WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD60; Attr:daInitial; Ch1:#$0633; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE SEEN WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD61; Attr:daInitial; Ch1:#$0633; Ch2:#$0645; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE SEEN WITH MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD62; Attr:daFinal; Ch1:#$0633; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE SEEN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD63; Attr:daInitial; Ch1:#$0633; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE SEEN WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD64; Attr:daFinal; Ch1:#$0635; Ch2:#$062D; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE SAD WITH HAH WITH HAH FINAL FORM
    (Unicode:#$FD65; Attr:daInitial; Ch1:#$0635; Ch2:#$062D; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE SAD WITH HAH WITH HAH INITIAL FORM
    (Unicode:#$FD66; Attr:daFinal; Ch1:#$0635; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE SAD WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD67; Attr:daFinal; Ch1:#$0634; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE SHEEN WITH HAH WITH MEEM FINAL FORM
    (Unicode:#$FD68; Attr:daInitial; Ch1:#$0634; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE SHEEN WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD69; Attr:daFinal; Ch1:#$0634; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE SHEEN WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FD6A; Attr:daFinal; Ch1:#$0634; Ch2:#$0645; Ch3:#$062E; Ch4:#$FFFF),    // ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH FINAL FORM
    (Unicode:#$FD6B; Attr:daInitial; Ch1:#$0634; Ch2:#$0645; Ch3:#$062E; Ch4:#$FFFF),  // ARABIC LIGATURE SHEEN WITH MEEM WITH KHAH INITIAL FORM
    (Unicode:#$FD6C; Attr:daFinal; Ch1:#$0634; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD6D; Attr:daInitial; Ch1:#$0634; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE SHEEN WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD6E; Attr:daFinal; Ch1:#$0636; Ch2:#$062D; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE DAD WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD6F; Attr:daFinal; Ch1:#$0636; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE DAD WITH KHAH WITH MEEM FINAL FORM
    (Unicode:#$FD70; Attr:daInitial; Ch1:#$0636; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE DAD WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD71; Attr:daFinal; Ch1:#$0637; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE TAH WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD72; Attr:daInitial; Ch1:#$0637; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE TAH WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD73; Attr:daInitial; Ch1:#$0637; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE TAH WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD74; Attr:daFinal; Ch1:#$0637; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE TAH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD75; Attr:daFinal; Ch1:#$0639; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE AIN WITH JEEM WITH MEEM FINAL FORM
    (Unicode:#$FD76; Attr:daFinal; Ch1:#$0639; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE AIN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD77; Attr:daInitial; Ch1:#$0639; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE AIN WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD78; Attr:daFinal; Ch1:#$0639; Ch2:#$0645; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE AIN WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD79; Attr:daFinal; Ch1:#$063A; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE GHAIN WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD7A; Attr:daFinal; Ch1:#$063A; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE GHAIN WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD7B; Attr:daFinal; Ch1:#$063A; Ch2:#$0645; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE GHAIN WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD7C; Attr:daFinal; Ch1:#$0641; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE FEH WITH KHAH WITH MEEM FINAL FORM
    (Unicode:#$FD7D; Attr:daInitial; Ch1:#$0641; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE FEH WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD7E; Attr:daFinal; Ch1:#$0642; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE QAF WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD7F; Attr:daFinal; Ch1:#$0642; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE QAF WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD80; Attr:daFinal; Ch1:#$0644; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH HAH WITH MEEM FINAL FORM
    (Unicode:#$FD81; Attr:daFinal; Ch1:#$0644; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FD82; Attr:daFinal; Ch1:#$0644; Ch2:#$062D; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD83; Attr:daInitial; Ch1:#$0644; Ch2:#$062C; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE LAM WITH JEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD84; Attr:daFinal; Ch1:#$0644; Ch2:#$062C; Ch3:#$062C; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH JEEM WITH JEEM FINAL FORM
    (Unicode:#$FD85; Attr:daFinal; Ch1:#$0644; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH KHAH WITH MEEM FINAL FORM
    (Unicode:#$FD86; Attr:daInitial; Ch1:#$0644; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE LAM WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD87; Attr:daFinal; Ch1:#$0644; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH MEEM WITH HAH FINAL FORM
    (Unicode:#$FD88; Attr:daInitial; Ch1:#$0644; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE LAM WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FD89; Attr:daInitial; Ch1:#$0645; Ch2:#$062D; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH HAH WITH JEEM INITIAL FORM
    (Unicode:#$FD8A; Attr:daInitial; Ch1:#$0645; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD8B; Attr:daFinal; Ch1:#$0645; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE MEEM WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FD8C; Attr:daInitial; Ch1:#$0645; Ch2:#$062C; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH JEEM WITH HAH INITIAL FORM
    (Unicode:#$FD8D; Attr:daInitial; Ch1:#$0645; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD8E; Attr:daInitial; Ch1:#$0645; Ch2:#$062E; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH KHAH WITH JEEM INITIAL FORM
    (Unicode:#$FD8F; Attr:daInitial; Ch1:#$0645; Ch2:#$062E; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH KHAH WITH MEEM INITIAL FORM
    (Unicode:#$FD92; Attr:daInitial; Ch1:#$0645; Ch2:#$062C; Ch3:#$062E; Ch4:#$FFFF),  // ARABIC LIGATURE MEEM WITH JEEM WITH KHAH INITIAL FORM
    (Unicode:#$FD93; Attr:daInitial; Ch1:#$0647; Ch2:#$0645; Ch3:#$062C; Ch4:#$FFFF),  // ARABIC LIGATURE HEH WITH MEEM WITH JEEM INITIAL FORM
    (Unicode:#$FD94; Attr:daInitial; Ch1:#$0647; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE HEH WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD95; Attr:daInitial; Ch1:#$0646; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE NOON WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FD96; Attr:daFinal; Ch1:#$0646; Ch2:#$062D; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD97; Attr:daFinal; Ch1:#$0646; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH JEEM WITH MEEM FINAL FORM
    (Unicode:#$FD98; Attr:daInitial; Ch1:#$0646; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE NOON WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD99; Attr:daFinal; Ch1:#$0646; Ch2:#$062C; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD9A; Attr:daFinal; Ch1:#$0646; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FD9B; Attr:daFinal; Ch1:#$0646; Ch2:#$0645; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FD9C; Attr:daFinal; Ch1:#$064A; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE YEH WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FD9D; Attr:daInitial; Ch1:#$064A; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE YEH WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FD9E; Attr:daFinal; Ch1:#$0628; Ch2:#$062E; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE BEH WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FD9F; Attr:daFinal; Ch1:#$062A; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDA0; Attr:daFinal; Ch1:#$062A; Ch2:#$062C; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH JEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA1; Attr:daFinal; Ch1:#$062A; Ch2:#$062E; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FDA2; Attr:daFinal; Ch1:#$062A; Ch2:#$062E; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH KHAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA3; Attr:daFinal; Ch1:#$062A; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDA4; Attr:daFinal; Ch1:#$062A; Ch2:#$0645; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE TEH WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA5; Attr:daFinal; Ch1:#$062C; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE JEEM WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDA6; Attr:daFinal; Ch1:#$062C; Ch2:#$062D; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE JEEM WITH HAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA7; Attr:daFinal; Ch1:#$062C; Ch2:#$0645; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE JEEM WITH MEEM WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA8; Attr:daFinal; Ch1:#$0633; Ch2:#$062E; Ch3:#$0649; Ch4:#$FFFF),    // ARABIC LIGATURE SEEN WITH KHAH WITH ALEF MAKSURA FINAL FORM
    (Unicode:#$FDA9; Attr:daFinal; Ch1:#$0635; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE SAD WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAA; Attr:daFinal; Ch1:#$0634; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE SHEEN WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAB; Attr:daFinal; Ch1:#$0636; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE DAD WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAC; Attr:daFinal; Ch1:#$0644; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDAD; Attr:daFinal; Ch1:#$0644; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDAE; Attr:daFinal; Ch1:#$064A; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE YEH WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDAF; Attr:daFinal; Ch1:#$064A; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE YEH WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDB0; Attr:daFinal; Ch1:#$064A; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE YEH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB1; Attr:daFinal; Ch1:#$0645; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE MEEM WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB2; Attr:daFinal; Ch1:#$0642; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE QAF WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB3; Attr:daFinal; Ch1:#$0646; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDB4; Attr:daInitial; Ch1:#$0642; Ch2:#$0645; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE QAF WITH MEEM WITH HAH INITIAL FORM
    (Unicode:#$FDB5; Attr:daInitial; Ch1:#$0644; Ch2:#$062D; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE LAM WITH HAH WITH MEEM INITIAL FORM
    (Unicode:#$FDB6; Attr:daFinal; Ch1:#$0639; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE AIN WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB7; Attr:daFinal; Ch1:#$0643; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE KAF WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDB8; Attr:daInitial; Ch1:#$0646; Ch2:#$062C; Ch3:#$062D; Ch4:#$FFFF),  // ARABIC LIGATURE NOON WITH JEEM WITH HAH INITIAL FORM
    (Unicode:#$FDB9; Attr:daFinal; Ch1:#$0645; Ch2:#$062E; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE MEEM WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FDBA; Attr:daInitial; Ch1:#$0644; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE LAM WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDBB; Attr:daFinal; Ch1:#$0643; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE KAF WITH MEEM WITH MEEM FINAL FORM
    (Unicode:#$FDBC; Attr:daFinal; Ch1:#$0644; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),    // ARABIC LIGATURE LAM WITH JEEM WITH MEEM FINAL FORM
    (Unicode:#$FDBD; Attr:daFinal; Ch1:#$0646; Ch2:#$062C; Ch3:#$062D; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH JEEM WITH HAH FINAL FORM
    (Unicode:#$FDBE; Attr:daFinal; Ch1:#$062C; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE JEEM WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDBF; Attr:daFinal; Ch1:#$062D; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE HAH WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDC0; Attr:daFinal; Ch1:#$0645; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE MEEM WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDC1; Attr:daFinal; Ch1:#$0641; Ch2:#$0645; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE FEH WITH MEEM WITH YEH FINAL FORM
    (Unicode:#$FDC2; Attr:daFinal; Ch1:#$0628; Ch2:#$062D; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE BEH WITH HAH WITH YEH FINAL FORM
    (Unicode:#$FDC3; Attr:daInitial; Ch1:#$0643; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE KAF WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDC4; Attr:daInitial; Ch1:#$0639; Ch2:#$062C; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE AIN WITH JEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDC5; Attr:daInitial; Ch1:#$0635; Ch2:#$0645; Ch3:#$0645; Ch4:#$FFFF),  // ARABIC LIGATURE SAD WITH MEEM WITH MEEM INITIAL FORM
    (Unicode:#$FDC6; Attr:daFinal; Ch1:#$0633; Ch2:#$062E; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE SEEN WITH KHAH WITH YEH FINAL FORM
    (Unicode:#$FDC7; Attr:daFinal; Ch1:#$0646; Ch2:#$062C; Ch3:#$064A; Ch4:#$FFFF),    // ARABIC LIGATURE NOON WITH JEEM WITH YEH FINAL FORM
    (Unicode:#$FDF0; Attr:daIsolated; Ch1:#$0635; Ch2:#$0644; Ch3:#$06D2; Ch4:#$FFFF), // ARABIC LIGATURE SALLA USED AS KORANIC STOP SIGN ISOLATED FORM
    (Unicode:#$FDF1; Attr:daIsolated; Ch1:#$0642; Ch2:#$0644; Ch3:#$06D2; Ch4:#$FFFF), // ARABIC LIGATURE QALA USED AS KORANIC STOP SIGN ISOLATED FORM
    (Unicode:#$FDF2; Attr:daIsolated; Ch1:#$0627; Ch2:#$0644; Ch3:#$0644; Ch4:#$0647; Ch5:#$FFFF),  // ARABIC LIGATURE ALLAH ISOLATED FORM
    (Unicode:#$FDF3; Attr:daIsolated; Ch1:#$0627; Ch2:#$0643; Ch3:#$0628; Ch4:#$0631; Ch5:#$FFFF),  // ARABIC LIGATURE AKBAR ISOLATED FORM
    (Unicode:#$FDF4; Attr:daIsolated; Ch1:#$0645; Ch2:#$062D; Ch3:#$0645; Ch4:#$062F; Ch5:#$FFFF),  // ARABIC LIGATURE MOHAMMAD ISOLATED FORM
    (Unicode:#$FDF5; Attr:daIsolated; Ch1:#$0635; Ch2:#$0644; Ch3:#$0639; Ch4:#$0645; Ch5:#$FFFF),  // ARABIC LIGATURE SALAM ISOLATED FORM
    (Unicode:#$FDF6; Attr:daIsolated; Ch1:#$0631; Ch2:#$0633; Ch3:#$0648; Ch4:#$0644; Ch5:#$FFFF),  // ARABIC LIGATURE RASOUL ISOLATED FORM
    (Unicode:#$FDF7; Attr:daIsolated; Ch1:#$0639; Ch2:#$0644; Ch3:#$064A; Ch4:#$0647; Ch5:#$FFFF),  // ARABIC LIGATURE ALAYHE ISOLATED FORM
    (Unicode:#$FDF8; Attr:daIsolated; Ch1:#$0648; Ch2:#$0633; Ch3:#$0644; Ch4:#$0645; Ch5:#$FFFF),  // ARABIC LIGATURE WASALLAM ISOLATED FORM
    (Unicode:#$FDF9; Attr:daIsolated; Ch1:#$0635; Ch2:#$0644; Ch3:#$0649; Ch4:#$FFFF), // ARABIC LIGATURE SALLA ISOLATED FORM
    (Unicode:#$FE30; Attr:daVertical; Ch1:#$2025; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL TWO DOT LEADER
    (Unicode:#$FE31; Attr:daVertical; Ch1:#$2014; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL EM DASH
    (Unicode:#$FE32; Attr:daVertical; Ch1:#$2013; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL EN DASH
    (Unicode:#$FE33; Attr:daVertical; Ch1:#$005F; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LOW LINE
    (Unicode:#$FE34; Attr:daVertical; Ch1:#$005F; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL WAVY LOW LINE
    (Unicode:#$FE35; Attr:daVertical; Ch1:#$0028; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT PARENTHESIS
    (Unicode:#$FE36; Attr:daVertical; Ch1:#$0029; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT PARENTHESIS
    (Unicode:#$FE37; Attr:daVertical; Ch1:#$007B; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT CURLY BRACKET
    (Unicode:#$FE38; Attr:daVertical; Ch1:#$007D; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT CURLY BRACKET
    (Unicode:#$FE39; Attr:daVertical; Ch1:#$3014; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT TORTOISE SHELL BRACKET
    (Unicode:#$FE3A; Attr:daVertical; Ch1:#$3015; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT TORTOISE SHELL BRACKET
    (Unicode:#$FE3B; Attr:daVertical; Ch1:#$3010; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT BLACK LENTICULAR BRACKET
    (Unicode:#$FE3C; Attr:daVertical; Ch1:#$3011; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT BLACK LENTICULAR BRACKET
    (Unicode:#$FE3D; Attr:daVertical; Ch1:#$300A; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT DOUBLE ANGLE BRACKET
    (Unicode:#$FE3E; Attr:daVertical; Ch1:#$300B; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT DOUBLE ANGLE BRACKET
    (Unicode:#$FE3F; Attr:daVertical; Ch1:#$3008; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT ANGLE BRACKET
    (Unicode:#$FE40; Attr:daVertical; Ch1:#$3009; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT ANGLE BRACKET
    (Unicode:#$FE41; Attr:daVertical; Ch1:#$300C; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT CORNER BRACKET
    (Unicode:#$FE42; Attr:daVertical; Ch1:#$300D; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT CORNER BRACKET
    (Unicode:#$FE43; Attr:daVertical; Ch1:#$300E; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL LEFT WHITE CORNER BRACKET
    (Unicode:#$FE44; Attr:daVertical; Ch1:#$300F; Ch2:#$FFFF),            // PRESENTATION FORM FOR VERTICAL RIGHT WHITE CORNER BRACKET
    (Unicode:#$FE49; Attr:daCompat; Ch1:#$203E; Ch2:#$FFFF),              // DASHED OVERLINE
    (Unicode:#$FE4A; Attr:daCompat; Ch1:#$203E; Ch2:#$FFFF),              // CENTRELINE OVERLINE
    (Unicode:#$FE4B; Attr:daCompat; Ch1:#$203E; Ch2:#$FFFF),              // WAVY OVERLINE
    (Unicode:#$FE4C; Attr:daCompat; Ch1:#$203E; Ch2:#$FFFF),              // DOUBLE WAVY OVERLINE
    (Unicode:#$FE4D; Attr:daCompat; Ch1:#$005F; Ch2:#$FFFF),              // DASHED LOW LINE
    (Unicode:#$FE4E; Attr:daCompat; Ch1:#$005F; Ch2:#$FFFF),              // CENTRELINE LOW LINE
    (Unicode:#$FE4F; Attr:daCompat; Ch1:#$005F; Ch2:#$FFFF),              // WAVY LOW LINE
    (Unicode:#$FE50; Attr:daSmall; Ch1:#$002C; Ch2:#$FFFF),               // SMALL COMMA
    (Unicode:#$FE51; Attr:daSmall; Ch1:#$3001; Ch2:#$FFFF),               // SMALL IDEOGRAPHIC COMMA
    (Unicode:#$FE52; Attr:daSmall; Ch1:#$002E; Ch2:#$FFFF),               // SMALL FULL STOP
    (Unicode:#$FE54; Attr:daSmall; Ch1:#$003B; Ch2:#$FFFF),               // SMALL SEMICOLON
    (Unicode:#$FE55; Attr:daSmall; Ch1:#$003A; Ch2:#$FFFF),               // SMALL COLON
    (Unicode:#$FE56; Attr:daSmall; Ch1:#$003F; Ch2:#$FFFF),               // SMALL QUESTION MARK
    (Unicode:#$FE57; Attr:daSmall; Ch1:#$0021; Ch2:#$FFFF),               // SMALL EXCLAMATION MARK
    (Unicode:#$FE58; Attr:daSmall; Ch1:#$2014; Ch2:#$FFFF),               // SMALL EM DASH
    (Unicode:#$FE59; Attr:daSmall; Ch1:#$0028; Ch2:#$FFFF),               // SMALL LEFT PARENTHESIS
    (Unicode:#$FE5A; Attr:daSmall; Ch1:#$0029; Ch2:#$FFFF),               // SMALL RIGHT PARENTHESIS
    (Unicode:#$FE5B; Attr:daSmall; Ch1:#$007B; Ch2:#$FFFF),               // SMALL LEFT CURLY BRACKET
    (Unicode:#$FE5C; Attr:daSmall; Ch1:#$007D; Ch2:#$FFFF),               // SMALL RIGHT CURLY BRACKET
    (Unicode:#$FE5D; Attr:daSmall; Ch1:#$3014; Ch2:#$FFFF),               // SMALL LEFT TORTOISE SHELL BRACKET
    (Unicode:#$FE5E; Attr:daSmall; Ch1:#$3015; Ch2:#$FFFF),               // SMALL RIGHT TORTOISE SHELL BRACKET
    (Unicode:#$FE5F; Attr:daSmall; Ch1:#$0023; Ch2:#$FFFF),               // SMALL NUMBER SIGN
    (Unicode:#$FE60; Attr:daSmall; Ch1:#$0026; Ch2:#$FFFF),               // SMALL AMPERSAND
    (Unicode:#$FE61; Attr:daSmall; Ch1:#$002A; Ch2:#$FFFF),               // SMALL ASTERISK
    (Unicode:#$FE62; Attr:daSmall; Ch1:#$002B; Ch2:#$FFFF),               // SMALL PLUS SIGN
    (Unicode:#$FE63; Attr:daSmall; Ch1:#$002D; Ch2:#$FFFF),               // SMALL HYPHEN-MINUS
    (Unicode:#$FE64; Attr:daSmall; Ch1:#$003C; Ch2:#$FFFF),               // SMALL LESS-THAN SIGN
    (Unicode:#$FE65; Attr:daSmall; Ch1:#$003E; Ch2:#$FFFF),               // SMALL GREATER-THAN SIGN
    (Unicode:#$FE66; Attr:daSmall; Ch1:#$003D; Ch2:#$FFFF),               // SMALL EQUALS SIGN
    (Unicode:#$FE68; Attr:daSmall; Ch1:#$005C; Ch2:#$FFFF),               // SMALL REVERSE SOLIDUS
    (Unicode:#$FE69; Attr:daSmall; Ch1:#$0024; Ch2:#$FFFF),               // SMALL DOLLAR SIGN
    (Unicode:#$FE6A; Attr:daSmall; Ch1:#$0025; Ch2:#$FFFF),               // SMALL PERCENT SIGN
    (Unicode:#$FE6B; Attr:daSmall; Ch1:#$0040; Ch2:#$FFFF),               // SMALL COMMERCIAL AT
    (Unicode:#$FE70; Attr:daIsolated; Ch1:#$0020; Ch2:#$064B; Ch3:#$FFFF),// ARABIC FATHATAN ISOLATED FORM
    (Unicode:#$FE71; Attr:daMedial; Ch1:#$0640; Ch2:#$064B; Ch3:#$FFFF),  // ARABIC TATWEEL WITH FATHATAN ABOVE
    (Unicode:#$FE72; Attr:daIsolated; Ch1:#$0020; Ch2:#$064C; Ch3:#$FFFF),// ARABIC DAMMATAN ISOLATED FORM
    (Unicode:#$FE74; Attr:daIsolated; Ch1:#$0020; Ch2:#$064D; Ch3:#$FFFF),// ARABIC KASRATAN ISOLATED FORM
    (Unicode:#$FE76; Attr:daIsolated; Ch1:#$0020; Ch2:#$064E; Ch3:#$FFFF),// ARABIC FATHA ISOLATED FORM
    (Unicode:#$FE77; Attr:daMedial; Ch1:#$0640; Ch2:#$064E; Ch3:#$FFFF),  // ARABIC FATHA MEDIAL FORM
    (Unicode:#$FE78; Attr:daIsolated; Ch1:#$0020; Ch2:#$064F; Ch3:#$FFFF),// ARABIC DAMMA ISOLATED FORM
    (Unicode:#$FE79; Attr:daMedial; Ch1:#$0640; Ch2:#$064F; Ch3:#$FFFF),  // ARABIC DAMMA MEDIAL FORM
    (Unicode:#$FE7A; Attr:daIsolated; Ch1:#$0020; Ch2:#$0650; Ch3:#$FFFF),// ARABIC KASRA ISOLATED FORM
    (Unicode:#$FE7B; Attr:daMedial; Ch1:#$0640; Ch2:#$0650; Ch3:#$FFFF),  // ARABIC KASRA MEDIAL FORM
    (Unicode:#$FE7C; Attr:daIsolated; Ch1:#$0020; Ch2:#$0651; Ch3:#$FFFF),// ARABIC SHADDA ISOLATED FORM
    (Unicode:#$FE7D; Attr:daMedial; Ch1:#$0640; Ch2:#$0651; Ch3:#$FFFF),  // ARABIC SHADDA MEDIAL FORM
    (Unicode:#$FE7E; Attr:daIsolated; Ch1:#$0020; Ch2:#$0652; Ch3:#$FFFF),// ARABIC SUKUN ISOLATED FORM
    (Unicode:#$FE7F; Attr:daMedial; Ch1:#$0640; Ch2:#$0652; Ch3:#$FFFF),  // ARABIC SUKUN MEDIAL FORM
    (Unicode:#$FE80; Attr:daIsolated; Ch1:#$0621; Ch2:#$FFFF),            // ARABIC LETTER HAMZA ISOLATED FORM
    (Unicode:#$FE81; Attr:daIsolated; Ch1:#$0622; Ch2:#$FFFF),            // ARABIC LETTER ALEF WITH MADDA ABOVE ISOLATED FORM
    (Unicode:#$FE82; Attr:daFinal; Ch1:#$0622; Ch2:#$FFFF),               // ARABIC LETTER ALEF WITH MADDA ABOVE FINAL FORM
    (Unicode:#$FE83; Attr:daIsolated; Ch1:#$0623; Ch2:#$FFFF),            // ARABIC LETTER ALEF WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FE84; Attr:daFinal; Ch1:#$0623; Ch2:#$FFFF),               // ARABIC LETTER ALEF WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FE85; Attr:daIsolated; Ch1:#$0624; Ch2:#$FFFF),            // ARABIC LETTER WAW WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FE86; Attr:daFinal; Ch1:#$0624; Ch2:#$FFFF),               // ARABIC LETTER WAW WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FE87; Attr:daIsolated; Ch1:#$0625; Ch2:#$FFFF),            // ARABIC LETTER ALEF WITH HAMZA BELOW ISOLATED FORM
    (Unicode:#$FE88; Attr:daFinal; Ch1:#$0625; Ch2:#$FFFF),               // ARABIC LETTER ALEF WITH HAMZA BELOW FINAL FORM
    (Unicode:#$FE89; Attr:daIsolated; Ch1:#$0626; Ch2:#$FFFF),            // ARABIC LETTER YEH WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FE8A; Attr:daFinal; Ch1:#$0626; Ch2:#$FFFF),               // ARABIC LETTER YEH WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FE8B; Attr:daInitial; Ch1:#$0626; Ch2:#$FFFF),             // ARABIC LETTER YEH WITH HAMZA ABOVE INITIAL FORM
    (Unicode:#$FE8C; Attr:daMedial; Ch1:#$0626; Ch2:#$FFFF),              // ARABIC LETTER YEH WITH HAMZA ABOVE MEDIAL FORM
    (Unicode:#$FE8D; Attr:daIsolated; Ch1:#$0627; Ch2:#$FFFF),            // ARABIC LETTER ALEF ISOLATED FORM
    (Unicode:#$FE8E; Attr:daFinal; Ch1:#$0627; Ch2:#$FFFF),               // ARABIC LETTER ALEF FINAL FORM
    (Unicode:#$FE8F; Attr:daIsolated; Ch1:#$0628; Ch2:#$FFFF),            // ARABIC LETTER BEH ISOLATED FORM
    (Unicode:#$FE90; Attr:daFinal; Ch1:#$0628; Ch2:#$FFFF),               // ARABIC LETTER BEH FINAL FORM
    (Unicode:#$FE91; Attr:daInitial; Ch1:#$0628; Ch2:#$FFFF),             // ARABIC LETTER BEH INITIAL FORM
    (Unicode:#$FE92; Attr:daMedial; Ch1:#$0628; Ch2:#$FFFF),              // ARABIC LETTER BEH MEDIAL FORM
    (Unicode:#$FE93; Attr:daIsolated; Ch1:#$0629; Ch2:#$FFFF),            // ARABIC LETTER TEH MARBUTA ISOLATED FORM
    (Unicode:#$FE94; Attr:daFinal; Ch1:#$0629; Ch2:#$FFFF),               // ARABIC LETTER TEH MARBUTA FINAL FORM
    (Unicode:#$FE95; Attr:daIsolated; Ch1:#$062A; Ch2:#$FFFF),            // ARABIC LETTER TEH ISOLATED FORM
    (Unicode:#$FE96; Attr:daFinal; Ch1:#$062A; Ch2:#$FFFF),               // ARABIC LETTER TEH FINAL FORM
    (Unicode:#$FE97; Attr:daInitial; Ch1:#$062A; Ch2:#$FFFF),             // ARABIC LETTER TEH INITIAL FORM
    (Unicode:#$FE98; Attr:daMedial; Ch1:#$062A; Ch2:#$FFFF),              // ARABIC LETTER TEH MEDIAL FORM
    (Unicode:#$FE99; Attr:daIsolated; Ch1:#$062B; Ch2:#$FFFF),            // ARABIC LETTER THEH ISOLATED FORM
    (Unicode:#$FE9A; Attr:daFinal; Ch1:#$062B; Ch2:#$FFFF),               // ARABIC LETTER THEH FINAL FORM
    (Unicode:#$FE9B; Attr:daInitial; Ch1:#$062B; Ch2:#$FFFF),             // ARABIC LETTER THEH INITIAL FORM
    (Unicode:#$FE9C; Attr:daMedial; Ch1:#$062B; Ch2:#$FFFF),              // ARABIC LETTER THEH MEDIAL FORM
    (Unicode:#$FE9D; Attr:daIsolated; Ch1:#$062C; Ch2:#$FFFF),            // ARABIC LETTER JEEM ISOLATED FORM
    (Unicode:#$FE9E; Attr:daFinal; Ch1:#$062C; Ch2:#$FFFF),               // ARABIC LETTER JEEM FINAL FORM
    (Unicode:#$FE9F; Attr:daInitial; Ch1:#$062C; Ch2:#$FFFF),             // ARABIC LETTER JEEM INITIAL FORM
    (Unicode:#$FEA0; Attr:daMedial; Ch1:#$062C; Ch2:#$FFFF),              // ARABIC LETTER JEEM MEDIAL FORM
    (Unicode:#$FEA1; Attr:daIsolated; Ch1:#$062D; Ch2:#$FFFF),            // ARABIC LETTER HAH ISOLATED FORM
    (Unicode:#$FEA2; Attr:daFinal; Ch1:#$062D; Ch2:#$FFFF),               // ARABIC LETTER HAH FINAL FORM
    (Unicode:#$FEA3; Attr:daInitial; Ch1:#$062D; Ch2:#$FFFF),             // ARABIC LETTER HAH INITIAL FORM
    (Unicode:#$FEA4; Attr:daMedial; Ch1:#$062D; Ch2:#$FFFF),              // ARABIC LETTER HAH MEDIAL FORM
    (Unicode:#$FEA5; Attr:daIsolated; Ch1:#$062E; Ch2:#$FFFF),            // ARABIC LETTER KHAH ISOLATED FORM
    (Unicode:#$FEA6; Attr:daFinal; Ch1:#$062E; Ch2:#$FFFF),               // ARABIC LETTER KHAH FINAL FORM
    (Unicode:#$FEA7; Attr:daInitial; Ch1:#$062E; Ch2:#$FFFF),             // ARABIC LETTER KHAH INITIAL FORM
    (Unicode:#$FEA8; Attr:daMedial; Ch1:#$062E; Ch2:#$FFFF),              // ARABIC LETTER KHAH MEDIAL FORM
    (Unicode:#$FEA9; Attr:daIsolated; Ch1:#$062F; Ch2:#$FFFF),            // ARABIC LETTER DAL ISOLATED FORM
    (Unicode:#$FEAA; Attr:daFinal; Ch1:#$062F; Ch2:#$FFFF),               // ARABIC LETTER DAL FINAL FORM
    (Unicode:#$FEAB; Attr:daIsolated; Ch1:#$0630; Ch2:#$FFFF),            // ARABIC LETTER THAL ISOLATED FORM
    (Unicode:#$FEAC; Attr:daFinal; Ch1:#$0630; Ch2:#$FFFF),               // ARABIC LETTER THAL FINAL FORM
    (Unicode:#$FEAD; Attr:daIsolated; Ch1:#$0631; Ch2:#$FFFF),            // ARABIC LETTER REH ISOLATED FORM
    (Unicode:#$FEAE; Attr:daFinal; Ch1:#$0631; Ch2:#$FFFF),               // ARABIC LETTER REH FINAL FORM
    (Unicode:#$FEAF; Attr:daIsolated; Ch1:#$0632; Ch2:#$FFFF),            // ARABIC LETTER ZAIN ISOLATED FORM
    (Unicode:#$FEB0; Attr:daFinal; Ch1:#$0632; Ch2:#$FFFF),               // ARABIC LETTER ZAIN FINAL FORM
    (Unicode:#$FEB1; Attr:daIsolated; Ch1:#$0633; Ch2:#$FFFF),            // ARABIC LETTER SEEN ISOLATED FORM
    (Unicode:#$FEB2; Attr:daFinal; Ch1:#$0633; Ch2:#$FFFF),               // ARABIC LETTER SEEN FINAL FORM
    (Unicode:#$FEB3; Attr:daInitial; Ch1:#$0633; Ch2:#$FFFF),             // ARABIC LETTER SEEN INITIAL FORM
    (Unicode:#$FEB4; Attr:daMedial; Ch1:#$0633; Ch2:#$FFFF),              // ARABIC LETTER SEEN MEDIAL FORM
    (Unicode:#$FEB5; Attr:daIsolated; Ch1:#$0634; Ch2:#$FFFF),            // ARABIC LETTER SHEEN ISOLATED FORM
    (Unicode:#$FEB6; Attr:daFinal; Ch1:#$0634; Ch2:#$FFFF),               // ARABIC LETTER SHEEN FINAL FORM
    (Unicode:#$FEB7; Attr:daInitial; Ch1:#$0634; Ch2:#$FFFF),             // ARABIC LETTER SHEEN INITIAL FORM
    (Unicode:#$FEB8; Attr:daMedial; Ch1:#$0634; Ch2:#$FFFF),              // ARABIC LETTER SHEEN MEDIAL FORM
    (Unicode:#$FEB9; Attr:daIsolated; Ch1:#$0635; Ch2:#$FFFF),            // ARABIC LETTER SAD ISOLATED FORM
    (Unicode:#$FEBA; Attr:daFinal; Ch1:#$0635; Ch2:#$FFFF),               // ARABIC LETTER SAD FINAL FORM
    (Unicode:#$FEBB; Attr:daInitial; Ch1:#$0635; Ch2:#$FFFF),             // ARABIC LETTER SAD INITIAL FORM
    (Unicode:#$FEBC; Attr:daMedial; Ch1:#$0635; Ch2:#$FFFF),              // ARABIC LETTER SAD MEDIAL FORM
    (Unicode:#$FEBD; Attr:daIsolated; Ch1:#$0636; Ch2:#$FFFF),            // ARABIC LETTER DAD ISOLATED FORM
    (Unicode:#$FEBE; Attr:daFinal; Ch1:#$0636; Ch2:#$FFFF),               // ARABIC LETTER DAD FINAL FORM
    (Unicode:#$FEBF; Attr:daInitial; Ch1:#$0636; Ch2:#$FFFF),             // ARABIC LETTER DAD INITIAL FORM
    (Unicode:#$FEC0; Attr:daMedial; Ch1:#$0636; Ch2:#$FFFF),              // ARABIC LETTER DAD MEDIAL FORM
    (Unicode:#$FEC1; Attr:daIsolated; Ch1:#$0637; Ch2:#$FFFF),            // ARABIC LETTER TAH ISOLATED FORM
    (Unicode:#$FEC2; Attr:daFinal; Ch1:#$0637; Ch2:#$FFFF),               // ARABIC LETTER TAH FINAL FORM
    (Unicode:#$FEC3; Attr:daInitial; Ch1:#$0637; Ch2:#$FFFF),             // ARABIC LETTER TAH INITIAL FORM
    (Unicode:#$FEC4; Attr:daMedial; Ch1:#$0637; Ch2:#$FFFF),              // ARABIC LETTER TAH MEDIAL FORM
    (Unicode:#$FEC5; Attr:daIsolated; Ch1:#$0638; Ch2:#$FFFF),            // ARABIC LETTER ZAH ISOLATED FORM
    (Unicode:#$FEC6; Attr:daFinal; Ch1:#$0638; Ch2:#$FFFF),               // ARABIC LETTER ZAH FINAL FORM
    (Unicode:#$FEC7; Attr:daInitial; Ch1:#$0638; Ch2:#$FFFF),             // ARABIC LETTER ZAH INITIAL FORM
    (Unicode:#$FEC8; Attr:daMedial; Ch1:#$0638; Ch2:#$FFFF),              // ARABIC LETTER ZAH MEDIAL FORM
    (Unicode:#$FEC9; Attr:daIsolated; Ch1:#$0639; Ch2:#$FFFF),            // ARABIC LETTER AIN ISOLATED FORM
    (Unicode:#$FECA; Attr:daFinal; Ch1:#$0639; Ch2:#$FFFF),               // ARABIC LETTER AIN FINAL FORM
    (Unicode:#$FECB; Attr:daInitial; Ch1:#$0639; Ch2:#$FFFF),             // ARABIC LETTER AIN INITIAL FORM
    (Unicode:#$FECC; Attr:daMedial; Ch1:#$0639; Ch2:#$FFFF),              // ARABIC LETTER AIN MEDIAL FORM
    (Unicode:#$FECD; Attr:daIsolated; Ch1:#$063A; Ch2:#$FFFF),            // ARABIC LETTER GHAIN ISOLATED FORM
    (Unicode:#$FECE; Attr:daFinal; Ch1:#$063A; Ch2:#$FFFF),               // ARABIC LETTER GHAIN FINAL FORM
    (Unicode:#$FECF; Attr:daInitial; Ch1:#$063A; Ch2:#$FFFF),             // ARABIC LETTER GHAIN INITIAL FORM
    (Unicode:#$FED0; Attr:daMedial; Ch1:#$063A; Ch2:#$FFFF),              // ARABIC LETTER GHAIN MEDIAL FORM
    (Unicode:#$FED1; Attr:daIsolated; Ch1:#$0641; Ch2:#$FFFF),            // ARABIC LETTER FEH ISOLATED FORM
    (Unicode:#$FED2; Attr:daFinal; Ch1:#$0641; Ch2:#$FFFF),               // ARABIC LETTER FEH FINAL FORM
    (Unicode:#$FED3; Attr:daInitial; Ch1:#$0641; Ch2:#$FFFF),             // ARABIC LETTER FEH INITIAL FORM
    (Unicode:#$FED4; Attr:daMedial; Ch1:#$0641; Ch2:#$FFFF),              // ARABIC LETTER FEH MEDIAL FORM
    (Unicode:#$FED5; Attr:daIsolated; Ch1:#$0642; Ch2:#$FFFF),            // ARABIC LETTER QAF ISOLATED FORM
    (Unicode:#$FED6; Attr:daFinal; Ch1:#$0642; Ch2:#$FFFF),               // ARABIC LETTER QAF FINAL FORM
    (Unicode:#$FED7; Attr:daInitial; Ch1:#$0642; Ch2:#$FFFF),             // ARABIC LETTER QAF INITIAL FORM
    (Unicode:#$FED8; Attr:daMedial; Ch1:#$0642; Ch2:#$FFFF),              // ARABIC LETTER QAF MEDIAL FORM
    (Unicode:#$FED9; Attr:daIsolated; Ch1:#$0643; Ch2:#$FFFF),            // ARABIC LETTER KAF ISOLATED FORM
    (Unicode:#$FEDA; Attr:daFinal; Ch1:#$0643; Ch2:#$FFFF),               // ARABIC LETTER KAF FINAL FORM
    (Unicode:#$FEDB; Attr:daInitial; Ch1:#$0643; Ch2:#$FFFF),             // ARABIC LETTER KAF INITIAL FORM
    (Unicode:#$FEDC; Attr:daMedial; Ch1:#$0643; Ch2:#$FFFF),              // ARABIC LETTER KAF MEDIAL FORM
    (Unicode:#$FEDD; Attr:daIsolated; Ch1:#$0644; Ch2:#$FFFF),            // ARABIC LETTER LAM ISOLATED FORM
    (Unicode:#$FEDE; Attr:daFinal; Ch1:#$0644; Ch2:#$FFFF),               // ARABIC LETTER LAM FINAL FORM
    (Unicode:#$FEDF; Attr:daInitial; Ch1:#$0644; Ch2:#$FFFF),             // ARABIC LETTER LAM INITIAL FORM
    (Unicode:#$FEE0; Attr:daMedial; Ch1:#$0644; Ch2:#$FFFF),              // ARABIC LETTER LAM MEDIAL FORM
    (Unicode:#$FEE1; Attr:daIsolated; Ch1:#$0645; Ch2:#$FFFF),            // ARABIC LETTER MEEM ISOLATED FORM
    (Unicode:#$FEE2; Attr:daFinal; Ch1:#$0645; Ch2:#$FFFF),               // ARABIC LETTER MEEM FINAL FORM
    (Unicode:#$FEE3; Attr:daInitial; Ch1:#$0645; Ch2:#$FFFF),             // ARABIC LETTER MEEM INITIAL FORM
    (Unicode:#$FEE4; Attr:daMedial; Ch1:#$0645; Ch2:#$FFFF),              // ARABIC LETTER MEEM MEDIAL FORM
    (Unicode:#$FEE5; Attr:daIsolated; Ch1:#$0646; Ch2:#$FFFF),            // ARABIC LETTER NOON ISOLATED FORM
    (Unicode:#$FEE6; Attr:daFinal; Ch1:#$0646; Ch2:#$FFFF),               // ARABIC LETTER NOON FINAL FORM
    (Unicode:#$FEE7; Attr:daInitial; Ch1:#$0646; Ch2:#$FFFF),             // ARABIC LETTER NOON INITIAL FORM
    (Unicode:#$FEE8; Attr:daMedial; Ch1:#$0646; Ch2:#$FFFF),              // ARABIC LETTER NOON MEDIAL FORM
    (Unicode:#$FEE9; Attr:daIsolated; Ch1:#$0647; Ch2:#$FFFF),            // ARABIC LETTER HEH ISOLATED FORM
    (Unicode:#$FEEA; Attr:daFinal; Ch1:#$0647; Ch2:#$FFFF),               // ARABIC LETTER HEH FINAL FORM
    (Unicode:#$FEEB; Attr:daInitial; Ch1:#$0647; Ch2:#$FFFF),             // ARABIC LETTER HEH INITIAL FORM
    (Unicode:#$FEEC; Attr:daMedial; Ch1:#$0647; Ch2:#$FFFF),              // ARABIC LETTER HEH MEDIAL FORM
    (Unicode:#$FEED; Attr:daIsolated; Ch1:#$0648; Ch2:#$FFFF),            // ARABIC LETTER WAW ISOLATED FORM
    (Unicode:#$FEEE; Attr:daFinal; Ch1:#$0648; Ch2:#$FFFF),               // ARABIC LETTER WAW FINAL FORM
    (Unicode:#$FEEF; Attr:daIsolated; Ch1:#$0649; Ch2:#$FFFF),            // ARABIC LETTER ALEF MAKSURA ISOLATED FORM
    (Unicode:#$FEF0; Attr:daFinal; Ch1:#$0649; Ch2:#$FFFF),               // ARABIC LETTER ALEF MAKSURA FINAL FORM
    (Unicode:#$FEF1; Attr:daIsolated; Ch1:#$064A; Ch2:#$FFFF),            // ARABIC LETTER YEH ISOLATED FORM
    (Unicode:#$FEF2; Attr:daFinal; Ch1:#$064A; Ch2:#$FFFF),               // ARABIC LETTER YEH FINAL FORM
    (Unicode:#$FEF3; Attr:daInitial; Ch1:#$064A; Ch2:#$FFFF),             // ARABIC LETTER YEH INITIAL FORM
    (Unicode:#$FEF4; Attr:daMedial; Ch1:#$064A; Ch2:#$FFFF),              // ARABIC LETTER YEH MEDIAL FORM
    (Unicode:#$FEF5; Attr:daIsolated; Ch1:#$0644; Ch2:#$0622; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE ISOLATED FORM
    (Unicode:#$FEF6; Attr:daFinal; Ch1:#$0644; Ch2:#$0622; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH ALEF WITH MADDA ABOVE FINAL FORM
    (Unicode:#$FEF7; Attr:daIsolated; Ch1:#$0644; Ch2:#$0623; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE ISOLATED FORM
    (Unicode:#$FEF8; Attr:daFinal; Ch1:#$0644; Ch2:#$0623; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH ALEF WITH HAMZA ABOVE FINAL FORM
    (Unicode:#$FEF9; Attr:daIsolated; Ch1:#$0644; Ch2:#$0625; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW ISOLATED FORM
    (Unicode:#$FEFA; Attr:daFinal; Ch1:#$0644; Ch2:#$0625; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH ALEF WITH HAMZA BELOW FINAL FORM
    (Unicode:#$FEFB; Attr:daIsolated; Ch1:#$0644; Ch2:#$0627; Ch3:#$FFFF),// ARABIC LIGATURE LAM WITH ALEF ISOLATED FORM
    (Unicode:#$FEFC; Attr:daFinal; Ch1:#$0644; Ch2:#$0627; Ch3:#$FFFF),   // ARABIC LIGATURE LAM WITH ALEF FINAL FORM
    (Unicode:#$FF01; Attr:daWide; Ch1:#$0021; Ch2:#$FFFF),                // FULLWIDTH EXCLAMATION MARK
    (Unicode:#$FF02; Attr:daWide; Ch1:#$0022; Ch2:#$FFFF),                // FULLWIDTH QUOTATION MARK
    (Unicode:#$FF03; Attr:daWide; Ch1:#$0023; Ch2:#$FFFF),                // FULLWIDTH NUMBER SIGN
    (Unicode:#$FF04; Attr:daWide; Ch1:#$0024; Ch2:#$FFFF),                // FULLWIDTH DOLLAR SIGN
    (Unicode:#$FF05; Attr:daWide; Ch1:#$0025; Ch2:#$FFFF),                // FULLWIDTH PERCENT SIGN
    (Unicode:#$FF06; Attr:daWide; Ch1:#$0026; Ch2:#$FFFF),                // FULLWIDTH AMPERSAND
    (Unicode:#$FF07; Attr:daWide; Ch1:#$0027; Ch2:#$FFFF),                // FULLWIDTH APOSTROPHE
    (Unicode:#$FF08; Attr:daWide; Ch1:#$0028; Ch2:#$FFFF),                // FULLWIDTH LEFT PARENTHESIS
    (Unicode:#$FF09; Attr:daWide; Ch1:#$0029; Ch2:#$FFFF),                // FULLWIDTH RIGHT PARENTHESIS
    (Unicode:#$FF0A; Attr:daWide; Ch1:#$002A; Ch2:#$FFFF),                // FULLWIDTH ASTERISK
    (Unicode:#$FF0B; Attr:daWide; Ch1:#$002B; Ch2:#$FFFF),                // FULLWIDTH PLUS SIGN
    (Unicode:#$FF0C; Attr:daWide; Ch1:#$002C; Ch2:#$FFFF),                // FULLWIDTH COMMA
    (Unicode:#$FF0D; Attr:daWide; Ch1:#$002D; Ch2:#$FFFF),                // FULLWIDTH HYPHEN-MINUS
    (Unicode:#$FF0E; Attr:daWide; Ch1:#$002E; Ch2:#$FFFF),                // FULLWIDTH FULL STOP
    (Unicode:#$FF0F; Attr:daWide; Ch1:#$002F; Ch2:#$FFFF),                // FULLWIDTH SOLIDUS
    (Unicode:#$FF10; Attr:daWide; Ch1:#$0030; Ch2:#$FFFF),                // FULLWIDTH DIGIT ZERO
    (Unicode:#$FF11; Attr:daWide; Ch1:#$0031; Ch2:#$FFFF),                // FULLWIDTH DIGIT ONE
    (Unicode:#$FF12; Attr:daWide; Ch1:#$0032; Ch2:#$FFFF),                // FULLWIDTH DIGIT TWO
    (Unicode:#$FF13; Attr:daWide; Ch1:#$0033; Ch2:#$FFFF),                // FULLWIDTH DIGIT THREE
    (Unicode:#$FF14; Attr:daWide; Ch1:#$0034; Ch2:#$FFFF),                // FULLWIDTH DIGIT FOUR
    (Unicode:#$FF15; Attr:daWide; Ch1:#$0035; Ch2:#$FFFF),                // FULLWIDTH DIGIT FIVE
    (Unicode:#$FF16; Attr:daWide; Ch1:#$0036; Ch2:#$FFFF),                // FULLWIDTH DIGIT SIX
    (Unicode:#$FF17; Attr:daWide; Ch1:#$0037; Ch2:#$FFFF),                // FULLWIDTH DIGIT SEVEN
    (Unicode:#$FF18; Attr:daWide; Ch1:#$0038; Ch2:#$FFFF),                // FULLWIDTH DIGIT EIGHT
    (Unicode:#$FF19; Attr:daWide; Ch1:#$0039; Ch2:#$FFFF),                // FULLWIDTH DIGIT NINE
    (Unicode:#$FF1A; Attr:daWide; Ch1:#$003A; Ch2:#$FFFF),                // FULLWIDTH COLON
    (Unicode:#$FF1B; Attr:daWide; Ch1:#$003B; Ch2:#$FFFF),                // FULLWIDTH SEMICOLON
    (Unicode:#$FF1C; Attr:daWide; Ch1:#$003C; Ch2:#$FFFF),                // FULLWIDTH LESS-THAN SIGN
    (Unicode:#$FF1D; Attr:daWide; Ch1:#$003D; Ch2:#$FFFF),                // FULLWIDTH EQUALS SIGN
    (Unicode:#$FF1E; Attr:daWide; Ch1:#$003E; Ch2:#$FFFF),                // FULLWIDTH GREATER-THAN SIGN
    (Unicode:#$FF1F; Attr:daWide; Ch1:#$003F; Ch2:#$FFFF),                // FULLWIDTH QUESTION MARK
    (Unicode:#$FF20; Attr:daWide; Ch1:#$0040; Ch2:#$FFFF),                // FULLWIDTH COMMERCIAL AT
    (Unicode:#$FF21; Attr:daWide; Ch1:#$0041; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER A
    (Unicode:#$FF22; Attr:daWide; Ch1:#$0042; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER B
    (Unicode:#$FF23; Attr:daWide; Ch1:#$0043; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER C
    (Unicode:#$FF24; Attr:daWide; Ch1:#$0044; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER D
    (Unicode:#$FF25; Attr:daWide; Ch1:#$0045; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER E
    (Unicode:#$FF26; Attr:daWide; Ch1:#$0046; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER F
    (Unicode:#$FF27; Attr:daWide; Ch1:#$0047; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER G
    (Unicode:#$FF28; Attr:daWide; Ch1:#$0048; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER H
    (Unicode:#$FF29; Attr:daWide; Ch1:#$0049; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER I
    (Unicode:#$FF2A; Attr:daWide; Ch1:#$004A; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER J
    (Unicode:#$FF2B; Attr:daWide; Ch1:#$004B; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER K
    (Unicode:#$FF2C; Attr:daWide; Ch1:#$004C; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER L
    (Unicode:#$FF2D; Attr:daWide; Ch1:#$004D; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER M
    (Unicode:#$FF2E; Attr:daWide; Ch1:#$004E; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER N
    (Unicode:#$FF2F; Attr:daWide; Ch1:#$004F; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER O
    (Unicode:#$FF30; Attr:daWide; Ch1:#$0050; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER P
    (Unicode:#$FF31; Attr:daWide; Ch1:#$0051; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER Q
    (Unicode:#$FF32; Attr:daWide; Ch1:#$0052; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER R
    (Unicode:#$FF33; Attr:daWide; Ch1:#$0053; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER S
    (Unicode:#$FF34; Attr:daWide; Ch1:#$0054; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER T
    (Unicode:#$FF35; Attr:daWide; Ch1:#$0055; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER U
    (Unicode:#$FF36; Attr:daWide; Ch1:#$0056; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER V
    (Unicode:#$FF37; Attr:daWide; Ch1:#$0057; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER W
    (Unicode:#$FF38; Attr:daWide; Ch1:#$0058; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER X
    (Unicode:#$FF39; Attr:daWide; Ch1:#$0059; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER Y
    (Unicode:#$FF3A; Attr:daWide; Ch1:#$005A; Ch2:#$FFFF),                // FULLWIDTH LATIN CAPITAL LETTER Z
    (Unicode:#$FF3B; Attr:daWide; Ch1:#$005B; Ch2:#$FFFF),                // FULLWIDTH LEFT SQUARE BRACKET
    (Unicode:#$FF3C; Attr:daWide; Ch1:#$005C; Ch2:#$FFFF),                // FULLWIDTH REVERSE SOLIDUS
    (Unicode:#$FF3D; Attr:daWide; Ch1:#$005D; Ch2:#$FFFF),                // FULLWIDTH RIGHT SQUARE BRACKET
    (Unicode:#$FF3E; Attr:daWide; Ch1:#$005E; Ch2:#$FFFF),                // FULLWIDTH CIRCUMFLEX ACCENT
    (Unicode:#$FF3F; Attr:daWide; Ch1:#$005F; Ch2:#$FFFF),                // FULLWIDTH LOW LINE
    (Unicode:#$FF40; Attr:daWide; Ch1:#$0060; Ch2:#$FFFF),                // FULLWIDTH GRAVE ACCENT
    (Unicode:#$FF41; Attr:daWide; Ch1:#$0061; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER A
    (Unicode:#$FF42; Attr:daWide; Ch1:#$0062; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER B
    (Unicode:#$FF43; Attr:daWide; Ch1:#$0063; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER C
    (Unicode:#$FF44; Attr:daWide; Ch1:#$0064; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER D
    (Unicode:#$FF45; Attr:daWide; Ch1:#$0065; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER E
    (Unicode:#$FF46; Attr:daWide; Ch1:#$0066; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER F
    (Unicode:#$FF47; Attr:daWide; Ch1:#$0067; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER G
    (Unicode:#$FF48; Attr:daWide; Ch1:#$0068; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER H
    (Unicode:#$FF49; Attr:daWide; Ch1:#$0069; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER I
    (Unicode:#$FF4A; Attr:daWide; Ch1:#$006A; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER J
    (Unicode:#$FF4B; Attr:daWide; Ch1:#$006B; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER K
    (Unicode:#$FF4C; Attr:daWide; Ch1:#$006C; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER L
    (Unicode:#$FF4D; Attr:daWide; Ch1:#$006D; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER M
    (Unicode:#$FF4E; Attr:daWide; Ch1:#$006E; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER N
    (Unicode:#$FF4F; Attr:daWide; Ch1:#$006F; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER O
    (Unicode:#$FF50; Attr:daWide; Ch1:#$0070; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER P
    (Unicode:#$FF51; Attr:daWide; Ch1:#$0071; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER Q
    (Unicode:#$FF52; Attr:daWide; Ch1:#$0072; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER R
    (Unicode:#$FF53; Attr:daWide; Ch1:#$0073; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER S
    (Unicode:#$FF54; Attr:daWide; Ch1:#$0074; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER T
    (Unicode:#$FF55; Attr:daWide; Ch1:#$0075; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER U
    (Unicode:#$FF56; Attr:daWide; Ch1:#$0076; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER V
    (Unicode:#$FF57; Attr:daWide; Ch1:#$0077; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER W
    (Unicode:#$FF58; Attr:daWide; Ch1:#$0078; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER X
    (Unicode:#$FF59; Attr:daWide; Ch1:#$0079; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER Y
    (Unicode:#$FF5A; Attr:daWide; Ch1:#$007A; Ch2:#$FFFF),                // FULLWIDTH LATIN SMALL LETTER Z
    (Unicode:#$FF5B; Attr:daWide; Ch1:#$007B; Ch2:#$FFFF),                // FULLWIDTH LEFT CURLY BRACKET
    (Unicode:#$FF5C; Attr:daWide; Ch1:#$007C; Ch2:#$FFFF),                // FULLWIDTH VERTICAL LINE
    (Unicode:#$FF5D; Attr:daWide; Ch1:#$007D; Ch2:#$FFFF),                // FULLWIDTH RIGHT CURLY BRACKET
    (Unicode:#$FF5E; Attr:daWide; Ch1:#$007E; Ch2:#$FFFF),                // FULLWIDTH TILDE
    (Unicode:#$FF61; Attr:daNarrow; Ch1:#$3002; Ch2:#$FFFF),              // HALFWIDTH IDEOGRAPHIC FULL STOP
    (Unicode:#$FF62; Attr:daNarrow; Ch1:#$300C; Ch2:#$FFFF),              // HALFWIDTH LEFT CORNER BRACKET
    (Unicode:#$FF63; Attr:daNarrow; Ch1:#$300D; Ch2:#$FFFF),              // HALFWIDTH RIGHT CORNER BRACKET
    (Unicode:#$FF64; Attr:daNarrow; Ch1:#$3001; Ch2:#$FFFF),              // HALFWIDTH IDEOGRAPHIC COMMA
    (Unicode:#$FF65; Attr:daNarrow; Ch1:#$30FB; Ch2:#$FFFF),              // HALFWIDTH KATAKANA MIDDLE DOT
    (Unicode:#$FF66; Attr:daNarrow; Ch1:#$30F2; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER WO
    (Unicode:#$FF67; Attr:daNarrow; Ch1:#$30A1; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL A
    (Unicode:#$FF68; Attr:daNarrow; Ch1:#$30A3; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL I
    (Unicode:#$FF69; Attr:daNarrow; Ch1:#$30A5; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL U
    (Unicode:#$FF6A; Attr:daNarrow; Ch1:#$30A7; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL E
    (Unicode:#$FF6B; Attr:daNarrow; Ch1:#$30A9; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL O
    (Unicode:#$FF6C; Attr:daNarrow; Ch1:#$30E3; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL YA
    (Unicode:#$FF6D; Attr:daNarrow; Ch1:#$30E5; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL YU
    (Unicode:#$FF6E; Attr:daNarrow; Ch1:#$30E7; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL YO
    (Unicode:#$FF6F; Attr:daNarrow; Ch1:#$30C3; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SMALL TU
    (Unicode:#$FF70; Attr:daNarrow; Ch1:#$30FC; Ch2:#$FFFF),              // HALFWIDTH KATAKANA-HIRAGANA PROLONGED SOUND MARK
    (Unicode:#$FF71; Attr:daNarrow; Ch1:#$30A2; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER A
    (Unicode:#$FF72; Attr:daNarrow; Ch1:#$30A4; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER I
    (Unicode:#$FF73; Attr:daNarrow; Ch1:#$30A6; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER U
    (Unicode:#$FF74; Attr:daNarrow; Ch1:#$30A8; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER E
    (Unicode:#$FF75; Attr:daNarrow; Ch1:#$30AA; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER O
    (Unicode:#$FF76; Attr:daNarrow; Ch1:#$30AB; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER KA
    (Unicode:#$FF77; Attr:daNarrow; Ch1:#$30AD; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER KI
    (Unicode:#$FF78; Attr:daNarrow; Ch1:#$30AF; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER KU
    (Unicode:#$FF79; Attr:daNarrow; Ch1:#$30B1; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER KE
    (Unicode:#$FF7A; Attr:daNarrow; Ch1:#$30B3; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER KO
    (Unicode:#$FF7B; Attr:daNarrow; Ch1:#$30B5; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SA
    (Unicode:#$FF7C; Attr:daNarrow; Ch1:#$30B7; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SI
    (Unicode:#$FF7D; Attr:daNarrow; Ch1:#$30B9; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SU
    (Unicode:#$FF7E; Attr:daNarrow; Ch1:#$30BB; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SE
    (Unicode:#$FF7F; Attr:daNarrow; Ch1:#$30BD; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER SO
    (Unicode:#$FF80; Attr:daNarrow; Ch1:#$30BF; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER TA
    (Unicode:#$FF81; Attr:daNarrow; Ch1:#$30C1; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER TI
    (Unicode:#$FF82; Attr:daNarrow; Ch1:#$30C4; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER TU
    (Unicode:#$FF83; Attr:daNarrow; Ch1:#$30C6; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER TE
    (Unicode:#$FF84; Attr:daNarrow; Ch1:#$30C8; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER TO
    (Unicode:#$FF85; Attr:daNarrow; Ch1:#$30CA; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER NA
    (Unicode:#$FF86; Attr:daNarrow; Ch1:#$30CB; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER NI
    (Unicode:#$FF87; Attr:daNarrow; Ch1:#$30CC; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER NU
    (Unicode:#$FF88; Attr:daNarrow; Ch1:#$30CD; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER NE
    (Unicode:#$FF89; Attr:daNarrow; Ch1:#$30CE; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER NO
    (Unicode:#$FF8A; Attr:daNarrow; Ch1:#$30CF; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER HA
    (Unicode:#$FF8B; Attr:daNarrow; Ch1:#$30D2; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER HI
    (Unicode:#$FF8C; Attr:daNarrow; Ch1:#$30D5; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER HU
    (Unicode:#$FF8D; Attr:daNarrow; Ch1:#$30D8; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER HE
    (Unicode:#$FF8E; Attr:daNarrow; Ch1:#$30DB; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER HO
    (Unicode:#$FF8F; Attr:daNarrow; Ch1:#$30DE; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER MA
    (Unicode:#$FF90; Attr:daNarrow; Ch1:#$30DF; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER MI
    (Unicode:#$FF91; Attr:daNarrow; Ch1:#$30E0; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER MU
    (Unicode:#$FF92; Attr:daNarrow; Ch1:#$30E1; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER ME
    (Unicode:#$FF93; Attr:daNarrow; Ch1:#$30E2; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER MO
    (Unicode:#$FF94; Attr:daNarrow; Ch1:#$30E4; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER YA
    (Unicode:#$FF95; Attr:daNarrow; Ch1:#$30E6; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER YU
    (Unicode:#$FF96; Attr:daNarrow; Ch1:#$30E8; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER YO
    (Unicode:#$FF97; Attr:daNarrow; Ch1:#$30E9; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER RA
    (Unicode:#$FF98; Attr:daNarrow; Ch1:#$30EA; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER RI
    (Unicode:#$FF99; Attr:daNarrow; Ch1:#$30EB; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER RU
    (Unicode:#$FF9A; Attr:daNarrow; Ch1:#$30EC; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER RE
    (Unicode:#$FF9B; Attr:daNarrow; Ch1:#$30ED; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER RO
    (Unicode:#$FF9C; Attr:daNarrow; Ch1:#$30EF; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER WA
    (Unicode:#$FF9D; Attr:daNarrow; Ch1:#$30F3; Ch2:#$FFFF),              // HALFWIDTH KATAKANA LETTER N
    (Unicode:#$FF9E; Attr:daNarrow; Ch1:#$3099; Ch2:#$FFFF),              // HALFWIDTH KATAKANA VOICED SOUND MARK
    (Unicode:#$FF9F; Attr:daNarrow; Ch1:#$309A; Ch2:#$FFFF),              // HALFWIDTH KATAKANA SEMI-VOICED SOUND MARK
    (Unicode:#$FFA0; Attr:daNarrow; Ch1:#$3164; Ch2:#$FFFF),              // HALFWIDTH HANGUL FILLER
    (Unicode:#$FFA1; Attr:daNarrow; Ch1:#$3131; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER KIYEOK
    (Unicode:#$FFA2; Attr:daNarrow; Ch1:#$3132; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER SSANGKIYEOK
    (Unicode:#$FFA3; Attr:daNarrow; Ch1:#$3133; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER KIYEOK-SIOS
    (Unicode:#$FFA4; Attr:daNarrow; Ch1:#$3134; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER NIEUN
    (Unicode:#$FFA5; Attr:daNarrow; Ch1:#$3135; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER NIEUN-CIEUC
    (Unicode:#$FFA6; Attr:daNarrow; Ch1:#$3136; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER NIEUN-HIEUH
    (Unicode:#$FFA7; Attr:daNarrow; Ch1:#$3137; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER TIKEUT
    (Unicode:#$FFA8; Attr:daNarrow; Ch1:#$3138; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER SSANGTIKEUT
    (Unicode:#$FFA9; Attr:daNarrow; Ch1:#$3139; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL
    (Unicode:#$FFAA; Attr:daNarrow; Ch1:#$313A; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-KIYEOK
    (Unicode:#$FFAB; Attr:daNarrow; Ch1:#$313B; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-MIEUM
    (Unicode:#$FFAC; Attr:daNarrow; Ch1:#$313C; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-PIEUP
    (Unicode:#$FFAD; Attr:daNarrow; Ch1:#$313D; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-SIOS
    (Unicode:#$FFAE; Attr:daNarrow; Ch1:#$313E; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-THIEUTH
    (Unicode:#$FFAF; Attr:daNarrow; Ch1:#$313F; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-PHIEUPH
    (Unicode:#$FFB0; Attr:daNarrow; Ch1:#$3140; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER RIEUL-HIEUH
    (Unicode:#$FFB1; Attr:daNarrow; Ch1:#$3141; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER MIEUM
    (Unicode:#$FFB2; Attr:daNarrow; Ch1:#$3142; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER PIEUP
    (Unicode:#$FFB3; Attr:daNarrow; Ch1:#$3143; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER SSANGPIEUP
    (Unicode:#$FFB4; Attr:daNarrow; Ch1:#$3144; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER PIEUP-SIOS
    (Unicode:#$FFB5; Attr:daNarrow; Ch1:#$3145; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER SIOS
    (Unicode:#$FFB6; Attr:daNarrow; Ch1:#$3146; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER SSANGSIOS
    (Unicode:#$FFB7; Attr:daNarrow; Ch1:#$3147; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER IEUNG
    (Unicode:#$FFB8; Attr:daNarrow; Ch1:#$3148; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER CIEUC
    (Unicode:#$FFB9; Attr:daNarrow; Ch1:#$3149; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER SSANGCIEUC
    (Unicode:#$FFBA; Attr:daNarrow; Ch1:#$314A; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER CHIEUCH
    (Unicode:#$FFBB; Attr:daNarrow; Ch1:#$314B; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER KHIEUKH
    (Unicode:#$FFBC; Attr:daNarrow; Ch1:#$314C; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER THIEUTH
    (Unicode:#$FFBD; Attr:daNarrow; Ch1:#$314D; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER PHIEUPH
    (Unicode:#$FFBE; Attr:daNarrow; Ch1:#$314E; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER HIEUH
    (Unicode:#$FFC2; Attr:daNarrow; Ch1:#$314F; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER A
    (Unicode:#$FFC3; Attr:daNarrow; Ch1:#$3150; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER AE
    (Unicode:#$FFC4; Attr:daNarrow; Ch1:#$3151; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YA
    (Unicode:#$FFC5; Attr:daNarrow; Ch1:#$3152; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YAE
    (Unicode:#$FFC6; Attr:daNarrow; Ch1:#$3153; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER EO
    (Unicode:#$FFC7; Attr:daNarrow; Ch1:#$3154; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER E
    (Unicode:#$FFCA; Attr:daNarrow; Ch1:#$3155; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YEO
    (Unicode:#$FFCB; Attr:daNarrow; Ch1:#$3156; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YE
    (Unicode:#$FFCC; Attr:daNarrow; Ch1:#$3157; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER O
    (Unicode:#$FFCD; Attr:daNarrow; Ch1:#$3158; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER WA
    (Unicode:#$FFCE; Attr:daNarrow; Ch1:#$3159; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER WAE
    (Unicode:#$FFCF; Attr:daNarrow; Ch1:#$315A; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER OE
    (Unicode:#$FFD2; Attr:daNarrow; Ch1:#$315B; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YO
    (Unicode:#$FFD3; Attr:daNarrow; Ch1:#$315C; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER U
    (Unicode:#$FFD4; Attr:daNarrow; Ch1:#$315D; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER WEO
    (Unicode:#$FFD5; Attr:daNarrow; Ch1:#$315E; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER WE
    (Unicode:#$FFD6; Attr:daNarrow; Ch1:#$315F; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER WI
    (Unicode:#$FFD7; Attr:daNarrow; Ch1:#$3160; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YU
    (Unicode:#$FFDA; Attr:daNarrow; Ch1:#$3161; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER EU
    (Unicode:#$FFDB; Attr:daNarrow; Ch1:#$3162; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER YI
    (Unicode:#$FFDC; Attr:daNarrow; Ch1:#$3163; Ch2:#$FFFF),              // HALFWIDTH HANGUL LETTER I
    (Unicode:#$FFE0; Attr:daWide; Ch1:#$00A2; Ch2:#$FFFF),                // FULLWIDTH CENT SIGN
    (Unicode:#$FFE1; Attr:daWide; Ch1:#$00A3; Ch2:#$FFFF),                // FULLWIDTH POUND SIGN
    (Unicode:#$FFE2; Attr:daWide; Ch1:#$00AC; Ch2:#$FFFF),                // FULLWIDTH NOT SIGN
    (Unicode:#$FFE3; Attr:daWide; Ch1:#$00AF; Ch2:#$FFFF),                // FULLWIDTH MACRON
    (Unicode:#$FFE4; Attr:daWide; Ch1:#$00A6; Ch2:#$FFFF),                // FULLWIDTH BROKEN BAR
    (Unicode:#$FFE5; Attr:daWide; Ch1:#$00A5; Ch2:#$FFFF),                // FULLWIDTH YEN SIGN
    (Unicode:#$FFE6; Attr:daWide; Ch1:#$20A9; Ch2:#$FFFF),                // FULLWIDTH WON SIGN
    (Unicode:#$FFE8; Attr:daNarrow; Ch1:#$2502; Ch2:#$FFFF),              // HALFWIDTH FORMS LIGHT VERTICAL
    (Unicode:#$FFE9; Attr:daNarrow; Ch1:#$2190; Ch2:#$FFFF),              // HALFWIDTH LEFTWARDS ARROW
    (Unicode:#$FFEA; Attr:daNarrow; Ch1:#$2191; Ch2:#$FFFF),              // HALFWIDTH UPWARDS ARROW
    (Unicode:#$FFEB; Attr:daNarrow; Ch1:#$2192; Ch2:#$FFFF),              // HALFWIDTH RIGHTWARDS ARROW
    (Unicode:#$FFEC; Attr:daNarrow; Ch1:#$2193; Ch2:#$FFFF),              // HALFWIDTH DOWNWARDS ARROW
    (Unicode:#$FFED; Attr:daNarrow; Ch1:#$25A0; Ch2:#$FFFF),              // HALFWIDTH BLACK SQUARE
    (Unicode:#$FFEE; Attr:daNarrow; Ch1:#$25CB; Ch2:#$FFFF)               // HALFWIDTH WHITE CIRCLE
    );

function LocateDecompositionInfo(const Ch: WideChar): Integer;
var L, H, I : Integer;
    D       : WideChar;
begin
  if Ord(Ch) < $A0 then // No decompositions for ASCII
    begin
      Result := -1;
      exit;
    end;

  // Binary search
  L := 0;
  H := UnicodeDecompositionEntries - 1;
  repeat
    I := (L + H) div 2;
    D := UnicodeDecompositionInfo[I].Unicode;
    if D = Ch then
      begin
        Result := I;
        exit;
      end else
    if D > Ch then
      H := I - 1 else
      L := I + 1;
  until L > H;
  Result := -1;
end;

{$IFDEF CLR}
function GetCharacterDecomposition(const Ch: WideChar): WideString;
var I, J : Integer;
begin
  I := LocateDecompositionInfo(Ch);
  if I < 0 then
    // Exceptionally long decompositions not stored in table
    case Ch of
      #$3316 : Result := #$30AD#$30ED#$30E1#$30FC#$30C8#$30EB;             // SQUARE KIROMEETORU
      #$33AF : Result := #$0072#$0061#$0064#$2215#$0073#$00B2;             // SQUARE RAD OVER S SQUARED
      #$FDFA : Result := #$0635#$0644#$0649#$0020#$0627#$0644#$0644#$0647#$0020#$0639#$0644#$064A#$0647#$0020#$0648#$0633#$0644#$0645; // ARABIC LIGATURE SALLALLAHOU ALAYHE WASALLAM
      #$FDFB : Result := #$062C#$0644#$0020#$062C#$0644#$0627#$0644#$0647; // ARABIC LIGATURE JALLAJALALOUHOU
    else
      Result := '';
    end
  else
    begin
      if UnicodeDecompositionInfo[I].Ch2 = #$FFFF then
        J := 1 else
      if UnicodeDecompositionInfo[I].Ch3 = #$FFFF then
        J := 2 else
      if UnicodeDecompositionInfo[I].Ch4 = #$FFFF then
        J := 3 else
      if UnicodeDecompositionInfo[I].Ch5 = #$FFFF then
        J := 4
      else
        J := 5;
      SetLength(Result, J);
      Result[1] := UnicodeDecompositionInfo[I].Ch1;
      if J > 1 then
        Result[2] := UnicodeDecompositionInfo[I].Ch2;
      if J > 2 then
        Result[3] := UnicodeDecompositionInfo[I].Ch3;
      if J > 3 then
        Result[4] := UnicodeDecompositionInfo[I].Ch4;
      if J > 4 then
        Result[5] := UnicodeDecompositionInfo[I].Ch5;
    end;
end;
{$ELSE}
function GetCharacterDecomposition(const Ch: WideChar): WideString;
var I, J : Integer;
    P, Q : PWideChar;
begin
  I := LocateDecompositionInfo(Ch);
  if I < 0 then
    // Exceptionally long decompositions not stored in table
    case Ch of
      #$3316 : Result := #$30AD#$30ED#$30E1#$30FC#$30C8#$30EB;             // SQUARE KIROMEETORU
      #$33AF : Result := #$0072#$0061#$0064#$2215#$0073#$00B2;             // SQUARE RAD OVER S SQUARED
      #$FDFA : Result := #$0635#$0644#$0649#$0020#$0627#$0644#$0644#$0647#$0020#$0639#$0644#$064A#$0647#$0020#$0648#$0633#$0644#$0645; // ARABIC LIGATURE SALLALLAHOU ALAYHE WASALLAM
      #$FDFB : Result := #$062C#$0644#$0020#$062C#$0644#$0627#$0644#$0647; // ARABIC LIGATURE JALLAJALALOUHOU
    else
      Result := '';
    end
  else
    begin
      P := @UnicodeDecompositionInfo[I].Ch1;
      Q := P;
      Inc(Q);
      I := 1;
      while (Q^ <> #$FFFF) and (I < 5) do
        begin
          Inc(Q);
          Inc(I);
        end;
      SetLength(Result, I);
      Q := P;
      P := Pointer(Result);
      for J := 1 to I do
        begin
          P^ := Q^;
          Inc(P);
          Inc(Q);
        end;
    end;
end;
{$ENDIF}

type
  TUnicodeUCS4DecompositionInfo = packed record
    Unicode : UCS4Char;
    Attr    : TUnicodeDecompositionAttr;
    Ch1     : WideChar;
    Ch2     : WideChar;
  end;
  PUnicodeUCS4DecompositionInfo = ^TUnicodeUCS4DecompositionInfo;

const
  UnicodeUCS4DecompositionEntries = 991;
  UnicodeUCS4DecompositionInfo : Array[0..UnicodeUCS4DecompositionEntries - 1] of TUnicodeUCS4DecompositionInfo = (
    (Unicode:$1D400; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL A
    (Unicode:$1D401; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL B
    (Unicode:$1D402; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL C
    (Unicode:$1D403; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL D
    (Unicode:$1D404; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL E
    (Unicode:$1D405; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL F
    (Unicode:$1D406; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL G
    (Unicode:$1D407; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL H
    (Unicode:$1D408; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL I
    (Unicode:$1D409; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL J
    (Unicode:$1D40A; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL K
    (Unicode:$1D40B; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL L
    (Unicode:$1D40C; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL M
    (Unicode:$1D40D; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL N
    (Unicode:$1D40E; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL O
    (Unicode:$1D40F; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL P
    (Unicode:$1D410; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL Q
    (Unicode:$1D411; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL R
    (Unicode:$1D412; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL S
    (Unicode:$1D413; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL T
    (Unicode:$1D414; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL U
    (Unicode:$1D415; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL V
    (Unicode:$1D416; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL W
    (Unicode:$1D417; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL X
    (Unicode:$1D418; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL Y
    (Unicode:$1D419; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL Z
    (Unicode:$1D41A; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL A
    (Unicode:$1D41B; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL B
    (Unicode:$1D41C; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL C
    (Unicode:$1D41D; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL D
    (Unicode:$1D41E; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL E
    (Unicode:$1D41F; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL F
    (Unicode:$1D420; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL G
    (Unicode:$1D421; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL H
    (Unicode:$1D422; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL I
    (Unicode:$1D423; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL J
    (Unicode:$1D424; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL K
    (Unicode:$1D425; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL L
    (Unicode:$1D426; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL M
    (Unicode:$1D427; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL N
    (Unicode:$1D428; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL O
    (Unicode:$1D429; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL P
    (Unicode:$1D42A; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL Q
    (Unicode:$1D42B; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL R
    (Unicode:$1D42C; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL S
    (Unicode:$1D42D; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL T
    (Unicode:$1D42E; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL U
    (Unicode:$1D42F; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL V
    (Unicode:$1D430; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL W
    (Unicode:$1D431; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL X
    (Unicode:$1D432; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL Y
    (Unicode:$1D433; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL Z
    (Unicode:$1D434; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL A
    (Unicode:$1D435; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL B
    (Unicode:$1D436; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL C
    (Unicode:$1D437; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL D
    (Unicode:$1D438; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL E
    (Unicode:$1D439; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL F
    (Unicode:$1D43A; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL G
    (Unicode:$1D43B; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL H
    (Unicode:$1D43C; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL I
    (Unicode:$1D43D; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL J
    (Unicode:$1D43E; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL K
    (Unicode:$1D43F; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL L
    (Unicode:$1D440; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL M
    (Unicode:$1D441; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL N
    (Unicode:$1D442; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL O
    (Unicode:$1D443; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL P
    (Unicode:$1D444; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL Q
    (Unicode:$1D445; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL R
    (Unicode:$1D446; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL S
    (Unicode:$1D447; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL T
    (Unicode:$1D448; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL U
    (Unicode:$1D449; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL V
    (Unicode:$1D44A; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL W
    (Unicode:$1D44B; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL X
    (Unicode:$1D44C; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL Y
    (Unicode:$1D44D; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL Z
    (Unicode:$1D44E; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL A
    (Unicode:$1D44F; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL B
    (Unicode:$1D450; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL C
    (Unicode:$1D451; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL D
    (Unicode:$1D452; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL E
    (Unicode:$1D453; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL F
    (Unicode:$1D454; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL G
    (Unicode:$1D456; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL I
    (Unicode:$1D457; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL J
    (Unicode:$1D458; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL K
    (Unicode:$1D459; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL L
    (Unicode:$1D45A; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL M
    (Unicode:$1D45B; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL N
    (Unicode:$1D45C; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL O
    (Unicode:$1D45D; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL P
    (Unicode:$1D45E; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL Q
    (Unicode:$1D45F; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL R
    (Unicode:$1D460; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL S
    (Unicode:$1D461; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL T
    (Unicode:$1D462; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL U
    (Unicode:$1D463; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL V
    (Unicode:$1D464; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL W
    (Unicode:$1D465; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL X
    (Unicode:$1D466; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL Y
    (Unicode:$1D467; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL Z
    (Unicode:$1D468; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL A
    (Unicode:$1D469; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL B
    (Unicode:$1D46A; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL C
    (Unicode:$1D46B; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL D
    (Unicode:$1D46C; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL E
    (Unicode:$1D46D; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL F
    (Unicode:$1D46E; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL G
    (Unicode:$1D46F; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL H
    (Unicode:$1D470; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL I
    (Unicode:$1D471; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL J
    (Unicode:$1D472; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL K
    (Unicode:$1D473; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL L
    (Unicode:$1D474; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL M
    (Unicode:$1D475; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL N
    (Unicode:$1D476; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL O
    (Unicode:$1D477; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL P
    (Unicode:$1D478; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL Q
    (Unicode:$1D479; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL R
    (Unicode:$1D47A; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL S
    (Unicode:$1D47B; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL T
    (Unicode:$1D47C; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL U
    (Unicode:$1D47D; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL V
    (Unicode:$1D47E; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL W
    (Unicode:$1D47F; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL X
    (Unicode:$1D480; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL Y
    (Unicode:$1D481; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL Z
    (Unicode:$1D482; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL A
    (Unicode:$1D483; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL B
    (Unicode:$1D484; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL C
    (Unicode:$1D485; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL D
    (Unicode:$1D486; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL E
    (Unicode:$1D487; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL F
    (Unicode:$1D488; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL G
    (Unicode:$1D489; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL H
    (Unicode:$1D48A; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL I
    (Unicode:$1D48B; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL J
    (Unicode:$1D48C; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL K
    (Unicode:$1D48D; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL L
    (Unicode:$1D48E; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL M
    (Unicode:$1D48F; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL N
    (Unicode:$1D490; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL O
    (Unicode:$1D491; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL P
    (Unicode:$1D492; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL Q
    (Unicode:$1D493; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL R
    (Unicode:$1D494; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL S
    (Unicode:$1D495; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL T
    (Unicode:$1D496; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL U
    (Unicode:$1D497; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL V
    (Unicode:$1D498; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL W
    (Unicode:$1D499; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL X
    (Unicode:$1D49A; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL Y
    (Unicode:$1D49B; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL Z
    (Unicode:$1D49C; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL A
    (Unicode:$1D49E; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL C
    (Unicode:$1D49F; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL D
    (Unicode:$1D4A2; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL G
    (Unicode:$1D4A5; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL J
    (Unicode:$1D4A6; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL K
    (Unicode:$1D4A9; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL N
    (Unicode:$1D4AA; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL O
    (Unicode:$1D4AB; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL P
    (Unicode:$1D4AC; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL Q
    (Unicode:$1D4AE; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL S
    (Unicode:$1D4AF; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL T
    (Unicode:$1D4B0; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL U
    (Unicode:$1D4B1; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL V
    (Unicode:$1D4B2; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL W
    (Unicode:$1D4B3; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL X
    (Unicode:$1D4B4; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL Y
    (Unicode:$1D4B5; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT CAPITAL Z
    (Unicode:$1D4B6; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL A
    (Unicode:$1D4B7; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL B
    (Unicode:$1D4B8; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL C
    (Unicode:$1D4B9; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL D
    (Unicode:$1D4BB; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL F
    (Unicode:$1D4BD; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL H
    (Unicode:$1D4BE; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL I
    (Unicode:$1D4BF; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL J
    (Unicode:$1D4C0; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL K
    (Unicode:$1D4C2; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL M
    (Unicode:$1D4C3; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL N
    (Unicode:$1D4C5; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL P
    (Unicode:$1D4C6; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL Q
    (Unicode:$1D4C7; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL R
    (Unicode:$1D4C8; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL S
    (Unicode:$1D4C9; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL T
    (Unicode:$1D4CA; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL U
    (Unicode:$1D4CB; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL V
    (Unicode:$1D4CC; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL W
    (Unicode:$1D4CD; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL X
    (Unicode:$1D4CE; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL Y
    (Unicode:$1D4CF; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL SCRIPT SMALL Z
    (Unicode:$1D4D0; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL A
    (Unicode:$1D4D1; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL B
    (Unicode:$1D4D2; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL C
    (Unicode:$1D4D3; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL D
    (Unicode:$1D4D4; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL E
    (Unicode:$1D4D5; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL F
    (Unicode:$1D4D6; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL G
    (Unicode:$1D4D7; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL H
    (Unicode:$1D4D8; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL I
    (Unicode:$1D4D9; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL J
    (Unicode:$1D4DA; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL K
    (Unicode:$1D4DB; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL L
    (Unicode:$1D4DC; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL M
    (Unicode:$1D4DD; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL N
    (Unicode:$1D4DE; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL O
    (Unicode:$1D4DF; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL P
    (Unicode:$1D4E0; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL Q
    (Unicode:$1D4E1; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL R
    (Unicode:$1D4E2; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL S
    (Unicode:$1D4E3; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL T
    (Unicode:$1D4E4; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL U
    (Unicode:$1D4E5; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL V
    (Unicode:$1D4E6; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL W
    (Unicode:$1D4E7; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL X
    (Unicode:$1D4E8; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL Y
    (Unicode:$1D4E9; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT CAPITAL Z
    (Unicode:$1D4EA; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL A
    (Unicode:$1D4EB; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL B
    (Unicode:$1D4EC; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL C
    (Unicode:$1D4ED; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL D
    (Unicode:$1D4EE; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL E
    (Unicode:$1D4EF; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL F
    (Unicode:$1D4F0; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL G
    (Unicode:$1D4F1; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL H
    (Unicode:$1D4F2; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL I
    (Unicode:$1D4F3; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL J
    (Unicode:$1D4F4; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL K
    (Unicode:$1D4F5; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL L
    (Unicode:$1D4F6; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL M
    (Unicode:$1D4F7; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL N
    (Unicode:$1D4F8; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL O
    (Unicode:$1D4F9; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL P
    (Unicode:$1D4FA; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL Q
    (Unicode:$1D4FB; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL R
    (Unicode:$1D4FC; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL S
    (Unicode:$1D4FD; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL T
    (Unicode:$1D4FE; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL U
    (Unicode:$1D4FF; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL V
    (Unicode:$1D500; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL W
    (Unicode:$1D501; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL X
    (Unicode:$1D502; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL Y
    (Unicode:$1D503; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL BOLD SCRIPT SMALL Z
    (Unicode:$1D504; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL A
    (Unicode:$1D505; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL B
    (Unicode:$1D507; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL D
    (Unicode:$1D508; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL E
    (Unicode:$1D509; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL F
    (Unicode:$1D50A; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL G
    (Unicode:$1D50D; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL J
    (Unicode:$1D50E; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL K
    (Unicode:$1D50F; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL L
    (Unicode:$1D510; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL M
    (Unicode:$1D511; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL N
    (Unicode:$1D512; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL O
    (Unicode:$1D513; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL P
    (Unicode:$1D514; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL Q
    (Unicode:$1D516; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL S
    (Unicode:$1D517; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL T
    (Unicode:$1D518; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL U
    (Unicode:$1D519; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL V
    (Unicode:$1D51A; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL W
    (Unicode:$1D51B; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL X
    (Unicode:$1D51C; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR CAPITAL Y
    (Unicode:$1D51E; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL A
    (Unicode:$1D51F; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL B
    (Unicode:$1D520; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL C
    (Unicode:$1D521; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL D
    (Unicode:$1D522; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL E
    (Unicode:$1D523; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL F
    (Unicode:$1D524; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL G
    (Unicode:$1D525; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL H
    (Unicode:$1D526; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL I
    (Unicode:$1D527; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL J
    (Unicode:$1D528; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL K
    (Unicode:$1D529; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL L
    (Unicode:$1D52A; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL M
    (Unicode:$1D52B; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL N
    (Unicode:$1D52C; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL O
    (Unicode:$1D52D; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL P
    (Unicode:$1D52E; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL Q
    (Unicode:$1D52F; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL R
    (Unicode:$1D530; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL S
    (Unicode:$1D531; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL T
    (Unicode:$1D532; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL U
    (Unicode:$1D533; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL V
    (Unicode:$1D534; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL W
    (Unicode:$1D535; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL X
    (Unicode:$1D536; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL Y
    (Unicode:$1D537; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL FRAKTUR SMALL Z
    (Unicode:$1D538; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL A
    (Unicode:$1D539; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL B
    (Unicode:$1D53B; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL D
    (Unicode:$1D53C; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL E
    (Unicode:$1D53D; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL F
    (Unicode:$1D53E; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL G
    (Unicode:$1D540; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL I
    (Unicode:$1D541; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL J
    (Unicode:$1D542; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL K
    (Unicode:$1D543; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL L
    (Unicode:$1D544; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL M
    (Unicode:$1D546; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL O
    (Unicode:$1D54A; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL S
    (Unicode:$1D54B; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL T
    (Unicode:$1D54C; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL U
    (Unicode:$1D54D; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL V
    (Unicode:$1D54E; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL W
    (Unicode:$1D54F; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL X
    (Unicode:$1D550; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK CAPITAL Y
    (Unicode:$1D552; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL A
    (Unicode:$1D553; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL B
    (Unicode:$1D554; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL C
    (Unicode:$1D555; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL D
    (Unicode:$1D556; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL E
    (Unicode:$1D557; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL F
    (Unicode:$1D558; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL G
    (Unicode:$1D559; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL H
    (Unicode:$1D55A; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL I
    (Unicode:$1D55B; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL J
    (Unicode:$1D55C; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL K
    (Unicode:$1D55D; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL L
    (Unicode:$1D55E; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL M
    (Unicode:$1D55F; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL N
    (Unicode:$1D560; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL O
    (Unicode:$1D561; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL P
    (Unicode:$1D562; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL Q
    (Unicode:$1D563; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL R
    (Unicode:$1D564; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL S
    (Unicode:$1D565; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL T
    (Unicode:$1D566; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL U
    (Unicode:$1D567; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL V
    (Unicode:$1D568; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL W
    (Unicode:$1D569; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL X
    (Unicode:$1D56A; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL Y
    (Unicode:$1D56B; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK SMALL Z
    (Unicode:$1D56C; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL A
    (Unicode:$1D56D; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL B
    (Unicode:$1D56E; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL C
    (Unicode:$1D56F; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL D
    (Unicode:$1D570; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL E
    (Unicode:$1D571; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL F
    (Unicode:$1D572; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL G
    (Unicode:$1D573; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL H
    (Unicode:$1D574; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL I
    (Unicode:$1D575; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL J
    (Unicode:$1D576; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL K
    (Unicode:$1D577; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL L
    (Unicode:$1D578; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL M
    (Unicode:$1D579; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL N
    (Unicode:$1D57A; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL O
    (Unicode:$1D57B; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL P
    (Unicode:$1D57C; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL Q
    (Unicode:$1D57D; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL R
    (Unicode:$1D57E; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL S
    (Unicode:$1D57F; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL T
    (Unicode:$1D580; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL U
    (Unicode:$1D581; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL V
    (Unicode:$1D582; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL W
    (Unicode:$1D583; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL X
    (Unicode:$1D584; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL Y
    (Unicode:$1D585; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR CAPITAL Z
    (Unicode:$1D586; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL A
    (Unicode:$1D587; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL B
    (Unicode:$1D588; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL C
    (Unicode:$1D589; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL D
    (Unicode:$1D58A; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL E
    (Unicode:$1D58B; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL F
    (Unicode:$1D58C; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL G
    (Unicode:$1D58D; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL H
    (Unicode:$1D58E; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL I
    (Unicode:$1D58F; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL J
    (Unicode:$1D590; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL K
    (Unicode:$1D591; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL L
    (Unicode:$1D592; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL M
    (Unicode:$1D593; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL N
    (Unicode:$1D594; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL O
    (Unicode:$1D595; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL P
    (Unicode:$1D596; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL Q
    (Unicode:$1D597; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL R
    (Unicode:$1D598; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL S
    (Unicode:$1D599; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL T
    (Unicode:$1D59A; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL U
    (Unicode:$1D59B; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL V
    (Unicode:$1D59C; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL W
    (Unicode:$1D59D; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL X
    (Unicode:$1D59E; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL Y
    (Unicode:$1D59F; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL BOLD FRAKTUR SMALL Z
    (Unicode:$1D5A0; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL A
    (Unicode:$1D5A1; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL B
    (Unicode:$1D5A2; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL C
    (Unicode:$1D5A3; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL D
    (Unicode:$1D5A4; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL E
    (Unicode:$1D5A5; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL F
    (Unicode:$1D5A6; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL G
    (Unicode:$1D5A7; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL H
    (Unicode:$1D5A8; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL I
    (Unicode:$1D5A9; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL J
    (Unicode:$1D5AA; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL K
    (Unicode:$1D5AB; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL L
    (Unicode:$1D5AC; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL M
    (Unicode:$1D5AD; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL N
    (Unicode:$1D5AE; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL O
    (Unicode:$1D5AF; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL P
    (Unicode:$1D5B0; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL Q
    (Unicode:$1D5B1; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL R
    (Unicode:$1D5B2; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL S
    (Unicode:$1D5B3; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL T
    (Unicode:$1D5B4; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL U
    (Unicode:$1D5B5; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL V
    (Unicode:$1D5B6; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL W
    (Unicode:$1D5B7; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL X
    (Unicode:$1D5B8; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL Y
    (Unicode:$1D5B9; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF CAPITAL Z
    (Unicode:$1D5BA; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL A
    (Unicode:$1D5BB; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL B
    (Unicode:$1D5BC; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL C
    (Unicode:$1D5BD; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL D
    (Unicode:$1D5BE; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL E
    (Unicode:$1D5BF; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL F
    (Unicode:$1D5C0; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL G
    (Unicode:$1D5C1; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL H
    (Unicode:$1D5C2; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL I
    (Unicode:$1D5C3; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL J
    (Unicode:$1D5C4; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL K
    (Unicode:$1D5C5; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL L
    (Unicode:$1D5C6; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL M
    (Unicode:$1D5C7; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL N
    (Unicode:$1D5C8; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL O
    (Unicode:$1D5C9; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL P
    (Unicode:$1D5CA; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL Q
    (Unicode:$1D5CB; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL R
    (Unicode:$1D5CC; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL S
    (Unicode:$1D5CD; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL T
    (Unicode:$1D5CE; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL U
    (Unicode:$1D5CF; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL V
    (Unicode:$1D5D0; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL W
    (Unicode:$1D5D1; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL X
    (Unicode:$1D5D2; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL Y
    (Unicode:$1D5D3; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF SMALL Z
    (Unicode:$1D5D4; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL A
    (Unicode:$1D5D5; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL B
    (Unicode:$1D5D6; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL C
    (Unicode:$1D5D7; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL D
    (Unicode:$1D5D8; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL E
    (Unicode:$1D5D9; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL F
    (Unicode:$1D5DA; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL G
    (Unicode:$1D5DB; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL H
    (Unicode:$1D5DC; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL I
    (Unicode:$1D5DD; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL J
    (Unicode:$1D5DE; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL K
    (Unicode:$1D5DF; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL L
    (Unicode:$1D5E0; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL M
    (Unicode:$1D5E1; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL N
    (Unicode:$1D5E2; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL O
    (Unicode:$1D5E3; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL P
    (Unicode:$1D5E4; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL Q
    (Unicode:$1D5E5; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL R
    (Unicode:$1D5E6; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL S
    (Unicode:$1D5E7; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL T
    (Unicode:$1D5E8; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL U
    (Unicode:$1D5E9; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL V
    (Unicode:$1D5EA; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL W
    (Unicode:$1D5EB; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL X
    (Unicode:$1D5EC; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL Y
    (Unicode:$1D5ED; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL Z
    (Unicode:$1D5EE; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL A
    (Unicode:$1D5EF; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL B
    (Unicode:$1D5F0; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL C
    (Unicode:$1D5F1; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL D
    (Unicode:$1D5F2; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL E
    (Unicode:$1D5F3; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL F
    (Unicode:$1D5F4; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL G
    (Unicode:$1D5F5; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL H
    (Unicode:$1D5F6; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL I
    (Unicode:$1D5F7; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL J
    (Unicode:$1D5F8; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL K
    (Unicode:$1D5F9; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL L
    (Unicode:$1D5FA; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL M
    (Unicode:$1D5FB; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL N
    (Unicode:$1D5FC; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL O
    (Unicode:$1D5FD; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL P
    (Unicode:$1D5FE; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL Q
    (Unicode:$1D5FF; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL R
    (Unicode:$1D600; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL S
    (Unicode:$1D601; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL T
    (Unicode:$1D602; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL U
    (Unicode:$1D603; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL V
    (Unicode:$1D604; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL W
    (Unicode:$1D605; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL X
    (Unicode:$1D606; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL Y
    (Unicode:$1D607; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL Z
    (Unicode:$1D608; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL A
    (Unicode:$1D609; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL B
    (Unicode:$1D60A; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL C
    (Unicode:$1D60B; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL D
    (Unicode:$1D60C; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL E
    (Unicode:$1D60D; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL F
    (Unicode:$1D60E; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL G
    (Unicode:$1D60F; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL H
    (Unicode:$1D610; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL I
    (Unicode:$1D611; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL J
    (Unicode:$1D612; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL K
    (Unicode:$1D613; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL L
    (Unicode:$1D614; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL M
    (Unicode:$1D615; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL N
    (Unicode:$1D616; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL O
    (Unicode:$1D617; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL P
    (Unicode:$1D618; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL Q
    (Unicode:$1D619; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL R
    (Unicode:$1D61A; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL S
    (Unicode:$1D61B; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL T
    (Unicode:$1D61C; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL U
    (Unicode:$1D61D; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL V
    (Unicode:$1D61E; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL W
    (Unicode:$1D61F; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL X
    (Unicode:$1D620; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL Y
    (Unicode:$1D621; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC CAPITAL Z
    (Unicode:$1D622; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL A
    (Unicode:$1D623; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL B
    (Unicode:$1D624; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL C
    (Unicode:$1D625; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL D
    (Unicode:$1D626; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL E
    (Unicode:$1D627; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL F
    (Unicode:$1D628; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL G
    (Unicode:$1D629; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL H
    (Unicode:$1D62A; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL I
    (Unicode:$1D62B; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL J
    (Unicode:$1D62C; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL K
    (Unicode:$1D62D; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL L
    (Unicode:$1D62E; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL M
    (Unicode:$1D62F; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL N
    (Unicode:$1D630; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL O
    (Unicode:$1D631; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL P
    (Unicode:$1D632; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL Q
    (Unicode:$1D633; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL R
    (Unicode:$1D634; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL S
    (Unicode:$1D635; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL T
    (Unicode:$1D636; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL U
    (Unicode:$1D637; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL V
    (Unicode:$1D638; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL W
    (Unicode:$1D639; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL X
    (Unicode:$1D63A; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL Y
    (Unicode:$1D63B; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF ITALIC SMALL Z
    (Unicode:$1D63C; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL A
    (Unicode:$1D63D; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL B
    (Unicode:$1D63E; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL C
    (Unicode:$1D63F; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL D
    (Unicode:$1D640; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL E
    (Unicode:$1D641; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL F
    (Unicode:$1D642; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL G
    (Unicode:$1D643; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL H
    (Unicode:$1D644; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL I
    (Unicode:$1D645; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL J
    (Unicode:$1D646; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL K
    (Unicode:$1D647; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL L
    (Unicode:$1D648; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL M
    (Unicode:$1D649; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL N
    (Unicode:$1D64A; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL O
    (Unicode:$1D64B; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL P
    (Unicode:$1D64C; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Q
    (Unicode:$1D64D; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL R
    (Unicode:$1D64E; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL S
    (Unicode:$1D64F; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL T
    (Unicode:$1D650; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL U
    (Unicode:$1D651; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL V
    (Unicode:$1D652; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL W
    (Unicode:$1D653; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL X
    (Unicode:$1D654; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Y
    (Unicode:$1D655; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL Z
    (Unicode:$1D656; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL A
    (Unicode:$1D657; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL B
    (Unicode:$1D658; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL C
    (Unicode:$1D659; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL D
    (Unicode:$1D65A; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL E
    (Unicode:$1D65B; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL F
    (Unicode:$1D65C; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL G
    (Unicode:$1D65D; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL H
    (Unicode:$1D65E; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL I
    (Unicode:$1D65F; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL J
    (Unicode:$1D660; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL K
    (Unicode:$1D661; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL L
    (Unicode:$1D662; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL M
    (Unicode:$1D663; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL N
    (Unicode:$1D664; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL O
    (Unicode:$1D665; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL P
    (Unicode:$1D666; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Q
    (Unicode:$1D667; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL R
    (Unicode:$1D668; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL S
    (Unicode:$1D669; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL T
    (Unicode:$1D66A; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL U
    (Unicode:$1D66B; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL V
    (Unicode:$1D66C; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL W
    (Unicode:$1D66D; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL X
    (Unicode:$1D66E; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Y
    (Unicode:$1D66F; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL Z
    (Unicode:$1D670; Attr:daFont; Ch1:#$0041; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL A
    (Unicode:$1D671; Attr:daFont; Ch1:#$0042; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL B
    (Unicode:$1D672; Attr:daFont; Ch1:#$0043; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL C
    (Unicode:$1D673; Attr:daFont; Ch1:#$0044; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL D
    (Unicode:$1D674; Attr:daFont; Ch1:#$0045; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL E
    (Unicode:$1D675; Attr:daFont; Ch1:#$0046; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL F
    (Unicode:$1D676; Attr:daFont; Ch1:#$0047; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL G
    (Unicode:$1D677; Attr:daFont; Ch1:#$0048; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL H
    (Unicode:$1D678; Attr:daFont; Ch1:#$0049; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL I
    (Unicode:$1D679; Attr:daFont; Ch1:#$004A; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL J
    (Unicode:$1D67A; Attr:daFont; Ch1:#$004B; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL K
    (Unicode:$1D67B; Attr:daFont; Ch1:#$004C; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL L
    (Unicode:$1D67C; Attr:daFont; Ch1:#$004D; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL M
    (Unicode:$1D67D; Attr:daFont; Ch1:#$004E; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL N
    (Unicode:$1D67E; Attr:daFont; Ch1:#$004F; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL O
    (Unicode:$1D67F; Attr:daFont; Ch1:#$0050; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL P
    (Unicode:$1D680; Attr:daFont; Ch1:#$0051; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL Q
    (Unicode:$1D681; Attr:daFont; Ch1:#$0052; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL R
    (Unicode:$1D682; Attr:daFont; Ch1:#$0053; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL S
    (Unicode:$1D683; Attr:daFont; Ch1:#$0054; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL T
    (Unicode:$1D684; Attr:daFont; Ch1:#$0055; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL U
    (Unicode:$1D685; Attr:daFont; Ch1:#$0056; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL V
    (Unicode:$1D686; Attr:daFont; Ch1:#$0057; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL W
    (Unicode:$1D687; Attr:daFont; Ch1:#$0058; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL X
    (Unicode:$1D688; Attr:daFont; Ch1:#$0059; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL Y
    (Unicode:$1D689; Attr:daFont; Ch1:#$005A; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE CAPITAL Z
    (Unicode:$1D68A; Attr:daFont; Ch1:#$0061; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL A
    (Unicode:$1D68B; Attr:daFont; Ch1:#$0062; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL B
    (Unicode:$1D68C; Attr:daFont; Ch1:#$0063; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL C
    (Unicode:$1D68D; Attr:daFont; Ch1:#$0064; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL D
    (Unicode:$1D68E; Attr:daFont; Ch1:#$0065; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL E
    (Unicode:$1D68F; Attr:daFont; Ch1:#$0066; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL F
    (Unicode:$1D690; Attr:daFont; Ch1:#$0067; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL G
    (Unicode:$1D691; Attr:daFont; Ch1:#$0068; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL H
    (Unicode:$1D692; Attr:daFont; Ch1:#$0069; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL I
    (Unicode:$1D693; Attr:daFont; Ch1:#$006A; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL J
    (Unicode:$1D694; Attr:daFont; Ch1:#$006B; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL K
    (Unicode:$1D695; Attr:daFont; Ch1:#$006C; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL L
    (Unicode:$1D696; Attr:daFont; Ch1:#$006D; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL M
    (Unicode:$1D697; Attr:daFont; Ch1:#$006E; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL N
    (Unicode:$1D698; Attr:daFont; Ch1:#$006F; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL O
    (Unicode:$1D699; Attr:daFont; Ch1:#$0070; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL P
    (Unicode:$1D69A; Attr:daFont; Ch1:#$0071; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL Q
    (Unicode:$1D69B; Attr:daFont; Ch1:#$0072; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL R
    (Unicode:$1D69C; Attr:daFont; Ch1:#$0073; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL S
    (Unicode:$1D69D; Attr:daFont; Ch1:#$0074; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL T
    (Unicode:$1D69E; Attr:daFont; Ch1:#$0075; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL U
    (Unicode:$1D69F; Attr:daFont; Ch1:#$0076; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL V
    (Unicode:$1D6A0; Attr:daFont; Ch1:#$0077; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL W
    (Unicode:$1D6A1; Attr:daFont; Ch1:#$0078; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL X
    (Unicode:$1D6A2; Attr:daFont; Ch1:#$0079; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL Y
    (Unicode:$1D6A3; Attr:daFont; Ch1:#$007A; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE SMALL Z
    (Unicode:$1D6A8; Attr:daFont; Ch1:#$0391; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL ALPHA
    (Unicode:$1D6A9; Attr:daFont; Ch1:#$0392; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL BETA
    (Unicode:$1D6AA; Attr:daFont; Ch1:#$0393; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL GAMMA
    (Unicode:$1D6AB; Attr:daFont; Ch1:#$0394; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL DELTA
    (Unicode:$1D6AC; Attr:daFont; Ch1:#$0395; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL EPSILON
    (Unicode:$1D6AD; Attr:daFont; Ch1:#$0396; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL ZETA
    (Unicode:$1D6AE; Attr:daFont; Ch1:#$0397; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL ETA
    (Unicode:$1D6AF; Attr:daFont; Ch1:#$0398; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL THETA
    (Unicode:$1D6B0; Attr:daFont; Ch1:#$0399; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL IOTA
    (Unicode:$1D6B1; Attr:daFont; Ch1:#$039A; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL KAPPA
    (Unicode:$1D6B2; Attr:daFont; Ch1:#$039B; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL LAMDA
    (Unicode:$1D6B3; Attr:daFont; Ch1:#$039C; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL MU
    (Unicode:$1D6B4; Attr:daFont; Ch1:#$039D; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL NU
    (Unicode:$1D6B5; Attr:daFont; Ch1:#$039E; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL XI
    (Unicode:$1D6B6; Attr:daFont; Ch1:#$039F; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL OMICRON
    (Unicode:$1D6B7; Attr:daFont; Ch1:#$03A0; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL PI
    (Unicode:$1D6B8; Attr:daFont; Ch1:#$03A1; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL RHO
    (Unicode:$1D6B9; Attr:daFont; Ch1:#$03F4; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL THETA SYMBOL
    (Unicode:$1D6BA; Attr:daFont; Ch1:#$03A3; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL SIGMA
    (Unicode:$1D6BB; Attr:daFont; Ch1:#$03A4; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL TAU
    (Unicode:$1D6BC; Attr:daFont; Ch1:#$03A5; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL UPSILON
    (Unicode:$1D6BD; Attr:daFont; Ch1:#$03A6; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL PHI
    (Unicode:$1D6BE; Attr:daFont; Ch1:#$03A7; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL CHI
    (Unicode:$1D6BF; Attr:daFont; Ch1:#$03A8; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL PSI
    (Unicode:$1D6C0; Attr:daFont; Ch1:#$03A9; Ch2:#$FFFF),       // MATHEMATICAL BOLD CAPITAL OMEGA
    (Unicode:$1D6C1; Attr:daFont; Ch1:#$2207; Ch2:#$FFFF),       // MATHEMATICAL BOLD NABLA
    (Unicode:$1D6C2; Attr:daFont; Ch1:#$03B1; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL ALPHA
    (Unicode:$1D6C3; Attr:daFont; Ch1:#$03B2; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL BETA
    (Unicode:$1D6C4; Attr:daFont; Ch1:#$03B3; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL GAMMA
    (Unicode:$1D6C5; Attr:daFont; Ch1:#$03B4; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL DELTA
    (Unicode:$1D6C6; Attr:daFont; Ch1:#$03B5; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL EPSILON
    (Unicode:$1D6C7; Attr:daFont; Ch1:#$03B6; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL ZETA
    (Unicode:$1D6C8; Attr:daFont; Ch1:#$03B7; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL ETA
    (Unicode:$1D6C9; Attr:daFont; Ch1:#$03B8; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL THETA
    (Unicode:$1D6CA; Attr:daFont; Ch1:#$03B9; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL IOTA
    (Unicode:$1D6CB; Attr:daFont; Ch1:#$03BA; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL KAPPA
    (Unicode:$1D6CC; Attr:daFont; Ch1:#$03BB; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL LAMDA
    (Unicode:$1D6CD; Attr:daFont; Ch1:#$03BC; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL MU
    (Unicode:$1D6CE; Attr:daFont; Ch1:#$03BD; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL NU
    (Unicode:$1D6CF; Attr:daFont; Ch1:#$03BE; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL XI
    (Unicode:$1D6D0; Attr:daFont; Ch1:#$03BF; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL OMICRON
    (Unicode:$1D6D1; Attr:daFont; Ch1:#$03C0; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL PI
    (Unicode:$1D6D2; Attr:daFont; Ch1:#$03C1; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL RHO
    (Unicode:$1D6D3; Attr:daFont; Ch1:#$03C2; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL FINAL SIGMA
    (Unicode:$1D6D4; Attr:daFont; Ch1:#$03C3; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL SIGMA
    (Unicode:$1D6D5; Attr:daFont; Ch1:#$03C4; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL TAU
    (Unicode:$1D6D6; Attr:daFont; Ch1:#$03C5; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL UPSILON
    (Unicode:$1D6D7; Attr:daFont; Ch1:#$03C6; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL PHI
    (Unicode:$1D6D8; Attr:daFont; Ch1:#$03C7; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL CHI
    (Unicode:$1D6D9; Attr:daFont; Ch1:#$03C8; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL PSI
    (Unicode:$1D6DA; Attr:daFont; Ch1:#$03C9; Ch2:#$FFFF),       // MATHEMATICAL BOLD SMALL OMEGA
    (Unicode:$1D6DB; Attr:daFont; Ch1:#$2202; Ch2:#$FFFF),       // MATHEMATICAL BOLD PARTIAL DIFFERENTIAL
    (Unicode:$1D6DC; Attr:daFont; Ch1:#$03F5; Ch2:#$FFFF),       // MATHEMATICAL BOLD EPSILON SYMBOL
    (Unicode:$1D6DD; Attr:daFont; Ch1:#$03D1; Ch2:#$FFFF),       // MATHEMATICAL BOLD THETA SYMBOL
    (Unicode:$1D6DE; Attr:daFont; Ch1:#$03F0; Ch2:#$FFFF),       // MATHEMATICAL BOLD KAPPA SYMBOL
    (Unicode:$1D6DF; Attr:daFont; Ch1:#$03D5; Ch2:#$FFFF),       // MATHEMATICAL BOLD PHI SYMBOL
    (Unicode:$1D6E0; Attr:daFont; Ch1:#$03F1; Ch2:#$FFFF),       // MATHEMATICAL BOLD RHO SYMBOL
    (Unicode:$1D6E1; Attr:daFont; Ch1:#$03D6; Ch2:#$FFFF),       // MATHEMATICAL BOLD PI SYMBOL
    (Unicode:$1D6E2; Attr:daFont; Ch1:#$0391; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL ALPHA
    (Unicode:$1D6E3; Attr:daFont; Ch1:#$0392; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL BETA
    (Unicode:$1D6E4; Attr:daFont; Ch1:#$0393; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL GAMMA
    (Unicode:$1D6E5; Attr:daFont; Ch1:#$0394; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL DELTA
    (Unicode:$1D6E6; Attr:daFont; Ch1:#$0395; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL EPSILON
    (Unicode:$1D6E7; Attr:daFont; Ch1:#$0396; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL ZETA
    (Unicode:$1D6E8; Attr:daFont; Ch1:#$0397; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL ETA
    (Unicode:$1D6E9; Attr:daFont; Ch1:#$0398; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL THETA
    (Unicode:$1D6EA; Attr:daFont; Ch1:#$0399; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL IOTA
    (Unicode:$1D6EB; Attr:daFont; Ch1:#$039A; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL KAPPA
    (Unicode:$1D6EC; Attr:daFont; Ch1:#$039B; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL LAMDA
    (Unicode:$1D6ED; Attr:daFont; Ch1:#$039C; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL MU
    (Unicode:$1D6EE; Attr:daFont; Ch1:#$039D; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL NU
    (Unicode:$1D6EF; Attr:daFont; Ch1:#$039E; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL XI
    (Unicode:$1D6F0; Attr:daFont; Ch1:#$039F; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL OMICRON
    (Unicode:$1D6F1; Attr:daFont; Ch1:#$03A0; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL PI
    (Unicode:$1D6F2; Attr:daFont; Ch1:#$03A1; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL RHO
    (Unicode:$1D6F3; Attr:daFont; Ch1:#$03F4; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL THETA SYMBOL
    (Unicode:$1D6F4; Attr:daFont; Ch1:#$03A3; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL SIGMA
    (Unicode:$1D6F5; Attr:daFont; Ch1:#$03A4; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL TAU
    (Unicode:$1D6F6; Attr:daFont; Ch1:#$03A5; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL UPSILON
    (Unicode:$1D6F7; Attr:daFont; Ch1:#$03A6; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL PHI
    (Unicode:$1D6F8; Attr:daFont; Ch1:#$03A7; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL CHI
    (Unicode:$1D6F9; Attr:daFont; Ch1:#$03A8; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL PSI
    (Unicode:$1D6FA; Attr:daFont; Ch1:#$03A9; Ch2:#$FFFF),       // MATHEMATICAL ITALIC CAPITAL OMEGA
    (Unicode:$1D6FB; Attr:daFont; Ch1:#$2207; Ch2:#$FFFF),       // MATHEMATICAL ITALIC NABLA
    (Unicode:$1D6FC; Attr:daFont; Ch1:#$03B1; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL ALPHA
    (Unicode:$1D6FD; Attr:daFont; Ch1:#$03B2; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL BETA
    (Unicode:$1D6FE; Attr:daFont; Ch1:#$03B3; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL GAMMA
    (Unicode:$1D6FF; Attr:daFont; Ch1:#$03B4; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL DELTA
    (Unicode:$1D700; Attr:daFont; Ch1:#$03B5; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL EPSILON
    (Unicode:$1D701; Attr:daFont; Ch1:#$03B6; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL ZETA
    (Unicode:$1D702; Attr:daFont; Ch1:#$03B7; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL ETA
    (Unicode:$1D703; Attr:daFont; Ch1:#$03B8; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL THETA
    (Unicode:$1D704; Attr:daFont; Ch1:#$03B9; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL IOTA
    (Unicode:$1D705; Attr:daFont; Ch1:#$03BA; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL KAPPA
    (Unicode:$1D706; Attr:daFont; Ch1:#$03BB; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL LAMDA
    (Unicode:$1D707; Attr:daFont; Ch1:#$03BC; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL MU
    (Unicode:$1D708; Attr:daFont; Ch1:#$03BD; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL NU
    (Unicode:$1D709; Attr:daFont; Ch1:#$03BE; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL XI
    (Unicode:$1D70A; Attr:daFont; Ch1:#$03BF; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL OMICRON
    (Unicode:$1D70B; Attr:daFont; Ch1:#$03C0; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL PI
    (Unicode:$1D70C; Attr:daFont; Ch1:#$03C1; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL RHO
    (Unicode:$1D70D; Attr:daFont; Ch1:#$03C2; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL FINAL SIGMA
    (Unicode:$1D70E; Attr:daFont; Ch1:#$03C3; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL SIGMA
    (Unicode:$1D70F; Attr:daFont; Ch1:#$03C4; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL TAU
    (Unicode:$1D710; Attr:daFont; Ch1:#$03C5; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL UPSILON
    (Unicode:$1D711; Attr:daFont; Ch1:#$03C6; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL PHI
    (Unicode:$1D712; Attr:daFont; Ch1:#$03C7; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL CHI
    (Unicode:$1D713; Attr:daFont; Ch1:#$03C8; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL PSI
    (Unicode:$1D714; Attr:daFont; Ch1:#$03C9; Ch2:#$FFFF),       // MATHEMATICAL ITALIC SMALL OMEGA
    (Unicode:$1D715; Attr:daFont; Ch1:#$2202; Ch2:#$FFFF),       // MATHEMATICAL ITALIC PARTIAL DIFFERENTIAL
    (Unicode:$1D716; Attr:daFont; Ch1:#$03F5; Ch2:#$FFFF),       // MATHEMATICAL ITALIC EPSILON SYMBOL
    (Unicode:$1D717; Attr:daFont; Ch1:#$03D1; Ch2:#$FFFF),       // MATHEMATICAL ITALIC THETA SYMBOL
    (Unicode:$1D718; Attr:daFont; Ch1:#$03F0; Ch2:#$FFFF),       // MATHEMATICAL ITALIC KAPPA SYMBOL
    (Unicode:$1D719; Attr:daFont; Ch1:#$03D5; Ch2:#$FFFF),       // MATHEMATICAL ITALIC PHI SYMBOL
    (Unicode:$1D71A; Attr:daFont; Ch1:#$03F1; Ch2:#$FFFF),       // MATHEMATICAL ITALIC RHO SYMBOL
    (Unicode:$1D71B; Attr:daFont; Ch1:#$03D6; Ch2:#$FFFF),       // MATHEMATICAL ITALIC PI SYMBOL
    (Unicode:$1D71C; Attr:daFont; Ch1:#$0391; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL ALPHA
    (Unicode:$1D71D; Attr:daFont; Ch1:#$0392; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL BETA
    (Unicode:$1D71E; Attr:daFont; Ch1:#$0393; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL GAMMA
    (Unicode:$1D71F; Attr:daFont; Ch1:#$0394; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL DELTA
    (Unicode:$1D720; Attr:daFont; Ch1:#$0395; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL EPSILON
    (Unicode:$1D721; Attr:daFont; Ch1:#$0396; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL ZETA
    (Unicode:$1D722; Attr:daFont; Ch1:#$0397; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL ETA
    (Unicode:$1D723; Attr:daFont; Ch1:#$0398; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL THETA
    (Unicode:$1D724; Attr:daFont; Ch1:#$0399; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL IOTA
    (Unicode:$1D725; Attr:daFont; Ch1:#$039A; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL KAPPA
    (Unicode:$1D726; Attr:daFont; Ch1:#$039B; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL LAMDA
    (Unicode:$1D727; Attr:daFont; Ch1:#$039C; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL MU
    (Unicode:$1D728; Attr:daFont; Ch1:#$039D; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL NU
    (Unicode:$1D729; Attr:daFont; Ch1:#$039E; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL XI
    (Unicode:$1D72A; Attr:daFont; Ch1:#$039F; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL OMICRON
    (Unicode:$1D72B; Attr:daFont; Ch1:#$03A0; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL PI
    (Unicode:$1D72C; Attr:daFont; Ch1:#$03A1; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL RHO
    (Unicode:$1D72D; Attr:daFont; Ch1:#$03F4; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL THETA SYMBOL
    (Unicode:$1D72E; Attr:daFont; Ch1:#$03A3; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL SIGMA
    (Unicode:$1D72F; Attr:daFont; Ch1:#$03A4; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL TAU
    (Unicode:$1D730; Attr:daFont; Ch1:#$03A5; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL UPSILON
    (Unicode:$1D731; Attr:daFont; Ch1:#$03A6; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL PHI
    (Unicode:$1D732; Attr:daFont; Ch1:#$03A7; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL CHI
    (Unicode:$1D733; Attr:daFont; Ch1:#$03A8; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL PSI
    (Unicode:$1D734; Attr:daFont; Ch1:#$03A9; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC CAPITAL OMEGA
    (Unicode:$1D735; Attr:daFont; Ch1:#$2207; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC NABLA
    (Unicode:$1D736; Attr:daFont; Ch1:#$03B1; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL ALPHA
    (Unicode:$1D737; Attr:daFont; Ch1:#$03B2; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL BETA
    (Unicode:$1D738; Attr:daFont; Ch1:#$03B3; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL GAMMA
    (Unicode:$1D739; Attr:daFont; Ch1:#$03B4; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL DELTA
    (Unicode:$1D73A; Attr:daFont; Ch1:#$03B5; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL EPSILON
    (Unicode:$1D73B; Attr:daFont; Ch1:#$03B6; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL ZETA
    (Unicode:$1D73C; Attr:daFont; Ch1:#$03B7; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL ETA
    (Unicode:$1D73D; Attr:daFont; Ch1:#$03B8; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL THETA
    (Unicode:$1D73E; Attr:daFont; Ch1:#$03B9; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL IOTA
    (Unicode:$1D73F; Attr:daFont; Ch1:#$03BA; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL KAPPA
    (Unicode:$1D740; Attr:daFont; Ch1:#$03BB; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL LAMDA
    (Unicode:$1D741; Attr:daFont; Ch1:#$03BC; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL MU
    (Unicode:$1D742; Attr:daFont; Ch1:#$03BD; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL NU
    (Unicode:$1D743; Attr:daFont; Ch1:#$03BE; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL XI
    (Unicode:$1D744; Attr:daFont; Ch1:#$03BF; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL OMICRON
    (Unicode:$1D745; Attr:daFont; Ch1:#$03C0; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL PI
    (Unicode:$1D746; Attr:daFont; Ch1:#$03C1; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL RHO
    (Unicode:$1D747; Attr:daFont; Ch1:#$03C2; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL FINAL SIGMA
    (Unicode:$1D748; Attr:daFont; Ch1:#$03C3; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL SIGMA
    (Unicode:$1D749; Attr:daFont; Ch1:#$03C4; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL TAU
    (Unicode:$1D74A; Attr:daFont; Ch1:#$03C5; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL UPSILON
    (Unicode:$1D74B; Attr:daFont; Ch1:#$03C6; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL PHI
    (Unicode:$1D74C; Attr:daFont; Ch1:#$03C7; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL CHI
    (Unicode:$1D74D; Attr:daFont; Ch1:#$03C8; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL PSI
    (Unicode:$1D74E; Attr:daFont; Ch1:#$03C9; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC SMALL OMEGA
    (Unicode:$1D74F; Attr:daFont; Ch1:#$2202; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC PARTIAL DIFFERENTIAL
    (Unicode:$1D750; Attr:daFont; Ch1:#$03F5; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC EPSILON SYMBOL
    (Unicode:$1D751; Attr:daFont; Ch1:#$03D1; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC THETA SYMBOL
    (Unicode:$1D752; Attr:daFont; Ch1:#$03F0; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC KAPPA SYMBOL
    (Unicode:$1D753; Attr:daFont; Ch1:#$03D5; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC PHI SYMBOL
    (Unicode:$1D754; Attr:daFont; Ch1:#$03F1; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC RHO SYMBOL
    (Unicode:$1D755; Attr:daFont; Ch1:#$03D6; Ch2:#$FFFF),       // MATHEMATICAL BOLD ITALIC PI SYMBOL
    (Unicode:$1D756; Attr:daFont; Ch1:#$0391; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL ALPHA
    (Unicode:$1D757; Attr:daFont; Ch1:#$0392; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL BETA
    (Unicode:$1D758; Attr:daFont; Ch1:#$0393; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL GAMMA
    (Unicode:$1D759; Attr:daFont; Ch1:#$0394; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL DELTA
    (Unicode:$1D75A; Attr:daFont; Ch1:#$0395; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL EPSILON
    (Unicode:$1D75B; Attr:daFont; Ch1:#$0396; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL ZETA
    (Unicode:$1D75C; Attr:daFont; Ch1:#$0397; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL ETA
    (Unicode:$1D75D; Attr:daFont; Ch1:#$0398; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA
    (Unicode:$1D75E; Attr:daFont; Ch1:#$0399; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL IOTA
    (Unicode:$1D75F; Attr:daFont; Ch1:#$039A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL KAPPA
    (Unicode:$1D760; Attr:daFont; Ch1:#$039B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL LAMDA
    (Unicode:$1D761; Attr:daFont; Ch1:#$039C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL MU
    (Unicode:$1D762; Attr:daFont; Ch1:#$039D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL NU
    (Unicode:$1D763; Attr:daFont; Ch1:#$039E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL XI
    (Unicode:$1D764; Attr:daFont; Ch1:#$039F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL OMICRON
    (Unicode:$1D765; Attr:daFont; Ch1:#$03A0; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL PI
    (Unicode:$1D766; Attr:daFont; Ch1:#$03A1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL RHO
    (Unicode:$1D767; Attr:daFont; Ch1:#$03F4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL THETA SYMBOL
    (Unicode:$1D768; Attr:daFont; Ch1:#$03A3; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL SIGMA
    (Unicode:$1D769; Attr:daFont; Ch1:#$03A4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL TAU
    (Unicode:$1D76A; Attr:daFont; Ch1:#$03A5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL UPSILON
    (Unicode:$1D76B; Attr:daFont; Ch1:#$03A6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL PHI
    (Unicode:$1D76C; Attr:daFont; Ch1:#$03A7; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL CHI
    (Unicode:$1D76D; Attr:daFont; Ch1:#$03A8; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL PSI
    (Unicode:$1D76E; Attr:daFont; Ch1:#$03A9; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD CAPITAL OMEGA
    (Unicode:$1D76F; Attr:daFont; Ch1:#$2207; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD NABLA
    (Unicode:$1D770; Attr:daFont; Ch1:#$03B1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL ALPHA
    (Unicode:$1D771; Attr:daFont; Ch1:#$03B2; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL BETA
    (Unicode:$1D772; Attr:daFont; Ch1:#$03B3; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL GAMMA
    (Unicode:$1D773; Attr:daFont; Ch1:#$03B4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL DELTA
    (Unicode:$1D774; Attr:daFont; Ch1:#$03B5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL EPSILON
    (Unicode:$1D775; Attr:daFont; Ch1:#$03B6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL ZETA
    (Unicode:$1D776; Attr:daFont; Ch1:#$03B7; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL ETA
    (Unicode:$1D777; Attr:daFont; Ch1:#$03B8; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL THETA
    (Unicode:$1D778; Attr:daFont; Ch1:#$03B9; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL IOTA
    (Unicode:$1D779; Attr:daFont; Ch1:#$03BA; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL KAPPA
    (Unicode:$1D77A; Attr:daFont; Ch1:#$03BB; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL LAMDA
    (Unicode:$1D77B; Attr:daFont; Ch1:#$03BC; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL MU
    (Unicode:$1D77C; Attr:daFont; Ch1:#$03BD; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL NU
    (Unicode:$1D77D; Attr:daFont; Ch1:#$03BE; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL XI
    (Unicode:$1D77E; Attr:daFont; Ch1:#$03BF; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL OMICRON
    (Unicode:$1D77F; Attr:daFont; Ch1:#$03C0; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL PI
    (Unicode:$1D780; Attr:daFont; Ch1:#$03C1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL RHO
    (Unicode:$1D781; Attr:daFont; Ch1:#$03C2; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL FINAL SIGMA
    (Unicode:$1D782; Attr:daFont; Ch1:#$03C3; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL SIGMA
    (Unicode:$1D783; Attr:daFont; Ch1:#$03C4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL TAU
    (Unicode:$1D784; Attr:daFont; Ch1:#$03C5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL UPSILON
    (Unicode:$1D785; Attr:daFont; Ch1:#$03C6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL PHI
    (Unicode:$1D786; Attr:daFont; Ch1:#$03C7; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL CHI
    (Unicode:$1D787; Attr:daFont; Ch1:#$03C8; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL PSI
    (Unicode:$1D788; Attr:daFont; Ch1:#$03C9; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD SMALL OMEGA
    (Unicode:$1D789; Attr:daFont; Ch1:#$2202; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD PARTIAL DIFFERENTIAL
    (Unicode:$1D78A; Attr:daFont; Ch1:#$03F5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD EPSILON SYMBOL
    (Unicode:$1D78B; Attr:daFont; Ch1:#$03D1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD THETA SYMBOL
    (Unicode:$1D78C; Attr:daFont; Ch1:#$03F0; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD KAPPA SYMBOL
    (Unicode:$1D78D; Attr:daFont; Ch1:#$03D5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD PHI SYMBOL
    (Unicode:$1D78E; Attr:daFont; Ch1:#$03F1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD RHO SYMBOL
    (Unicode:$1D78F; Attr:daFont; Ch1:#$03D6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD PI SYMBOL
    (Unicode:$1D790; Attr:daFont; Ch1:#$0391; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ALPHA
    (Unicode:$1D791; Attr:daFont; Ch1:#$0392; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL BETA
    (Unicode:$1D792; Attr:daFont; Ch1:#$0393; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL GAMMA
    (Unicode:$1D793; Attr:daFont; Ch1:#$0394; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL DELTA
    (Unicode:$1D794; Attr:daFont; Ch1:#$0395; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL EPSILON
    (Unicode:$1D795; Attr:daFont; Ch1:#$0396; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ZETA
    (Unicode:$1D796; Attr:daFont; Ch1:#$0397; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL ETA
    (Unicode:$1D797; Attr:daFont; Ch1:#$0398; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA
    (Unicode:$1D798; Attr:daFont; Ch1:#$0399; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL IOTA
    (Unicode:$1D799; Attr:daFont; Ch1:#$039A; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL KAPPA
    (Unicode:$1D79A; Attr:daFont; Ch1:#$039B; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL LAMDA
    (Unicode:$1D79B; Attr:daFont; Ch1:#$039C; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL MU
    (Unicode:$1D79C; Attr:daFont; Ch1:#$039D; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL NU
    (Unicode:$1D79D; Attr:daFont; Ch1:#$039E; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL XI
    (Unicode:$1D79E; Attr:daFont; Ch1:#$039F; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMICRON
    (Unicode:$1D79F; Attr:daFont; Ch1:#$03A0; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PI
    (Unicode:$1D7A0; Attr:daFont; Ch1:#$03A1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL RHO
    (Unicode:$1D7A1; Attr:daFont; Ch1:#$03F4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL THETA SYMBOL
    (Unicode:$1D7A2; Attr:daFont; Ch1:#$03A3; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL SIGMA
    (Unicode:$1D7A3; Attr:daFont; Ch1:#$03A4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL TAU
    (Unicode:$1D7A4; Attr:daFont; Ch1:#$03A5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL UPSILON
    (Unicode:$1D7A5; Attr:daFont; Ch1:#$03A6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PHI
    (Unicode:$1D7A6; Attr:daFont; Ch1:#$03A7; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL CHI
    (Unicode:$1D7A7; Attr:daFont; Ch1:#$03A8; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL PSI
    (Unicode:$1D7A8; Attr:daFont; Ch1:#$03A9; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC CAPITAL OMEGA
    (Unicode:$1D7A9; Attr:daFont; Ch1:#$2207; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC NABLA
    (Unicode:$1D7AA; Attr:daFont; Ch1:#$03B1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ALPHA
    (Unicode:$1D7AB; Attr:daFont; Ch1:#$03B2; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL BETA
    (Unicode:$1D7AC; Attr:daFont; Ch1:#$03B3; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL GAMMA
    (Unicode:$1D7AD; Attr:daFont; Ch1:#$03B4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL DELTA
    (Unicode:$1D7AE; Attr:daFont; Ch1:#$03B5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL EPSILON
    (Unicode:$1D7AF; Attr:daFont; Ch1:#$03B6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ZETA
    (Unicode:$1D7B0; Attr:daFont; Ch1:#$03B7; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL ETA
    (Unicode:$1D7B1; Attr:daFont; Ch1:#$03B8; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL THETA
    (Unicode:$1D7B2; Attr:daFont; Ch1:#$03B9; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL IOTA
    (Unicode:$1D7B3; Attr:daFont; Ch1:#$03BA; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL KAPPA
    (Unicode:$1D7B4; Attr:daFont; Ch1:#$03BB; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL LAMDA
    (Unicode:$1D7B5; Attr:daFont; Ch1:#$03BC; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL MU
    (Unicode:$1D7B6; Attr:daFont; Ch1:#$03BD; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL NU
    (Unicode:$1D7B7; Attr:daFont; Ch1:#$03BE; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL XI
    (Unicode:$1D7B8; Attr:daFont; Ch1:#$03BF; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMICRON
    (Unicode:$1D7B9; Attr:daFont; Ch1:#$03C0; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PI
    (Unicode:$1D7BA; Attr:daFont; Ch1:#$03C1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL RHO
    (Unicode:$1D7BB; Attr:daFont; Ch1:#$03C2; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL FINAL SIGMA
    (Unicode:$1D7BC; Attr:daFont; Ch1:#$03C3; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL SIGMA
    (Unicode:$1D7BD; Attr:daFont; Ch1:#$03C4; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL TAU
    (Unicode:$1D7BE; Attr:daFont; Ch1:#$03C5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL UPSILON
    (Unicode:$1D7BF; Attr:daFont; Ch1:#$03C6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PHI
    (Unicode:$1D7C0; Attr:daFont; Ch1:#$03C7; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL CHI
    (Unicode:$1D7C1; Attr:daFont; Ch1:#$03C8; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL PSI
    (Unicode:$1D7C2; Attr:daFont; Ch1:#$03C9; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC SMALL OMEGA
    (Unicode:$1D7C3; Attr:daFont; Ch1:#$2202; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC PARTIAL DIFFERENTIAL
    (Unicode:$1D7C4; Attr:daFont; Ch1:#$03F5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC EPSILON SYMBOL
    (Unicode:$1D7C5; Attr:daFont; Ch1:#$03D1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC THETA SYMBOL
    (Unicode:$1D7C6; Attr:daFont; Ch1:#$03F0; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC KAPPA SYMBOL
    (Unicode:$1D7C7; Attr:daFont; Ch1:#$03D5; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC PHI SYMBOL
    (Unicode:$1D7C8; Attr:daFont; Ch1:#$03F1; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC RHO SYMBOL
    (Unicode:$1D7C9; Attr:daFont; Ch1:#$03D6; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD ITALIC PI SYMBOL
    (Unicode:$1D7CE; Attr:daFont; Ch1:#$0030; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT ZERO
    (Unicode:$1D7CF; Attr:daFont; Ch1:#$0031; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT ONE
    (Unicode:$1D7D0; Attr:daFont; Ch1:#$0032; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT TWO
    (Unicode:$1D7D1; Attr:daFont; Ch1:#$0033; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT THREE
    (Unicode:$1D7D2; Attr:daFont; Ch1:#$0034; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT FOUR
    (Unicode:$1D7D3; Attr:daFont; Ch1:#$0035; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT FIVE
    (Unicode:$1D7D4; Attr:daFont; Ch1:#$0036; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT SIX
    (Unicode:$1D7D5; Attr:daFont; Ch1:#$0037; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT SEVEN
    (Unicode:$1D7D6; Attr:daFont; Ch1:#$0038; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT EIGHT
    (Unicode:$1D7D7; Attr:daFont; Ch1:#$0039; Ch2:#$FFFF),       // MATHEMATICAL BOLD DIGIT NINE
    (Unicode:$1D7D8; Attr:daFont; Ch1:#$0030; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT ZERO
    (Unicode:$1D7D9; Attr:daFont; Ch1:#$0031; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT ONE
    (Unicode:$1D7DA; Attr:daFont; Ch1:#$0032; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT TWO
    (Unicode:$1D7DB; Attr:daFont; Ch1:#$0033; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT THREE
    (Unicode:$1D7DC; Attr:daFont; Ch1:#$0034; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT FOUR
    (Unicode:$1D7DD; Attr:daFont; Ch1:#$0035; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT FIVE
    (Unicode:$1D7DE; Attr:daFont; Ch1:#$0036; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT SIX
    (Unicode:$1D7DF; Attr:daFont; Ch1:#$0037; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT SEVEN
    (Unicode:$1D7E0; Attr:daFont; Ch1:#$0038; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT EIGHT
    (Unicode:$1D7E1; Attr:daFont; Ch1:#$0039; Ch2:#$FFFF),       // MATHEMATICAL DOUBLE-STRUCK DIGIT NINE
    (Unicode:$1D7E2; Attr:daFont; Ch1:#$0030; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT ZERO
    (Unicode:$1D7E3; Attr:daFont; Ch1:#$0031; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT ONE
    (Unicode:$1D7E4; Attr:daFont; Ch1:#$0032; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT TWO
    (Unicode:$1D7E5; Attr:daFont; Ch1:#$0033; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT THREE
    (Unicode:$1D7E6; Attr:daFont; Ch1:#$0034; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT FOUR
    (Unicode:$1D7E7; Attr:daFont; Ch1:#$0035; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT FIVE
    (Unicode:$1D7E8; Attr:daFont; Ch1:#$0036; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT SIX
    (Unicode:$1D7E9; Attr:daFont; Ch1:#$0037; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT SEVEN
    (Unicode:$1D7EA; Attr:daFont; Ch1:#$0038; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT EIGHT
    (Unicode:$1D7EB; Attr:daFont; Ch1:#$0039; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF DIGIT NINE
    (Unicode:$1D7EC; Attr:daFont; Ch1:#$0030; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT ZERO
    (Unicode:$1D7ED; Attr:daFont; Ch1:#$0031; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT ONE
    (Unicode:$1D7EE; Attr:daFont; Ch1:#$0032; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT TWO
    (Unicode:$1D7EF; Attr:daFont; Ch1:#$0033; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT THREE
    (Unicode:$1D7F0; Attr:daFont; Ch1:#$0034; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT FOUR
    (Unicode:$1D7F1; Attr:daFont; Ch1:#$0035; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT FIVE
    (Unicode:$1D7F2; Attr:daFont; Ch1:#$0036; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT SIX
    (Unicode:$1D7F3; Attr:daFont; Ch1:#$0037; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT SEVEN
    (Unicode:$1D7F4; Attr:daFont; Ch1:#$0038; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT EIGHT
    (Unicode:$1D7F5; Attr:daFont; Ch1:#$0039; Ch2:#$FFFF),       // MATHEMATICAL SANS-SERIF BOLD DIGIT NINE
    (Unicode:$1D7F6; Attr:daFont; Ch1:#$0030; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT ZERO
    (Unicode:$1D7F7; Attr:daFont; Ch1:#$0031; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT ONE
    (Unicode:$1D7F8; Attr:daFont; Ch1:#$0032; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT TWO
    (Unicode:$1D7F9; Attr:daFont; Ch1:#$0033; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT THREE
    (Unicode:$1D7FA; Attr:daFont; Ch1:#$0034; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT FOUR
    (Unicode:$1D7FB; Attr:daFont; Ch1:#$0035; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT FIVE
    (Unicode:$1D7FC; Attr:daFont; Ch1:#$0036; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT SIX
    (Unicode:$1D7FD; Attr:daFont; Ch1:#$0037; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT SEVEN
    (Unicode:$1D7FE; Attr:daFont; Ch1:#$0038; Ch2:#$FFFF),       // MATHEMATICAL MONOSPACE DIGIT EIGHT
    (Unicode:$1D7FF; Attr:daFont; Ch1:#$0039; Ch2:#$FFFF)        // MATHEMATICAL MONOSPACE DIGIT NINE
    );

function LocateHighUCS4DecompositionInfo(const Ch: UCS4Char): Integer;
var L, H, I : Integer;
    D       : UCS4Char;
begin
  if (Ch < $1D000) or (Ch > $1D7FF) then
    begin
      Result := -1;
      exit;
    end;
  // Binary search
  L := 0;
  H := UnicodeUCS4DecompositionEntries - 1;
  repeat
    I := (L + H) div 2;
    D := UnicodeUCS4DecompositionInfo[I].Unicode;
    if D = Ch then
      begin
        Result := I;
        exit;
      end else
    if D > Ch then
      H := I - 1 else
      L := I + 1;
  until L > H;
  Result := -1;
end;

{$IFDEF CLR}
function GetCharacterDecomposition(const Ch: UCS4Char): WideString;
var I : Integer;
    P : TUnicodeUCS4DecompositionInfo;
begin
  if Ch < $10000 then
    Result := GetCharacterDecomposition(WideChar(Ch))
  else
    begin
      if Ch and $FFF00 = $1D100 then // UCS4 decompositions
        begin
          (*
              (Unicode:$1D15E; Attr:daNone; Ch1:#$1D157; Ch2:#$1D165),     // MUSICAL SYMBOL HALF NOTE
              (Unicode:$1D15F; Attr:daNone; Ch1:#$1D158; Ch2:#$1D165),     // MUSICAL SYMBOL QUARTER NOTE
              (Unicode:$1D160; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D16E),     // MUSICAL SYMBOL EIGHTH NOTE
              (Unicode:$1D161; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D16F),     // MUSICAL SYMBOL SIXTEENTH NOTE
              (Unicode:$1D162; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D170),     // MUSICAL SYMBOL THIRTY-SECOND NOTE
              (Unicode:$1D163; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D171),     // MUSICAL SYMBOL SIXTY-FOURTH NOTE
              (Unicode:$1D164; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D172),     // MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
              (Unicode:$1D1BB; Attr:daNone; Ch1:#$1D1B9; Ch2:#$1D165),     // MUSICAL SYMBOL MINIMA
              (Unicode:$1D1BC; Attr:daNone; Ch1:#$1D1BA; Ch2:#$1D165),     // MUSICAL SYMBOL MINIMA BLACK
              (Unicode:$1D1BD; Attr:daNone; Ch1:#$1D1BB; Ch2:#$1D16E),     // MUSICAL SYMBOL SEMIMINIMA WHITE
              (Unicode:$1D1BE; Attr:daNone; Ch1:#$1D1BC; Ch2:#$1D16E),     // MUSICAL SYMBOL SEMIMINIMA BLACK
              (Unicode:$1D1BF; Attr:daNone; Ch1:#$1D1BB; Ch2:#$1D16F),     // MUSICAL SYMBOL FUSA WHITE
              (Unicode:$1D1C0; Attr:daNone; Ch1:#$1D1BC; Ch2:#$1D16F),     // MUSICAL SYMBOL FUSA BLACK
          *)
        end;
      I := LocateHighUCS4DecompositionInfo(Ch);
      if I < 0 then
        Result := ''
      else
        begin
          P := UnicodeUCS4DecompositionInfo[I];
          if P.Ch2 = #$FFFF then
            Result := P.Ch1
          else
            begin
              SetLength(Result, 2);
              Result[1] := P.Ch1;
              Result[2] := P.Ch2;
            end;
        end;
    end;
end;
{$ELSE}
function GetCharacterDecomposition(const Ch: UCS4Char): WideString;
var I : Integer;
    P : PUnicodeUCS4DecompositionInfo;
begin
  if Ch < $10000 then
    Result := GetCharacterDecomposition(WideChar(Ch))
  else
    begin
      if Ch and $FFF00 = $1D100 then // UCS4 decompositions
        begin
          (*
              (Unicode:$1D15E; Attr:daNone; Ch1:#$1D157; Ch2:#$1D165),     // MUSICAL SYMBOL HALF NOTE
              (Unicode:$1D15F; Attr:daNone; Ch1:#$1D158; Ch2:#$1D165),     // MUSICAL SYMBOL QUARTER NOTE
              (Unicode:$1D160; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D16E),     // MUSICAL SYMBOL EIGHTH NOTE
              (Unicode:$1D161; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D16F),     // MUSICAL SYMBOL SIXTEENTH NOTE
              (Unicode:$1D162; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D170),     // MUSICAL SYMBOL THIRTY-SECOND NOTE
              (Unicode:$1D163; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D171),     // MUSICAL SYMBOL SIXTY-FOURTH NOTE
              (Unicode:$1D164; Attr:daNone; Ch1:#$1D15F; Ch2:#$1D172),     // MUSICAL SYMBOL ONE HUNDRED TWENTY-EIGHTH NOTE
              (Unicode:$1D1BB; Attr:daNone; Ch1:#$1D1B9; Ch2:#$1D165),     // MUSICAL SYMBOL MINIMA
              (Unicode:$1D1BC; Attr:daNone; Ch1:#$1D1BA; Ch2:#$1D165),     // MUSICAL SYMBOL MINIMA BLACK
              (Unicode:$1D1BD; Attr:daNone; Ch1:#$1D1BB; Ch2:#$1D16E),     // MUSICAL SYMBOL SEMIMINIMA WHITE
              (Unicode:$1D1BE; Attr:daNone; Ch1:#$1D1BC; Ch2:#$1D16E),     // MUSICAL SYMBOL SEMIMINIMA BLACK
              (Unicode:$1D1BF; Attr:daNone; Ch1:#$1D1BB; Ch2:#$1D16F),     // MUSICAL SYMBOL FUSA WHITE
              (Unicode:$1D1C0; Attr:daNone; Ch1:#$1D1BC; Ch2:#$1D16F),     // MUSICAL SYMBOL FUSA BLACK
          *)
        end;
      I := LocateHighUCS4DecompositionInfo(Ch);
      if I < 0 then
        Result := ''
      else
        begin
          P := @UnicodeUCS4DecompositionInfo[I];
          if P^.Ch2 = #$FFFF then
            Result := P^.Ch1
          else
            begin
              SetLength(Result, 2);
              Result[1] := P^.Ch1;
              Result[2] := P^.Ch2;
            end;
        end;
    end;
end;
{$ENDIF}



{                                                                              }
{ Match                                                                        }
{                                                                              }
function WideMatchAnsiCharNoCase(const M: AnsiChar; const C: WideChar): Boolean;
const ASCIICaseOffset = Ord('a') - Ord('A');
var D, N : AnsiChar;
begin
  if Ord(C) > $7F then
    begin
      Result := False;
      exit;
    end;
  D := AnsiChar(Ord(C));
  if D in ['A'..'Z'] then
    D := AnsiChar(Ord(D) + ASCIICaseOffset);
  N := M;
  if N in ['A'..'Z'] then
    N := AnsiChar(Ord(N) + ASCIICaseOffset);
  Result := D = N;
end;

{$IFNDEF CLR}
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
  repeat
    if not CharMatchFunc(C) then
      exit;
    Inc(Result);
    Inc(Q);
    if L > 0 then
      Dec(L);
    C := Q^;
  until (L = 0) or ((L < 0) and (Ord(C) = 0));
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
  repeat
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
  until (L = 0) or ((L < 0) and (Ord(C) = 0));
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
  repeat
    if CharMatchFunc(C) then
      Inc(Result);
    Inc(Q);
    if L > 0 then
      Dec(L);
    C := Q^;
  until (L = 0) or ((L < 0) and (Ord(C) = 0));
end;

function WidePMatchAnsiStr(const M: AnsiString; const P: PWideChar;
    const CaseSensitive: Boolean): Boolean;
var I, L : Integer;
    Q : PWideChar;
    R : PAnsiChar;
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
      for I := 1 to L do
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
      for I := 1 to L do
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
  for I := 1 to L do
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
{$ENDIF}

function WideEqualAnsiStr(const M: AnsiString; const S: WideString;
    const CaseSensitive: Boolean): Boolean;
var L : Integer;
begin
  L := Length(M);
  Result := L = Length(S);
  if not Result or (L = 0) then
    exit;
  Result := WidePMatchAnsiStr(M, Pointer(S), CaseSensitive);
end;

function WideMatchLeftAnsiStr(const M: AnsiString; const S: WideString;
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
{$IFNDEF CLR}
function WideZPosAnsiChar(const F: AnsiChar; const P: PWideChar): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  while Ord(Q^) <> 0 do
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
  while Ord(Q^) <> 0 do
    if (Ord(Q^) < $80) and (AnsiChar(Ord(Q^)) in F) then
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
  while Ord(Q^) <> 0 do
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

function WideZPosAnsiStr(const F: AnsiString; const P: PWideChar; const CaseSensitive: Boolean): Integer;
var Q : PWideChar;
    I : Integer;
begin
  Result := -1;
  Q := P;
  if not Assigned(Q) then
    exit;
  I := 0;
  while Ord(Q^) <> 0 do
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
{$ENDIF}



{                                                                              }
{ Skip                                                                         }
{                                                                              }
{$IFNDEF CLR}
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
  while Ord(C) <> 0 do
    if not CharMatchFunc(C) then
      exit
    else
      begin
        Inc(P);
        Inc(Result);
        C := P^;
      end;
end;

function WidePSkipAnsiChar(const Ch: AnsiChar; var P: PWideChar): Boolean;
begin
  Result := Ord(P^) = Ord(Ch);
  if Result then
    Inc(P);
end;

function WidePSkipAnsiStr(const M: AnsiString; var P: PWideChar; const CaseSensitive: Boolean): Boolean;
begin
  Result := WidePMatchAnsiStr(M, P, CaseSensitive);
  if Result then
    Inc(P, Length(M));
end;
{$ENDIF}



{                                                                              }
{ Extract                                                                      }
{                                                                              }
{$IFNDEF CLR}
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

function WideZExtractBeforeAnsiChar(const Ch: AnsiChar; var P: PWideChar; var S: WideString): Boolean;
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

function WideZExtractAnsiCharDelimited(const LeftDelimiter, RightDelimiter: AnsiChar;
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

function WideZExtractAnsiCharQuoted(const Delimiter: AnsiChar; var P: PWideChar; var S: WideString): Boolean;
begin
  Result := WideZExtractAnsiCharDelimited(Delimiter, Delimiter, P, S);
end;
{$ENDIF}



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
  for I := 1 to Count do
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
    F : WideCharMatchFunction;
begin
  L := Length(S);
  if L = 0 then
    exit;
  F := MatchFunc;
  if not Assigned(F) then
    F := IsWhiteSpace;
  I := 0;
  P := Pointer(S);
  while F(P^) do
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
    F : WideCharMatchFunction;
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
  while F(P^) do
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
  while F(P^) or IsControl(P^) do
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
  while F(P^) or IsControl(P^) do
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
  for I := 1 to Length(S) do
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
  for I := StartIndex to L do
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
  for I := StartIndex to L do
    if (Ord(P^) <= $FF) and (AnsiChar(P^) in F) then
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
  for I := StartIndex to L do
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
  for I := 1 to M do
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
  for I := 1 to Length(S) do
    Result := Result + WideUpCaseFolding(S[I]);
end;

function WideLowerCase(const S: WideString): WideString;
var I : Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
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
      for I := 0 to LR - 1 do
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
  repeat
    I := WidePos(D, S, I);
    if I = 0 then
      break;
    Inc(L);
    Inc(I, M);
  until False;
  SetLength(Result, L + 1);
  if L = 0 then
    begin
      Result[0] := S;
      exit;
    end;
  L := 0;
  I := 1;
  repeat
    J := WidePos(D, S, I);
    if J = 0 then
      begin
        Result[L] := WideCopyFrom(S, I);
        break;
      end;
    Result[L] := WideCopyRange(S, I, J - 1);
    Inc(L);
    I := J + M;
  until False;
end;



{                                                                              }
{ Self-testing code                                                            }
{                                                                              }
{$IFDEF DEBUG}{$IFDEF SELFTEST}
{$ASSERTIONS ON}
procedure SelfTest;
var S : WideString;
begin
  Assert(IsASCIIChar('A'), 'IsASCIIChar');
  Assert(not IsASCIIChar(#$1234), 'IsASCIIChar');
  Assert(IsWhiteSpace(' '), 'IsWhiteSpace');
  Assert(IsPunctuation('.'), 'IsPunctuation');
  Assert(not IsPunctuation('A'), 'IsPunctuation');
  Assert(IsDecimalDigit(WideChar('0')), 'IsDecimalDigit');
  Assert(DecimalDigitValue(WideChar('5')) = 5, 'DecimalDigitValue');
  Assert(IsUpperCase('A'), 'IsUpperCase');
  Assert(not IsUpperCase('a'), 'IsUpperCase');
  Assert(WideUpCase('a') = 'A', 'WideUpCase');
  Assert(WideUpCase('A') = 'A', 'WideUpCase');
  Assert(WideLowCase('a') = 'a', 'WideUpCase');
  Assert(WideLowCase('A') = 'a', 'WideUpCase');
  Assert(IsLetter('A'), 'IsLetter');
  Assert(not IsLetter('1'), 'IsLetter');

  Assert(WideMatchAnsiCharNoCase('A', 'a'), 'WideMatchAnsiCharNoCase');
  Assert(WideMatchAnsiCharNoCase('z', 'Z'), 'WideMatchAnsiCharNoCase');
  Assert(WideMatchAnsiCharNoCase('1', '1'), 'WideMatchAnsiCharNoCase');
  Assert(not WideMatchAnsiCharNoCase('A', 'B'), 'WideMatchAnsiCharNoCase');
  Assert(not WideMatchAnsiCharNoCase('0', 'A'), 'WideMatchAnsiCharNoCase');

  Assert(WidePMatchAnsiStr('Unicode', 'uNicode', False), 'WidePMatchAnsiStr');
  Assert(not WidePMatchAnsiStr('Unicode', 'uNicode', True), 'WidePMatchAnsiStr');
  Assert(WidePMatchAnsiStr('Unicode', 'Unicode', True), 'WidePMatchAnsiStr');

  S := ' X ';
  WideTrimLeftInPlace(S, nil);
  Assert(S = 'X ', 'WideTrimLeftInPlace');
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
  {$IFNDEF CLR}
  Assert(WideZPosAnsiChar('A', 'XYZABCAACDEF') = 3, 'WideZPosAnsiChar');
  Assert(WideZPosAnsiChar('Q', 'XYZABCAACDEF') = -1, 'WideZPosAnsiChar');
  {$ENDIF}
end;
{$ENDIF}{$ENDIF}



end.

