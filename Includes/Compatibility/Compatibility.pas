unit Compatibility;

{TSI:IGNORE ON} // Disable this unit localization in TsiLang.

{$I jedi.inc}

interface

uses
  {$IFDEF HAS_UNIT_TYPES}Types,{$ENDIF}
  SysConst,
  Windows,
  SysUtils,
  Classes,
  ActiveX;

{$IFNDEF SUPPORTS_UNICODE_STRING}
type
  UnicodeString = WideString;
{$ENDIF}
{$IFNDEF Compiler12_Up} // Delphi11_Down
type
  RawByteString = AnsiString;
{$ENDIF}

{$IFNDEF Compiler5_Up} // Delphi4_Down
type
  TOleEnum = type LongWord;
{$ENDIF}

{$IFNDEF Compiler6_Up} // Delphi5_Down
type
  UTF8String = AnsiString;
  TSeekOrigin = (soBeginning, soCurrent, soEnd);
  PBoolean = ^Boolean;
  PPointer = ^Pointer;
  PCardinal = ^Cardinal;
  IInterface = IUnknown;
{$ENDIF}

{$IFNDEF Compiler7_Up} // Delphi6_Down
type
  UInt64 = Int64;
{$ENDIF}

{$IFNDEF Compiler11_Up} // Delphi10_Down
type
  TBytes = array of Byte;
  TSysCharSet = set of AnsiChar;
{$ENDIF}

{$IFNDEF Compiler12_Up} // Delphi11_Down
type
  TCharArray = array of Char;
  Int8   = ShortInt;
  Int16  = SmallInt;
  Int32  = LongInt;
  UInt8  = Byte;
  UInt16 = Word;
  UInt32 = {$IFDEF SUPPORTS_LONGWORD}LongWord{$ELSE}Cardinal{$ENDIF}; // TODO: verify
  NativeInt  = Int32;
  NativeUInt = UInt32;
{$ENDIF}

type
  WCHAR = Windows.WChar;
  PWChar = Windows.PWChar;
  LPSTR = Windows.LPSTR;
  PLPSTR = Windows.PLPSTR;
  LPCSTR = Windows.LPCSTR;
  LPCTSTR = Windows.LPCTSTR;
  LPTSTR = Windows.LPTSTR;
  LPWSTR = Windows.LPWSTR;
  PLPWSTR = Windows.PLPWSTR;
  LPCWSTR = Windows.LPCWSTR;
  BOOL = Windows.BOOL;
  PBOOL = Windows.PBOOL;
  PINT = Windows.PINT;
  PSingle = Windows.PSingle;
  PWORD = Windows.PWORD;
  PDWORD = Windows.PDWORD;
  LPDWORD = Windows.LPDWORD;
  UCHAR = Windows.UCHAR;
  PUCHAR = Windows.PUCHAR;
  SHORT = Windows.SHORT;
  UINT = Windows.UINT;
  PUINT = Windows.PUINT;
  ULONG = Windows.ULONG;
  PULONG = Windows.PULONG;

  {$IFDEF HAS_UNIT_TYPES}
  DWORD = Types.DWORD;
  PByte = Types.PByte;
  {$ELSE}
  DWORD = Windows.DWORD;
  PByte = Windows.PByte;
  {$ENDIF}
  
  {$IFDEF Compiler11_up}
  INT_PTR = Windows.INT_PTR;
  LONG_PTR = Windows.LONG_PTR;
  UINT_PTR = Windows.UINT_PTR;
  ULONG_PTR = Windows.ULONG_PTR;
  DWORD_PTR = Windows.DWORD_PTR;
  {$ELSE}
  INT_PTR = Integer;
  LONG_PTR = Longint;
  UINT_PTR = Cardinal;
  ULONG_PTR = LongWord;
  DWORD_PTR = ULONG_PTR;
  {$ENDIF}

  {$IFDEF Compiler14_up}
  PLPCTSTR = Windows.PLPCTSTR;
  PLPTSTR = Windows.PLPTSTR;
  LPBYTE = Windows.LPBYTE;
  USHORT = Windows.USHORT;
  HANDLE_PTR = Windows.HANDLE_PTR;
  {$ELSE}
  PPAnsiChar = ^PAnsiChar;
  PPWideChar = ^PWideChar;
  PLPCTSTR = {$IFDEF UNICODE}PPWideChar{$ELSE}PPAnsiChar{$ENDIF};
  PLPTSTR = {$IFDEF UNICODE}PPWideChar{$ELSE}PPAnsiChar{$ENDIF};
  LPBYTE = PByte;
  USHORT = Word;
  HANDLE_PTR = type LongWord;
  {$ENDIF}

  {$IFDEF Compiler15_up}
  LPVOID = Windows.LPVOID;
  LPCVOID = Windows.LPCVOID;
  ULONG32 = Windows.ULONG32;
  PVOID = Windows.PVOID;
  PPVOID = Windows.PPVOID;
  LONG64 = Windows.LONG64;
  ULONG64 = Windows.ULONG64;
  PLONG64 = Windows.PLONG64;
  PULONG64 = Windows.PULONG64;
  {$ELSE}
  LPVOID = Pointer;
  LPCVOID = Pointer;
  ULONG32 = LongWord;
  PVOID = Pointer;
  PPVOID = ^PVOID;
  LONG64 = Int64;
  ULONG64 = UInt64;
  PLONG64 = ^LONG64;
  PULONG64 = ^ULONG64;
  {$ENDIF}

  HINTERNET = Pointer;
  INTERNET_PORT = Word;
  DWORD64  = UInt64;
  PDWORD64 = ^DWORD64;
  PtrInt32   = Int32;
  PtrUInt32  = UInt32;
  PtrInt64   = Int64;
  PtrUInt64  = UInt64;
  PPtrInt32  = ^PtrInt32;
  PPtrUInt32 = ^PtrUInt32;
  PPtrInt64  = ^PtrInt64;
  PPtrUInt64 = ^PtrUInt64;
  {$IFDEF 64BIT}
  PtrInt     = PtrInt64;
  PtrUInt    = PtrUInt64;
  {$ELSE ~64BIT}
  PtrInt     = PtrInt32;
  PtrUInt    = PtrUInt32;
  {$ENDIF}
  PPtrInt    = ^PtrInt;
  PPtrUInt   = ^PtrUInt;

{$IFDEF MSWINDOWS}
  TModuleHandle = HINST;
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
  TModuleHandle = Pointer;
{$ENDIF LINUX}

const
  MAX_MODULE_NAME32 = 255;

const
  {$IFNDEF Compiler6_up}
  PathDelim  = {$IFDEF MSWINDOWS} '\'; {$ELSE} '/'; {$ENDIF} // Do Not Localize
  DriveDelim = {$IFDEF MSWINDOWS} ':'; {$ELSE} '';  {$ENDIF} // Do Not Localize
  PathSep    = {$IFDEF MSWINDOWS} ';'; {$ELSE} ':'; {$ENDIF} // Do Not Localize
  {$ENDIF}
  PathDevicePrefix        = '\\.\';    // Do Not Localize
  PathUncPrefix           = '\\';      // Do Not Localize
  DoubleQuote             = '"';       // Do Not Localize
  ClassAndProcSeparator   = '.';       // Do Not Localize
  AnyChar                 = '?';       // Do Not Localize
  AnyChars                = '*';       // Do Not Localize
  AllFilesMask            = '*.*';     // Do Not Localize
  ThisDir                 = '.';       // Do Not Localize
  UpperDir                = '..';      // Do Not Localize
  CarriageReturn          = #13;       // Do Not Localize
  LineFeed                = #10;       // Do Not Localize
  ModuleCodeOffset        = $1000;

{$IFNDEF Compiler6_Up} // Delphi5_Down
const
  SwitchChars = {$IFDEF MSWINDOWS} ['/','-']; {$ENDIF} // Do Not Localize
                {$IFDEF LINUX}  ['-'];  {$ENDIF} // Do Not Localize
  sLineBreak = {$IFDEF LINUX} #10 {$ENDIF} {$IFDEF MSWINDOWS} #13#10 {$ENDIF}; // Do Not Localize

  HoursPerDay           = 24;
  MinsPerHour           = 60;
  SecsPerMin            = 60;
  MSecsPerSec           = 1000;
  MinsPerDay            = HoursPerDay * MinsPerHour;
  SecsPerDay            = MinsPerDay * SecsPerMin;
  MSecsPerDay           = SecsPerDay * MSecsPerSec;

  DaysPerWeek           = 7;
  WeeksPerFortnight     = 2;
  MonthsPerYear         = 12;
  YearsPerDecade        = 10;
  YearsPerCentury       = 100;
  YearsPerMillennium    = 1000;

  DayMonday             = 1;
  DayTuesday            = 2;
  DayWednesday          = 3;
  DayThursday           = 4;
  DayFriday             = 5;
  DaySaturday           = 6;
  DaySunday             = 7;

  OneHour               = 1 / HoursPerDay;
  OneMinute             = 1 / MinsPerDay;
  OneSecond             = 1 / SecsPerDay;
  OneMillisecond        = 1 / MSecsPerDay;

  { This is actual days per year but you need to know if it's a leap year}
  DaysPerYear: array [Boolean] of Word = (365, 366);
{$ENDIF}

{$IFNDEF Compiler12_Up} // Delphi11_Down
const
  INVALID_FILE_ATTRIBUTES             = Cardinal($FFFFFFFF);
{$ENDIF}

const
  INVALID_MODULEHANDLE_VALUE = TModuleHandle(0);

{$IFNDEF Compiler6_Up} // Delphi5_Down
type
  EOSError = class(EWin32Error);
{$ENDIF}

{$IFNDEF Compiler5_Up} // Delphi4_Down
function AnsiSameStr(const S1, S2: string): Boolean;
function AnsiSameText(const S1, S2: string): Boolean;
function SameText(const S1, S2: string): Boolean; assembler;
procedure FreeAndNil(var Obj); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function SafeLoadLibrary(const FileName: String; ErrorMode: UINT{$IFDEF SUPPORTS_DEFAULTPARAMS} = SEM_NOOPENFILEERRORBOX{$ENDIF}): HMODULE;
function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;{$IFDEF SUPPORTS_OVERLOAD}overload;
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;{$ENDIF}
{$ENDIF}

{$IFNDEF Compiler6_Up} // Delphi5_Down
const
  DefaultTrueBoolStr = 'True';   // DO NOT LOCALIZE
  DefaultFalseBoolStr = 'False'; // DO NOT LOCALIZE

var
  TrueBoolStrs: array of String;
  FalseBoolStrs: array of String;

function Rect(Left, Top, Right, Bottom: Integer): TRect;
function SameFileName(const S1, S2: string): Boolean;
function AnsiCompareFileName(const S1, S2: string): Integer;
function AnsiLowerCaseFileName(const S: string): string;
function ExcludeTrailingPathDelimiter(const S: string): string;
function IncludeTrailingPathDelimiter(const S: string): string;
function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
function AnsiToUtf8(const S: string): UTF8String;
function Utf8ToAnsi(const S: UTF8String): string;
procedure RaiseLastOSError;
function MinuteSpan(const ANow, AThen: TDateTime): Double;
function MinutesBetween(const ANow, AThen: TDateTime): Int64;
function GetModuleName(Module: HMODULE): string;
function GetEnvironmentVariable(const Name: string): string; {$IFDEF SUPPORTS_OVERLOAD} overload;
function GetEnvironmentVariable(lpName: PChar; lpBuffer: PChar; nSize: DWORD): DWORD; overload; {$ENDIF}
function AnsiStartsText(const ASubText, AText: string): Boolean;
function AnsiStartsStr(const ASubText, AText: string): Boolean;
function AnsiEndsText(const ASubText, AText: string): Boolean;
function AnsiEndsStr(const ASubText, AText: string): Boolean;
function FindCmdLineSwitch(const Switch: string;
  SwitchChars: TSysCharSet = ['/']; // Do Not Localize
  IgnoreCase: Boolean = True): Boolean;
function TryStrToInt(const S: string; out Value: Integer): Boolean;
function TryStrToInt64(const S: string; out Value: Int64): Boolean;
function StrToBool(const S: string): Boolean; overload;
function StrToBoolDef(const S: string; const Default: Boolean): Boolean; overload;
function TryStrToBool(const S: string; out Value: Boolean): Boolean; overload;
function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
function AnsiQuotedStr(const S: string; Quote: Char): string;
function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
function AnsiDequotedStr(const S: string; AQuote: Char): string;
function TryStrToDateTime(const S: string;
  out Value: TDateTime): Boolean;
function StrToDateTimeDef(const S: string;
  const Default: TDateTime): TDateTime;
function TryStrToFloat(const S: string; out Value: Extended): Boolean;
function StrToFloatDef(const S: string; const Default: Extended): Extended;
function CreateGUID(out Guid: TGUID): HResult;
function StringToGUID(const S: string): TGUID;
function GUIDToString(const ClassID: TGUID): string;
function InRange(const AValue, AMin, AMax: Integer): Boolean; overload;
function InRange(const AValue, AMin, AMax: Int64): Boolean; overload;
function InRange(const AValue, AMin, AMax: Double): Boolean; overload;
function EnsureRange(const AValue, AMin, AMax: Integer): Integer; overload;
function EnsureRange(const AValue, AMin, AMax: Int64): Int64; overload;
function EnsureRange(const AValue, AMin, AMax: Double): Double; overload;

{$ENDIF}

{$IFNDEF Compiler7_Up} // Delphi6_Down
function CheckWin32Version(AMajor: Integer; AMinor: Integer{$IFDEF SUPPORTS_DEFAULTPARAMS} = 0{$ENDIF}): Boolean; // see comments in QC 2457

const
  CSTR_LESS_THAN           = 1;             { string 1 less than string 2 }
  CSTR_EQUAL               = 2;             { string 1 equal to string 2 }
  CSTR_GREATER_THAN        = 3;             { string 1 greater than string 2 }
{$ENDIF}

{$IFNDEF Compiler9_up} // Delphi7_down
function SameStr(const S1, S2: string): Boolean;
{$ENDIF}

{$IFNDEF Compiler10_Up} // Delphi9_Down
{$IFDEF Compiler6_Up} // unsupported assembler
procedure Move(const Source; var Dest; Count: Integer); // see QC 14083
{$ENDIF}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
{$ENDIF}

{$IFNDEF Compiler11_Up} // Delphi10_Down
function FileExists(const FileName: string): Boolean; // see QC 52905, 3513
{$ENDIF}

{$IFNDEF Compiler12_Up} // Delphi11_Down
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; {$IFDEF SUPPORTS_OVERLOAD}overload;{$ENDIF}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function {$IFDEF SUPPORTS_OVERLOAD}CharInSet{$ELSE}CharInSetW{$ENDIF}(C: WideChar; const CharSet: TSysCharSet): Boolean; {$IFDEF SUPPORTS_OVERLOAD}overload;{$ENDIF}{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

function DirectoryExists(const Directory: string): Boolean; // see QC 68017

procedure MoveChars(const Source; var Dest; Length: Integer);{$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UIntToStr(Value: Cardinal): string; overload;
function UIntToStr(Value: UInt64): string; overload;
function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comperand: Pointer): Pointer;

function UTF8Encode(const WS: WideString): RawByteString; {$IFDEF Compiler6_up}overload; // buggy D5 unable to select correct version: Wide vs Ansi String
function UTF8Encode(const A: RawByteString): RawByteString; overload;{$ENDIF}
function UTF8EncodeToShortString(const WS: WideString): ShortString; {$IFDEF Compiler6_up}overload;
function UTF8EncodeToShortString(const A: RawByteString): ShortString; overload;{$ENDIF}
function UTF8ToWideString(const S: RawByteString): WideString; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
function UTF8ToUnicodeString(const S: RawByteString): UnicodeString; {$IFDEF Compiler6_up} overload;
function UTF8ToUnicodeString(const S: ShortString): UnicodeString; overload;
function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString; overload;{$ENDIF SUPPORTS_OVERLOAD}
function UTF8ToString(const S: RawByteString): string;{$IFDEF Compiler6_up} overload;
function UTF8ToString(const S: ShortString): string; overload;
function UTF8ToString(const S: PAnsiChar): string; overload;{$ENDIF SUPPORTS_OVERLOAD}

const
  IS_TEXT_UNICODE_ASCII16             = $0001;
  IS_TEXT_UNICODE_REVERSE_ASCII16     = $0010;
  IS_TEXT_UNICODE_STATISTICS          = $0002;
  IS_TEXT_UNICODE_REVERSE_STATISTICS  = $0020;
  IS_TEXT_UNICODE_CONTROLS            = $0004;
  IS_TEXT_UNICODE_REVERSE_CONTROLS    = $0040;
  IS_TEXT_UNICODE_SIGNATURE           = $0008;
  IS_TEXT_UNICODE_REVERSE_SIGNATURE   = $0080;
  IS_TEXT_UNICODE_ILLEGAL_CHARS       = $0100;
  IS_TEXT_UNICODE_ODD_LENGTH          = $0200;
  IS_TEXT_UNICODE_DBCS_LEADBYTE       = $0400;
  IS_TEXT_UNICODE_NULL_BYTES          = $1000;
  IS_TEXT_UNICODE_UNICODE_MASK        = $000F;
  IS_TEXT_UNICODE_REVERSE_MASK        = $00F0;
  IS_TEXT_UNICODE_NOT_UNICODE_MASK    = $0F00;
  IS_TEXT_UNICODE_NOT_ASCII_MASK      = $F000;
{$ENDIF}

{$IFDEF Compiler12_Up}

type
  EEncodingError = SysUtils.EEncodingError;
  TEncoding = SysUtils.TEncoding;
  TMBCSEncoding = SysUtils.TMBCSEncoding;
  TUTF7Encoding = SysUtils.TUTF7Encoding;
  TUTF8Encoding = SysUtils.TUTF8Encoding;
  TUnicodeEncoding = SysUtils.TUnicodeEncoding;
  TBigEndianUnicodeEncoding = SysUtils.TBigEndianUnicodeEncoding;

{$ELSE Compiler12_Up}

type
  EEncodingError = class(Exception);

  TEncoding = class
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF} private
    {$IFDEF SUPPORTS_CLASS_FIELDS}
    class var
      FASCIIEncoding: TEncoding;
      FBigEndianUnicodeEncoding: TEncoding;
      FDefaultEncoding: TEncoding;
      FUnicodeEncoding: TEncoding;
      FUTF7Encoding: TEncoding;
      FUTF8Encoding: TEncoding;
    {$ENDIF}
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF} protected
    FIsSingleByte: Boolean;
    FMaxCharSize: Integer;
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; virtual; abstract;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; virtual; abstract;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; virtual; abstract;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; virtual; abstract;
  public
    class function Convert(Source, Destination: TEncoding; Bytes: TBytes): TBytes; overload;
    class function Convert(Source, Destination: TEncoding; Bytes: TBytes; StartIndex, Count: Integer): TBytes; overload;
    class procedure FreeEncodings;
    class function IsStandardEncoding(AEncoding: TEncoding): Boolean; {$IFDEF SUPPORTS_STATIC}static;{$ENDIF}
    class function GetBufferEncoding(const Buffer: TBytes; var AEncoding: TEncoding): Integer; {$IFDEF SUPPORTS_STATIC}static;{$ENDIF}
    function GetByteCount(const Chars: TCharArray): Integer; overload;
    function GetByteCount(const Chars: TCharArray; CharIndex, CharCount: Integer): Integer; overload;
    function GetByteCount(const S: string): Integer; overload;
    function GetByteCount(const S: string; CharIndex, CharCount: Integer): Integer; overload;
    function GetBytes(const Chars: TCharArray): TBytes; overload;
    function GetBytes(const Chars: TCharArray; CharIndex, CharCount: Integer;
      var Bytes: TBytes; ByteIndex: Integer): Integer; overload;
    function GetBytes(const S: string): TBytes; overload;
    function GetBytes(const S: string; CharIndex, CharCount: Integer;
      var Bytes: TBytes; ByteIndex: Integer): Integer; overload;
    function GetCharCount(const Bytes: TBytes): Integer; overload;
    function GetCharCount(const Bytes: TBytes; ByteIndex, ByteCount: Integer): Integer; overload;
    function GetChars(const Bytes: TBytes): TCharArray; overload;
    function GetChars(const Bytes: TBytes; ByteIndex, ByteCount: Integer): TCharArray; overload;
    function GetChars(const Bytes: TBytes; ByteIndex, ByteCount: Integer;
      var Chars: TCharArray; CharIndex: Integer): Integer; overload;
    class function GetEncoding(CodePage: Integer): TEncoding; {$IFDEF SUPPORTS_STATIC}static;{$ENDIF}
    function GetMaxByteCount(CharCount: Integer): Integer; virtual; abstract;
    function GetMaxCharCount(ByteCount: Integer): Integer; virtual; abstract;
    function GetPreamble: TBytes; virtual; abstract;
    function GetString(const Bytes: TBytes): string; overload;
    function GetString(const Bytes: TBytes; ByteIndex, ByteCount: Integer): string; overload;
    class function ASCII: TEncoding;
    class function BigEndianUnicode: TEncoding;
    class function Default: TEncoding;
    property IsSingleByte: Boolean read FIsSingleByte;
    class function Unicode: TEncoding;
    class function UTF7: TEncoding;
    class function UTF8: TEncoding;
  end;

  TMBCSEncoding = class(TEncoding)
  private
    FCodePage: Cardinal;
    FMBToWCharFlags: Cardinal;
    FWCharToMBFlags: Cardinal;
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF} protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
  public
    constructor Create; overload; virtual;
    constructor Create(CodePage: Integer); overload; virtual;
    constructor Create(CodePage, MBToWCharFlags, WCharToMBFlags: Integer); overload; virtual;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;

  TUTF7Encoding = class(TMBCSEncoding)
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF} protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
  public
    constructor Create; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
  end;

  TUTF8Encoding = class(TUTF7Encoding)
  public
    constructor Create; override;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;

  TUnicodeEncoding = class(TEncoding)
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF} protected
    function GetByteCount(Chars: PChar; CharCount: Integer): Integer; overload; override;
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetCharCount(Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
  public
    constructor Create; virtual;
    function GetMaxByteCount(CharCount: Integer): Integer; override;
    function GetMaxCharCount(ByteCount: Integer): Integer; override;
    function GetPreamble: TBytes; override;
  end;

  TBigEndianUnicodeEncoding = class(TUnicodeEncoding)
  {$IFDEF SUPPORTS_STRICT}strict{$ENDIF} protected
    function GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte; ByteCount: Integer): Integer; overload; override;
    function GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar; CharCount: Integer): Integer; overload; override;
  public
    function GetPreamble: TBytes; override;
  end;

{$ENDIF Compiler12_Up}

implementation

{$IFNDEF Compiler5_Up} // Delphi4_Down

function AnsiSameStr(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareStr(S1, S2) = 0;
end;

function AnsiSameText(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareText(S1, S2) = 0;
end;

function SameText(const S1, S2: string): Boolean; assembler;
asm
        CMP     EAX,EDX
        JZ      @1
        OR      EAX,EAX
        JZ      @2
        OR      EDX,EDX
        JZ      @3
        MOV     ECX,[EAX-4]
        CMP     ECX,[EDX-4]
        JNE     @3
        CALL    CompareText
        TEST    EAX,EAX
        JNZ     @3
@1:     MOV     AL,1
@2:     RET
@3:     XOR     EAX,EAX
end;

procedure FreeAndNil(var Obj);
var
  Temp: TObject;
begin
  Temp := TObject(Obj);
  Pointer(Obj) := nil;
  Temp.Free;
end;

function SafeLoadLibrary(const Filename: string; ErrorMode: UINT): HMODULE;
var
  OldMode: UINT;
  FPUControlWord: Word;
begin
  OldMode := SetErrorMode(ErrorMode);
  try
    SetErrorMode(OldMode or ErrorMode);
    asm
      FNSTCW  FPUControlWord
    end;
    try
      Result := LoadLibrary(PChar(Filename));
    finally
      asm
        FNCLEX
        FLDCW FPUControlWord
      end;
    end;
  finally
    SetErrorMode(OldMode);
  end;
end;

function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean;
begin
  Result := (Instance <> nil) and (Instance.QueryInterface(IID, Intf) = 0);
end;

{$IFDEF SUPPORTS_OVERLOAD}
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean;
var
  LUnknown: IUnknown;
begin
  Result := (Instance <> nil) and
            ((Instance.GetInterface(IUnknown, LUnknown) and Supports(LUnknown, IID, Intf)) or
             Instance.GetInterface(IID, Intf));
end;
{$ENDIF}

{$ENDIF}

{$IFNDEF Compiler6_Up}

function AnsiCompareFileName(const S1, S2: string): Integer;
begin
  Result := AnsiCompareStr(AnsiLowerCaseFileName(S1), AnsiLowerCaseFileName(S2));
end;

function SameFileName(const S1, S2: string): Boolean;
begin
  Result := AnsiCompareFileName(S1, S2) = 0;
end;

function Rect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

function AnsiLowerCaseFileName(const S: string): string;
var
  I,L: Integer;
begin
  if SysLocale.FarEast then
  begin
    L := Length(S);
    SetLength(Result, L);
    I := 1;
    while I <= L do
    begin
      Result[I] := S[I];
      if S[I] in LeadBytes then
      begin
        Inc(I);
        Result[I] := S[I];
      end
      else
        if Result[I] in ['A'..'Z'] then // Do Not Localize
          Inc(Byte(Result[I]), 32);
      Inc(I);
    end;
  end
  else
    Result := AnsiLowerCase(S);
end;

function ExcludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if IsPathDelimiter(Result, Length(Result)) then
    SetLength(Result, Length(Result)-1);
end;

function IncludeTrailingPathDelimiter(const S: string): string;
begin
  Result := S;
  if not IsPathDelimiter(Result, Length(Result)) then
    Result := Result + PathDelim;
end;

function UnicodeToUtf8(Dest: PAnsiChar; MaxDestBytes: Cardinal; Source: PWideChar; SourceChars: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Cardinal;
begin
  Result := 0;
  if Source = nil then Exit;
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceChars) and (count < MaxDestBytes) do
    begin
      c := Cardinal(Source[i]);
      Inc(i);
      if c <= $7F then
      begin
        Dest[count] := AnsiChar(c);
        Inc(count);
      end
      else if c > $7FF then
      begin
        if count + 3 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($E0 or (c shr 12));
        Dest[count+1] := AnsiChar($80 or ((c shr 6) and $3F));
        Dest[count+2] := AnsiChar($80 or (c and $3F));
        Inc(count,3);
      end
      else //  $7F < Source[i] <= $7FF
      begin
        if count + 2 > MaxDestBytes then
          break;
        Dest[count] := AnsiChar($C0 or (c shr 6));
        Dest[count+1] := AnsiChar($80 or (c and $3F));
        Inc(count,2);
      end;
    end;
    if count >= MaxDestBytes then count := MaxDestBytes-1;
    Dest[count] := #0;
  end
  else
  begin
    while i < SourceChars do
    begin
      c := Integer(Source[i]);
      Inc(i);
      if c > $7F then
      begin
        if c > $7FF then
          Inc(count);
        Inc(count);
      end;
      Inc(count);
    end;
  end;
  Result := count+1;  // convert zero based index to byte count
end;

function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal; Source: PAnsiChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function AnsiToUtf8(const S: string): UTF8String;
begin
  Result := Utf8Encode(S);
end;

function Utf8ToAnsi(const S: UTF8String): string;
begin
  Result := UTF8ToUnicodeString(S);
end;

procedure RaiseLastOSError;
var
  LastError: Integer;
  Error: EOSError;
begin
  LastError := GetLastError;
  if LastError <> 0 then
    Error := EOSError.CreateFmt(SWin32Error, [LastError,
      SysErrorMessage(LastError)])
  else
    Error := EOSError.Create(SUnkWin32Error);
  Error.ErrorCode := LastError;
  raise Error;
end;

function SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
begin
  if ANow < AThen then
    Result := AThen - ANow
  else
    Result := ANow - AThen;
end;

function MinuteSpan(const ANow, AThen: TDateTime): Double;
begin
  Result := MinsPerDay * SpanOfNowAndThen(ANow, AThen);
end;

function MinutesBetween(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(MinuteSpan(ANow, AThen));
end;

function GetModuleName(Module: HMODULE): string;
var
  ModName: array[0..MAX_PATH] of Char;
begin
  SetString(Result, ModName, GetModuleFileName(Module, ModName, Length(ModName)));
end;

function GetEnvironmentVariable(const Name: string): string;
const
  BufSize = 1024;
var
  Len: Integer;
  Buffer: array[0..BufSize - 1] of Char;
begin
  Result := ''; // Do Not Localize
  Len := Windows.GetEnvironmentVariable(PChar(Name), @Buffer, BufSize);
  if Len < BufSize then
    SetString(Result, PChar(@Buffer), Len)
  else
  begin
    SetLength(Result, Len - 1);
    Windows.GetEnvironmentVariable(PChar(Name), PChar(Result), Len);
  end;
end;

function AnsiEndsText(const ASubText, AText: string): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') and // Do Not Localize
     (ByteType(AText, SubTextLocation) <> mbTrailByte) then
    Result := AnsiStrIComp(PChar(ASubText), PChar(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;

function AnsiEndsStr(const ASubText, AText: string): Boolean;
var
  SubTextLocation: Integer;
begin
  SubTextLocation := Length(AText) - Length(ASubText) + 1;
  if (SubTextLocation > 0) and (ASubText <> '') and // Do Not Localize
     (ByteType(AText, SubTextLocation) <> mbTrailByte) then
    Result := AnsiStrComp(PChar(ASubText), PChar(@AText[SubTextLocation])) = 0
  else
    Result := False;
end;

function AnsiStartsText(const ASubText, AText: string): Boolean;
begin
  Result := AnsiSameText(ASubText, Copy(AText, 1, Length(ASubText)));
end;

function AnsiStartsStr(const ASubText, AText: string): Boolean;
begin
  Result := AnsiSameStr(ASubText, Copy(AText, 1, Length(ASubText)));
end;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function TryStrToInt64(const S: string; out Value: Int64): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

procedure VerifyBoolStrArray;
begin
  if Length(TrueBoolStrs) = 0 then
  begin
    SetLength(TrueBoolStrs, 1);
    TrueBoolStrs[0] := DefaultTrueBoolStr;
  end;
  if Length(FalseBoolStrs) = 0 then
  begin
    SetLength(FalseBoolStrs, 1);
    FalseBoolStrs[0] := DefaultFalseBoolStr;
  end;
end;

resourcestring
  SInvalidBoolean = '''%s'' is not a valid boolean value';

function StrToBool(const S: string): Boolean;
begin
  if not TryStrToBool(S, Result) then
    raise EConvertError.Create(Format(SInvalidBoolean, [S]));
end;

function StrToBoolDef(const S: string; const Default: Boolean): Boolean;
begin
  if not TryStrToBool(S, Result) then
    Result := Default;
end;

function TryStrToBool(const S: string; out Value: Boolean): Boolean;
  function CompareWith(const aArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Low(aArray) to High(aArray) do
      if AnsiSameText(S, aArray[I]) then
      begin
        Result := True;
        Break;
      end;
  end;
var
  LResult: Extended;
begin
  Result := TryStrToFloat(S, LResult);
  if Result then
    Value := LResult <> 0
  else
  begin
    VerifyBoolStrArray;
    Result := CompareWith(TrueBoolStrs);
    if Result then
      Value := True
    else
    begin
      Result := CompareWith(FalseBoolStrs);
      if Result then
        Value := False;
    end;
  end;
end;

function BoolToStr(B: Boolean; UseBoolStrs: Boolean = False): string;
const
  cSimpleBoolStrs: array [boolean] of String = ('0', '-1');
begin
  if UseBoolStrs then
  begin
    VerifyBoolStrArray;
    if B then
      Result := TrueBoolStrs[0]
    else
      Result := FalseBoolStrs[0];
  end
  else
    Result := cSimpleBoolStrs[B];
end;

function AnsiQuotedStr(const S: string; Quote: Char): string;
var
  P, Src, Dest: PChar;
  AddCount: Integer;
begin
  AddCount := 0;
  P := AnsiStrScan(PChar(S), Quote);
  while P <> nil do
  begin
    Inc(P);
    Inc(AddCount);
    P := AnsiStrScan(P, Quote);
  end;
  if AddCount = 0 then
  begin
    Result := Quote + S + Quote;
    Exit;
  end;
  SetLength(Result, Length(S) + AddCount + 2);
  Dest := Pointer(Result);
  Dest^ := Quote;
  Inc(Dest);
  Src := Pointer(S);
  P := AnsiStrScan(Src, Quote);
  repeat
    Inc(P);
    Move(Src^, Dest^, P - Src);
    Inc(Dest, P - Src);
    Dest^ := Quote;
    Inc(Dest);
    Src := P;
    P := AnsiStrScan(Src, Quote);
  until P = nil;
  P := StrEnd(Src);
  Move(Src^, Dest^, P - Src);
  Inc(Dest, P - Src);
  Dest^ := Quote;
end;

function AnsiExtractQuotedStr(var Src: PChar; Quote: Char): string;
var
  P, Dest: PChar;
  DropCount: Integer;
begin
  Result := '';
  if (Src = nil) or (Src^ <> Quote) then Exit;
  Inc(Src);
  DropCount := 1;
  P := Src;
  Src := AnsiStrScan(Src, Quote);
  while Src <> nil do   // count adjacent pairs of quote chars
  begin
    Inc(Src);
    if Src^ <> Quote then Break;
    Inc(Src);
    Inc(DropCount);
    Src := AnsiStrScan(Src, Quote);
  end;
  if Src = nil then Src := StrEnd(P);
  if ((Src - P) <= 1) then Exit;
  if DropCount = 1 then
    SetString(Result, P, Src - P - 1)
  else
  begin
    SetLength(Result, Src - P - DropCount);
    Dest := PChar(Result);
    Src := AnsiStrScan(P, Quote);
    while Src <> nil do
    begin
      Inc(Src);
      if Src^ <> Quote then Break;
      Move(P^, Dest^, Src - P);
      Inc(Dest, Src - P);
      Inc(Src);
      P := Src;
      Src := AnsiStrScan(Src, Quote);
    end;
    if Src = nil then Src := StrEnd(P);
    Move(P^, Dest^, Src - P - 1);
  end;
end;

function TryStrToDateTime(const S: string; out Value: TDateTime): Boolean;
begin
  try
    Value := StrToDateTime(S);
    Result := True;
  except
    on EConvertError do
    begin
      Value := 0;
      Result := False;
    end;
  end;
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

function StrToDateTimeDef(const S: string;
  const Default: TDateTime): TDateTime;
begin
  if not TryStrToDateTime(S, Result) then
    Result := Default;
end;

function StrToFloatDef(const S: string; const Default: Extended): Extended;
begin
  if not TextToFloat(PChar(S), Result, fvExtended) then
    Result := Default;
end;

function TryStrToFloat(const S: string; out Value: Extended): Boolean;
begin
  Result := TextToFloat(PChar(S), Value, fvExtended);
end;

function FindCmdLineSwitch(const Switch: string;
  SwitchChars: TSysCharSet = ['/']; // Do Not Localize
  IgnoreCase: Boolean = True): Boolean;
begin
  Result := SysUtils.FindCmdLineSwitch(Switch, SwitchChars, IgnoreCase);
end;

{$IFDEF SUPPORTS_OVERLOAD}

function GetEnvironmentVariable(lpName: PChar; lpBuffer: PChar; nSize: DWORD): DWORD;
begin
  Result := Windows.GetEnvironmentVariable(lpName, lpBuffer, nSize);
end;

{$ENDIF SUPPORTS_OVERLOAD}

resourcestring
  SInvalidGUID1 = '''%s'' is not a valid GUID value';
  SInvalidGUID2 = 'Invalid GUID value';

function CreateGUID(out Guid: TGUID): HResult;
begin
  Result := CoCreateGuid(GUID);
end;

function StringToGUID(const S: string): TGUID;
begin
  if Failed(CLSIDFromString(PWideChar(WideString(S)), Result)) then
    raise EConvertError.CreateFmt(SInvalidGUID1, [S]);
end;

function GUIDToString(const ClassID: TGUID): string;
var
  P: PWideChar;
begin
  if Failed(StringFromCLSID(ClassID, P)) then
    raise EConvertError.Create(SInvalidGUID2);
  Result := P;
  CoTaskMemFree(P);
end;

{ Range testing functions }

function InRange(const AValue, AMin, AMax: Integer): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Int64): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

function InRange(const AValue, AMin, AMax: Double): Boolean;
begin
  Result := (AValue >= AMin) and (AValue <= AMax);
end;

{ Range truncation functions }

function EnsureRange(const AValue, AMin, AMax: Integer): Integer;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Int64): Int64;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

function EnsureRange(const AValue, AMin, AMax: Double): Double;
begin
  Result := AValue;
  assert(AMin <= AMax);
  if Result < AMin then
    Result := AMin;
  if Result > AMax then
    Result := AMax;
end;

{$ENDIF}

{$IFNDEF Compiler7_Up}

function CheckWin32Version(AMajor: Integer; AMinor: Integer): Boolean; // see comments in QC 2457
begin
  Result := (Win32MajorVersion > AMajor) or
            ((Win32MajorVersion = AMajor) and
             (Win32MinorVersion >= AMinor));
end;

{$ENDIF}

{$IFNDEF Compiler9_up}
function SameStr(const S1, S2: string): Boolean;
begin
  Result := CompareStr(S1, S2) = 0;
end;
{$ENDIF}

{$IFNDEF Compiler10_Up}

{$IFDEF Compiler6_Up}
(* ***** BEGIN LICENSE BLOCK *****
 *
 * The assembly function Move is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): John O'Harrow
 *
 * ***** END LICENSE BLOCK ***** *)
procedure Move(const Source; var Dest; count : Integer);
{$IFDEF PUREPASCAL}
var
  S, D: PAnsiChar;
  I: Integer;
begin
  S := PAnsiChar(@Source);
  D := PAnsiChar(@Dest);
  if S = D then Exit;
  if Cardinal(D) > Cardinal(S) then
    for I := count-1 downto 0 do
      D[I] := S[I]
  else
    for I := 0 to count-1 do
      D[I] := S[I];
end;
{$ELSE}
asm
  cmp     eax, edx
  je      @@Exit {Source = Dest}
  cmp     ecx, 32
  ja      @@LargeMove {Count > 32 or Count < 0}
  sub     ecx, 8
  jg      @@SmallMove
@@TinyMove: {0..8 Byte Move}
  jmp     dword ptr [@@JumpTable+32+ecx*4]
@@SmallMove: {9..32 Byte Move}
  fild    qword ptr [eax+ecx] {Load Last 8}
  fild    qword ptr [eax] {Load First 8}
  cmp     ecx, 8
  jle     @@Small16
  fild    qword ptr [eax+8] {Load Second 8}
  cmp     ecx, 16
  jle     @@Small24
  fild    qword ptr [eax+16] {Load Third 8}
  fistp   qword ptr [edx+16] {Save Third 8}
@@Small24:
  fistp   qword ptr [edx+8] {Save Second 8}
@@Small16:
  fistp   qword ptr [edx] {Save First 8}
  fistp   qword ptr [edx+ecx] {Save Last 8}
@@Exit:
  ret
  nop {4-Byte Align JumpTable}
  nop
@@JumpTable: {4-Byte Aligned}
  dd      @@Exit, @@M01, @@M02, @@M03, @@M04, @@M05, @@M06, @@M07, @@M08
@@LargeForwardMove: {4-Byte Aligned}
  push    edx
  fild    qword ptr [eax] {First 8}
  lea     eax, [eax+ecx-8]
  lea     ecx, [ecx+edx-8]
  fild    qword ptr [eax] {Last 8}
  push    ecx
  neg     ecx
  and     edx, -8 {8-Byte Align Writes}
  lea     ecx, [ecx+edx+8]
  pop     edx
@FwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  add     ecx, 8
  jl      @FwdLoop
  fistp   qword ptr [edx] {Last 8}
  pop     edx
  fistp   qword ptr [edx] {First 8}
  ret
@@LargeMove:
  jng     @@LargeDone {Count < 0}
  cmp     eax, edx
  ja      @@LargeForwardMove
  sub     edx, ecx
  cmp     eax, edx
  lea     edx, [edx+ecx]
  jna     @@LargeForwardMove
  sub     ecx, 8 {Backward Move}
  push    ecx
  fild    qword ptr [eax+ecx] {Last 8}
  fild    qword ptr [eax] {First 8}
  add     ecx, edx
  and     ecx, -8 {8-Byte Align Writes}
  sub     ecx, edx
@BwdLoop:
  fild    qword ptr [eax+ecx]
  fistp   qword ptr [edx+ecx]
  sub     ecx, 8
  jg      @BwdLoop
  pop     ecx
  fistp   qword ptr [edx] {First 8}
  fistp   qword ptr [edx+ecx] {Last 8}
@@LargeDone:
  ret
@@M01:
  movzx   ecx, [eax]
  mov     [edx], cl
  ret
@@M02:
  movzx   ecx, word ptr [eax]
  mov     [edx], cx
  ret
@@M03:
  mov     cx, [eax]
  mov     al, [eax+2]
  mov     [edx], cx
  mov     [edx+2], al
  ret
@@M04:
  mov     ecx, [eax]
  mov     [edx], ecx
  ret
@@M05:
  mov     ecx, [eax]
  mov     al, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], al
  ret
@@M06:
  mov     ecx, [eax]
  mov     ax, [eax+4]
  mov     [edx], ecx
  mov     [edx+4], ax
  ret
@@M07:
  mov     ecx, [eax]
  mov     eax, [eax+3]
  mov     [edx], ecx
  mov     [edx+3], eax
  ret
@@M08:
  fild    qword ptr [eax]
  fistp   qword ptr [edx]
end;
{$ENDIF}
{$ENDIF}

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The function CompareMem is licensed under the CodeGear license terms.
 *
 * The initial developer of the original code is Fastcode
 *
 * Portions created by the initial developer are Copyright (C) 2002-2004
 * the initial developer. All Rights Reserved.
 *
 * Contributor(s): Aleksandr Sharahov
 *
 * ***** END LICENSE BLOCK ***** *)
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
   add   eax, ecx
   add   edx, ecx
   xor   ecx, -1
   add   eax, -8
   add   edx, -8
   add   ecx, 9
   push  ebx
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   lea   ebx, [eax+ecx]
   add   ecx, 4
   and   ebx, 3
   sub   ecx, ebx
   jg    @Dword
@DwordLoop:
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jg    @Dword
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   mov   ebx, [eax+ecx+4]
   cmp   ebx, [edx+ecx+4]
   jne   @Ret0
   add   ecx, 8
   jle   @DwordLoop
@Dword:
   cmp   ecx, 4
   jg    @Word
   mov   ebx, [eax+ecx]
   cmp   ebx, [edx+ecx]
   jne   @Ret0
   add   ecx, 4
@Word:
   cmp   ecx, 6
   jg    @Byte
   movzx ebx, word ptr [eax+ecx]
   cmp   bx, [edx+ecx]
   jne   @Ret0
   add   ecx, 2
@Byte:
   cmp   ecx, 7
   jg    @Ret1
   movzx ebx, byte ptr [eax+7]
   cmp   bl, [edx+7]
   jne   @Ret0
@Ret1:
   mov   eax, 1
   pop   ebx
   ret
@Ret0:
   xor   eax, eax
   pop   ebx
end;

{$ENDIF}

{$IFNDEF Compiler11_Up}

function FileExists(const FileName: string): Boolean;
{$IFDEF MSWINDOWS}

  function ExistsLockedOrShared(const Filename: string): Boolean;
  var
    FindData: TWin32FindData;
    LHandle: THandle;
  begin
    { Either the file is locked/share_exclusive or we got an access denied }
    LHandle := FindFirstFile(PChar(Filename), FindData);
    if LHandle <> INVALID_HANDLE_VALUE then
    begin
      Windows.FindClose(LHandle);
      Result := FindData.dwFileAttributes and FILE_ATTRIBUTE_DIRECTORY = 0;
    end
    else
      Result := False;
  end;

var
  Code: Integer;
  LastError: Cardinal;
begin
  Code := Integer(GetFileAttributes(PChar(FileName)));
  if Code <> -1 then
    Result := (FILE_ATTRIBUTE_DIRECTORY and Code = 0)
  else
  begin
    LastError := GetLastError;
    Result := (LastError <> ERROR_FILE_NOT_FOUND) and
      (LastError <> ERROR_PATH_NOT_FOUND) and
      (LastError <> ERROR_INVALID_NAME) and ExistsLockedOrShared(Filename);
  end;
end;
{$ENDIF}
{$IFDEF LINUX}
begin
  Result := euidaccess(PChar(FileName), F_OK) = 0;
end;
{$ENDIF}

{$ENDIF}

{$IFNDEF Compiler12_Up}

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;

function {$IFDEF SUPPORTS_OVERLOAD}CharInSet{$ELSE}CharInSetW{$ENDIF}(C: WideChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := (C < #$0100) and (AnsiChar(C) in CharSet);
end;

function DirectoryExists(const Directory: string): Boolean;
{$IFDEF LINUX}
var
  st: TStatBuf;
begin
  if stat(PChar(Directory), st) = 0 then
    Result := S_ISDIR(st.st_mode)
  else
    Result := False;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var
  Code: Cardinal;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> INVALID_FILE_ATTRIBUTES) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;
{$ENDIF}

procedure MoveChars(const Source; var Dest; Length: Integer);
begin
  Move(Source, Dest, Length * SizeOf(Char));
end;

function UIntToStr(Value: Cardinal): string;
begin
  FmtStr(Result, '%u', [Value]);
end;

function UIntToStr(Value: UInt64): string;
begin
  FmtStr(Result, '%u', [Value]);
end;

function UTF8Encode(const WS: WideString): RawByteString;
var
  L: Integer;
  Temp: UTF8String;
begin
  Result := ''; // Do Not Localize
  if WS = '' then // Do Not Localize
    Exit;
  L := Length(WS);
  SetLength(Temp, L * 3); // SetLength includes space for null terminator

  L := UnicodeToUtf8(PAnsiChar(Temp), Length(Temp) + 1, PWideChar(WS), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := ''; // Do Not Localize
  Result := Temp;
end;

function InterlockedCompareExchangePointer(var Destination: Pointer; Exchange: Pointer; Comperand: Pointer): Pointer;
begin
  {$IFDEF Compiler9_up}
  Result := Pointer(InterlockedCompareExchange(Integer(Destination), Integer(Exchange), Integer(Comperand)));
  {$ELSE}
  Result := Pointer(InterlockedCompareExchange(Destination, Exchange, Comperand));
  {$ENDIF}
end;

{$IFDEF Compiler6_up}

function UTF8Encode(const A: RawByteString): RawByteString;
begin
  Result := UTF8Encode(UnicodeString(A));
end;

{$ENDIF Compiler6_up}

function Max(I1, I2: Integer): Integer; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
begin
  if I1 > I2 then
    Result := I1
  else
    Result := I2;
end;

function UTF8EncodeToShortString(const WS: WideString): ShortString;
begin
  Result[0] := AnsiChar(Max(0, UnicodeToUtf8(@Result[1], High(Result), PWideChar(WS), Length(WS)) - 1));
end;

{$IFDEF Compiler6_up}

function UTF8EncodeToShortString(const A: RawByteString): ShortString;
begin
  Result := UTF8EncodeToShortString(A);
end;

{$ENDIF Compiler6_up}

function UTF8ToWideString(const S: RawByteString): WideString; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
begin
  Result := UTF8ToUnicodeString(RawByteString(S));
end;

function UTF8ToUnicodeString(const S: RawByteString): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := ''; // Do Not Localize
  if S = '' then // Do Not Localize
    Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, PAnsiChar(S), L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := ''; // Do Not Localize
  Result := Temp;
end;

{$IFDEF Compiler6_up}

function UTF8ToUnicodeString(const S: ShortString): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := ''; // Do Not Localize
  if S = '' then // Do Not Localize
    Exit;
  L := Length(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, @S[1], L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := ''; // Do Not Localize
  Result := Temp;
end;

function UTF8ToUnicodeString(const S: PAnsiChar): UnicodeString;
var
  L: Integer;
  Temp: UnicodeString;
begin
  Result := ''; // Do Not Localize
  if S = '' then // Do Not Localize
    Exit;
  L := StrLen(S);
  SetLength(Temp, L);

  L := Utf8ToUnicode(PWideChar(Temp), L + 1, S, L);
  if L > 0 then
    SetLength(Temp, L - 1)
  else
    Temp := ''; // Do Not Localize
  Result := Temp;
end;

{$ENDIF Compiler6_up}

function UTF8ToString(const S: RawByteString): string;
begin
  Result := UTf8ToAnsi(S);
end;

{$IFDEF Compiler6_up}

function UTF8ToString(const S: ShortString): string;
begin
  Result := UTf8ToAnsi(S);
end;

function UTF8ToString(const S: PAnsiChar): string;
begin
  Result := UTf8ToAnsi(S);
end;

{$ENDIF Compiler6_up}

{$ENDIF}

{$IFNDEF Compiler12_Up}

{$IFNDEF SUPPORTS_CLASS_FIELDS}
var
  FASCIIEncoding: TEncoding;
  FBigEndianUnicodeEncoding: TEncoding;
  FDefaultEncoding: TEncoding;
  FUnicodeEncoding: TEncoding;
  FUTF7Encoding: TEncoding;
  FUTF8Encoding: TEncoding;
{$ENDIF}

{ TEncoding }

class function TEncoding.Convert(Source, Destination: TEncoding; Bytes: TBytes): TBytes;
begin
  Result := Destination.GetBytes(Source.GetChars(Bytes));
end;

class function TEncoding.Convert(Source, Destination: TEncoding; Bytes: TBytes;
  StartIndex, Count: Integer): TBytes;
begin
  Result := Destination.GetBytes(Source.GetChars(Bytes, StartIndex, Count));
end;

class procedure TEncoding.FreeEncodings;
begin
  FreeAndNil(FDefaultEncoding);
  FreeAndNil(FASCIIEncoding);
  FreeAndNil(FUTF7Encoding);
  FreeAndNil(FUTF8Encoding);
  FreeAndNil(FUnicodeEncoding);
  FreeAndNil(FBigEndianUnicodeEncoding);
end;

class function TEncoding.ASCII: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FASCIIEncoding = nil then
  begin
    LEncoding := TMBCSEncoding.Create(20127, 0, 0);
    if InterlockedCompareExchangePointer(Pointer(FASCIIEncoding), LEncoding, nil) <> nil then
      FreeAndNil(LEncoding);
  end;
  Result := FASCIIEncoding;
end;

class function TEncoding.BigEndianUnicode: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FBigEndianUnicodeEncoding = nil then
  begin
    LEncoding := TBigEndianUnicodeEncoding.Create;
    if InterlockedCompareExchangePointer(Pointer(FBigEndianUnicodeEncoding), LEncoding, nil) <> nil then
      FreeAndNil(LEncoding);
  end;
  Result := FBigEndianUnicodeEncoding;
end;

class function TEncoding.GetBufferEncoding(const Buffer: TBytes; var AEncoding: TEncoding): Integer;

  function ContainsPreamble(const Buffer, Signature: TBytes): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    if Length(Buffer) >= Length(Signature) then
    begin
      for I := 1 to Length(Signature) do
        if Buffer[I - 1] <> Signature [I - 1] then
        begin
          Result := False;
          Break;
        end;
    end
    else
      Result := False;
  end;

var
  Preamble: TBytes;
begin
  Result := 0;
  Preamble := nil;
  if AEncoding = nil then
  begin
    // Find the appropraite encoding
    AEncoding := TEncoding.Unicode;
    if not ContainsPreamble(Buffer, AEncoding.GetPreamble) then
    begin
      AEncoding := TEncoding.BigEndianUnicode;
      if not ContainsPreamble(Buffer, AEncoding.GetPreamble) then
      begin
        AEncoding := TEncoding.UTF8;
        if not ContainsPreamble(Buffer, AEncoding.GetPreamble) then
          AEncoding := TEncoding.Default;
      end;
    end;
    Result := Length(AEncoding.GetPreamble);
  end
  else
  begin
    Preamble := AEncoding.GetPreamble;
    if ContainsPreamble(Buffer, Preamble) then
      Result := Length(Preamble);
  end;
end;

function TEncoding.GetByteCount(const Chars: TCharArray): Integer;
begin
  Result := GetByteCount(Chars, 0, Length(Chars));
end;

function TEncoding.GetByteCount(const Chars: TCharArray; CharIndex,
  CharCount: Integer): Integer;
begin
  Result := GetByteCount(@Chars[CharIndex], CharCount);
end;

function TEncoding.GetByteCount(const S: string): Integer;
begin
  Result := GetByteCount(PChar(S), Length(S));
end;

resourcestring
  rsInvalidSourceArray                           = 'Invalid source array';
  rsInvalidDestinationArray                      = 'Invalid destination array';
  rsCharIndexOutOfBoundsFmt                      = 'Character index out of bounds (%d)';
  rsByteIndexOutOfBoundsFmt                      = 'Start index out of bounds (%d)';
  rsInvalidCharCountFmt                          = 'Invalid count (%d)';
  rsInvalidDestinationIndexFmt                   = 'Invalid destination index (%d)';
  rsInvalidCodePage                              = 'Invalid code page';

function TEncoding.GetByteCount(const S: string; CharIndex, CharCount: Integer): Integer;
begin
  if CharIndex < 1 then
    raise EEncodingError.CreateFmt(rsCharIndexOutOfBoundsFmt, [CharIndex]);
  if CharCount < 0 then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [CharCount]);
  if (Length(S) - CharIndex + 1) < CharCount then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [CharCount]);

  Result := GetByteCount(PChar(@S[CharIndex]), CharCount);
end;

function TEncoding.GetBytes(const Chars: TCharArray): TBytes;
var
  Len: Integer;
begin
  Len := GetByteCount(Chars);
  SetLength(Result, Len);
  GetBytes(Chars, 0, Length(Chars), Result, 0);
end;

function TEncoding.GetBytes(const Chars: TCharArray; CharIndex, CharCount: Integer;
  var Bytes: TBytes; ByteIndex: Integer): Integer;
var
  Len: Integer;
begin
  if (Chars = nil) and (CharCount <> 0) then
    raise EEncodingError.Create(rsInvalidSourceArray);
  if (Bytes = nil) and (CharCount <> 0) then
    raise EEncodingError.Create(rsInvalidDestinationArray);
  if CharIndex < 0 then
    raise EEncodingError.CreateFmt(rsCharIndexOutOfBoundsFmt, [CharIndex]);
  if CharCount < 0 then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [CharCount]);
  if (Length(Chars) - CharIndex) < CharCount then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [CharCount]);
  Len := Length(Bytes);
  if (ByteIndex < 0) or (ByteIndex > Len) then
    raise EEncodingError.CreateFmt(rsInvalidDestinationIndexFmt, [ByteIndex]);
  if Len - ByteIndex < GetByteCount(Chars, CharIndex, CharCount) then
    raise EEncodingError.Create(rsInvalidDestinationArray);

  Result := GetBytes(@Chars[CharIndex], CharCount, @Bytes[ByteIndex], Len - ByteIndex);
end;

function TEncoding.GetBytes(const S: string): TBytes;
var
  Len: Integer;
begin
  Len := GetByteCount(S);
  SetLength(Result, Len);
  GetBytes(S, 1, Length(S), Result, 0);
end;

function TEncoding.GetBytes(const S: string; CharIndex, CharCount: Integer;
  var Bytes: TBytes; ByteIndex: Integer): Integer;
var
  Len: Integer;
begin
  if (Bytes = nil) and (CharCount <> 0) then
    raise EEncodingError.Create(rsInvalidSourceArray);
  if CharIndex < 1 then
    raise EEncodingError.CreateFmt(rsCharIndexOutOfBoundsFmt, [CharIndex]);
  if CharCount < 0 then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [CharCount]);
  if (Length(S) - CharIndex + 1) < CharCount then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [CharCount]);
  Len := Length(Bytes);
  if (ByteIndex < 0) or (ByteIndex > Len) then
    raise EEncodingError.CreateFmt(rsInvalidDestinationIndexFmt, [ByteIndex]);
  if Len - ByteIndex < GetByteCount(S, CharIndex, CharCount) then
    raise EEncodingError.Create(rsInvalidDestinationArray);

  Result := GetBytes(@S[CharIndex], CharCount, @Bytes[ByteIndex], Len - ByteIndex);
end;

function TEncoding.GetCharCount(const Bytes: TBytes): Integer;
begin
  Result := GetCharCount(Bytes, 0, Length(Bytes));
end;

function TEncoding.GetCharCount(const Bytes: TBytes; ByteIndex, ByteCount: Integer): Integer;
begin
  if (Bytes = nil) and (ByteCount <> 0) then
    raise EEncodingError.Create(rsInvalidSourceArray);
  if ByteIndex < 0 then
    raise EEncodingError.CreateFmt(rsByteIndexOutOfBoundsFmt, [ByteIndex]);
  if ByteCount < 0 then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [ByteCount]);
  if (Length(Bytes) - ByteIndex) < ByteCount then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [ByteCount]);

  Result := GetCharCount(@Bytes[ByteIndex], ByteCount);
end;

function TEncoding.GetChars(const Bytes: TBytes): TCharArray;
begin
  Result := GetChars(Bytes, 0, Length(Bytes));
end;

function TEncoding.GetChars(const Bytes: TBytes; ByteIndex, ByteCount: Integer): TCharArray;
var
  Len: Integer;
begin
  if (Bytes = nil) and (ByteCount <> 0) then
    raise EEncodingError.Create(rsInvalidSourceArray);
  if ByteIndex < 0 then
    raise EEncodingError.CreateFmt(rsByteIndexOutOfBoundsFmt, [ByteIndex]);
  if ByteCount < 0 then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [ByteCount]);
  if (Length(Bytes) - ByteIndex) < ByteCount then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [ByteCount]);

  Len := GetCharCount(Bytes, ByteIndex, ByteCount);
  SetLength(Result, Len);
  GetChars(@Bytes[ByteIndex], ByteCount, PChar(Result), Len);
end;

function TEncoding.GetChars(const Bytes: TBytes; ByteIndex, ByteCount: Integer;
  var Chars: TCharArray; CharIndex: Integer): Integer;
var
  LCharCount: Integer;
begin
  if (Bytes = nil) and (ByteCount <> 0) then
    raise EEncodingError.Create(rsInvalidSourceArray);
  if ByteIndex < 0 then
    raise EEncodingError.CreateFmt(rsByteIndexOutOfBoundsFmt, [ByteIndex]);
  if ByteCount < 0 then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [ByteCount]);
  if (Length(Bytes) - ByteIndex) < ByteCount then
    raise EEncodingError.CreateFmt(rsInvalidCharCountFmt, [ByteCount]);

  LCharCount := GetCharCount(Bytes, ByteIndex, ByteCount);
  if (CharIndex < 0) or (CharIndex> Length(Chars)) then
    raise EEncodingError.CreateFmt(rsInvalidDestinationIndexFmt, [CharIndex]);
  if CharIndex + LCharCount > Length(Chars) then
    raise EEncodingError.Create(rsInvalidDestinationArray);

  Result := GetChars(@Bytes[ByteIndex], ByteCount, @Chars[CharIndex], Length(Chars) - CharIndex);
end;

class function TEncoding.Default: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FDefaultEncoding = nil then
  begin
    LEncoding := TMBCSEncoding.Create(CP_ACP, 0, 0);
    if InterlockedCompareExchangePointer(Pointer(FDefaultEncoding), LEncoding, nil) <> nil then
      FreeAndNil(LEncoding);
  end;
  Result := FDefaultEncoding;
end;

class function TEncoding.GetEncoding(CodePage: Integer): TEncoding;
begin
  Result := TMBCSEncoding.Create(CodePage);
end;

function TEncoding.GetString(const Bytes: TBytes): string;
begin
  Result := GetString(Bytes, 0, Length(Bytes));
end;

function TEncoding.GetString(const Bytes: TBytes; ByteIndex, ByteCount: Integer): string;
var
  LChars: TCharArray;
begin
  LChars := GetChars(Bytes, ByteIndex, ByteCount);
  SetString(Result, PChar(LChars), Length(LChars));
end;

class function TEncoding.Unicode: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FUnicodeEncoding = nil then
  begin
    LEncoding := TUnicodeEncoding.Create;
    if InterlockedCompareExchangePointer(Pointer(FUnicodeEncoding), LEncoding, nil) <> nil then
      FreeAndNil(LEncoding);
  end;
  Result := FUnicodeEncoding;
end;

class function TEncoding.UTF7: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FUTF7Encoding = nil then
  begin
    LEncoding := TUTF7Encoding.Create;
    if InterlockedCompareExchangePointer(Pointer(FUTF7Encoding), LEncoding, nil) <> nil then
      FreeAndNil(LEncoding);
  end;
  Result := FUTF7Encoding;
end;

class function TEncoding.UTF8: TEncoding;
var
  LEncoding: TEncoding;
begin
  if FUTF8Encoding = nil then
  begin
    LEncoding := TUTF8Encoding.Create;
    if InterlockedCompareExchangePointer(Pointer(FUTF8Encoding), LEncoding, nil) <> nil then
      FreeAndNil(LEncoding);
  end;
  Result := FUTF8Encoding;
end;

class function TEncoding.IsStandardEncoding(AEncoding: TEncoding): Boolean;
begin
  Result :=
    (AEncoding = FASCIIEncoding) or
    (AEncoding = FBigEndianUnicodeEncoding) or
    (AEncoding = FDefaultEncoding) or
    (AEncoding = FUnicodeEncoding) or
    (AEncoding = FUTF7Encoding) or
    (AEncoding = FUTF8Encoding);
end;

{ TMBCSEncoding }

constructor TMBCSEncoding.Create;
begin
  Create(CP_ACP, 0, 0);
end;

constructor TMBCSEncoding.Create(CodePage: Integer);
begin
  FCodePage := CodePage;
  Create(CodePage, 0, 0);
end;

constructor TMBCSEncoding.Create(CodePage, MBToWCharFlags, WCharToMBFlags: Integer);
var
  LCPInfo: TCPInfo;
begin
  FCodePage := CodePage;
  FMBToWCharFlags := MBToWCharFlags;
  FWCharToMBFlags := WCharToMBFlags;

  if not GetCPInfo(FCodePage, LCPInfo) then
    raise EEncodingError.Create(rsInvalidCodePage);
  FMaxCharSize := LCPInfo.MaxCharSize;
  FIsSingleByte := FMaxCharSize = 1;
end;

function TMBCSEncoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := WideCharToMultiByte(FCodePage, FWCharToMBFlags,
    PChar(Chars), CharCount, nil, 0, nil, nil);
{$ELSE}
  Result := CharCount;
{$ENDIF}
end;

function TMBCSEncoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte;
  ByteCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := WideCharToMultiByte(FCodePage, FWCharToMBFlags,
    PChar(Chars), CharCount, PAnsiChar(Bytes), ByteCount, nil, nil);
{$ELSE}
  Result := CharCount;
  Move(Chars^, Bytes^, Result);
{$ENDIF}
end;

function TMBCSEncoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := MultiByteToWideChar(FCodePage, FMBToWCharFlags,
    PAnsiChar(Bytes), ByteCount, nil, 0);
{$ELSE}
  Result := ByteCount;
{$ENDIF}
end;

function TMBCSEncoding.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar;
  CharCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := MultiByteToWideChar(FCodePage, FMBToWCharFlags,
    PAnsiChar(Bytes), ByteCount, PChar(Chars), CharCount);
{$ELSE}
  Result := CharCount;
  Move(Bytes^, Chars^, CharCount * SizeOf(AnsiChar));
{$ENDIF}
end;

function TMBCSEncoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * FMaxCharSize;
end;

function TMBCSEncoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

function TMBCSEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 0);
end;

{ TUTF7Encoding }

constructor TUTF7Encoding.Create;
begin
  inherited Create(CP_UTF7);
end;

function TUTF7Encoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := inherited GetByteCount(Chars, CharCount);
{$ELSE}
  Result := WideCharToMultiByte(FCodePage, FWCharToMBFlags,
    PWideChar(UnicodeString(string(Chars))), CharCount, nil, 0, nil, nil);
{$ENDIF}
end;

function TUTF7Encoding.GetBytes(Chars: PChar; CharCount: Integer; Bytes: PByte;
  ByteCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := inherited GetBytes(Chars, CharCount, Bytes, ByteCount);
{$ELSE}
  Result := WideCharToMultiByte(FCodePage, FWCharToMBFlags,
    PWideChar(UnicodeString(string(Chars))), CharCount, PAnsiChar(Bytes),
    ByteCount, nil, nil);
{$ENDIF}
end;

function TUTF7Encoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := inherited GetCharCount(Bytes, ByteCount);
{$ELSE}
  Result := MultiByteToWideChar(FCodePage, FWCharToMBFlags,
    PAnsiChar(Bytes), ByteCount, nil, 0);
{$ENDIF}
end;

function TUTF7Encoding.GetChars(Bytes: PByte; ByteCount: Integer; Chars: PChar;
  CharCount: Integer): Integer;
{$IFNDEF UNICODE}
var
  AStr: AnsiString;
  UStr: UnicodeString;
{$ENDIF}
begin
{$IFDEF UNICODE}
  Result := inherited GetChars(Bytes, ByteCount, Chars, CharCount);
{$ELSE}
  SetLength(UStr, CharCount);
  Result := MultiByteToWideChar(FCodePage, FWCharToMBFlags,
    PAnsiChar(Bytes), ByteCount, PWideChar(UStr), CharCount);
  AStr := AnsiString(UStr);
  Move(AStr[1], Chars^, Length(AStr));
{$ENDIF}
end;

function TUTF7Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount * 3) + 2;
end;

function TUTF7Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount;
end;

{ TUTF8Encoding }

constructor TUTF8Encoding.Create;
begin
  inherited Create(CP_UTF8);
end;

function TUTF8Encoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 3;
end;

function TUTF8Encoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := ByteCount + 1;
end;

function TUTF8Encoding.GetPreamble: TBytes;
begin
  SetLength(Result, 3);
  Result[0] := $EF;
  Result[1] := $BB;
  Result[2] := $BF;
end;

{ TUnicodeEncoding }

constructor TUnicodeEncoding.Create;
begin
  FIsSingleByte := False;
  FMaxCharSize := 4;
end;

function TUnicodeEncoding.GetByteCount(Chars: PChar; CharCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := CharCount * SizeOf(Char);
{$ELSE}
  Result := MultiByteToWideChar(CP_ACP, 0, PChar(Chars), CharCount,
    nil, 0) * SizeOf(WideChar);
{$ENDIF}
end;

function TUnicodeEncoding.GetBytes(Chars: PChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := CharCount * SizeOf(Char);
  Move(Chars^, Bytes^, Result);
{$ELSE}
  Result := MultiByteToWideChar(CP_ACP, 0, PChar(Chars), CharCount,
    PWideChar(Bytes), ByteCount) * SizeOf(WideChar);
{$ENDIF}
end;

function TUnicodeEncoding.GetCharCount(Bytes: PByte; ByteCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := ByteCount div SizeOf(Char);
{$ELSE}
  Result := WideCharToMultiByte(CP_ACP, 0, PWideChar(Bytes), ByteCount, nil, 0,
    nil, nil) div SizeOf(WideChar);
{$ENDIF}
end;

function TUnicodeEncoding.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PChar; CharCount: Integer): Integer;
begin
{$IFDEF UNICODE}
  Result := CharCount;
  Move(Bytes^, Chars^, CharCount * SizeOf(Char));
{$ELSE}
  Result := WideCharToMultiByte(CP_ACP, 0, PWideChar(Bytes), CharCount,
    PChar(Chars), ByteCount, nil, nil);
{$ENDIF}
end;

function TUnicodeEncoding.GetMaxByteCount(CharCount: Integer): Integer;
begin
  Result := (CharCount + 1) * 2;
end;

function TUnicodeEncoding.GetMaxCharCount(ByteCount: Integer): Integer;
begin
  Result := (ByteCount div 2) + (ByteCount and 1) + 1;
end;

function TUnicodeEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $FF;
  Result[1] := $FE;
end;

{ TBigEndianUnicodeEncoding }

procedure SwapBytes(P1, P2: PByte); {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}
var
  B: Byte;
begin
  B := P1^;
  P1^ := P2^;
  P2^ := B;
end;

function TBigEndianUnicodeEncoding.GetBytes(Chars: PChar; CharCount: Integer;
  Bytes: PByte; ByteCount: Integer): Integer;
var
  I: Integer;
{$IFNDEF UNICODE}
  B2: PByte;
{$ENDIF}
begin
{$IFDEF UNICODE}
  for I := 0 to CharCount - 1 do
  begin
    Bytes^ := Hi(Word(Chars^));
    Inc(Bytes);
    Bytes^ := Lo(Word(Chars^));
    Inc(Bytes);
    Inc(Chars);
  end;
  Result := CharCount * SizeOf(WideChar);
{$ELSE}
  Result := inherited GetBytes(Chars, CharCount, Bytes, ByteCount);
  for I := 1 to Result do
  begin
    B2 := Bytes;
    Inc(B2);
    SwapBytes(Bytes, B2);
    Inc(Bytes, 2);
  end;
{$ENDIF}
end;

function TBigEndianUnicodeEncoding.GetChars(Bytes: PByte; ByteCount: Integer;
  Chars: PChar; CharCount: Integer): Integer;
var
  P: PByte;
  I: Integer;
{$IFNDEF UNICODE}
  B2: PByte;
{$ENDIF}
begin
{$IFDEF UNICODE}
  P := Bytes;
  Inc(P);
  for I := 0 to CharCount - 1 do
  begin
    Chars^ := WideChar(MakeWord(P^, Bytes^));
    Inc(Bytes, 2);
    Inc(P, 2);
    Inc(Chars);
  end;
  Result := CharCount;
{$ELSE}
  P := Bytes;
  for I := 1 to CharCount do
  begin
    B2 := P;
    Inc(B2);
    SwapBytes(P, B2);
    Inc(P, 2);
  end;
  Result := inherited GetChars(Bytes, ByteCount, Chars, CharCount);
{$ENDIF}
end;

function TBigEndianUnicodeEncoding.GetPreamble: TBytes;
begin
  SetLength(Result, 2);
  Result[0] := $FE;
  Result[1] := $FF;
end;

initialization
  // Does nothing

finalization
  TEncoding.FreeEncodings;

{$ENDIF Compiler12_Up}

end.
