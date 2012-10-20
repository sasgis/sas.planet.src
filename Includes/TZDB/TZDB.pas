(*
* Copyright (c) 2010, Ciobanu Alexandru
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of this library nor the
*       names of its contributors may be used to endorse or promote products
*       derived from this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

{ General conditional section. Checks for specific RTL "features" shared across
  FreePascal and different versions of Delphi. Recommended version is Delphi XE though ... }


unit TZDB;

{$DEFINE UNSUPPORTED_VERSION}
{$IFDEF FPC}
  {$IFNDEF VER1_0}
    {$MODE OBJFPC}
    {$DEFINE SUPPORTS_INLINE}
    {$UNDEF UNSUPPORTED_VERSION}
  {$ENDIF}
{$ELSE}
  {$IFDEF CONDITIONALEXPRESSIONS}

    {$IF DECLARED(CompilerVersion)}
      {$IF CompilerVersion >= 14}
        {$UNDEF UNSUPPORTED_VERSION}
      {$IFEND}
      {$IF CompilerVersion >= 17}
        {$DEFINE SUPPORTS_INLINE}
      {$IFEND}
    {$IFEND}

    {$IF DECLARED(RTLVersion)}
      {$IF RTLVersion >= 20}
        {$DEFINE SUPPORTS_MONITOR}
        {$DEFINE SUPPORTS_TSTRINGS_OWNSOBJECTS}
      {$IFEND}
      {$IF RTLVersion >= 21}
        {$DEFINE SUPPORTS_TARRAY}
        {$DEFINE SUPPORTS_TTIMESPAN}
      {$IFEND}
      {$IF RTLVersion >= 22}
        {$DEFINE SUPPORTS_TTIMEZONE}
      {$IFEND}
    {$IFEND}

  {$ENDIF}
{$ENDIF}
{$IFDEF UNSUPPORTED_VERSION}
  {$MESSAGE ERROR 'TZDB requires at least Delphi 6 or FreePascal 2.0 to compile!'}
{$ENDIF}

interface
uses
  SysUtils,
  DateUtils,
  Classes
{$IFNDEF SUPPORTS_TARRAY}, Types{$ENDIF}
{$IFDEF SUPPORTS_TTIMESPAN}, TimeSpan{$ENDIF};


type
{$IFNDEF SUPPORTS_TTIMEZONE}
  ///  <summary>Exception thrown when the passed local time is invalid.</summary>
  ELocalTimeInvalid = class(Exception);

  ///  <summary>Defines four types that a local date/time type can be in.</summary>
  TLocalTimeType = (
    ///  <summary>The local time is in the Standard year period.</summary>
    lttStandard,
    ///  <summary>The local time is in the DST year period.</summary>
    lttDaylight,
    ///  <summary>The local time is in DST -> Standard year period.</summary>
    lttAmbiguous,
    ///  <summary>The local time is in the Standard -> DST year period.</summary>
    lttInvalid
  );
{$ENDIF}

  ///  <summary>Exception type used to signal the caller code that a requested time zone
  ///  is not present in the bundled database or that its format is invalid.</summary>
  ETimeZoneInvalid = class(Exception);

  ///  <summary>A timezone class implementation that retreives its data from the bundled database.</summary>
  ///  <remarks>This class inherits the standard <c>TTimeZone</c> class in Delphi XE.</remarks>
  TBundledTimeZone = class{$IFDEF SUPPORTS_TTIMEZONE}(TTimeZone){$ENDIF}
  private
    FZone: Pointer;         { PZone }
    FPeriods: TList;        { TCompiledPeriod }

    { Compile periods into something useful }
    procedure CompilePeriods;

    { Helpers }                                                  { TCompiledPeriod }       { TCompiledRule }
    function GetPeriodAndRule(const ADateTime: TDateTime; out APeriod: TObject; out ARule: TObject): Boolean;

    procedure GetTZData(const ADateTime: TDateTime; out AOffset,
      ADstSave: Int64; out AType: TLocalTimeType; out ADisplayName, ADstDisplayName: string);

{$IFNDEF SUPPORTS_TTIMEZONE}
    { Purely internal getters }
    function GetCurrentAbbreviation: string;
    function GetCurrentDisplayName: string;
    function GetCurrentUtcOffset: {$IFDEF SUPPORTS_TTIMESPAN}TTimeSpan{$ELSE}Int64{$ENDIF};

    { Other good stuff }
    function GetUtcOffsetInternal(const ADateTime: TDateTime; const ForceDaylight: Boolean = false): Int64;
{$ENDIF}

  protected
{$IFDEF SUPPORTS_TTIMEZONE}
    ///  <summary>Retrieves the standard bias, DST bias and the type of the given local time.</summary>
    ///  <param name="ADateTime">The local time for which to retrieve the data.</param>
    ///  <param name="AOffset">The returned standard bias of the time zone for the given time.</param>
    ///  <param name="ADstSave">The returned DST bias of the time zone for the given time.</param>
    ///  <param name="AType">The returned type of the local time.</param>
    ///  <remarks>The value of <paramref name="ADstSave"/> is only relevant if
    ///  <paramref name="AType"/> is <c>lttAmbiguous</c> or <c>lttDaylight</c>.</remarks>
    procedure DoGetOffsetsAndType(
      const ADateTime: TDateTime; out AOffset, ADstSave: Int64; out AType: TLocalTimeType); override;

    ///  <summary>Retrieves the display name for the time zone based on a given local time.</summary>
    ///  <param name="ADateTime">The local time for which to retrieve the display name.</param>
    ///  <param name="ForceDaylight">Forces the timezone class to select the DST display name if the local time
    ///  is whithin the ambiguous period.</param>
    ///  <returns>The display name used to accompany the given local time.</returns>
    function DoGetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean): string; override;
{$ENDIF}

    ///  <summary>Returns the ID of the timezone. An ID is a string that should uniquely identify the timezone.</summary>
    ///  <returns>The ID of the timezone.</returns>
    function DoGetID: string; {$IFDEF SUPPORTS_TTIMEZONE}override;{$ENDIF}
   public
    ///  <summary>Creates a new instance of this timezone class.</summary>
    ///  <param name="ATimeZoneID">The ID of the timezone to use (ex. "Europe/Bucharest").</param>
    ///  <exception cref="TZDB|ETimeZoneInvalid">The specified ID cannot be found in the bundled database.</exception>
    constructor Create(const ATimeZoneID: string);

    ///  <summary>Destroys the current instance.</summary>
    destructor Destroy; override;

    ///  <summary>Returns a list of known time zones.</summary>
    ///  <param name="AIncludeAliases">Pass <c>True</c> to include time zone aliases into the list.</param>
    ///  <returns>An array of strings representing the IDs of the known time zones.</returns>
    class function KnownTimeZones(const 
      AIncludeAliases: Boolean = False): {$IFDEF SUPPORTS_TARRAY}TArray<string>{$ELSE}TStringDynArray{$ENDIF};

    ///  <summary>Returns an instance of this time zone class.</summary>
    ///  <param name="ATimeZoneID">The ID of the timezone to use (ex. "Europe/Bucharest").</param>
    ///  <exception cref="TZDB|ETimeZoneInvalid">The specified ID cannot be found in the bundled database.</exception>
    class function GetTimeZone(const ATimeZoneID: string): TBundledTimeZone;

{$IFNDEF SUPPORTS_TTIMEZONE}
    ///  <summary>Generates an abbreviation string for the given local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>A string containing the abbreviation.</returns>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    function GetAbbreviation(const ADateTime: TDateTime; const ForceDaylight: Boolean = false): string;

    ///  <summary>Generates a diplay string for the given local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>A string containing the display name.</returns>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    function GetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean = false): string;

    ///  <summary>Returns the type of the local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns>A enumeration value specifying the type of the local time.</returns>
    function GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    ///  <summary>Checks whether the specified local time is ambiguous.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns><c>True</c> if the local time is ambiguous; <c>False</c> otherwise.</returns>
    function IsAmbiguousTime(const ADateTime: TDateTime): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    ///  <summary>Checks whether the specified local time is daylight.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns><c>True</c> if the local time is ambiguous; <c>False</c> otherwise.</returns>
    function IsDaylightTime(const ADateTime: TDateTime; const ForceDaylight: Boolean = false): Boolean;

    ///  <summary>Checks whether the specified local time is invalid.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <returns><c>True</c> if the local time is invalid; <c>False</c> otherwise.</returns>
    function IsInvalidTime(const ADateTime: TDateTime): Boolean; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    ///  <summary>Checks whether the specified local time is standard.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns><c>True</c> if the local time is standard; <c>False</c> otherwise.</returns>
    function IsStandardTime(const ADateTime: TDateTime; const ForceDaylight: Boolean = false): Boolean;

    ///  <summary>Returns the UTC offset of the given local time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>The UTC offset of the given local time. Subtract this value from the passed local time to obtain an UTC time.</returns>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    function GetUtcOffset(const ADateTime: TDateTime; const ForceDaylight: Boolean = false): 
      {$IFDEF SUPPORTS_TTIMESPAN}TTimeSpan{$ELSE}Int64{$ENDIF}; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    ///  <summary>Converts an UTC time to a local time.</summary>
    ///  <param name="ADateTime">The UTC time.</param>
    ///  <returns>The local time that corresponds to the passed UTC time.</returns>
    function ToLocalTime(const ADateTime: TDateTime): TDateTime;

    ///  <summary>Converts a local time to an UTC time.</summary>
    ///  <param name="ADateTime">The local time.</param>
    ///  <param name="ForceDaylight">Specify a <c>True</c> value if ambiguous periods should be treated as DST.</param>
    ///  <returns>The UTC time that corresponds to the passed local time.</returns>
    ///  <exception cref="TZDB|ELocalTimeInvalid">The specified local time is invalid.</exception>
    function ToUniversalTime(const ADateTime: TDateTime; 
      const ForceDaylight: Boolean = false): TDateTime; {$IFDEF SUPPORTS_INLINE}inline;{$ENDIF}

    ///  <summary>Returns the ID of the timezone. An ID is a string that should uniquely identify the timezone.</summary>
    ///  <returns>The ID of the timezone.</returns>
    property ID: string read DoGetID;

    ///  <summary>Returns the current time zone's display name string.</summary>
    ///  <returns>A string containing the display name.</returns>
    property DisplayName: string read GetCurrentDisplayName;

    ///  <summary>Returns the current time zone's abbreviation string.</summary>
    ///  <returns>A string containing the abbreviation.</returns>
    property Abbreviation: string read GetCurrentAbbreviation;

    ///  <summary>Returns the current time zone's UTC offset.</summary>
    ///  <returns>The current UTC offset.</returns>
    property UtcOffset: {$IFDEF SUPPORTS_TTIMESPAN}TTimeSpan{$ELSE}Int64{$ENDIF} read GetCurrentUtcOffset;
{$ENDIF}
  end;

implementation

uses
{$IFNDEF SUPPORTS_MONITOR}
  SyncObjs,
{$ENDIF}
  Contnrs,
  IniFiles;

resourcestring
  SNoBundledTZForName = 'Could not find any data for timezone "%s".';
  STimeZoneHasNoPeriod =
    'There is no matching period that matches date [%s] in timezone "%s".';

{$IFNDEF SUPPORTS_TTIMEZONE}
  SInvalidLocalTime = 'Local date/time value %s is invalid (does not exist in the time zone).';
{$ENDIF}

type
  { Day type. Specifies the "relative" day in a month }
  TDayType = (dtFixed, dtLastOfMonth, tdNthOfMonth);

  { Specifies the mode in which a time value is specified }
  TTimeMode = (trLocal, trStandard, trUniversal);

  { Stores the information about the relative days }
  TRelativeDay = record
    case FDayType: TDayType of
      dtFixed:
        (FFixedDay: Word);
      dtLastOfMonth:
        (FLastDayOfWeek: Word);
      tdNthOfMonth:
        (FNthDayOfWeek: Word;
          FDayIndex: Word);
  end;

  { Pointer to a relative day }
  PRelativeDay = ^TRelativeDay;

  { Defines a rule used for DST changes }
  TRule = record
    FInMonth: Word; { The month (1 - 12) when DST change occurs }
    FOnDay: PRelativeDay; { Pointer to a TRelativeDay value }
    FAt: Int64; { Time, in seconds }
    FAtMode: TTimeMode; { Time relation mode }
    FOffset: Int64; { Offset from GMT in seconds }
    FFmtPart: string;
    { A symbolic string used later when building short TZ names }
  end;

  { Pointer to a rule }
  PRule = ^TRule;

  { Defines a rule that also has a validity date defined }
  TYearBoundRule = record
    FStart: Word; { The year in which the rule starts to apply }
    FEnd: Word; { The year in which the rule ends to apply }
    FRule: PRule; { A pointer to the actual rule }
  end;

  { Pointer to a year-bound rule entry }
  PYearBoundRule = ^TYearBoundRule;

  { Defines a rule family. If fact it is a set of rules combined under the same ID }
  TRuleFamily = record
    FCount: Integer; { Count of rule in the current family }
    FFirstRule: PYearBoundRule;
    { Pointer to the first rule in a static array defined previously }
  end;

  { A pointer to a rule family }
  PRuleFamily = ^TRuleFamily;

  { A period of some years (for a zone) that defines specific DST rules and offsets }
  TPeriod = record
    FOffset: Integer; { GMT offset in seconds for this period of time }
    FRuleFamily: PRuleFamily;
    { Pointer to the family if rules that apply to this period }
    FFmtStr: string;
    { Format string that will get translated in certain conditions }
    FUntilYear, FUntilMonth: Word; { Period is valid until this Year/Month }
    FUntilDay: PRelativeDay; { Period is valid until this Day in Year/Month }
    FUntilTime: Int64;
    FUntilTimeMode: TTimeMode; { Time relation mode }
    { Period is valid until this time of day Day in Year/Month. In seconds }
  end;

  { Pointer to a TPeriod }
  PPeriod = ^TPeriod;

  { Defines a time-zone. }
  TZone = record
    FName: string; { Zone name (aka Europe/Romania, Europe/London etc) }
    FCount: Integer; { Count of periods defined by this zone }
    FFirstPeriod: PPeriod; { Pointer to the first TPeriod for this zone }
  end;

  { Pointer to a zone object }
  PZone = ^TZone;

  { Alias to a zone }
  TZoneAlias = record
    FName: string; { Name of the zone to alias }
    FAliasTo: PZone; { Pointer to aliased zone }
  end;

  {$I TZDB.inc}

function EncodeDateMonthLastDayOfWeek(const AYear, AMonth, ADayOfWeek: Word): TDateTime;
var
  LDoW: Word;
begin
  { Generate a date that looks like: Year/Month/(Last Day of Month) }
  Result := EncodeDate(AYear, AMonth, DaysInAMonth(AYear, AMonth));

  { Get the day of week for this newly crafted date }
  LDoW := DayOfTheWeek(Result);

  { We're too far off now, let's decrease the number of days till we get to the desired one }
  if LDoW > ADayOfWeek then
    Result := IncDay(Result, -1 * (LDoW - ADayOfWeek))
  else if LDoW < ADayOfWeek then
    Result := IncDay(Result, -1 * (DaysPerWeek - ADayOfWeek + LDoW));
end;

function EncodeDateMonthFirstDayOfWeek(const AYear, AMonth, ADayOfWeek: Word): TDateTime;
var
  LDoW: Word;
begin
  { Generate a date that looks like: Year/Month/1st }
  Result := EncodeDate(AYear, AMonth, 1);

  { Get the day of week for this newly crafted date }
  LDoW := DayOfTheWeek(Result);

  { We're too far off now, let's decrease the number of days till we get to the desired one }
  if LDoW > ADayOfWeek then
    Result := IncDay(Result, DaysPerWeek - LDoW + ADayOfWeek)
  else if (LDoW < ADayOfWeek) Then
    Result := IncDay(Result, ADayOfWeek - LDoW);
end;

function EncodeDateMonthFirstDayOfWeekAfter(const AYear, AMonth, ADayOfWeek, AAfter: Word): TDateTime;
begin
  { Generate a date with the given day of week as first in month }
  Result := EncodeDateMonthFirstDayOfWeek(AYear, AMonth, ADayOfWeek);

  { Iterate until we've surpassed our min requirement }
  while DayOf(Result) < AAfter do
  begin
    Result := IncWeek(Result);

    { Safe-guard! If we've gotten to another month, get back a week and stop. }
    if MonthOf(Result) <> AMonth then
    begin
      Result := IncWeek(Result, -1);
      break;
    end
  end;
end;

function RelativeToDateTime(const AYear, AMonth: Word; const ARelativeDay: PRelativeDay; const ATimeOfDay: Int64): TDateTime;
begin
  Result := 0;

  { Special case - if there is no day defined then there is no time also. Exit with only the date part. }
  if ARelativeDay = nil then
    Result := EncodeDate(AYear, AMonth, 1)
  else if ARelativeDay^.FDayType = dtFixed then
    Result := EncodeDate(AYear, AMonth, ARelativeDay^.FFixedDay)
  else if ARelativeDay^.FDayType = dtLastOfMonth then
    Result := EncodeDateMonthLastDayOfWeek(AYear, AMonth, ARelativeDay^.FLastDayOfWeek)
  else if ARelativeDay^.FDayType = tdNthOfMonth then
    Result := EncodeDateMonthFirstDayOfWeekAfter(AYear, AMonth, ARelativeDay^.FNthDayOfWeek, ARelativeDay^.FDayIndex);

  { Attach the time part now }
  Result := IncSecond(Result, ATimeOfDay);
end;

function FormatAbbreviation(const APeriod: PPeriod; const ARule: PRule): string;
begin
  if Pos('%s', APeriod^.FFmtStr) > 0 then
  begin
    { There is a place holder in the format string. Replace if with the current letter in the rule }
    if ARule <> nil then
      Result := Format(APeriod^.FFmtStr, [ARule^.FFmtPart])
    else
      Result := Format(APeriod^.FFmtStr, ['']);

    { In case no rule is defined, replace the placeholder with an empty string }
  end else
    Result := APeriod^.FFmtStr;
end;

type
  { Contains a compiled rule }
  TCompiledRule = class
  private
    FRule: PRule;
    FStartsOn: TDateTime;
    FOffset: Int64;
    FNext, FPrev: TCompiledRule;

  public
    constructor Create(const ARule: PRule; const AStartsOn: TDateTime;
      const AOffset: Int64);

    function GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;
  end;

  { Contains a compiled period (easier for lookup) }
  TCompiledPeriod = class
  private
    FPeriod: PPeriod;
    FFrom, FUntil: TDateTime;

{$IFNDEF SUPPORTS_MONITOR}
    FRulesByYearLock: TCriticalSection;
{$ENDIF}
    { Year -> List of Rules for that year }
    FRulesByYear: TBucketList;  { Word, TList<TCompiledRule> }

    { Obtain the last rule that is active in a given year }
    function GetLastRuleForYear(const AYear: Word): PRule;

    { Compiles the Rules for a given year }
    function CompileRulesForYear(const AYear: Word): TList;  { TCompiledRule }
  public
    { Basic stuffs }
    constructor Create(const APeriod: PPeriod; const AFrom, AUntil: TDateTime);
    destructor Destroy(); override;

    { Finds a matching rule }
    function FindMatchingRule(const ADateTime: TDateTime): TCompiledRule;
  end;

var
{$IFNDEF SUPPORTS_MONITOR}
  FTimeZoneCacheLock: TCriticalSection;
{$ENDIF}
  FTimeZoneCache: TStringList; { <String, TBundledTimeZone> }

function CompiledPeriodComparison(ALeft, ARight: Pointer): Integer;
begin
  { Use standard DT comparison operation }
  Result := CompareDateTime(TCompiledPeriod(ALeft).FUntil,
    TCompiledPeriod(ARight).FUntil);
end;

function CompiledRuleComparison(ALeft, ARight: Pointer): Integer;
begin
  { Use standard DT comparison operation }
  Result := CompareDateTime(TCompiledRule(ALeft).FStartsOn,
    TCompiledRule(ARight).FStartsOn);
end;

procedure ForEachYearlyRule(AInfo, AItem, AData: Pointer; out AContinue: Boolean);
begin
  { Free the value list }
  TList(AData).Free;
  AContinue := True;
end;

{ TCompiledPeriod }

function TCompiledPeriod.CompileRulesForYear(const AYear: Word): TList;
var
  LCurrRule: PYearBoundRule;
  LLastYearRule: PRule;
  LAbsolute: TDateTime;
  I: Integer;
begin
  { Initialize the compiled list }
  Result := TObjectList.Create(true);

  { Check whether we actually have a fule family attached }
  if FPeriod^.FRuleFamily <> nil then
  begin
    { Let's start with the last active rule from last year }
    LLastYearRule := GetLastRuleForYear(AYear - 1);

    { Add the the last year rule since 1 jan 00:00 this year }
    if LLastYearRule <> nil then
      Result.Add(TCompiledRule.Create(LLastYearRule, EncodeDate(AYear, 1, 1), LLastYearRule^.FOffset));

    { Obtain the first rule in chain }
    LCurrRule := FPeriod^.FRuleFamily^.FFirstRule;

    for I := 0 to FPeriod^.FRuleFamily^.FCount - 1 do
    begin
      { Check we're in the required year }
      if (AYear >= LCurrRule^.FStart) and (AYear <= LCurrRule^.FEnd) then
      begin
        { Obtain the absolute date when the rule activates in this year }
        LAbsolute := RelativeToDateTime(AYear,
            LCurrRule^.FRule^.FInMonth, LCurrRule^.FRule^.FOnDay,
            LCurrRule^.FRule^.FAt);

        { Adjust the value based on the specified time mode (do nothing for local mode) }
        case LCurrRule^.FRule^.FAtMode of
          trStandard:
            { This value is specified in the currect period's statndard time. Add the rule offset to get to local time. }
            LAbsolute := IncSecond(LAbsolute, LCurrRule^.FRule^.FOffset);

          trUniversal:
            { This value is specified in universal time. Add both the standard deviation plus the local time }
            LAbsolute := IncSecond(LAbsolute, FPeriod^.FOffset + LCurrRule^.FRule^.FOffset);
        end;

        { Add the new compiled rule to the list }
        Result.Add(TCompiledRule.Create(LCurrRule^.FRule, LAbsolute,
            LCurrRule^.FRule^.FOffset));
      end;

      { Go to next rule }
      Inc(LCurrRule);
    end;

    { Sort the list ascending by the activation date/time }
    Result.Sort(@CompiledRuleComparison);

    { Create a linked list based on offsets and their nexts (will be used on type getting) }
    for I := 0 to Result.Count - 1 do
    begin
      { Set N[I].Next -> N[I + 1] }
      if I < (Result.Count - 1) then
        TCompiledRule(Result[I]).FNext := TCompiledRule(Result[I + 1]);

      { Set N[I].Prev -> N[I - 1] }
      if I > 0 then
        TCompiledRule(Result[I]).FPrev := TCompiledRule(Result[I - 1]);
    end;
  end;

  { Register the new list into the dictionary }
{$WARNINGS OFF}
  FRulesByYear.Add(Pointer(AYear), Result);
{$WARNINGS ON}
end;

constructor TCompiledPeriod.Create(const APeriod: PPeriod; const AFrom, AUntil: TDateTime);
begin
  FPeriod := APeriod;
  FUntil := AUntil;
  FFrom := AFrom;

{$IFNDEF SUPPORTS_MONITOR}
  FRulesByYearLock := TCriticalSection.Create;
{$ENDIF}
  FRulesByYear := TBucketList.Create();
end;

destructor TCompiledPeriod.Destroy;
begin
{$IFNDEF SUPPORTS_MONITOR}
  FRulesByYearLock.Free;
{$ENDIF}

  { Free each rule }
  if Assigned(FRulesByYear) then
  begin
{$IFDEF FPC}
    FRulesByYear.ForEach(@ForEachYearlyRule);
{$ELSE}
    FRulesByYear.ForEach(ForEachYearlyRule);
{$ENDIF}

    FRulesByYear.Free;
  end;

  inherited;
end;

function TCompiledPeriod.FindMatchingRule(const ADateTime: TDateTime): TCompiledRule;
var
  LYear: Word;
  LCompiledList: TList;
  I, LCompResult: Integer;
begin
  Result := nil;
  LYear := YearOf(ADateTime);

  { Protect this part of the code since it may change internal structures over time }
{$IFDEF SUPPORTS_MONITOR}
  MonitorEnter(FRulesByYear);
{$ELSE}
  FRulesByYearLock.Enter();
{$ENDIF}
  try
{$WARNINGS OFF}
    { Check if we have a cached list of matching rules for this date's year }
    if not FRulesByYear.Find(Pointer(LYear), Pointer(LCompiledList)) then
      LCompiledList := CompileRulesForYear(LYear);
{$WARNINGS ON}

    { Iterate over and search what we like. Do not stop on the first one obviously }
    for I := 0 to LCompiledList.Count - 1 do
    begin
      LCompResult := CompareDateTime(ADateTime,
        TCompiledRule(LCompiledList[I]).FStartsOn);

      if LCompResult >= 0 then
        Result := TCompiledRule(LCompiledList[I]);
    end;
  finally
{$IFDEF SUPPORTS_MONITOR}
    MonitorExit(FRulesByYear);
{$ELSE}
    FRulesByYearLock.Leave();
{$ENDIF}
  end;
end;

function TCompiledPeriod.GetLastRuleForYear(const AYear: Word): PRule;
var
  LCurrRule: PYearBoundRule;
  LAbsolute, LBestChoice: TDateTime;
  I: Integer;
begin
  { Default to nothing obviously }
  Result := nil;

  { Check whether we actually have a fule family attached }
  if FPeriod^.FRuleFamily = nil then
    exit;

  { Obtain the first rule in chain }
  LCurrRule := FPeriod^.FRuleFamily^.FFirstRule;
  LBestChoice := 0;

  for I := 0 to FPeriod^.FRuleFamily^.FCount - 1 do
  begin
    { Check we're in the required year }
    if (AYear >= LCurrRule^.FStart) and (AYear <= LCurrRule^.FEnd) then
    begin
      { Obtain the absolute date when the rule activates in this year }
      LAbsolute := RelativeToDateTime(AYear, LCurrRule^.FRule^.FInMonth,
        LCurrRule^.FRule^.FOnDay, LCurrRule^.FRule^.FAt);

      { Select this rule if it's better suited }
      if CompareDateTime(LAbsolute, LBestChoice) >= 0 then
      begin
        LBestChoice := LAbsolute;
        Result := LCurrRule^.FRule;
      end;
    end;

    { Go to next rule }
    Inc(LCurrRule);
  end;
end;

{ TCompiledRule }

constructor TCompiledRule.Create(const ARule: PRule;
  const AStartsOn: TDateTime; const AOffset: Int64);
begin
  FRule := ARule;
  FStartsOn := AStartsOn;
  FOffset := AOffset;
end;

function TCompiledRule.GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;
begin
  { Try with the ending of the rule }
  if (FNext <> nil) and (FNext.FOffset > FOffset) and
     (CompareDateTime(ADateTime, IncSecond(FNext.FStartsOn, FOffset - FNext.FOffset)) >= 0) then
     Result := lttInvalid
  else if (FPrev = nil) and (FOffset < 0) and
       (CompareDateTime(ADateTime, IncSecond(FStartsOn, -FOffset)) < 0) then
       Result := lttAmbiguous
  else if (FPrev <> nil) and (FPrev.FOffset > FOffset) and
     (CompareDateTime(ADateTime, IncSecond(FStartsOn, FPrev.FOffset - FOffset)) < 0) then
       Result := lttAmbiguous
  else if FOffset <> 0 then
      Result := lttDaylight
  else
    Result := lttStandard;
end;

{ TBundledTimeZone }

procedure TBundledTimeZone.CompilePeriods;
var
  LCompiledPeriod: TCompiledPeriod;
  LCurrentPeriod: PPeriod;
  LStart: TDateTime;
  LAbsolute: TDateTime;
  LRule: PRule;
  I: Integer;
begin
  LCurrentPeriod := PZone(FZone)^.FFirstPeriod;
  LStart := 0;

  for I := 0 to PZone(FZone)^.FCount - 1 do
  begin
    { Calculate the end date }
    LAbsolute := RelativeToDateTime(LCurrentPeriod^.FUntilYear,
        LCurrentPeriod^.FUntilMonth, LCurrentPeriod^.FUntilDay,
        LCurrentPeriod^.FUntilTime);

    { Set the approperiate values }
    LCompiledPeriod := TCompiledPeriod.Create(LCurrentPeriod, LStart, LAbsolute);

    { Get the last rule defined in the period }
    if LCurrentPeriod^.FUntilDay <> nil then
    begin
      LRule := LCompiledPeriod.GetLastRuleForYear(LCurrentPeriod^.FUntilYear);

      if LRule <> nil then
      begin
        { Adjust the value based on the specified time mode (do nothing for local mode) }
        case LCurrentPeriod^.FUntilTimeMode of
          trStandard:
            { The period uses its standard time. Adjust to it }
            LCompiledPeriod.FUntil := IncSecond(LAbsolute, LRule^.FOffset);

          trUniversal:
            { This value is specified in universal time. Add both the standard deviation plus the local time }
            LCompiledPeriod.FUntil := IncSecond(LAbsolute, LCurrentPeriod^.FOffset + LRule^.FOffset);
        end;
      end;
    end;

    { Put the compiled period to a list }
    FPeriods.Add(LCompiledPeriod);

    { Set the last "until" }
    LStart := LCompiledPeriod.FUntil;

    { Move to the next period in the zone }
    Inc(LCurrentPeriod);
  end;

  { Sort the list ascending }
  FPeriods.Sort(@CompiledPeriodComparison);
end;

constructor TBundledTimeZone.Create(const ATimeZoneID: string);
var
  LIndex: Integer;
begin
  { First, search in the CZones array }
  for LIndex := Low(CZones) to High(CZones) do
    if SameText(CZones[LIndex].FName, ATimeZoneID) then
    begin
      FZone := @CZones[LIndex];
      break;
    end;

  { Second, search in the aliases array }
  if FZone = nil then
    for LIndex := Low(CAliases) to High(CAliases) do
      if SameText(CAliases[LIndex].FName, ATimeZoneID) then
      begin
        FZone := CAliases[LIndex].FAliasTo;
        break;
      end;

  { Throw exception on error }
  if FZone = nil then
    raise ETimeZoneInvalid.CreateResFmt(@SNoBundledTZForName, [ATimeZoneID]);

  { Initialize internals }
  FPeriods := TObjectList.Create(true);
  CompilePeriods();
end;

destructor TBundledTimeZone.Destroy;
begin
  FPeriods.Free;
  inherited;
end;

{$IFDEF SUPPORTS_TTIMEZONE}
function TBundledTimeZone.DoGetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean): string;
var
  LOffset, LDstSave: Int64;
  LTimeType: TLocalTimeType;
  LStd, LDst: string;
begin
  { Call the mega-utility method }
  GetTZData(ADateTime, LOffset, LDstSave, LTimeType, LStd, LDst);

  { It's a bit unclear naming here. LStd is not always the standard name. It's the "standard output" string. LDst
    only makes sense if the type of the local time if ambiguous. }
  if (LTimeType = lttAmbiguous) and ForceDaylight then
    Result := LDst
  else
    Result := LStd;
end;

procedure TBundledTimeZone.DoGetOffsetsAndType(
  const ADateTime: TDateTime;
  out AOffset, ADstSave: Int64;
  out AType: TLocalTimeType);
var
  LDummy, LDummy2: string;
begin
  { Call the mega-utility method }
  GetTZData(ADateTime, AOffset, ADstSave, AType, LDummy, LDummy2);
end;
{$ENDIF}

function TBundledTimeZone.DoGetID: string;
begin
  { Get the Id of the time zone from the stored var }
  Result := PZone(FZone)^.FName;
end;

{$IFNDEF SUPPORTS_TTIMEZONE}
function TBundledTimeZone.GetAbbreviation(const ADateTime: TDateTime; const ForceDaylight: Boolean): string;
const
  CGMT = 'GMT';
  CMinus = '-';
  CPlus = '+';
  CSemi = ':';
  CDigitFmt = '%.2d';

  function FmtPart(const APart: Word): string;
  begin
    Result := Format(CDigitFmt, [APart]);
  end;

var
  LOffset, LHours, LMinutes, LSeconds: Int64;
begin
  { Get the UTC offset for the given time. }
  LOffset := GetUtcOffsetInternal(ADateTime, ForceDaylight);

  { Start with GMT }
  Result := CGMT;

  { Nothing for zero offset }
  if LOffset = 0 then
    Exit;

  { Calculate the hh:mm:ss parts }
  LSeconds := Abs(LOffset);
  LHours := LSeconds div (SecsPerMin * MinsPerHour); Dec(LSeconds, LHours * SecsPerMin * MinsPerHour);
  LMinutes := LSeconds div SecsPerMin; Dec(LSeconds, LMinutes * SecsPerMin);

  { Add the sign }
  if LOffset < 0 then
    Result := Result + CMinus
  else
    Result := Result + CPlus;

  { And now add the remaining pieces }
  Result := Result + FmtPart(LHours);
  if (LMinutes <> 0) or (LSeconds <> 0) then
    Result := Result + CSemi + FmtPart(LMinutes);
  if LSeconds <> 0 then
    Result := Result + CSemi + FmtPart(LSeconds);
end;

function TBundledTimeZone.GetCurrentAbbreviation: string;
begin
  { Call GetAbbreviation for current local time. }
  Result := GetAbbreviation(Now);
end;

function TBundledTimeZone.GetCurrentDisplayName: string;
begin
  { Call GetDisplayName for current local time. }
  Result := GetDisplayName(Now);
end;

function TBundledTimeZone.GetCurrentUtcOffset: {$IFDEF SUPPORTS_TTIMESPAN}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
  { Call GetUtcOffset for current local time. }
  Result := GetUtcOffset(Now);
end;

function TBundledTimeZone.GetDisplayName(const ADateTime: TDateTime; const ForceDaylight: Boolean): string;
var
  LOffset, LDstSave: Int64;
  LTimeType: TLocalTimeType;
  LStd, LDst: string;
begin
  { Call the mega-utility method }
  GetTZData(ADateTime, LOffset, LDstSave, LTimeType, LStd, LDst);

  { It's a bit unclear naming here. LStd is not always the standard name. It's the "standard output" string. LDst
    only makes sense if the type of the local time if ambiguous. }
  if LTimeType = lttInvalid then
    raise ELocalTimeInvalid.CreateResFmt(@SInvalidLocalTime, [DateTimeToStr(ADateTime)])
  else if (LTimeType = lttAmbiguous) and ForceDaylight then
    Result := LDst
  else
    Result := LStd;
end;

function TBundledTimeZone.GetLocalTimeType(const ADateTime: TDateTime): TLocalTimeType;
var
  LOffset, LDstSave: Int64; // Dummy
  LStd, LDst: string;       // Dummy
begin
  { Call the mega-utility method }
  GetTZData(ADateTime, LOffset, LDstSave, Result, LStd, LDst);
end;

function TBundledTimeZone.GetUtcOffset(const ADateTime: TDateTime; const ForceDaylight: Boolean): 
  {$IFDEF SUPPORTS_TTIMESPAN}TTimeSpan{$ELSE}Int64{$ENDIF};
begin
{$IFDEF SUPPORTS_TTIMESPAN}
  { Call the internal helper and generate a TTimeSpan out of it }
  Result := TTimeSpan.FromSeconds(
    GetUtcOffsetInternal(ADateTime, ForceDaylight)
  );
{$ELSE}
  { Call internal method directly if no TTimeSpan is available }
  Result := GetUtcOffsetInternal(ADateTime, ForceDaylight);
{$ENDIF}
end;

function TBundledTimeZone.GetUtcOffsetInternal(const ADateTime: TDateTime; const ForceDaylight: Boolean): Int64;
var
  LDstSave: Int64;
  LTimeType: TLocalTimeType;
  LStd, LDst: string; // Dummy!
begin
  { Get all the expected data for this local time }
  GetTZData(ADateTime, Result, LDstSave, LTimeType, LStd, LDst);

  { And properly calculate teh offsets }
  if (LTimeType = lttInvalid) then
    raise ELocalTimeInvalid.CreateResFmt(@SInvalidLocalTime, [DateTimeToStr(ADateTime)])
  else if (LTimeType = lttDaylight) or ((LTimeType = lttAmbiguous) and ForceDaylight) then
    Inc(Result, LDSTSave);
end;

function TBundledTimeZone.IsAmbiguousTime(const ADateTime: TDateTime): Boolean;
begin
  { Call GetLocalTimeType and check the result for lttInvalid }
  Result := GetLocalTimeType(ADateTime) = lttAmbiguous;
end;

function TBundledTimeZone.IsDaylightTime(const ADateTime: TDateTime; const ForceDaylight: Boolean): Boolean;
var
  LType: TLocalTimeType;
begin
  { Call GetLocalTimeType and store the result }
  LType := GetLocalTimeType(ADateTime);

  { If the type is daylight or ambiguous with forcing set to on. }
  Result := (LType = lttDaylight) or
    ((LType = lttAmbiguous) and ForceDaylight);
end;

function TBundledTimeZone.IsInvalidTime(const ADateTime: TDateTime): Boolean;
begin
  { Call GetLocalTimeType and check the result for lttInvalid }
  Result := GetLocalTimeType(ADateTime) = lttInvalid;
end;

function TBundledTimeZone.IsStandardTime(const ADateTime: TDateTime; const ForceDaylight: Boolean): Boolean;
var
  LType: TLocalTimeType;
begin
  { Call GetLocalTimeType and store the result }
  LType := GetLocalTimeType(ADateTime);

  { If the type is standard or ambiguous with forcing set to off. }
  Result := (LType = lttStandard) or
    ((LType = lttAmbiguous) and not ForceDaylight);
end;

function TBundledTimeZone.ToLocalTime(const ADateTime: TDateTime): TDateTime;
var
  LBias, LDstSave: Int64;
  LTimeType: TLocalTimeType;
  LStd, LDst: string; // Dummy!
  LAdjusted: TDateTime;
begin
  { Get all the expected data for this UTC time. }
  GetTZData(ADateTime, LBias, LDstSave, LTimeType, LStd, LDst);

  { Create a new date-time adjusted by the standard bias. Now, we might have landed into an
    invalid yer period or an ambiguous year period. We will check for that and adjust properly. }
  LAdjusted := IncSecond(ADateTime, LBias);

  { Get all the expected data for the adjust UTC (now local) time. }
  GetTZData(LAdjusted, LBias, LDstSave, LTimeType, LStd, LDst);

  { If we have indeed landed into the 2 nasty periods, simply add the DST save so we can get into the safe zone. }
  if (LTimeType = lttInvalid) or (LTimeType = lttDaylight) then
    Result := IncSecond(LAdjusted, LDSTSave)
  else
    Result := LAdjusted;
end;

function TBundledTimeZone.ToUniversalTime(const ADateTime: TDateTime; const ForceDaylight: Boolean): TDateTime;
begin
  { Very simple, get the UTC offset for the local time and decrement it to get to UTC }
  Result := IncSecond(ADateTime,
    -GetUtcOffsetInternal(ADateTime, ForceDaylight));
end;
{$ENDIF}

function TBundledTimeZone.GetPeriodAndRule(const ADateTime: TDateTime; out APeriod: TObject; out ARule: TObject): Boolean;
var
  I: Integer;
begin
  { Defaults }
  Result := false;
  APeriod := nil;

  { Got backwards. We probably are closer to present than past :P }
  for I := FPeriods.Count - 1 downto 0 do
  begin
    APeriod := TObject(FPeriods[I]);

    { Check that we're in this period }
    if (CompareDateTime(ADateTime, TCompiledPeriod(APeriod).FFrom) >= 0) and
      (CompareDateTime(ADateTime, TCompiledPeriod(APeriod).FUntil) < 0) then
    begin
      Result := true;
      break;
    end;
  end;

  { Exit if there is no period found. }
  if not Result then
    exit;

  { Find the rule that matches this period }
  ARule := TCompiledPeriod(APeriod).FindMatchingRule(ADateTime);
end;

class function TBundledTimeZone.GetTimeZone(const ATimeZoneID: string): TBundledTimeZone;
var
  LIndex: Integer;
begin
  { Access the cache }
{$IFDEF SUPPORTS_MONITOR}
  MonitorEnter(FTimeZoneCache);
{$ELSE}
  FTimeZoneCacheLock.Enter();
{$ENDIF}
  try
    { Check if we know this TZ }
    LIndex := FTimeZoneCache.IndexOf(ATimeZoneID);

    if LIndex = -1 then
    begin
      Result := TBundledTimeZone.Create(ATimeZoneID);

      { Check for ID and not alias }
      LIndex := FTimeZoneCache.IndexOf(Result.ID);

      { Check if maybe we used an alias and need to change things }
      if LIndex > -1 then
      begin
        Result.Free;
        Result := TBundledTimeZone(FTimeZoneCache.Objects[LIndex]);
      end else
        FTimeZoneCache.AddObject(Result.ID, Result);

    end else
      Result := TBundledTimeZone(FTimeZoneCache.Objects[LIndex]);

  finally
{$IFDEF SUPPORTS_MONITOR}
  MonitorExit(FTimeZoneCache);
{$ELSE}
  FTimeZoneCacheLock.Leave();
{$ENDIF}
  end;
end;

procedure TBundledTimeZone.GetTZData(
  const ADateTime: TDateTime;
  out AOffset, ADstSave: Int64;
  out AType: TLocalTimeType;
  out ADisplayName, ADstDisplayName: string);
var
  LPeriod: TCompiledPeriod;
  LRule: TCompiledRule;
  LPRule: PRule;
begin
  { Get period and rule }
  if not GetPeriodAndRule(ADateTime, TObject(LPeriod), TObject(LRule)) then
    raise ETimeZoneInvalid.CreateResFmt(@STimeZoneHasNoPeriod,
      [DateTimeToStr(ADateTime), DoGetID()]);

  { Go ahead baby }
  AOffset := LPeriod.FPeriod^.FOffset;
  ADstSave := 0;

  { Get rule specific data }
  if LRule <> nil then
  begin
    { Some little hacks to integrate this more powerful system in DateUtils' TTimeZone system.
      AOffset in TTimeZone is always set to the same value all year long. ADstSave is provided in case of
      ambiguous and invalid times. }
    AType := LRule.GetLocalTimeType(ADateTime);

    if AType = lttDaylight then
      ADstSave := LRule.FOffset
    else if AType = lttAmbiguous then
    begin
      { In case of ambiguous, fill in the dst save accordingly }
      if LRule.FPrev <> nil then
        ADstSave := LRule.FPrev.FOffset - LRule.FOffset
      else
        ADstSave := LRule.FOffset;
    end else if AType = lttInvalid then
    begin
      { In case of invalid, fill in the dst save accordingly }
      if LRule.FNext <> nil then
        ADstSave := LRule.FNext.FOffset - LRule.FOffset
      else
        ADstSave := LRule.FOffset;
    end;
  end else
    AType := lttStandard;

  { The normal display name based on rule relationships }
  if LRule <> nil then
    LPRule := LRule.FRule
  else
    LPRule := nil;

  ADisplayName := FormatAbbreviation(LPeriod.FPeriod, LPRule);

  { The DST display name, only of ambiguity was found and we have a rule to prove it -- otherwise
    its just the standard name. }
  if (AType = lttAmbiguous) and (LRule.FPrev <> nil) then
    ADstDisplayName := FormatAbbreviation(LPeriod.FPeriod, LRule.FPrev.FRule)
  else
    ADstDisplayName := ADisplayName;
end;

class function TBundledTimeZone.KnownTimeZones(const AIncludeAliases: Boolean): 
  {$IFDEF SUPPORTS_TARRAY}TArray<string>{$ELSE}TStringDynArray{$ENDIF};
var
  I, LIndex: Integer;
begin
  { Prepare the output array }
  if AIncludeAliases then
    SetLength(Result, Length(CZones) + Length(CAliases))
  else
    SetLength(Result, Length(CZones));

  { Copy the zones in }
  LIndex := 0;
  for I := Low(CZones) to High(CZones) do
  begin
    Result[LIndex] := CZones[I].FName;
    Inc(LIndex);
  end;

  { Copy the aliases in (if requested) }
  if AIncludeAliases then
    for I := Low(CAliases) to High(CAliases) do
    begin
      Result[LIndex] := CAliases[I].FName;
      Inc(LIndex);
    end;
end;

{$IFNDEF SUPPORTS_TSTRINGS_OWNSOBJECTS}
procedure FreeStringListObjects(const AStrings: TStrings);
var
  LCurrent: Integer;
begin
  for LCurrent := 0 to AStrings.Count - 1 do
  begin
    AStrings.Objects[LCurrent].Free;
    AStrings.Objects[LCurrent] := nil;
  end;
end;
{$ENDIF}

initialization
  { Create a lock for the time zone hash }
{$IFNDEF SUPPORTS_MONITOR}
  FTimeZoneCacheLock := TCriticalSection.Create();
{$ENDIF}

  { Use THashedStringList for fast lookup. Also set ows objects to true. }
  FTimeZoneCache := THashedStringList.Create();
{$IFDEF SUPPORTS_TSTRINGS_OWNSOBJECTS}
  FTimeZoneCache.OwnsObjects := True;
{$ENDIF}
  FTimeZoneCache.CaseSensitive := False;

finalization
{$IFNDEF SUPPORTS_MONITOR}
  FTimeZoneCacheLock.Free;
{$ENDIF}
{$IFNDEF SUPPORTS_TSTRINGS_OWNSOBJECTS}
  FreeStringListObjects(FTimeZoneCache);
{$ENDIF}
  FTimeZoneCache.Free;

end.

