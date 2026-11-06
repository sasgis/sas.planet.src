unit u_TestFilter;

interface

uses
  Classes,
  TestFramework;

type
  // Selects the tests to run by case-insensitive substring masks.
  //
  // A mask is matched against the full test path ("SuiteName.MethodName"), so
  // both class names and method names can be used as masks. When a node
  // matches, its whole subtree is enabled; otherwise a suite is enabled only
  // if at least one of its children is enabled. The root node (which is named
  // after the executable) is never matched - the traversal starts with its
  // children.
  TSASTestFilter = class
  private
    FMasks: TStringList;
    FListOnly: Boolean;
    FHelpRequested: Boolean;
    FPause: Boolean;

    function MatchesAnyMask(const AText: string): Boolean;
    function Select(
      const ATest: ITest;
      const APath: string;
      const AParentMatched: Boolean
    ): Boolean;
    procedure ListEnabled(
      const ATest: ITest;
      const AIndent: string;
      const AList: TStrings
    );
  public
    constructor Create;
    destructor Destroy; override;

    // Parses ParamStr(1..ParamCount): '--list' switches to the listing mode,
    // '--pause' requests a key press before exiting, '-h' / '--help' / '/?'
    // request the usage text, every other non-empty argument is treated as
    // a mask.
    procedure ParseCommandLine;

    function HasMasks: Boolean;
    function MasksAsText: string;

    // Applies the masks to the test tree and returns the number of enabled
    // test cases. Without masks the tree is left untouched.
    // ARoot must not be nil.
    function Apply(const ARoot: ITest): Integer;

    // Applies the masks and fills AList with the enabled part of the test
    // tree: a suite name line followed by the indented method names.
    procedure CollectList(const ARoot: ITest; const AList: TStrings);

    property Masks: TStringList read FMasks;
    property ListOnly: Boolean read FListOnly;
    property HelpRequested: Boolean read FHelpRequested;
    property Pause: Boolean read FPause;
  end;

implementation

uses
  SysUtils,
  StrUtils;

{ TSASTestFilter }

constructor TSASTestFilter.Create;
begin
  inherited Create;
  FMasks := TStringList.Create;
end;

destructor TSASTestFilter.Destroy;
begin
  FMasks.Free;
  inherited Destroy;
end;

procedure TSASTestFilter.ParseCommandLine;
var
  I: Integer;
  VArg: string;
begin
  for I := 1 to ParamCount do begin
    VArg := Trim(ParamStr(I));
    if SameText(VArg, '--list') then begin
      FListOnly := True;
    end else if SameText(VArg, '--pause') then begin
      FPause := True;
    end else if MatchText(VArg, ['-h', '--help', '/?']) then begin
      FHelpRequested := True;
    end else if VArg <> '' then begin
      FMasks.Add(VArg);
    end;
  end;
end;

function TSASTestFilter.HasMasks: Boolean;
begin
  Result := FMasks.Count > 0;
end;

function TSASTestFilter.MasksAsText: string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to FMasks.Count - 1 do begin
    Result := Result + IfThen(I > 0, ' ') + '"' + FMasks[I] + '"';
  end;
end;

function TSASTestFilter.MatchesAnyMask(const AText: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FMasks.Count - 1 do begin
    if ContainsText(AText, FMasks[I]) then begin
      Result := True;
      Exit;
    end;
  end;
end;

// Enables ATest if it matches a mask, if its parent matched, or if any of its
// children ends up enabled; disables it otherwise. A leaf has no children, so
// it is enabled only by a match.
function TSASTestFilter.Select(
  const ATest: ITest;
  const APath: string;
  const AParentMatched: Boolean
): Boolean;
var
  I: Integer;
  VPath: string;
  VMatched: Boolean;
  VTests: IInterfaceList;
begin
  VPath := APath + ATest.Name;
  VMatched := AParentMatched or MatchesAnyMask(VPath);
  Result := VMatched;

  VTests := ATest.Tests;
  for I := 0 to VTests.Count - 1 do begin
    if Select(VTests[I] as ITest, VPath + '.', VMatched) then begin
      Result := True;
    end;
  end;

  ATest.Enabled := Result;
end;

function TSASTestFilter.Apply(const ARoot: ITest): Integer;
var
  I: Integer;
  VTests: IInterfaceList;
begin
  if HasMasks then begin
    VTests := ARoot.Tests;
    for I := 0 to VTests.Count - 1 do begin
      Select(VTests[I] as ITest, '', False);
    end;
  end;

  Result := ARoot.CountEnabledTestCases;
end;

procedure TSASTestFilter.ListEnabled(
  const ATest: ITest;
  const AIndent: string;
  const AList: TStrings
);
var
  I: Integer;
  VTests: IInterfaceList;
begin
  if not ATest.Enabled then begin
    Exit;
  end;

  AList.Add(AIndent + ATest.Name);

  VTests := ATest.Tests;
  for I := 0 to VTests.Count - 1 do begin
    ListEnabled(VTests[I] as ITest, AIndent + '  ', AList);
  end;
end;

procedure TSASTestFilter.CollectList(const ARoot: ITest; const AList: TStrings);
var
  I: Integer;
  VTests: IInterfaceList;
begin
  Apply(ARoot);

  VTests := ARoot.Tests;
  for I := 0 to VTests.Count - 1 do begin
    ListEnabled(VTests[I] as ITest, '', AList);
  end;
end;

end.
