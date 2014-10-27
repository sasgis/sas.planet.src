// Licence: MIT
// Sources: https://github.com/zedxxx/delphi-argparse

unit ArgumentParser;

interface

{$IF CompilerVersion >= 25}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  {$IF CompilerVersion >= 23}
  System.Classes,
  System.Contnrs,
  System.SysUtils,
  System.StrUtils;
  {$ELSE}
  Classes,
  Contnrs,
  SysUtils,
  StrUtils;
  {$IFEND}

type
  ENoMatchArgument = class(Exception);
  EInvalidArgument = class(Exception);
  EParameterMissing = class(Exception);
  ENoSuchArgument = class(Exception);

  TStoreAction = (saBool, saStore);

  TStringListContainer = class(TStringList)
  private
    function GetValue(const Key: String): String;
    procedure SetValue(const Key, Value: String);
  public
    property Values[const Key: String]: String read GetValue write SetValue; default;
    function Add(const Key, Value: String): Integer; reintroduce;
    function ContainsKey(const Key: String): Boolean;
  end;

  TParseResult = class
  private
    FStoredValues: TStringListContainer;
    FStoredBools: TStringList;
    FUnnamedValues: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    function HasArgument(const Dest: String): Boolean;
    function GetValue(const Dest: String): String;
    procedure StoreBool(const Dest: String);
    procedure StoreValue(const Dest: String; const Value: String);
    property StoredBools: TStringList read FStoredBools;
    property StoredValues: TStringListContainer read FStoredValues;
    property Args: TStringList read FUnnamedValues;
  end;

  TArgument = class
  private
    FOption: String;
    FDest: String;
    FStoreAction: TStoreAction;
  public
    constructor Create(const Option, Dest: String; StoreAction: TStoreAction);
    destructor Destroy; override;
    property Option: String read FOption;
    property Dest: String read FDest;
    property StoreAction: TStoreAction read FStoreAction;
  end;

  TArgumentParser = class
  private
    FDescription: String;
    FArguments: TObjectList;
    function GetArgumentsCount: Integer;
  public
    constructor Create(const Description: String = '');
    destructor Destroy; override;
    function ParseArgs: TParseResult; overload;
    function ParseArgs(const TargetArgs: TStringList): TParseResult; overload;
    procedure AddArgument(const Argument: TArgument); overload;
    procedure AddArgument(const Option, Dest: String;
        StoreAction: TStoreAction = saBool); overload;
    procedure AddArgument(const Option: String;
        StoreAction: TStoreAction = saBool); overload;
    function HasArgument(const Option: String;
        StoreAction: TStoreAction = saBool): Boolean;
    function GetArgument(const Option: String): TArgument; overload;
    function GetArgument(Index: Integer): TArgument; overload;
    property Description: String read FDescription write FDescription;
    property ArgumentsCount: Integer read GetArgumentsCount;
    property Arguments[Index: Integer]: TArgument read GetArgument;
  end;

function GetParamStrAsList(IsIncludingAppName: Boolean = True): TStringList;

implementation

{ TStringListContainer }

function TStringListContainer.GetValue(const Key: String): String;
begin
  Result := inherited Values[Key];
end;

procedure TStringListContainer.SetValue(const Key, Value: String);
begin
  if (Values[Key] <> '') and (Value <> '') then
    inherited Values[Key] := Values[Key] + ' ' + Value
  else
    inherited Values[Key] := Value;
end;

function TStringListContainer.Add(const Key, Value: String): Integer;
begin
  Result := inherited Add(Key);
  SetValue(Key, Value);
end;

function TStringListContainer.ContainsKey(const Key: String): Boolean;
begin
  Result := Values[Key] <> '';
end;

(* TParseResult *)
constructor TParseResult.Create;
begin
  FStoredValues := TStringListContainer.Create;
  FStoredBools := TStringList.Create;
  FUnnamedValues := TStringList.Create;
end;

destructor TParseResult.Destroy;
begin
  FStoredValues.Free;
  FStoredBools.Free;
  FUnnamedValues.Free;
  inherited Destroy;
end;

function TParseResult.HasArgument(const Dest: String): Boolean;
begin
  Result := (FStoredBools.IndexOf(Dest) <> -1) or FStoredValues.ContainsKey(Dest);
end;

function TParseResult.GetValue(const Dest: String): String;
begin
  if HasArgument(Dest) then
    Result := FStoredValues[Dest]
  else
    raise ENoSuchArgument.CreateFmt('No such argument "%s"', [Dest]);
end;

procedure TParseResult.StoreBool(const Dest: String);
begin
  if not HasArgument(Dest) then
    FStoredBools.Add(Dest);
end;

procedure TParseResult.StoreValue(const Dest: String; const Value: String);
begin
  if not HasArgument(Dest) then
    FStoredValues.Add(Dest, Value);
end;
(* End of TParseResult *)

(* TArgument *)
constructor TArgument.Create(const Option, Dest: String; StoreAction: TStoreAction);
begin
  FOption := Option;
  FDest := Dest;
  FStoreAction := StoreAction;
end;

destructor TArgument.Destroy;
begin
  inherited Destroy;
end;
(* End of TArgument *)

(* TArgumentParser *)
constructor TArgumentParser.Create(const Description: String = '');
begin
  FDescription := Description;
  FArguments := TObjectList.Create;
end;

destructor TArgumentParser.Destroy;
begin
  FArguments.Free;
  inherited Destroy;
end;

function TArgumentParser.ParseArgs: TParseResult;
var
  Params: TStringList;
begin
  Params := GetParamStrAsList(False);
  try
    Result := ParseArgs(Params);
  finally
    Params.Free;
  end;
end;

function TArgumentParser.ParseArgs(const TargetArgs: TStringList): TParseResult;
var
  CurrentIndex: Integer;
  CurrentParam: String;
  SeparatorPosition: Integer;
  Key, Value: String;
  Argument: TArgument;
begin
  Result := TParseResult.Create;
  CurrentIndex := 0;
  while CurrentIndex < TargetArgs.Count do
  begin
    CurrentParam := TargetArgs[CurrentIndex];
    if LeftStr(CurrentParam, 1) = '-' then
    begin
      SeparatorPosition := Pos('=', CurrentParam);
      if SeparatorPosition <> 0 then
      begin
        Key := LeftStr(CurrentParam, SeparatorPosition - 1);
        Value := RightStr(CurrentParam, Length(CurrentParam) - SeparatorPosition);
        if HasArgument(Key, saStore) then
        begin
          Argument := GetArgument(Key);
          Result.StoreValue(Argument.Dest, Value);
        end
        else
          raise EInvalidArgument.CreateFmt('Invalid argument "%s"', [Key]);
      end
      else
      begin
        Key := CurrentParam;
        if HasArgument(Key, saBool) then
        begin
          Argument := GetArgument(Key);
          Result.StoreBool(Argument.Dest);
        end
        else
          if HasArgument(Key, saStore) then
          begin
            Inc(CurrentIndex);
            if CurrentIndex >= TargetArgs.Count then
              raise EParameterMissing.CreateFmt('Missing value for "%s"', [Key]);
            Value := TargetArgs[CurrentIndex];
            Argument := GetArgument(Key);
            Result.StoreValue(Argument.Dest, Value);
          end
          else
            raise EInvalidArgument.CreateFmt('Invalid argument "%s"', [Key]);
      end;
    end
    else
    begin
      Value := TargetArgs[CurrentIndex];
      Result.Args.Add(Value);
    end;
    Inc(CurrentIndex);
  end;
end;

procedure TArgumentParser.AddArgument(const Argument: TArgument);
begin
  FArguments.Add(Argument);
end;

procedure TArgumentParser.AddArgument(const Option, Dest: String;
    StoreAction: TStoreAction = saBool);
var
  Argument: TArgument;
begin
  Argument := TArgument.Create(Option, Dest, StoreAction);
  AddArgument(Argument);
end;

procedure TArgumentParser.AddArgument(const Option: String;
    StoreAction: TStoreAction = saBool);
var
  Dest: String;
begin
  if LeftStr(Option, 2) = '--' then
    Dest := Copy(Option, 3, Length(Option) - 2)
  else if LeftStr(Option, 1) = '-' then
    Dest := Copy(Option, 2, Length(Option) - 1)
  else
    Dest := Option;
  AddArgument(Option, Dest, StoreAction);
end;

function TArgumentParser.HasArgument(const Option: String;
    StoreAction: TStoreAction = saBool): Boolean;
var
  I: Integer;
  Argument: TArgument;
begin
  for I := 0 to ArgumentsCount - 1 do begin
    Argument := Arguments[I];
    if (Argument.Option = Option) and (Argument.StoreAction = StoreAction) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

function TArgumentParser.GetArgument(const Option: String): TArgument;
var
  I: Integer;
  Argument: TArgument;
begin
  for I := 0 to ArgumentsCount - 1 do begin
    Argument := Arguments[I];
    if Argument.Option = Option then
    begin
      Result := Argument;
      Exit;
    end;
  end;
  raise ENoMatchArgument.CreateFmt('No such argument "%s"', [Option]);
end;

function TArgumentParser.GetArgumentsCount: Integer;
begin
  Result := FArguments.Count;
end;

function TArgumentParser.GetArgument(Index: Integer): TArgument;
begin
  Result := TArgument(FArguments[Index]);
end;

(* End of TArgumentParser *)

(* Utility function *)
function GetParamStrAsList(IsIncludingAppName: Boolean = True): TStringList;
var
  I: Integer;
  StartIndex: Integer;
begin
  Result := TStringList.Create;
  if IsIncludingAppName then
    StartIndex := 0
  else
    StartIndex := 1;
  for I := StartIndex to ParamCount do
    Result.Add(ParamStr(I));
end;
(* End of Utility function *)

end.
