{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_BenchmarkTestRunner;

interface

uses
  i_Timer,
  i_BenchmarkItem,
  i_BenchmarkItemList,
  i_BenchmarkResult,
  i_BenchmarkResultList,
  i_BenchmarkTestRunner;

type
  TBenchmarkTestRunner = class(TInterfacedObject, IBenchmarkTestRunner)
  private
    FUseConsoleOutput: Boolean;
    FTimer: ITimer;
    FMinRunTimeInSec: Double;
  private
    function RunSingleTestSingleRun(
      const ATest: IBenchmarkItem
    ): Double;
    function RunSingleTest(
      const ATest: IBenchmarkItem;
      const ARunCount: Integer
    ): IBenchmarkResult;
  private
    function RunTests(
      const AList: IBenchmarkItemList;
      const ARunCount: Integer
    ): IBenchmarkResultList;
  public
    constructor Create(
      AUseConsoleOutput: Boolean;
      const AMinRunTimeInSec: Double;
      const ATimer: ITimer
    );
  end;

implementation

uses
  SysUtils,
  i_InterfaceListSimple,
  u_InterfaceListSimple,
  u_BenchmarkResult,
  u_BenchmarkResultList;

{ TBenchmarkTestRunner }

constructor TBenchmarkTestRunner.Create(
  AUseConsoleOutput: Boolean;
  const AMinRunTimeInSec: Double;
  const ATimer: ITimer
);
begin
  Assert(AMinRunTimeInSec > 0.0001);
  Assert(Assigned(ATimer));
  inherited Create;
  FUseConsoleOutput := AUseConsoleOutput;
  FMinRunTimeInSec := AMinRunTimeInSec;
  FTimer := ATimer;
end;

function TBenchmarkTestRunner.RunSingleTest(
  const ATest: IBenchmarkItem;
  const ARunCount: Integer
): IBenchmarkResult;
var
  i: Integer;
  VWarnUp: Double;
  VTime: Double;
  VResults: array of Double;
  VAvg: Double;
begin
  if not ATest.Enabled then begin
    if FUseConsoleOutput then
      Writeln('Test ', ATest.Name, ' disabled');
    Result := TBenchmarkResult.Create(ATest, 0, []);
    Exit;
  end;
  if FUseConsoleOutput then
    Writeln('Test ', ATest.Name);
  SetLength(VResults, ARunCount);
  ATest.SetUp;
  try
    VWarnUp := RunSingleTestSingleRun(ATest);
    if FUseConsoleOutput then
      Writeln(Format('Ignore warn up run %.7f seconds per step', [VWarnUp]));
    VAvg := 0;
    for i := 0 to ARunCount - 1 do begin
      VTime := RunSingleTestSingleRun(ATest);
      VResults[i] := VTime;
      VAvg := VAvg + VTime;
      if FUseConsoleOutput then
        Writeln(Format('Run %d %.7f seconds per step', [i, VTime]));
    end;
  finally
    ATest.TearDown;
  end;
  VAvg := VAvg / ARunCount;
  if FUseConsoleOutput then
    Writeln(Format('Averedge time %.7f seconds per step', [VAvg]));

  Result :=
    TBenchmarkResult.Create(
      ATest,
      VWarnUp,
      VResults
    );
end;

function TBenchmarkTestRunner.RunSingleTestSingleRun(
  const ATest: IBenchmarkItem
): Double;
var
  VStartTime: Int64;
  VMinEndTime: Int64;
  VCurrTime: Int64;
  VFrec: Int64;
  VCnt: Integer;
begin
  VFrec := FTimer.Freq;
  VMinEndTime := Trunc(VFrec * FMinRunTimeInSec);
  VCnt := 0;
  VStartTime := FTimer.CurrentTime;
  VMinEndTime := VMinEndTime + VStartTime;
  repeat
    ATest.RunOneStep;
    Inc(VCnt);
    VCurrTime := FTimer.CurrentTime;
  until VCurrTime >= VMinEndTime;
  Result := (VCurrTime - VStartTime) / (VFrec * VCnt);
end;

function TBenchmarkTestRunner.RunTests(
  const AList: IBenchmarkItemList;
  const ARunCount: Integer
): IBenchmarkResultList;
var
  VList: IInterfaceListSimple;
  i: Integer;
  VTestResult: IBenchmarkResult;
begin
  Assert(Assigned(AList));
  Assert(ARunCount > 0);
  VList := TInterfaceListSimple.Create;
  for i := 0 to AList.Count - 1 do begin
    VTestResult := RunSingleTest(AList.Items[i], ARunCount);
    VList.Add(VTestResult);
  end;
  Result := TBenchmarkResultList.Create(VList.MakeStaticAndClear);
end;

end.
