{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BenchmarkSystem;

interface

uses
  i_Timer,
  i_BenchmarkTestRunner,
  i_BenchmarkItemList,
  i_BenchmarkResultList,
  i_BenchmarkSystem;

type
  TBenchmarkSystem = class(TInterfacedObject, IBenchmarkSystem)
  private
    FTestRunner: IBenchmarkTestRunner;
    FBaseTestList: IBenchmarkItemList;
    FLastResults: IBenchmarkResultList;
  private
    procedure InitTestList;
  private
    procedure RunTests;
  public
    constructor Create(
      const ATimer: ITimer
    );
  end;

implementation

uses
  i_InterfaceListSimple,
  i_BenchmarkItem,
  u_InterfaceListSimple,
  u_BenchmarkItemList,
  u_BenchmarkItemEmpty,
  u_BenchmarkTestRunner;

{ TBenchmarkSystem }

constructor TBenchmarkSystem.Create(const ATimer: ITimer);
begin
  Assert(Assigned(ATimer));
  inherited Create;
  FTestRunner := TBenchmarkTestRunner.Create(ATimer);
  InitTestList;
end;

procedure TBenchmarkSystem.InitTestList;
var
  VList: IInterfaceListSimple;
  VItem: IBenchmarkItem;
begin
  VList := TInterfaceListSimple.Create;
  VItem := TBenchmarkItemEmpty.Create;
  VList.Add(VItem);
  FBaseTestList := TBenchmarkItemList.Create(VList.MakeStaticAndClear);
end;

procedure TBenchmarkSystem.RunTests;
begin
  FLastResults := FTestRunner.RunTests(FBaseTestList, 10);
end;

end.
