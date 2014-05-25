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

unit u_BenchmarkItemBase;

interface

uses
  i_BenchmarkItem;

type
  TBenchmarkItemBase = class(TInterfacedObject, IBenchmarkItem)
  private
    FEnabled: Boolean;
    FName: string;
    FCountOperationsPerStep: Integer;
  protected
    function GetEnabled: Boolean;
    function GetName: string;
    function GetCountOperationsPerStep: Integer;

    procedure SetUp; virtual;
    function RunOneStep: Integer; virtual; abstract;
    procedure TearDown; virtual;
  public
    constructor Create(
      const AEnabled: Boolean;
      const AName: string;
      const ACountOperationsPerStep: Integer
    );
  end;

implementation

{ TBenchmarkItemBase }

constructor TBenchmarkItemBase.Create(
  const AEnabled: Boolean;
  const AName: string;
  const ACountOperationsPerStep: Integer
);
begin
  Assert(ACountOperationsPerStep > 0);
  inherited Create;
  FEnabled := AEnabled;
  FName := AName;
  FCountOperationsPerStep := ACountOperationsPerStep;
end;

function TBenchmarkItemBase.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TBenchmarkItemBase.GetCountOperationsPerStep: Integer;
begin
  Result := FCountOperationsPerStep;
end;

function TBenchmarkItemBase.GetName: string;
begin
  Result := FName;
end;

procedure TBenchmarkItemBase.SetUp;
begin
  // Do nothing
end;

procedure TBenchmarkItemBase.TearDown;
begin
  // Do nothing
end;

end.
