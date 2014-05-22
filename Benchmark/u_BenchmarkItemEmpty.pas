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

unit u_BenchmarkItemEmpty;

interface

uses
  i_BenchmarkItem;

type
  TBenchmarkItemEmpty = class(TInterfacedObject, IBenchmarkItem)
  private
    function GetName: string;
    function GetMultiplier: Integer;

    procedure SetUp;
    function RunOneStep: Integer;
    procedure TearDown;
  public
    constructor Create;
  end;

implementation

{ TBenchmarkItemEmpty }

constructor TBenchmarkItemEmpty.Create;
begin
  inherited Create;
end;

function TBenchmarkItemEmpty.GetMultiplier: Integer;
begin
  Result := 1;
end;

function TBenchmarkItemEmpty.GetName: string;
begin
  Result := 'Empty test';
end;

function TBenchmarkItemEmpty.RunOneStep: Integer;
begin
  // Do nothing
  Result := 0;
end;

procedure TBenchmarkItemEmpty.SetUp;
begin
  // Do nothing
end;

procedure TBenchmarkItemEmpty.TearDown;
begin
  // Do nothing
end;

end.
