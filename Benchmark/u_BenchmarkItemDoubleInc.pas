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

unit u_BenchmarkItemDoubleInc;

interface

uses
  u_BenchmarkItemBase;

type
  TBenchmarkItemDoubleInc = class(TBenchmarkItemBase)
  private
    FData: Double;
  protected
    function RunOneStep: Integer; override;
  public
    constructor Create;
  end;

implementation

const CRepeatCount = 1000;

{ TBenchmarkItemDoubleInc }

constructor TBenchmarkItemDoubleInc.Create;
begin
  inherited Create(True, 'Double Increment', CRepeatCount);
end;

function TBenchmarkItemDoubleInc.RunOneStep: Integer;
var
  i: Integer;
begin
  for i := 0 to CRepeatCount - 1 do begin
    FData := FData + 1;
  end;
  Result := Trunc(FData);
end;

end.
