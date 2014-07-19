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

unit u_BenchmarkItemDoublePointBaseTest;

interface

uses
  t_GeoTypes,
  u_BenchmarkItemBase;

type
  TBenchmarkItemDoublePointBaseTest = class(TBenchmarkItemBase)
  private
    FGenRect: TDoubleRect;
  protected
    FCount: Integer;
    FPoints: array of TDoublePoint;
    FDst: array of TDoublePoint;
  protected
    procedure SetUp; override;
  public
    constructor Create(
      const AEnabled: Boolean;
      const AName: string;
      const ACount: Integer;
      const AGenRect: TDoubleRect
    );
  end;

implementation

{ TBenchmarkItemDoublePointBaseTest }

constructor TBenchmarkItemDoublePointBaseTest.Create(
  const AEnabled: Boolean;
  const AName: string;
  const ACount: Integer;
  const AGenRect: TDoubleRect
);
begin
  Assert(ACount > 0);
  Assert(AGenRect.Left < AGenRect.Right);
  Assert(AGenRect.Top < AGenRect.Bottom);
  inherited Create(
    AEnabled,
    AName,
    ACount
  );
  FCount := ACount;
  FGenRect := AGenRect;
end;

procedure TBenchmarkItemDoublePointBaseTest.SetUp;
var
  i: Integer;
  VDelta: TDoublePoint;
begin
  inherited;
  RandSeed := 1000;
  SetLength(FPoints, FCount);
  SetLength(FDst, FCount);
  VDelta.X := FGenRect.Right - FGenRect.Left;
  VDelta.Y := FGenRect.Bottom - FGenRect.Top;
  for i := 0 to FCount - 1 do begin
    FPoints[i].X := Random * VDelta.X + FGenRect.Left;
    FPoints[i].Y := Random * VDelta.Y + FGenRect.Top;
  end;
end;

end.
