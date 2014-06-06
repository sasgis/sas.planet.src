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

unit u_BenchmarkItemCoordConverter;

interface

uses
  i_CoordConverter,
  u_BenchmarkItemBase;

type
  TBenchmarkItemCoordConverterForvard = class(TBenchmarkItemBase)
  private
    FCoordConverter: ICoordConverter;
    FX: array of Double;
    FY: array of Double;
  protected
    procedure SetUp; override;
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const ACoordConverterName: string;
      const ACoordConverter: ICoordConverter
    );
  end;

  TBenchmarkItemCoordConverterBackvard = class(TBenchmarkItemBase)
  private
    FCoordConverter: ICoordConverter;
    FX: array of Double;
    FY: array of Double;
  protected
    procedure SetUp; override;
    function RunOneStep: Integer; override;
  public
    constructor Create(
      const ACoordConverterName: string;
      const ACoordConverter: ICoordConverter
    );
  end;

implementation

uses
  t_GeoTypes;

const
  CItemCount = 100;

{ TBenchmarkItemCoordConverterForvard }

constructor TBenchmarkItemCoordConverterForvard.Create(
  const ACoordConverterName: string;
  const ACoordConverter: ICoordConverter
);
begin
  inherited Create(
    Assigned(ACoordConverter),
    'CoordConverter LlToRel ' + ACoordConverterName,
    CItemCount * CItemCount * CItemCount
  );
  FCoordConverter := ACoordConverter;
end;

function TBenchmarkItemCoordConverterForvard.RunOneStep: Integer;
var
  i: Integer;
  j: Integer;
  VPoint: TDoublePoint;
  VResult: TDoublePoint;
begin
  Result := 0;
  for i := 0 to CItemCount - 1 do begin
    VPoint.X := FX[i];
    for j := 0 to CItemCount - 1 do begin
      VPoint.Y := FY[j];
      VResult := FCoordConverter.LonLat2Relative(VPoint);
      Inc(Result);
    end;
  end;
end;

procedure TBenchmarkItemCoordConverterForvard.SetUp;
var
  i: Integer;
const
  CDelta: Double = 160 / CItemCount;
begin
  inherited;
  SetLength(FX, CItemCount);
  SetLength(FY, CItemCount);
  for i := 0 to CItemCount - 1 do begin
    FX[i] := 79 - i * CDelta;
  end;
  for i := 0 to CItemCount - 1 do begin
    FY[i] := 81 - i * CDelta;
  end;
end;

{ TBenchmarkItemCoordConverterBackvard }

constructor TBenchmarkItemCoordConverterBackvard.Create(
  const ACoordConverterName: string;
  const ACoordConverter: ICoordConverter
);
begin
  inherited Create(
    Assigned(ACoordConverter),
    'CoordConverter RelToLl ' + ACoordConverterName,
    CItemCount * CItemCount * CItemCount
  );
  FCoordConverter := ACoordConverter;
end;

function TBenchmarkItemCoordConverterBackvard.RunOneStep: Integer;
var
  i: Integer;
  j: Integer;
  VPoint: TDoublePoint;
  VResult: TDoublePoint;
begin
  Result := 0;
  for i := 0 to CItemCount - 1 do begin
    VPoint.X := FX[i];
    for j := 0 to CItemCount - 1 do begin
      VPoint.Y := FY[j];
      VResult := FCoordConverter.Relative2LonLat(VPoint);
      Inc(Result);
    end;
  end;
end;

procedure TBenchmarkItemCoordConverterBackvard.SetUp;
var
  i: Integer;
const
  CDelta: Double = 0.99 / CItemCount;
begin
  inherited;
  SetLength(FX, CItemCount);
  SetLength(FY, CItemCount);
  for i := 0 to CItemCount - 1 do begin
    FX[i] := i * CDelta;
    FY[i] := FX[i];
  end;
end;

end.
