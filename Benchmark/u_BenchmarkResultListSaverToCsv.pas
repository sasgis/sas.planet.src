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

unit u_BenchmarkResultListSaverToCsv;

interface

uses
  SysUtils,
  i_BinaryData,
  i_BenchmarkResult,
  i_BenchmarkResultList,
  i_BenchmarkResultListSaver;

type
  TBenchmarkResultListSaverToCsv = class(TInterfacedObject, IBenchmarkResultListSaver)
  private
    FHeader: string;
    FFormat: TFormatSettings;
  private
    procedure PrepareStatistic(
      const AItem: IBenchmarkResult;
      out AMin: Double;
      out AMax: Double;
      out AMedian: Double;
      out AMean: Double;
      out AStdev: Double
    );
    function PrepareLineBySingleResult(const AItem: IBenchmarkResult): string;
  private
    function Save(const AResultList: IBenchmarkResultList): IBinaryData;
  public
    constructor Create;
  end;

implementation

uses
  Classes,
  u_SortFunc,
  u_BinaryData;

{ TBenchmarkResultListSaverToCsv }

constructor TBenchmarkResultListSaverToCsv.Create;
begin
  inherited Create;
  FHeader := 'Name';
  FHeader := FHeader + ',MedianTimePerStep';
  FHeader := FHeader + ',MinTimePerStep';
  FHeader := FHeader + ',MaxTimePerStep';
  FHeader := FHeader + ',MeanTimePerStep';
  FHeader := FHeader + ',StdevTimePerStep';
  FHeader := FHeader + ',CountOperationsPerStep';
  FHeader := FHeader + ',RunCount';
  FFormat.DecimalSeparator := '.';
end;

function TBenchmarkResultListSaverToCsv.PrepareLineBySingleResult(
  const AItem: IBenchmarkResult
): string;
var
  VResult: string;
  VMin: Double;
  VMax: Double;
  VMedian: Double;
  VMean: Double;
  VStdev: Double;
const
  CSecondsToMicroSeconds: Double = 1000000;
begin
  Assert(Assigned(AItem));
  VResult := AItem.BenchmarkItem.Name;
  if AItem.BenchmarkItem.Enabled then begin
    PrepareStatistic(AItem, VMin, VMax, VMedian, VMean, VStdev);
    VResult := VResult + ',' + FloatToStr(VMedian * CSecondsToMicroSeconds, FFormat);
    VResult := VResult + ',' + FloatToStr(VMin * CSecondsToMicroSeconds, FFormat);
    VResult := VResult + ',' + FloatToStr(VMax * CSecondsToMicroSeconds, FFormat);
    VResult := VResult + ',' + FloatToStr(VMean * CSecondsToMicroSeconds, FFormat);
    VResult := VResult + ',' + FloatToStr(VStdev * CSecondsToMicroSeconds, FFormat);
    VResult := VResult + ',' + IntToStr(AItem.BenchmarkItem.CountOperationsPerStep);
    VResult := VResult + ',' + IntToStr(AItem.RunCount);
  end else begin
    VResult := VResult + 'Disabled';
  end;
  Result := VResult;
end;

procedure TBenchmarkResultListSaverToCsv.PrepareStatistic(
  const AItem: IBenchmarkResult;
  out AMin: Double;
  out AMax: Double;
  out AMedian: Double;
  out AMean: Double;
  out AStdev: Double
);
var
  VData: array of Double;
  i: Integer;
  VRunCount: Integer;
  VSquareSum: Double;
  VDelta: Double;
  VVariance: Double;
begin
  VRunCount := AItem.RunCount;
  SetLength(VData, VRunCount);
  for i := 0 to VRunCount - 1 do begin
    VData[i] := AItem.RunResultTimePerStep[i];
  end;
  SortDoubleArray(VData);
  AMin := VData[0];
  AMax := VData[VRunCount - 1];
  AMedian := VData[VRunCount div 2];

  VSquareSum := 0;
  AMean := 0;
  for i := 0 to VRunCount - 1 do begin
    VDelta := VData[i] - AMean;
    AMean := AMean + VDelta / (i + 1);
    VSquareSum := VSquareSum + VDelta * (VData[i] - AMean);
  end;
  if VRunCount <= 1 then begin
    VVariance := 0;
  end else begin
    VVariance := VSquareSum / (VRunCount - 1);
  end;
  AStdev := Sqrt(VVariance);
end;

function TBenchmarkResultListSaverToCsv.Save(
  const AResultList: IBenchmarkResultList
): IBinaryData;
var
  i:  Integer;
  VItem: IBenchmarkResult;
  VResult: string;
  VLine: string;
begin
  Assert(Assigned(AResultList));
  VResult := FHeader + #13#10;
  for i := 0 to AResultList.Count - 1 do begin
    VItem := AResultList.Items[i];
    VLine := PrepareLineBySingleResult(VItem);
    VResult := VResult + VLine + #13#10;;
  end;
  Result := TBinaryData.CreateByString(VResult);
end;

end.
