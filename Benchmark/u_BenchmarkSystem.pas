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
  i_BenchmarkTestRunner,
  i_BenchmarkItemList,
  i_BenchmarkResultList,
  i_BenchmarkResultListSaver,
  i_BenchmarkSystem;

type
  TBenchmarkSystem = class(TInterfacedObject, IBenchmarkSystem)
  private
    FUseConsoleOutput: Boolean;
    FMinRunTime: Double;
    FRunCount: Integer;
    FTestRunner: IBenchmarkTestRunner;
    FResultListSaver: IBenchmarkResultListSaver;
    FBaseTestList: IBenchmarkItemList;
    FLastResults: IBenchmarkResultList;
  private
    procedure InitTestRunner;
    procedure InitTestList;
  private
    procedure RunTests;
  public
    constructor Create(
      ARunCount: Integer;
      AMinRunTime: Double;
      AUseConsoleOutput: Boolean
    );
  end;

implementation

uses
  Classes,
  SysUtils,
  GR32,
  i_InterfaceListSimple,
  i_Timer,
  i_HashFunctionImpl,
  i_BinaryData,
  i_ReadWriteSyncFactory,
  i_BenchmarkItem,
  u_InterfaceListSimple,
  u_TimerByNtQueryPerformanceCounter,
  u_TimerByQueryPerformanceCounter,
  u_TimerByGetTickCount,
  u_HashFunctionCityHash,
  u_HashFunctionCRC64,
  u_ReadWriteSyncAbstract,
  u_ReadWriteSyncSRW,
  u_ReadWriteSyncRtlResource,
  u_ReadWriteSyncCriticalSection,
  u_BenchmarkItemList,
  u_BenchmarkItemEmpty,
  u_BenchmarkItemIncSimple,
  u_BenchmarkItemIncInterlocked,
  u_BenchmarkItemSyncRead,
  u_BenchmarkItemSyncWrite,
  u_BenchmarkItemTimer,
  u_BenchmarkItemHashFunction,
  u_BenchmarkItemBitmap32BlockTransferFull,
  u_BenchmarkItemBitmap32BlockTransferQuarter,
  u_BenchmarkItemBitmap32FillRect,
  u_BenchmarkItemBitmap32LineVertical,
  u_BenchmarkItemBitmap32LineHorizontal,
  u_BenchmarkItemBitmap32Line,
  u_BenchmarkResultListSaverToCsv,
  u_BenchmarkTestRunner;

{ TBenchmarkSystem }

constructor TBenchmarkSystem.Create(
  ARunCount: Integer;
  AMinRunTime: Double;
  AUseConsoleOutput: Boolean
);
begin
  Assert(ARunCount > 0);
  Assert(AMinRunTime >= 0.001);
  inherited Create;
  FMinRunTime := AMinRunTime;
  FRunCount := ARunCount;
  FUseConsoleOutput := AUseConsoleOutput;
  InitTestRunner;
  InitTestList;
  FResultListSaver := TBenchmarkResultListSaverToCsv.Create;
end;

procedure TBenchmarkSystem.InitTestList;
var
  VList: IInterfaceListSimple;
  VItem: IBenchmarkItem;
  VSyncFactory: IReadWriteSyncFactory;
  VSync: IReadWriteSync;
  VTimer: ITimer;
  VHash: IHashFunctionImpl;
begin
  VList := TInterfaceListSimple.Create;

  VItem := TBenchmarkItemEmpty.Create;
  VList.Add(VItem);

  VItem := TBenchmarkItemIncSimple.Create;
  VList.Add(VItem);

  VItem := TBenchmarkItemIncInterlocked.Create;
  VList.Add(VItem);

  VSync := nil;
  VSyncFactory := MakeSynchronizerRtlResourceFactory;
  if Assigned(VSyncFactory) then begin
    VSync := VSyncFactory.Make('TestRead');
  end;

  VItem := TBenchmarkItemSyncRead.Create('RtlResource', VSync);
  VList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('RtlResource', VSync);
  VList.Add(VItem);

  VSync := nil;
  VSyncFactory := MakeSynchronizerSRWFactory;
  if Assigned(VSyncFactory) then begin
    VSync := VSyncFactory.Make('TestRead');
  end;

  VItem := TBenchmarkItemSyncRead.Create('SRW', VSync);
  VList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('SRW', VSync);
  VList.Add(VItem);

  VSyncFactory := TSynchronizerCSFactory.Create;
  VSync := VSyncFactory.Make('TestRead');

  VItem := TBenchmarkItemSyncRead.Create('CriticalSection', VSync);
  VList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('CriticalSection', VSync);
  VList.Add(VItem);

  VSyncFactory := TSynchronizerCSSCFactory.Create(4096);
  VSync := VSyncFactory.Make('TestRead');

  VItem := TBenchmarkItemSyncRead.Create('CriticalSection with spin lcok', VSync);
  VList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('CriticalSection with spin lcok', VSync);
  VList.Add(VItem);

  VSyncFactory := TSynchronizerMREWFactory.Create;
  VSync := VSyncFactory.Make('TestRead');

  VItem := TBenchmarkItemSyncRead.Create('MREW', VSync);
  VList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('MREW', VSync);
  VList.Add(VItem);

  VTimer := MakeTimerByQueryPerformanceCounter;
  VItem := TBenchmarkItemTimer.Create('QueryPerformanceCounter', VTimer);
  VList.Add(VItem);

  VTimer := MakeTimerByNtQueryPerformanceCounter;
  VItem := TBenchmarkItemTimer.Create('NtQueryPerformanceCounter', VTimer);
  VList.Add(VItem);

  VTimer := MakeTimerByGetTickCount;
  VItem := TBenchmarkItemTimer.Create('GetTickCount', VTimer);
  VList.Add(VItem);

  VHash := THashFunctionCityHash.Create;
  VItem := TBenchmarkItemHashFunction.Create('CityHash', 1, VHash);
  VList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 16, VHash);
  VList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 1024, VHash);
  VList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 65535, VHash);
  VList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 1024*1024, VHash);
  VList.Add(VItem);

  VHash := THashFunctionCRC64.Create;
  VItem := TBenchmarkItemHashFunction.Create('CRC64', 16, VHash);
  VList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CRC64', 65535, VHash);
  VList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CRC64', 1024*1024, VHash);
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32BlockTransferFull.Create(
      256,
      dmBlend,
      cmMerge,
      255
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32BlockTransferFull.Create(
      256,
      dmBlend,
      cmMerge,
      128
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32BlockTransferFull.Create(
      256,
      dmBlend,
      cmBlend,
      255
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32BlockTransferFull.Create(
      256,
      dmBlend,
      cmBlend,
      128
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32BlockTransferFull.Create(
      256,
      dmOpaque,
      cmMerge,
      255
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32BlockTransferQuarter.Create(
      256,
      dmBlend,
      cmMerge,
      255
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32FillRect.Create(
      256,
      True,
      False,
      cmMerge
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32FillRect.Create(
      256,
      False,
      True,
      cmMerge
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32FillRect.Create(
      256,
      True,
      True,
      cmMerge
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32LineVertical.Create(
      256,
      True,
      True,
      cmMerge
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32LineHorizontal.Create(
      256,
      True,
      True,
      cmMerge
    );
  VList.Add(VItem);

  VItem :=
    TBenchmarkItemBitmap32Line.Create(
      256,
      True,
      True,
      True,
      cmMerge
    );
  VList.Add(VItem);

  FBaseTestList := TBenchmarkItemList.Create(VList.MakeStaticAndClear);
end;

procedure TBenchmarkSystem.InitTestRunner;
var
  VTimer: ITimer;
  VMinTime: Double;
begin
  VMinTime := FMinRunTime;
  VTimer := MakeTimerByQueryPerformanceCounter;
  if not Assigned(VTimer) then begin
    if FUseConsoleOutput then
      Writeln('QueryPerformanceCounter does not work');
    VTimer := MakeTimerByNtQueryPerformanceCounter;
  end;
  if not Assigned(VTimer) then begin
    if FUseConsoleOutput then
      Writeln('NtQueryPerformanceCounter does not work');
    VTimer := MakeTimerByGetTickCount;
    if VMinTime < 0.1 then begin
      VMinTime := 0.1;
    end;
  end;

  FTestRunner := TBenchmarkTestRunner.Create(FUseConsoleOutput, VMinTime, VTimer);
end;

procedure TBenchmarkSystem.RunTests;
var
  VStream: TStream;
  VResults: IBinaryData;
  VFileName: string;
begin
  VFileName := 'Results_' + FormatDateTime('yyyymmdd_hhnn', Now) + '.csv';
  FLastResults := FTestRunner.RunTests(FBaseTestList, FRunCount);
  VResults := FResultListSaver.Save(FLastResults);
  VStream := TFileStream.Create(VFileName, fmCreate);
  try
    VStream.WriteBuffer(VResults.Buffer^, VResults.Size);
  finally
    VStream.Free;
  end;
end;

end.
