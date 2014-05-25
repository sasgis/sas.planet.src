program BenchmarkCmd;

{$APPTYPE CONSOLE}

uses
  SysUtils,
  i_BenchmarkResult in 'i_BenchmarkResult.pas',
  i_BenchmarkItem in 'i_BenchmarkItem.pas',
  i_BenchmarkItemList in 'i_BenchmarkItemList.pas',
  i_BenchmarkResultList in 'i_BenchmarkResultList.pas',
  i_BenchmarkResultListSaver in 'i_BenchmarkResultListSaver.pas',
  i_BenchmarkSystem in 'i_BenchmarkSystem.pas',
  i_BenchmarkTestRunner in 'i_BenchmarkTestRunner.pas',
  u_BenchmarkItemBase in 'u_BenchmarkItemBase.pas',
  u_BenchmarkItemEmpty in 'u_BenchmarkItemEmpty.pas',
  u_BenchmarkItemList in 'u_BenchmarkItemList.pas',
  u_BenchmarkTestRunner in 'u_BenchmarkTestRunner.pas',
  u_BenchmarkResult in 'u_BenchmarkResult.pas',
  u_BenchmarkResultList in 'u_BenchmarkResultList.pas',
  u_BenchmarkSystem in 'u_BenchmarkSystem.pas',
  u_BenchmarkItemIncSimple in 'u_BenchmarkItemIncSimple.pas',
  u_BenchmarkItemIncInterlocked in 'u_BenchmarkItemIncInterlocked.pas',
  u_BenchmarkItemSyncRead in 'u_BenchmarkItemSyncRead.pas',
  u_BenchmarkItemSyncWrite in 'u_BenchmarkItemSyncWrite.pas',
  u_BenchmarkItemTimer in 'u_BenchmarkItemTimer.pas',
  u_BenchmarkItemBitmap32BlockTransferFull in 'u_BenchmarkItemBitmap32BlockTransferFull.pas',
  u_BenchmarkItemBitmap32BlockTransferQuarter in 'u_BenchmarkItemBitmap32BlockTransferQuarter.pas',
  u_BenchmarkItemBitmap32FillRect in 'u_BenchmarkItemBitmap32FillRect.pas';

var
  GBenchmarkSystem: IBenchmarkSystem;
begin

  try
    GBenchmarkSystem := TBenchmarkSystem.Create(True);
    GBenchmarkSystem.RunTests;
  except
    on E:Exception do
      Writeln(E.Classname, ': ', E.Message);
  end;
  readln;
end.
