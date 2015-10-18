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
  i_InterfaceListSimple,
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
    procedure AddSync(const AList: IInterfaceListSimple);
    procedure AddTimer(const AList: IInterfaceListSimple);
    procedure AddHash(const AList: IInterfaceListSimple);
    procedure AddGr32(const AList: IInterfaceListSimple);
    procedure AddProjectionType(const AList: IInterfaceListSimple);
    procedure AddDoublePoint(const AList: IInterfaceListSimple);
    procedure AddBasic(const AList: IInterfaceListSimple);
    procedure AddCalcTileInPolygon(const AList: IInterfaceListSimple);
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
  t_GeoTypes,
  c_CoordConverter,
  i_Timer,
  i_HashFunctionImpl,
  i_BinaryData,
  i_ReadWriteSyncFactory,
  i_Datum,
  i_ProjectionType,
  i_ProjectionTypeFactory,
  i_DatumFactory,
  i_GeometryProjected,
  i_GeometryProjectedFactory,
  i_Projection,
  i_HashFunction,
  i_DoublePointsAggregator,
  i_BenchmarkItem,
  u_InterfaceListSimple,
  u_TimerByNtQueryPerformanceCounter,
  u_TimerByQueryPerformanceCounter,
  u_TimerByGetTickCount,
  u_HashFunctionCityHash,
  u_HashFunctionCRC64,
  u_HashFunctionWithCounter,
  u_Datum,
  u_DatumFactory,
  u_GeometryProjectedFactory,
  u_InternalPerformanceCounterFake,
  u_ProjectionTypeMercatorOnSphere,
  u_ProjectionTypeMercatorOnEllipsoid,
  u_ProjectionTypeGELonLat,
  u_ProjectionTypeFactorySimple,
  u_ProjectionBasic256x256,
  u_DoublePointsAggregator,
  u_GeoFunc,
  u_ReadWriteSyncAbstract,
  u_ReadWriteSyncSRW,
  u_ReadWriteSyncRtlResource,
  u_ReadWriteSyncCriticalSection,
  u_BenchmarkItemList,
  u_BenchmarkItemEmpty,
  u_BenchmarkItemIncSimple,
  u_BenchmarkItemDoubleInc,
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
  u_BenchmarkItemProjectionType,
  u_BenchmarkItemCalcTilesInPolygon,
  u_BenchmarkItemDoublePointIncrement,
  u_BenchmarkItemDoublePointIncrementWithEmpty,
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

procedure TBenchmarkSystem.AddBasic(const AList: IInterfaceListSimple);
var
  VItem: IBenchmarkItem;
begin
  VItem := TBenchmarkItemEmpty.Create;
  AList.Add(VItem);

  VItem := TBenchmarkItemIncSimple.Create;
  AList.Add(VItem);

  VItem := TBenchmarkItemIncInterlocked.Create;
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddCalcTileInPolygon(
  const AList: IInterfaceListSimple
);
var
  VItem: IBenchmarkItem;
  VHashFunction: IHashFunction;
  VProjectionTypeFactory: IProjectionTypeFactory;
  VProjectionType: IProjectionType;
  VDatumFactory: IDatumFactory;
  VFactory: IGeometryProjectedFactory;
  VProjection: IProjection;
  VPolygonBuilder: IGeometryProjectedPolygonBuilder;
  VPoints: IDoublePointsAggregator;
  VPolygon: IGeometryProjectedPolygon;
  VLevel: Byte;
  VName: String;
  VMultiplier: Double;
  function PreparePolygon(const AMultiplier: Double): IGeometryProjectedPolygon;
  begin
    VPoints.Add(DoublePoint(0, 0));
    VPoints.Add(DoublePoint(AMultiplier*9.9, 0));
    VPoints.Add(DoublePoint(AMultiplier*9.9, AMultiplier*49.9));
    VPoints.Add(DoublePoint(AMultiplier*3, AMultiplier*10));
    VPoints.Add(DoublePoint(0, AMultiplier*49.9));
    VPolygonBuilder.AddOuter(DoubleRect(0, 0, AMultiplier*9.9, AMultiplier*49.9), VPoints.MakeStaticAndClear);

    VPoints.Add(DoublePoint(AMultiplier*0.9, AMultiplier*0.9));
    VPoints.Add(DoublePoint(AMultiplier*2.1, AMultiplier*0.9));
    VPoints.Add(DoublePoint(AMultiplier*2.1, AMultiplier*2.1));
    VPoints.Add(DoublePoint(AMultiplier*0.9, AMultiplier*2.1));
    VPolygonBuilder.AddHole(DoubleRect(AMultiplier*0.9, AMultiplier*0.9, AMultiplier*2.1, AMultiplier*2.1), VPoints.MakeStaticAndClear);

    VPoints.Add(DoublePoint(AMultiplier*20.1, AMultiplier*20.1));
    VPoints.Add(DoublePoint(AMultiplier*20.9, AMultiplier*20.1));
    VPoints.Add(DoublePoint(AMultiplier*20.9, AMultiplier*20.9));
    VPoints.Add(DoublePoint(AMultiplier*20.1, AMultiplier*20.9));
    VPolygonBuilder.AddOuter(DoubleRect(AMultiplier*20.1, AMultiplier*20.1, AMultiplier*20.9, AMultiplier*20.9), VPoints.MakeStaticAndClear);

    Result := VPolygonBuilder.MakeStaticAndClear;
  end;
begin
  VHashFunction :=
    THashFunctionWithCounter.Create(
      THashFunctionCityHash.Create,
      TInternalPerformanceCounterFake.Create
    );
  VDatumFactory := TDatumFactory.Create(VHashFunction);
  VProjectionTypeFactory :=
    TProjectionTypeFactorySimple.Create(
      VHashFunction,
      VDatumFactory
    );
  VProjectionType := VProjectionTypeFactory.GetByCode(CGoogleProjectionEPSG);
  VProjection := TProjectionBasic256x256.Create(0, VProjectionType, 20);
  VFactory := TGeometryProjectedFactory.Create;
  VPolygonBuilder := VFactory.MakePolygonBuilder;
  VPoints := TDoublePointsAggregator.Create;

  VLevel := 0;
  VMultiplier := (1 shl 8) shl VLevel;
  VPolygon := PreparePolygon(VMultiplier);
  VName := 'CalcTileInPolygon ' + IntToStr(1 shl (2 * VLevel));
  VItem := TBenchmarkItemCalcTilesInPolygon.Create(VName, VProjection, VPolygon);
  AList.Add(VItem);

  VLevel := 1;
  VMultiplier := (1 shl 8) shl VLevel;
  VPolygon := PreparePolygon(VMultiplier);
  VName := 'CalcTileInPolygon ' + IntToStr(1 shl (2 * VLevel));
  VItem := TBenchmarkItemCalcTilesInPolygon.Create(VName, VProjection, VPolygon);
  AList.Add(VItem);

  VLevel := 2;
  VMultiplier := (1 shl 8) shl VLevel;
  VPolygon := PreparePolygon(VMultiplier);
  VName := 'CalcTileInPolygon ' + IntToStr(1 shl (2 * VLevel));
  VItem := TBenchmarkItemCalcTilesInPolygon.Create(VName, VProjection, VPolygon);
  AList.Add(VItem);

  VLevel := 3;
  VMultiplier := (1 shl 8) shl VLevel;
  VPolygon := PreparePolygon(VMultiplier);
  VName := 'CalcTileInPolygon ' + IntToStr(1 shl (2 * VLevel));
  VItem := TBenchmarkItemCalcTilesInPolygon.Create(VName, VProjection, VPolygon);
  AList.Add(VItem);

  VLevel := 4;
  VMultiplier := (1 shl 8) shl VLevel;
  VPolygon := PreparePolygon(VMultiplier);
  VName := 'CalcTileInPolygon ' + IntToStr(1 shl (2 * VLevel));
  VItem := TBenchmarkItemCalcTilesInPolygon.Create(VName, VProjection, VPolygon);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddDoublePoint(const AList: IInterfaceListSimple);
var
  VItem: IBenchmarkItem;
begin
  VItem := TBenchmarkItemDoubleInc.Create;
  AList.Add(VItem);

  VItem := TBenchmarkItemDoublePointIncrement.Create;
  AList.Add(VItem);

  VItem := TBenchmarkItemDoublePointIncrementInplace.Create;
  AList.Add(VItem);

  VItem := TBenchmarkItemDoublePointIncrementWithEmpty.Create(10);
  AList.Add(VItem);

  VItem := TBenchmarkItemDoublePointIncrementWithEmpty.Create(100);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddProjectionType(const AList: IInterfaceListSimple);
var
  VItem: IBenchmarkItem;
  VDatum: IDatum;
  VProjectionType: IProjectionType;
begin
  VDatum := TDatum.Create(0, 0, 6378137);
  VProjectionType := TProjectionTypeMercatorOnSphere.Create(0, VDatum, 0);

  VItem := TBenchmarkItemProjectionTypeForvard.Create('MercatorOnSphere', VProjectionType);
  AList.Add(VItem);

  VItem := TBenchmarkItemProjectionTypeBackvard.Create('MercatorOnSphere', VProjectionType);
  AList.Add(VItem);

  VDatum := TDatum.Create(0, 0, 6378137, 6356752);
  VProjectionType := TProjectionTypeMercatorOnEllipsoid.Create(0, VDatum, 0);

  VItem := TBenchmarkItemProjectionTypeForvard.Create('MercatorOnEllipsoid', VProjectionType);
  AList.Add(VItem);

  VItem := TBenchmarkItemProjectionTypeBackvard.Create('MercatorOnEllipsoid', VProjectionType);
  AList.Add(VItem);

  VDatum := TDatum.Create(0, 0, 6378137, 6356752);
  VProjectionType := TProjectionTypeGELonLat.Create(0, VDatum, 0);

  VItem := TBenchmarkItemProjectionTypeForvard.Create('SimpleLonLat', VProjectionType);
  AList.Add(VItem);
  
  VItem := TBenchmarkItemProjectionTypeBackvard.Create('SimpleLonLat', VProjectionType);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddGr32(const AList: IInterfaceListSimple);
var
  VItem: IBenchmarkItem;
begin
  VItem := TBenchmarkItemBitmap32BlockTransferFull.Create(256, dmBlend, cmMerge, 255);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32BlockTransferFull.Create(256, dmBlend, cmMerge, 128);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32BlockTransferFull.Create(256, dmBlend, cmBlend, 255);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32BlockTransferFull.Create(256, dmBlend, cmBlend, 128);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32BlockTransferFull.Create(256, dmOpaque, cmMerge, 255);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32BlockTransferQuarter.Create(256, dmBlend, cmMerge, 255);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32FillRect.Create(256, True, False, cmMerge);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32FillRect.Create(256, False, True, cmMerge);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32FillRect.Create(256, True, True, cmMerge);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32LineVertical.Create(256, True, True, cmMerge);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32LineHorizontal.Create(256, True, True, cmMerge);
  AList.Add(VItem);

  VItem := TBenchmarkItemBitmap32Line.Create(256, True, True, True, cmMerge);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddHash(const AList: IInterfaceListSimple);
var
  VHash: IHashFunctionImpl;
  VItem: IBenchmarkItem;
begin
  VHash := THashFunctionCityHash.Create;
  VItem := TBenchmarkItemHashFunction.Create('CityHash', 1, VHash);
  AList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 16, VHash);
  AList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 1024, VHash);
  AList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 65535, VHash);
  AList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CityHash', 1024 * 1024, VHash);
  AList.Add(VItem);

  VHash := THashFunctionCRC64.Create;
  VItem := TBenchmarkItemHashFunction.Create('CRC64', 16, VHash);
  AList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CRC64', 65535, VHash);
  AList.Add(VItem);

  VItem := TBenchmarkItemHashFunction.Create('CRC64', 1024 * 1024, VHash);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddTimer(const AList: IInterfaceListSimple);
var
  VTimer: ITimer;
  VItem: IBenchmarkItem;
begin
  VTimer := MakeTimerByQueryPerformanceCounter;
  VItem := TBenchmarkItemTimer.Create('QueryPerformanceCounter', VTimer);
  AList.Add(VItem);

  VTimer := MakeTimerByNtQueryPerformanceCounter;
  VItem := TBenchmarkItemTimer.Create('NtQueryPerformanceCounter', VTimer);
  AList.Add(VItem);

  VTimer := MakeTimerByGetTickCount;
  VItem := TBenchmarkItemTimer.Create('GetTickCount', VTimer);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.AddSync(const AList: IInterfaceListSimple);
var
  VSync: IReadWriteSync;
  VSyncFactory: IReadWriteSyncFactory;
  VItem: IBenchmarkItem;
begin
  VSync := nil;
  VSyncFactory := MakeSynchronizerRtlResourceFactory;
  if Assigned(VSyncFactory) then begin
    VSync := VSyncFactory.Make('TestRead');
  end;

  VItem := TBenchmarkItemSyncRead.Create('RtlResource', VSync);
  AList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('RtlResource', VSync);
  AList.Add(VItem);

  VSync := nil;
  VSyncFactory := MakeSynchronizerSRWFactory;
  if Assigned(VSyncFactory) then begin
    VSync := VSyncFactory.Make('TestRead');
  end;

  VItem := TBenchmarkItemSyncRead.Create('SRW', VSync);
  AList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('SRW', VSync);
  AList.Add(VItem);

  VSyncFactory := TSynchronizerCSFactory.Create;
  VSync := VSyncFactory.Make('TestRead');

  VItem := TBenchmarkItemSyncRead.Create('CriticalSection', VSync);
  AList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('CriticalSection', VSync);
  AList.Add(VItem);

  VSyncFactory := TSynchronizerCSSCFactory.Create(4096);
  VSync := VSyncFactory.Make('TestRead');

  VItem := TBenchmarkItemSyncRead.Create('CriticalSection with spin lcok', VSync);
  AList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('CriticalSection with spin lcok', VSync);
  AList.Add(VItem);

  VSyncFactory := TSynchronizerMREWFactory.Create;
  VSync := VSyncFactory.Make('TestRead');

  VItem := TBenchmarkItemSyncRead.Create('MREW', VSync);
  AList.Add(VItem);

  VItem := TBenchmarkItemSyncWrite.Create('MREW', VSync);
  AList.Add(VItem);
end;

procedure TBenchmarkSystem.InitTestList;
var
  VList: IInterfaceListSimple;
begin
  VList := TInterfaceListSimple.Create;

  AddBasic(VList);
  AddSync(VList);
  AddTimer(VList);
  AddHash(VList);
  AddGr32(VList);
  AddProjectionType(VList);
  AddDoublePoint(VList);
  AddCalcTileInPolygon(VList);

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
