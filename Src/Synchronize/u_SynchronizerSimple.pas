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

unit u_SynchronizerSimple;

interface

uses
  i_ReadWriteSyncFactory,
  i_InternalPerformanceCounter,
  u_SynchronizerBase;

type
  TSynchronizerSimple = class(TSynchronizerBase)
  private
    function MakeWrappers(
      const ALockFactory: IReadWriteSyncFactory;
      const AClassName: string;
      const ASyncTypeName: string;
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncVariable(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncVariableRecursive(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncSymmetrical(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncSymmetricalRecursive(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncStd(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncStdRecursive(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncBig(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
    function MakeSyncBigRecursive(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    ): IReadWriteSyncFactory;
  public
    constructor Create(
      const APerfList: IInternalPerformanceCounterList;
      const AUseDebug: Boolean;
      const AUseDestroyCounters: Boolean;
      const AUseBeginEndCounters: Boolean
    );
  end;

implementation

uses
  SysUtils,
  u_ReadWriteSyncAbstract,
  u_ReadWriteSyncCriticalSection,
  u_ReadWriteSyncRtlResource,
  u_ReadWriteSyncSRW;

{ TSynchronizerSimple }

constructor TSynchronizerSimple.Create(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
);
begin
  inherited Create(
    MakeSyncVariable(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncVariableRecursive(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncSymmetrical(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncSymmetricalRecursive(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncStd(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncStdRecursive(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncBig(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters),
    MakeSyncBigRecursive(APerfList, AUseDebug, AUseDestroyCounters, AUseBeginEndCounters)
  );
end;

function TSynchronizerSimple.MakeSyncBig(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerRtlResourceFactory;
  VClassName := TSynchronizerRtlResource.ClassName;

  if Result = nil then begin
    Result := TSynchronizerMREWFactory.Create;
    VClassName := TMREWSync.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'Big',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncBigRecursive(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug,
  AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerRtlResourceFactory;
  VClassName := TSynchronizerRtlResource.ClassName;

  if Result = nil then begin
    Result := TSynchronizerMREWFactory.Create;
    VClassName := TMREWSync.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'BigRecursive',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncStd(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerSRWFactory;
  VClassName := TSynchronizerSRW.ClassName;

  if Result = nil then begin
    Result := MakeSynchronizerRtlResourceFactory;
    VClassName := TSynchronizerRtlResource.ClassName;
  end;

  if Result = nil then begin
    Result := TSynchronizerMREWFactory.Create;
    VClassName := TMREWSync.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'Std',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncStdRecursive(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerRtlResourceFactory;
  VClassName := TSynchronizerRtlResource.ClassName;

  if Result = nil then begin
    Result := TSynchronizerMREWFactory.Create;
    VClassName := TMREWSync.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'StdRecursive',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncSymmetrical(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerRtlResourceFactory;
  VClassName := TSynchronizerRtlResource.ClassName;

  if Result = nil then begin
    Result := MakeSynchronizerSRWFactory;
    VClassName := TSynchronizerSRW.ClassName;
  end;

  if Result = nil then begin
    Result := TSynchronizerMREWFactory.Create;
    VClassName := TMREWSync.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'Symmetrical',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncSymmetricalRecursive(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerRtlResourceFactory;
  VClassName := TSynchronizerRtlResource.ClassName;

  if Result = nil then begin
    Result := TSynchronizerMREWFactory.Create;
    VClassName := TMREWSync.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'SymmetricalRecursive',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncVariable(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := MakeSynchronizerSRWFactory;
  VClassName := TSynchronizerSRW.ClassName;

  if Result = nil then begin
    Result := TSynchronizerCSSCFactory.Create(4096);
    VClassName := TSynchronizerCSSC.ClassName;
  end;
  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'Variable',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeSyncVariableRecursive(
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug, AUseDestroyCounters, AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VClassName: string;
begin
  Result := TSynchronizerCSSCFactory.Create(4096);
  VClassName := TSynchronizerCSSC.ClassName;

  Result :=
    MakeWrappers(
      Result,
      VClassName,
      'VariableRecursive',
      APerfList,
      AUseDebug,
      AUseDestroyCounters,
      AUseBeginEndCounters
    );
end;

function TSynchronizerSimple.MakeWrappers(
  const ALockFactory: IReadWriteSyncFactory;
  const AClassName: string;
  const ASyncTypeName: string;
  const APerfList: IInternalPerformanceCounterList;
  const AUseDebug: Boolean;
  const AUseDestroyCounters: Boolean;
  const AUseBeginEndCounters: Boolean
): IReadWriteSyncFactory;
var
  VPerfList: IInternalPerformanceCounterList;
  VMakeCounterName: string;
  VBeginReadCounter: IInternalPerformanceCounter;
  VEndReadCounter: IInternalPerformanceCounter;
  VBeginWriteCounter: IInternalPerformanceCounter;
  VEndWriteCounter: IInternalPerformanceCounter;
  VDestroyCounter: IInternalPerformanceCounter;
begin
  Result := ALockFactory;
  Assert(ALockFactory <> nil);
  Assert(ASyncTypeName <> '');
  if AUseDebug then begin
    Assert(AClassName <> '');
  end;
  if AUseDestroyCounters or AUseBeginEndCounters then begin
    Assert(APerfList <> nil);
    VPerfList := APerfList.CreateAndAddNewSubList(ASyncTypeName + '_' + AClassName);
    VMakeCounterName := 'Create';
  end else begin
    VPerfList := APerfList;
    VMakeCounterName := ASyncTypeName + '_' + AClassName + '_Create';
  end;

  if AUseDebug and AUseBeginEndCounters then begin
    VBeginReadCounter := VPerfList.CreateAndAddNewCounter('BeginReadClear');
    VEndReadCounter := VPerfList.CreateAndAddNewCounter('EndReadClear');
    VBeginWriteCounter := VPerfList.CreateAndAddNewCounter('BeginWriteClear');
    VEndWriteCounter := VPerfList.CreateAndAddNewCounter('EndWriteClear');
    VDestroyCounter := nil;
    Result :=
      TSynchronizerFactoryWithCounters.Create(
        Result,
        VBeginReadCounter,
        VEndReadCounter,
        VBeginWriteCounter,
        VEndWriteCounter,
        VDestroyCounter
      );
  end;

  if AUseDebug then begin
    Result :=
      TSynchronizerFactoryWithDebug.Create(
        Result,
        AClassName
      );
  end;

  if AUseDestroyCounters or AUseBeginEndCounters then begin
    if AUseBeginEndCounters then begin
      if AUseDebug then begin
        VBeginReadCounter := VPerfList.CreateAndAddNewCounter('BeginReadDebug');
        VEndReadCounter := VPerfList.CreateAndAddNewCounter('EndReadDebug');
        VBeginWriteCounter := VPerfList.CreateAndAddNewCounter('BeginWriteDebug');
        VEndWriteCounter := VPerfList.CreateAndAddNewCounter('EndWriteDebug');
      end else begin
        VBeginReadCounter := VPerfList.CreateAndAddNewCounter('BeginRead');
        VEndReadCounter := VPerfList.CreateAndAddNewCounter('EndRead');
        VBeginWriteCounter := VPerfList.CreateAndAddNewCounter('BeginWrite');
        VEndWriteCounter := VPerfList.CreateAndAddNewCounter('EndWrite');
      end;
    end else begin
      VBeginReadCounter := nil;
      VEndReadCounter := nil;
      VBeginWriteCounter := nil;
      VEndWriteCounter := nil;
    end;
    if AUseDestroyCounters then begin
      VDestroyCounter := VPerfList.CreateAndAddNewCounter('Destroy');
    end else begin
      VDestroyCounter := nil;
    end;

    Result :=
      TSynchronizerFactoryWithCounters.Create(
        Result,
        VBeginReadCounter,
        VEndReadCounter,
        VBeginWriteCounter,
        VEndWriteCounter,
        VDestroyCounter
      );
  end;

  if VPerfList <> nil then begin
    Result :=
      TSynchronizerFactoryWithMakeCounter.Create(
        Result,
        VPerfList.CreateAndAddNewCounter(VMakeCounterName)
      );
  end;
end;

end.
