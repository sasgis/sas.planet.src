{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_DebugInfoSubSystem;

interface

uses
  SysUtils,
  i_InternalPerformanceCounter,
  i_DebugInfoSubSystem,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_InternalDebugConfig;

type
  TDebugInfoSubSystem = class(TInterfacedObject, IDebugInfoSubSystem)
  private
    FRootCounterList: IInternalPerformanceCounterList;
    FListCS: IReadWriteSync;
    FList: IInterfaceListSimple;
  private
    function GetRootCounterList: IInternalPerformanceCounterList;
    function GetStaticDataList: IInterfaceListStatic;
  public
    constructor Create(const AConfig: IInternalDebugConfig);
  end;

implementation

uses
  i_Timer,
  u_TimerByQueryPerformanceCounter,
  u_BaseInterfacedObject,
  u_BaseInterfacedObjectDebug,
  u_InternalPerformanceCounter,
  u_InternalPerformanceCounterFake,
  u_InternalPerformanceCounterList,
  u_InterfaceListSimple,
  u_Synchronizer;

{ TDebugInfoSubSystem }

constructor TDebugInfoSubSystem.Create(const AConfig: IInternalDebugConfig);
var
  VTimer: ITimer;
  VFactory: IInternalPerformanceCounterFactory;
begin
  inherited Create;
  FListCS := GSync.SyncVariable.Make(Self.ClassName);
  if AConfig.IsShowDebugInfo then begin
    VTimer := MakeTimerByQueryPerformanceCounter;
    if Assigned(VTimer) then begin
      VFactory := TInternalPerformanceCounterFactory.Create(VTimer);
      FList := TInterfaceListSimple.Create;
      FList.Capacity := 1000;
      FRootCounterList := TInternalPerformanceCounterList.Create('', FListCS, FList, VFactory);
    end;
  end;
  if not Assigned(FRootCounterList) then begin
    FRootCounterList := TInternalPerformanceCounterFake.Create;
  end;
end;

function TDebugInfoSubSystem.GetRootCounterList: IInternalPerformanceCounterList;
begin
  Result := FRootCounterList;
end;

function TDebugInfoSubSystem.GetStaticDataList: IInterfaceListStatic;
var
  I: Integer;
  VList: IInterfaceListSimple;
  VItem: IInternalPerformanceCounter;
begin
  Result := nil;
  if Assigned(FList) then begin
    VList := TInterfaceListSimple.Create;
    VList.Capacity := FList.Count;
    FListCS.BeginRead;
    try
      for I := 0 to FList.Count - 1 do begin
        VItem := IInternalPerformanceCounter(FList[I]);
        VList.Add(VItem.GetStaticData);
      end;
    finally
      FListCS.EndRead;
    end;
    if TBaseInterfacedObject = TBaseInterfacedObjectDebug then begin
      TBaseInterfacedObjectDebug.AddStaticDataToList(VList);
    end;
    Result := VList.MakeStaticAndClear;
  end;
end;

end.
