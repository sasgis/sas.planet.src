{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_InternalPerformanceCounterList;

interface

uses
  Windows,
  Classes,
  ActiveX,
  i_IDList,
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterList = class(TInterfacedObject, IInternalPerformanceCounterList)
  private
    FList: IInterfaceList;
    FNtQPC: Pointer;
    FName: string;
    procedure AppendStaticListByCounterList(
      const AResultList: IIDInterfaceList;
      const ACounterList: IInternalPerformanceCounterList
    );
  private
    function GetName: string;

    function GetStaticDataList: IIDInterfaceList;
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
    procedure AddSubList(const ASubList: IInternalPerformanceCounterList);
  public
    constructor Create(const AName: string);
  end;

  TInternalPerformanceCounterFake = class(TInterfacedObject, IInternalPerformanceCounterList, IInternalPerformanceCounter)
  private
    { IInternalPerformanceCounterList }
    function GetName: string;
    function GetStaticDataList: IIDInterfaceList;
    function GetEunm: IEnumUnknown;
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
    procedure AddSubList(const ASubList: IInternalPerformanceCounterList);
  private
    { IInternalPerformanceCounter }
    function GetId: Integer;
    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);
    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetMaxTime: TDateTime;
    function GetMinTime: TDateTime;
    function GetStaticData: IInternalPerformanceCounterStaticData;
  end;

implementation

uses
  SysUtils,
  u_QueryPerfCounter,
  u_EnumUnknown,
  u_IDInterfaceList,
  u_InternalPerformanceCounter;

{ TInternalPerformanceCounterList }

procedure TInternalPerformanceCounterList.AppendStaticListByCounterList(
  const AResultList: IIDInterfaceList;
  const ACounterList: IInternalPerformanceCounterList
);
var
  VEnum: IEnumUnknown;
  Vcnt: Integer;
  VUnknown: IInterface;
  VCounter: IInternalPerformanceCounter;
  VList: IInternalPerformanceCounterList;
begin
  VEnum := ACounterList.GetEunm;
  while VEnum.Next(1, VUnknown, @Vcnt) = S_OK do begin
    if Supports(VUnknown, IInternalPerformanceCounter, VCounter) then begin
      AResultList.Add(VCounter.Id, VCounter.GetStaticData);
    end else if Supports(VUnknown, IInternalPerformanceCounterList, VList) then begin
      AppendStaticListByCounterList(AResultList, VList);
    end;
  end;
end;

constructor TInternalPerformanceCounterList.Create(const AName: string);
begin
  inherited Create;
  FList := TInterfaceList.Create;
  FName := AName;
  FNtQPC := NtQueryPerformanceCounterPtr;
end;

function TInternalPerformanceCounterList.CreateAndAddNewCounter(
  const AName: string): IInternalPerformanceCounter;
begin
  Result := TInternalPerformanceCounter.Create(AName, FNtQPC);
  FList.Add(Result);
end;

function TInternalPerformanceCounterList.CreateAndAddNewSubList(
  const AName: string): IInternalPerformanceCounterList;
begin
  Result := TInternalPerformanceCounterList.Create(AName);
  FList.Add(Result);
end;

procedure TInternalPerformanceCounterList.AddSubList(const ASubList: IInternalPerformanceCounterList);
begin
  Assert(ASubList <> nil);
  FList.Add(ASubList);
end;

function TInternalPerformanceCounterList.GetEunm: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TInternalPerformanceCounterList.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounterList.GetStaticDataList: IIDInterfaceList;
begin
  Result := TIDInterfaceList.Create;
  AppendStaticListByCounterList(Result, Self);
end;

{ TInternalPerformanceCounterFake }

function TInternalPerformanceCounterFake.CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
begin
  Result := Self;
end;

function TInternalPerformanceCounterFake.CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
begin
  Result := Self;
end;

procedure TInternalPerformanceCounterFake.AddSubList(
  const ASubList: IInternalPerformanceCounterList
);
begin
  // empty
end;

procedure TInternalPerformanceCounterFake.FinishOperation(const AContext: TInternalPerformanceCounterContext);
begin
  // empty
end;

function TInternalPerformanceCounterFake.GetCounter: Cardinal;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.GetEunm: IEnumUnknown;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.GetId: Integer;
begin
  Result := Integer(Self);
end;

function TInternalPerformanceCounterFake.GetName: string;
begin
  Result := '';
end;

function TInternalPerformanceCounterFake.GetStaticData: IInternalPerformanceCounterStaticData;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.GetStaticDataList: IIDInterfaceList;
begin
  Result := nil;
end;

function TInternalPerformanceCounterFake.GetTotalTime: TDateTime;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.GetMaxTime: TDateTime;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.GetMinTime: TDateTime;
begin
  Result := 0;
end;

function TInternalPerformanceCounterFake.StartOperation: TInternalPerformanceCounterContext;
begin
  Result := 0;
end;

end.
