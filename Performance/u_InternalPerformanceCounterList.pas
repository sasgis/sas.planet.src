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
    FName: string;
    FFactory: IInternalPerformanceCounterFactory;
    FList: IInterfaceList;
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
    constructor Create(
      const AName: string;
      const AFactory: IInternalPerformanceCounterFactory
    );
  end;

implementation

uses
  SysUtils,
  u_EnumUnknown,
  u_IDInterfaceList;

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

constructor TInternalPerformanceCounterList.Create(
  const AName: string;
  const AFactory: IInternalPerformanceCounterFactory
);
begin
  inherited Create;
  FName := AName;
  FFactory := AFactory;
  FList := TInterfaceList.Create;
end;

function TInternalPerformanceCounterList.CreateAndAddNewCounter(
  const AName: string): IInternalPerformanceCounter;
begin
  Result := FFactory.Build(AName);
  FList.Add(Result);
end;

function TInternalPerformanceCounterList.CreateAndAddNewSubList(
  const AName: string): IInternalPerformanceCounterList;
begin
  Result := TInternalPerformanceCounterList.Create(AName, FFactory);
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

end.
