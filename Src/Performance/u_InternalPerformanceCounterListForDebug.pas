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

unit u_InternalPerformanceCounterListForDebug;

interface

uses
  SysUtils,
  ActiveX,
  i_IDList,
  i_InterfaceListSimple,
  i_InternalPerformanceCounter,
  i_InternalPerformanceCounterListForDebug;

type
  TInternalPerformanceCounterListForDebug = class(TInterfacedObject, IInternalPerformanceCounterListForDebug)
  private
    FName: string;
    FFactory: IInternalPerformanceCounterFactory;
    FList: IIDInterfaceList;
    FCS: IReadWriteSync;
  private
    function GetCounterByClass(AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
    procedure AddStaticDataToList(const AList: IInterfaceListSimple);
  public
    constructor Create(
      const AName: string;
      const AFactory: IInternalPerformanceCounterFactory
    );
  end;

implementation

uses
  u_IDInterfaceList,
  u_Synchronizer,
  u_InternalPerformanceCounterListForDebugOneClass;

{ TInternalPerformanceCounterListForDebug }

constructor TInternalPerformanceCounterListForDebug.Create(
  const AName: string;
  const AFactory: IInternalPerformanceCounterFactory
);
begin
  inherited Create;
  FName := AName;
  FFactory := AFactory;
  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FList := TIDInterfaceList.Create(False, 4000);
end;

function TInternalPerformanceCounterListForDebug.GetCounterByClass(
  AClass: TClass): IInternalPerformanceCounterListForDebugOneClass;
var
  VId: Integer;
begin
  VId := Integer(AClass);
  FCS.BeginRead;
  try
    Result := IInternalPerformanceCounterListForDebugOneClass(FList.GetByID(VId));
  finally
    FCS.EndRead;
  end;
  if Result = nil then begin
    FCS.BeginWrite;
    try
      Result := IInternalPerformanceCounterListForDebugOneClass(FList.GetByID(VId));
      if Result = nil then begin
        Result := TInternalPerformanceCounterListForDebugOneClass.Create(FName + '/' + AClass.ClassName, FFactory);
        FList.Add(VId, Result);
      end;
    finally
      FCS.EndWrite;
    end;
  end;
end;

procedure TInternalPerformanceCounterListForDebug.AddStaticDataToList(
  const AList: IInterfaceListSimple);
var
  VEnum: IEnumUnknown;
  VItem: IInternalPerformanceCounterListForDebugOneClass;
  VCnt: Integer;
begin
  VEnum := FList.GetEnumUnknown;
  while VEnum.Next(1, VItem, @VCnt) = S_OK do begin
    if Assigned(VItem) then begin
      AList.Add(VItem.CounterCreate.GetStaticData);
      AList.Add(VItem.CounterDestroy.GetStaticData);
    end;
  end;
end;

end.
