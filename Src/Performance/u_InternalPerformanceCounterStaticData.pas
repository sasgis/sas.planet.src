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

unit u_InternalPerformanceCounterStaticData;

interface

uses
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterStaticData = class(TInterfacedObject, IInternalPerformanceCounterStaticData)
  private
    FId: NativeInt;
    FName: string;
    FCounter: Cardinal;
    FTotalTime: TDateTime;
    FCounterInMain: Cardinal;
    FTotalTimeInMain: TDateTime;
    FMaxTime: TDateTime;
    FMinTime: TDateTime;
    FLastTime: TDateTime;
  private
    function GetId: NativeInt;
    function GetName: string;
    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetCounterInMain: Cardinal;
    function GetTotalTimeInMain: TDateTime;
    function GetMaxTime: TDateTime;
    function GetMinTime: TDateTime;
    function GetLastTime: TDateTime;
  public
    constructor Create(
      const AId: NativeInt;
      const AName: string;
      const ACounter: Cardinal;
      const ATotalTime: TDateTime;
      const ACounterInMain: Cardinal;
      const ATotalTimeInMain: TDateTime;
      const AMaxTime: TDateTime;
      const AMinTime: TDateTime;
      const ALastTime: TDateTime
    );
  end;

implementation

{ TInternalPerformanceCounterStaticData }

constructor TInternalPerformanceCounterStaticData.Create(
  const AId: NativeInt;
  const AName: string;
  const ACounter: Cardinal;
  const ATotalTime: TDateTime;
  const ACounterInMain: Cardinal;
  const ATotalTimeInMain: TDateTime;
  const AMaxTime: TDateTime;
  const AMinTime: TDateTime;
  const ALastTime: TDateTime
);
begin
  inherited Create;
  FId := AId;
  FName := AName;
  FCounter := ACounter;
  FTotalTime := ATotalTime;
  FCounterInMain := ACounterInMain;
  FTotalTimeInMain := ATotalTimeInMain;
  FMaxTime := AMaxTime;
  FMinTime := AMinTime;
  FLastTime := ALastTime;
end;

function TInternalPerformanceCounterStaticData.GetCounter: Cardinal;
begin
  Result := FCounter;
end;

function TInternalPerformanceCounterStaticData.GetCounterInMain: Cardinal;
begin
  Result := FCounterInMain;
end;

function TInternalPerformanceCounterStaticData.GetId: NativeInt;
begin
  Result := FId;
end;

function TInternalPerformanceCounterStaticData.GetName: string;
begin
  Result := FName;
end;

function TInternalPerformanceCounterStaticData.GetTotalTime: TDateTime;
begin
  Result := FTotalTime;
end;

function TInternalPerformanceCounterStaticData.GetTotalTimeInMain: TDateTime;
begin
  Result := FTotalTimeInMain;
end;

function TInternalPerformanceCounterStaticData.GetMaxTime: TDateTime;
begin
  Result := FMaxTime;
end;

function TInternalPerformanceCounterStaticData.GetMinTime: TDateTime;
begin
  Result := FMinTime;
end;

function TInternalPerformanceCounterStaticData.GetLastTime: TDateTime;
begin
  Result := FLastTime;
end;

end.
