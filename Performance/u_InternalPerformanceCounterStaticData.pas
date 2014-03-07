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

unit u_InternalPerformanceCounterStaticData;

interface

uses
  i_InternalPerformanceCounter;

type
  TInternalPerformanceCounterStaticData = class(TInterfacedObject, IInternalPerformanceCounterStaticData)
  private
    FId: Integer;
    FName: string;
    FCounter: Cardinal;
    FTotalTime: TDateTime;
    FCounterInMain: Cardinal;
    FTotalTimeInMain: TDateTime;
    FMaxTime: TDateTime;
    FMinTime: TDateTime;
  private
    function GetId: Integer;
    function GetName: string;
    function GetCounter: Cardinal;
    function GetTotalTime: TDateTime;
    function GetCounterInMain: Cardinal;
    function GetTotalTimeInMain: TDateTime;
    function GetMaxTime: TDateTime;
    function GetMinTime: TDateTime;
  public
    constructor Create(
      AId: Integer;
      const AName: string;
      ACounter: Cardinal;
      const ATotalTime: TDateTime;
      const ACounterInMain: Cardinal;
      const ATotalTimeInMain: TDateTime;
      const AMaxTime: TDateTime;
      const AMinTime: TDateTime
    );
  end;

implementation

{ TInternalPerformanceCounterStaticData }

constructor TInternalPerformanceCounterStaticData.Create(
  AId: Integer;
  const AName: string;
  ACounter: Cardinal;
  const ATotalTime: TDateTime;
  const ACounterInMain: Cardinal;
  const ATotalTimeInMain: TDateTime;
  const AMaxTime: TDateTime;
  const AMinTime: TDateTime
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
end;

function TInternalPerformanceCounterStaticData.GetCounter: Cardinal;
begin
  Result := FCounter;
end;

function TInternalPerformanceCounterStaticData.GetCounterInMain: Cardinal;
begin
  Result := FCounterInMain;
end;

function TInternalPerformanceCounterStaticData.GetId: Integer;
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

end.
