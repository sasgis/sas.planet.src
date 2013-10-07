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

unit i_InternalPerformanceCounter;

interface

type
  TInternalPerformanceCounterContext = Int64;

  IInternalPerformanceCounterStaticData = interface
    ['{64BD3E69-4EAB-41CE-8A87-8324FE9E81D2}']
    function GetId: Integer;
    property Id: Integer read GetId;

    function GetName: string;
    property Name: string read GetName;

    function GetCounter: Cardinal;
    property Counter: Cardinal read GetCounter;

    function GetTotalTime: TDateTime;
    property TotalTime: TDateTime read GetTotalTime;

    function GetMaxTime: TDateTime;
    property MaxTime: TDateTime read GetMaxTime;

    function GetMinTime: TDateTime;
    property MinTime: TDateTime read GetMinTime;
  end;

  IInternalPerformanceCounter = interface
    ['{2D5EE758-A5EA-467D-A679-C3CD1B116973}']
    function StartOperation: TInternalPerformanceCounterContext;
    procedure FinishOperation(const AContext: TInternalPerformanceCounterContext);

    function GetStaticData: IInternalPerformanceCounterStaticData;
  end;

  IInternalPerformanceCounterFactory = interface
    ['{D87A5792-5568-4A46-A693-160FB95945E4}']
    function Build(const AName: string): IInternalPerformanceCounter;
  end;

  IInternalPerformanceCounterList = interface
    ['{75567269-AD8D-443F-AA45-9336C9890719}']
    function CreateAndAddNewCounter(const AName: string): IInternalPerformanceCounter;
    function CreateAndAddNewSubList(const AName: string): IInternalPerformanceCounterList;
  end;

implementation

end.
