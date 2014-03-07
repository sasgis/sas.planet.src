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

unit i_RegionProcessProgressInfo;

interface

uses
  i_NotifierOperation;

type
  IProgressInfoBase = interface
    ['{B636F3D1-3F17-4BE5-8D8E-E161F945E42D}']
    function GetProcessedRatio: Double;
    property ProcessedRatio: Double read GetProcessedRatio;

    function GetFinished: Boolean;
    property Finished: Boolean read GetFinished;
  end;

  IProgressInfoInternalBase = interface
    ['{D5D89EE7-AAB5-485D-B497-30E1DE9EBBFC}']
    function GetCancelNotifier: INotifierOperation;
    property CancelNotifier: INotifierOperation read GetCancelNotifier;

    function GetOperationID: Integer;
    property OperationID: Integer read GetOperationID;

    procedure SetProcessedRatio(const AValue: Double);
    procedure Finish;
  end;

  IRegionProcessProgressInfo = interface(IProgressInfoBase)
    ['{58559CEF-9233-4E25-87E0-F88E1A78C5AD}']
    function GetCaption: string;
    property Caption: string read GetCaption;

    function GetFirstLine: string;
    property FirstLine: string read GetFirstLine;

    function GetSecondLine: string;
    property SecondLine: string read GetSecondLine;
  end;

  IRegionProcessProgressInfoInternal = interface(IProgressInfoInternalBase)
    ['{7E22954C-EF2D-4D5B-BBDE-8B6346D3C1B0}']
    procedure SetCaption(const AValue: string);
    procedure SetFirstLine(const AValue: string);
    procedure SetSecondLine(const AValue: string);
  end;

implementation

end.
