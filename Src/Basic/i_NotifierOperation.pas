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

unit i_NotifierOperation;

interface

uses
  i_Notifier,
  i_Listener;

type
  INotifierOperation = interface
    ['{96D3C3D0-7B07-4F63-AE3D-6E32516AE56B}']
    function GetCurrentOperation: Integer;
    property CurrentOperation: Integer read GetCurrentOperation;

    function IsOperationCanceled(AID: Integer): Boolean;

    procedure AddListener(const AListener: IListener);
    procedure RemoveListener(const AListener: IListener);
  end;

  INotifierOperationInternal = interface(INotifierOperation)
    procedure NextOperation(const AMsg: IInterface = nil);
  end;

  INotifierOneOperation = interface(INotifier)
    ['{EA058BC8-6764-412B-93A5-F1AB4032C38F}']
    function GetIsExecuted: Boolean;
    property IsExecuted: Boolean read GetIsExecuted;

    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  end;

  INotifierOneOperationInternal = interface(INotifierOneOperation)
    procedure ExecuteOperation(const AMsg: IInterface = nil);
  end;

implementation

end.
