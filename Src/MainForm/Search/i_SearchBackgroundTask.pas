{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit i_SearchBackgroundTask;

interface

uses
  i_GeoCoder,
  i_NotifierOperation;

type
  TSearchTaskData = record
    Text: string;
    GeoCoder: IGeoCoder;
    OperationID: Integer;
    CancelNotifier: INotifierOperation;
  end;
  PSearchTaskData = ^TSearchTaskData;

  TOnSearchTaskResult = procedure(
    const ATaskData: PSearchTaskData;
    const AGeoCodeResult: IGeoCodeResult
  ) of object;

  ISearchBackgroundTask = interface
    ['{2F99C571-E84A-4313-8C53-37C515BB1802}']
    procedure Run(
      const ATaskData: PSearchTaskData;
      const AOnTaskResult: TOnSearchTaskResult
    );
  end;

implementation

end.
