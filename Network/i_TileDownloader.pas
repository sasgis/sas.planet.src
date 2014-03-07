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

unit i_TileDownloader;

interface

uses
  i_NotifierOperation,
  i_TileRequest,
  i_TileRequestResult,
  i_TileRequestTask;

type
  ITileDownloader = interface
    ['{79AB7B90-1F22-4B2E-B14A-BBAD3F94E26C}']
    function Download(
      const ASoftCancelNotifier: INotifierOneOperation;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ATileRequest: ITileRequest
    ): ITileRequestResult;
  end;

  ITileDownloaderAsync = interface
    ['{93075321-B145-459A-9347-1F81EA73C177}']
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );
  end;

implementation

end.
