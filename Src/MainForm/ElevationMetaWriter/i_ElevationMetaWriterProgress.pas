{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit i_ElevationMetaWriterProgress;

interface

type
  TElevationMetaWriterProgressStatus = (
    emwIdle,
    emwBusy,
    emwDone,
    emwCanceled
  );

  TElevationMetaWriterProgressInfo = record
    TotalCount: Integer;
    ReadyCount: Integer;
  end;

  IElevationMetaWriterProgress = interface
    ['{603F97E1-7932-4F8F-9D23-27278F5AB67B}']
    procedure Reset;

    function GetInfo: TElevationMetaWriterProgressInfo;
    procedure SetInfo(const AValue: TElevationMetaWriterProgressInfo);
    property Info: TElevationMetaWriterProgressInfo read GetInfo write SetInfo;

    function GetStatus: TElevationMetaWriterProgressStatus;
    procedure SetStatus(const AValue: TElevationMetaWriterProgressStatus);
    property Status: TElevationMetaWriterProgressStatus read GetStatus write SetStatus;
  end;

implementation

end.
