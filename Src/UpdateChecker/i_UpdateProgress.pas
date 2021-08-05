{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2021, SAS.Planet development team.                      *}
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

unit i_UpdateProgress;

interface

uses
  t_UpdateChecker,
  i_NotifierOperation;

type
  TUpdateProgressStatus = (
    psBusy,
    psFinished,
    psCanceled
  );

  IUpdateProgress = interface
    ['{74411BA1-D449-4482-8E20-017ED3287F5E}']
    procedure Reset;

    function GetCancelNotifier: INotifierOperation;
    property CancelNotifier: INotifierOperation read GetCancelNotifier;

    function GetCurrentOperationID: Integer;
    property CurrentOperationID: Integer read GetCurrentOperationID;
  end;

  IUpdateCheckerProgress = interface(IUpdateProgress)
    ['{70D2873D-5227-410D-936A-4406C363B683}']
    procedure SetResult(
      const AOperationID: Integer;
      const AResult: TUpdateCheckerResult
    );

    function GetResult(
      const AOperationID: Integer;
      out AResult: TUpdateCheckerResult
    ): TUpdateProgressStatus;
  end;

  TUpdateDownloaderResult = record
    IsFinished: Boolean;
    IsError: Boolean;

    BytesDownloaded: Integer;
    BytesTotal: Integer;

    Text: string;
  end;

  IUpdateDownloaderProgress = interface(IUpdateProgress)
    ['{A3C2D499-A3FA-4458-BF73-44DC7B2FE7C3}']
    procedure SetResult(
      const AOperationID: Integer;
      const AResult: TUpdateDownloaderResult
    );

    function GetResult(
      const AOperationID: Integer;
      out AResult: TUpdateDownloaderResult
    ): TUpdateProgressStatus;
  end;

implementation

end.
