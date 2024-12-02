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

unit u_UpdateChecker;

interface

uses
  t_UpdateChecker,
  i_InetConfig,
  i_Downloader,
  i_DownloaderFactory,
  i_DownloadResult,
  i_DownloadRequest,
  i_NotifierOperation;

type
  TUpdateChecker = class
  private
    FOnCheckResult: TOnUpdateCheckerResult;
    FUpdateSource: TUpdateSource;
    FUpdateChannel: TUpdateChannel;
    FInetConfig: IInetConfig;
    FCancelNotifier: INotifierOperation;

    FDownloader: IDownloaderAsync;

    procedure OnRequestAsyncCallBack(
      const AResult: IDownloadResult;
      const AOperationID: Integer
    );
  public
    procedure Perform(const AOperationID: Integer);
  public
    constructor Create(
      const AUpdateSource: TUpdateSource;
      const AUpdateChannel: TUpdateChannel;
      const AOnCheckResult: TOnUpdateCheckerResult;
      const ADownloaderFactory: IDownloaderFactory;
      const AInetConfig: IInetConfig;
      const ACancelNotifier: INotifierOperation
    );
  end;

implementation

uses
  SysUtils,
  u_DownloadRequest,
  u_UpdateCheckerFunc;

{ TUpdateChecker }

constructor TUpdateChecker.Create(
  const AUpdateSource: TUpdateSource;
  const AUpdateChannel: TUpdateChannel;
  const AOnCheckResult: TOnUpdateCheckerResult;
  const ADownloaderFactory: IDownloaderFactory;
  const AInetConfig: IInetConfig;
  const ACancelNotifier: INotifierOperation
);
begin
  inherited Create;

  FOnCheckResult := AOnCheckResult;
  FUpdateSource := AUpdateSource;
  FUpdateChannel := AUpdateChannel;
  FInetConfig := AInetConfig;
  FCancelNotifier := ACancelNotifier;

  FDownloader := ADownloaderFactory.BuildDownloaderAsync(True, True, False, nil);
end;

function GetRequestUrl(
  const AUpdateSource: TUpdateSource;
  const AUpdateChannel: TUpdateChannel
): AnsiString;
const
  CBitBucketRequestUrl = 'https://bitbucket.org/sas_team/sas.planet.bin/downloads/';
  CGitHubRequestUrl = 'https://api.github.com/repos/sasgis/sas.planet.src/releases/latest';
  CSasGisRequestUrl: array [TUpdateChannel] of AnsiString = (
    'http://www.sasgis.org/programs/sasplanet/nightly.php',
    'http://www.sasgis.org/programs/sasplanet/release.php'
  );
begin
  case AUpdateSource of
    usSasGis: begin
      Result := CSasGisRequestUrl[AUpdateChannel];
    end;
    usBitBucket: begin
      Result := CBitBucketRequestUrl;
    end;
    usGitHub: begin
      Result := CGitHubRequestUrl;
    end
  else
    raise Exception.CreateFmt(
      'Unexpected UpdateSource value: %d', [Integer(AUpdateSource)]
    );
  end;
end;

procedure TUpdateChecker.Perform(
  const AOperationID: Integer
);
var
  VUrl: AnsiString;
  VRequest: IDownloadRequest;
begin
  VUrl := GetRequestUrl(FUpdateSource, FUpdateChannel);
  VRequest := TDownloadRequest.Create(VUrl, '', FInetConfig.GetStatic);

  FDownloader.DoRequestAsync(
    VRequest,
    FCancelNotifier,
    AOperationID,
    Self.OnRequestAsyncCallBack
  );
end;

procedure TUpdateChecker.OnRequestAsyncCallBack(
  const AResult: IDownloadResult;
  const AOperationID: Integer
);
var
  VResultOk: IDownloadResultOk;
  VResult: TUpdateCheckerResult;
begin
  VResult.IsFound := False;
  try
    if Supports(AResult, IDownloadResultOk, VResultOk) then begin
      VResult :=
        TUpdateCheckerFunc.ParseDownloadResult(
          FUpdateChannel,
          FUpdateSource,
          VResultOk
        );
    end;
  except
    // ignore
  end;

  FOnCheckResult(VResult);
end;

end.
