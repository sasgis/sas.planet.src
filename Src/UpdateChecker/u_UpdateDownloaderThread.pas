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

unit u_UpdateDownloaderThread;

interface

uses
  Classes,
  SysUtils,
  t_UpdateChecker,
  i_InetConfig,
  i_PathConfig,
  i_DownloadResult,
  i_DownloaderFactory,
  i_UpdateProgress;

type
  EUpdateDownloaderThread = class(Exception);

  TUpdateDownloaderThread = class(TThread)
  private
    FUpdateCheckerResult: TUpdateCheckerResult;
    FUpdatesPath: IPathConfig;
    FDownloaderFactory: IDownloaderFactory;
    FInetConfig: IInetConfig;
    FDownloaderProgress: IUpdateDownloaderProgress;

    FOperationID: Integer;
    FResult: TUpdateDownloaderResult;

    function GetOutFileName: string; inline;

    procedure OnDownloadProgress(
      const ARead: Integer;
      const ATotal: Integer
    );
    procedure OnDownloadResult(
      const AResult: IDownloadResultOk
    );
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AUpdateCheckerResult: TUpdateCheckerResult;
      const AUpdatesPath: IPathConfig;
      const ADownloaderFactory: IDownloaderFactory;
      const AInetConfig: IInetConfig;
      const ADownloaderProgress: IUpdateDownloaderProgress
    );
  end;

implementation

uses
  i_Downloader,
  i_DownloadRequest,
  u_DownloadRequest;

{ TUpdateDownloaderThread }

constructor TUpdateDownloaderThread.Create(
  const AUpdateCheckerResult: TUpdateCheckerResult;
  const AUpdatesPath: IPathConfig;
  const ADownloaderFactory: IDownloaderFactory;
  const AInetConfig: IInetConfig;
  const ADownloaderProgress: IUpdateDownloaderProgress
);
begin
  inherited Create(False);
  FreeOnTerminate := True;

  FUpdateCheckerResult := AUpdateCheckerResult;
  FUpdatesPath := AUpdatesPath;
  FDownloaderFactory := ADownloaderFactory;
  FInetConfig := AInetConfig;

  FDownloaderProgress := ADownloaderProgress;
  FOperationID := FDownloaderProgress.CurrentOperationID;
end;

procedure TUpdateDownloaderThread.Execute;
var
  VResult: IDownloadResult;
  VResultOk: IDownloadResultOk;
  VResultError: IDownloadResultError;
  VRequest: IDownloadRequest;
  VDownloader: IDownloader;
begin
  try
    FResult.IsFinished := False;
    FResult.IsError := False;

    FResult.BytesDownloaded := 0;
    FResult.BytesTotal := 0;

    VRequest :=
      TDownloadRequest.Create(
        AnsiString(FUpdateCheckerResult.DownloadUrl),
        '',
        FInetConfig.GetStatic
      );

    VDownloader :=
      FDownloaderFactory.BuildDownloader(
        True,  // AllowUseCookie
        True,  // AllowRedirects
        False, // DetectContentType
        Self.OnDownloadProgress
      );

    VResult :=
      VDownloader.DoRequest(
        VRequest,
        FDownloaderProgress.CancelNotifier,
        FOperationID
      );

    if Supports(VResult, IDownloadResultOk, VResultOk) then begin
      Self.OnDownloadResult(VResultOk);
      FResult.IsFinished := True;
      FResult.Text := Self.GetOutFileName;
    end else
    if Supports(VResult, IDownloadResultError, VResultError) then begin
      raise EUpdateDownloaderThread.Create(VResultError.ErrorText);
    end else begin
      raise EUpdateDownloaderThread.Create('Unknown error!');
    end;
  except
    on E: Exception do begin
      FResult.IsError := True;
      FResult.Text := E.ClassName + ': ' + E.Message;
    end;
  end;

  FDownloaderProgress.SetResult(FOperationID, FResult);
end;

procedure TUpdateDownloaderThread.OnDownloadResult(const AResult: IDownloadResultOk);
var
  VStream: TMemoryStream;
begin
  if FDownloaderProgress.CancelNotifier.IsOperationCanceled(FOperationID) then begin
    Exit;
  end;

  if not ForceDirectories(FUpdatesPath.FullPath) then begin
    RaiseLastOSError;
  end;

  VStream := TMemoryStream.Create;
  try
    VStream.WriteBuffer(AResult.Data.Buffer^, AResult.Data.Size);
    VStream.SaveToFile(Self.GetOutFileName);
  finally
    VStream.Free;
  end;
end;

procedure TUpdateDownloaderThread.OnDownloadProgress(const ARead, ATotal: Integer);
begin
  FResult.BytesDownloaded := ARead;
  FResult.BytesTotal := ATotal;

  FDownloaderProgress.SetResult(FOperationID, FResult);
end;

function TUpdateDownloaderThread.GetOutFileName: string;
begin
  Result :=
    IncludeTrailingPathDelimiter(FUpdatesPath.FullPath) +
    FUpdateCheckerResult.OutFileName;
end;

end.
