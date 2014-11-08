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

unit u_TileDownloaderSimple;

interface

uses
  Windows,
  SysUtils,
  SyncObjs,
  i_Notifier,
  i_Listener,
  i_NotifierOperation,
  i_LastResponseInfo,
  i_Downloader,
  i_TileRequest,
  i_TileRequestResult,
  i_TileDownloadResultSaver,
  i_TileDownloaderConfig,
  i_TileDownloader,
  i_TileDownloadRequestBuilder,
  u_BaseInterfacedObject;

type
  TTileDownloaderSimple = class(TBaseInterfacedObject, ITileDownloader)
  private
    FTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FHttpDownloader: IDownloader;
    FResultSaver: ITileDownloadResultSaver;
    FAppClosingNotifier: INotifierOneOperation;
    FLastResponseInfo: ILastResponseInfo;

    FAppClosingListener: IListener;
    FCS: IReadWriteSync;
    FCancelListener: IListener;
    FConfigChangeListener: IListener;
    FCancelEvent: TEvent;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;

    FDownloadTryCount: Integer;
    FSleepOnResetConnection: Cardinal;
    FSleepAfterDownload: Cardinal;
    procedure OnConfigChange;
    procedure OnCancelEvent;
    procedure OnAppClosing;
    procedure SleepCancelable(ATime: Cardinal);
    procedure SleepIfConnectErrorOrWaitInterval;
  private
    { ITileDownloader }
    function Download(
      const ASoftCancelNotifier: INotifierOneOperation;
      const ACancelNotifier: INotifierOperation;
      const AOperationID: Integer;
      const ATileRequest: ITileRequest
    ): ITileRequestResult;
  public
    procedure AfterConstruction; override;
  public
    constructor Create(
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileDownloadRequestBuilder: ITileDownloadRequestBuilder;
      const ATileDownloaderConfig: ITileDownloaderConfig;
      const AHttpDownloader: IDownloader;
      const AResultSaver: ITileDownloadResultSaver;
      const ALastResponseInfo: ILastResponseInfo
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  i_InetConfig,
  i_DownloadResult,
  i_TileDownloadRequest,
  u_ListenerByEvent,
  u_TileRequestResult;

{ TITileDownloaderSimple }

constructor TTileDownloaderSimple.Create(
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileDownloadRequestBuilder: ITileDownloadRequestBuilder;
  const ATileDownloaderConfig: ITileDownloaderConfig;
  const AHttpDownloader: IDownloader;
  const AResultSaver: ITileDownloadResultSaver;
  const ALastResponseInfo: ILastResponseInfo
);
begin
  inherited Create;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileDownloadRequestBuilder := ATileDownloadRequestBuilder;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FHttpDownloader := AHttpDownloader;
  FResultSaver := AResultSaver;
  FLastResponseInfo := ALastResponseInfo;
  Assert(FResultSaver <> nil);

  FCS := GSync.SyncStd.Make(Self.ClassName);
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancelEvent);
  FConfigChangeListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  FTileDownloaderConfig.ChangeNotifier.Add(FConfigChangeListener);
  FWasConnectError := False;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
end;

destructor TTileDownloaderSimple.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;

  if Assigned(FTileDownloaderConfig) and Assigned(FConfigChangeListener) then begin
    FTileDownloaderConfig.ChangeNotifier.Remove(FConfigChangeListener);
    FConfigChangeListener := nil;
    FTileDownloaderConfig := nil;
  end;

  FCS := nil;
  FreeAndNil(FCancelEvent);
  inherited;
end;

procedure TTileDownloaderSimple.AfterConstruction;
begin
  inherited;
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end else begin
    OnConfigChange;
  end;
end;

function TTileDownloaderSimple.Download(
  const ASoftCancelNotifier: INotifierOneOperation;
  const ACancelNotifier: INotifierOperation;
  const AOperationID: Integer;
  const ATileRequest: ITileRequest
): ITileRequestResult;
var
  VDownloadRequest: ITileDownloadRequest;
  VDownloadResult: IDownloadResult;
  VDownloadResultError: IDownloadResultError;
  VCount: Integer;
  VTryCount: Integer;
  VResultWithRespond: IDownloadResultWithServerRespond;
begin
  Result := nil;
  if not ASoftCancelNotifier.IsExecuted then begin
    FCS.BeginWrite;
    try
      if not ASoftCancelNotifier.IsExecuted then begin
        FCancelEvent.ResetEvent;
        ASoftCancelNotifier.Add(FCancelListener);
        try
          if not ASoftCancelNotifier.IsExecuted then begin
            VCount := 0;
            VTryCount := FDownloadTryCount;
            repeat
              SleepIfConnectErrorOrWaitInterval;
              if ASoftCancelNotifier.IsExecuted then begin
                Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
                Break;
              end;
              try
                VDownloadRequest :=
                  FTileDownloadRequestBuilder.BuildRequest(
                    ATileRequest,
                    FLastResponseInfo,
                    ACancelNotifier,
                    AOperationID
                  );
              except
                on E: Exception do begin
                  Result :=
                    TTileRequestResultErrorBeforBuildDownloadRequest.Create(
                      ATileRequest,
                      E.Message
                    );
                  Break;
                end;
              end;
              if VDownloadRequest = nil then begin
                Result :=
                  TTileRequestResultErrorBeforBuildDownloadRequest.Create(
                    ATileRequest,
                    'Tile does not exist'
                  );
                Break;
              end;
              if ASoftCancelNotifier.IsExecuted then begin
                Result := TTileRequestResultCanceledAfterBuildDownloadRequest.Create(VDownloadRequest);
                Break;
              end;
              VDownloadResult :=
                FHttpDownloader.DoRequest(
                  VDownloadRequest,
                  ACancelNotifier,
                  AOperationID
                );
              Inc(VCount);
              FLastDownloadTime := GetTickCount;
              if VDownloadResult <> nil then begin
                if VDownloadResult.IsServerExists then begin
                  FWasConnectError := False;
                  if Supports(VDownloadResult, IDownloadResultWithServerRespond, VResultWithRespond) then begin
                    FLastResponseInfo.ResponseHead := VResultWithRespond.RawResponseHeader;
                  end;
                end else begin
                  FWasConnectError := True;
                end;
                try
                  Result := FResultSaver.SaveDownloadResult(VDownloadResult);
                except
                  on E: Exception do begin
                    Result := TTileRequestResultErrorAfterDownloadRequest.Create(
                      VDownloadResult,
                      E.Message
                    );
                  end;
                end;
                if Result = nil then begin
                  if Supports(VDownloadResult, IDownloadResultError, VDownloadResultError) then begin
                    Result := TTileRequestResultDownloadError.Create(VDownloadResultError)
                  end else begin
                    Result :=
                      TTileRequestResultErrorAfterDownloadRequest.Create(
                        VDownloadResult,
                        'Unknown error'
                      );
                  end;
                end;
              end else begin
                Result := TTileRequestResultCanceledAfterBuildDownloadRequest.Create(VDownloadRequest);
              end;
            until (not FWasConnectError) or (VCount >= VTryCount);
          end else begin
            Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
          end;
        finally
          ASoftCancelNotifier.Remove(FCancelListener);
        end;
      end else begin
        Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
      end;
    finally
      FCS.EndWrite;
    end;
  end else begin
    Result := TTileRequestResultCanceledBeforBuildDownloadRequest.Create(ATileRequest);
  end;
end;

procedure TTileDownloaderSimple.OnAppClosing;
begin
  FCancelEvent.SetEvent;
end;

procedure TTileDownloaderSimple.OnCancelEvent;
begin
  FCancelEvent.SetEvent;
end;

procedure TTileDownloaderSimple.OnConfigChange;
var
  VTileDownloaderConfigStatic: ITileDownloaderConfigStatic;
begin
  VTileDownloaderConfigStatic := FTileDownloaderConfig.GetStatic;
  FDownloadTryCount := VTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
  FSleepOnResetConnection := VTileDownloaderConfigStatic.InetConfigStatic.SleepOnResetConnection;
  FSleepAfterDownload := VTileDownloaderConfigStatic.WaitInterval;
end;

procedure TTileDownloaderSimple.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderSimple.SleepIfConnectErrorOrWaitInterval;
var
  VNow: Cardinal;
  VTimeFromLastDownload: Cardinal;
  VSleepTime: Cardinal;
  VInterval: Cardinal;
begin
  VNow := GetTickCount;

  if VNow >= FLastDownloadTime then begin
    VTimeFromLastDownload := VNow - FLastDownloadTime;
  end else begin
    VTimeFromLastDownload := MaxInt;
  end;

  if FWasConnectError then begin
    VInterval := FSleepOnResetConnection;
  end else begin
    VInterval := FSleepAfterDownload;
  end;

  if VTimeFromLastDownload < VInterval then begin
    VSleepTime := VInterval - VTimeFromLastDownload;
    SleepCancelable(VSleepTime);
  end;
end;

end.
