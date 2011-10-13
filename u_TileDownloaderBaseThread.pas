{******************************************************************************}
{* SAS.Planet (SAS.ѕланета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_TileDownloaderBaseThread;

interface

uses
  Windows,
  SysUtils,
  Classes,
  SyncObjs,
  i_AntiBan,
  i_JclNotify,
  i_OperationNotifier,
  i_InetConfig,
  i_TileRequestBuilder,
  i_TileDownloader,
  i_TileDownloaderConfig,
  u_TileDownloaderHttp;

type
  TThreadTTLEvent = procedure(Sender: TObject; AThreadID: Cardinal) of object;

  TTileDownloaderBaseThread = class(TThread)
  private
    FCancelListener: IJclListener;
    FCancelEvent: TEvent;
    FTileRequestBuilder: ITileRequestBuilder;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FHttpDownloader: TTileDownloaderHttp;
    FEvent: ITileDownloaderEvent;
    FSemaphore: THandle;
    FParentSemaphore: THandle;
    FBusy: Boolean;
    FWasConnectError: Boolean;
    FLastDownloadTime: Cardinal;
    FIsCanceled: Boolean;
    FSessionCS: TCriticalSection;
    FOnTTLEvent: TThreadTTLEvent;
    FLastUsedTime: Cardinal;
    procedure DoRequest;
    function IsCanceled: Boolean;
    procedure SetIsCanceled;
    procedure SetNotCanceled;
    procedure SleepCancelable(ATime: Cardinal);
  protected
    procedure Execute; override;
  public
    constructor Create(AAntiBan: IAntiBan);
    destructor Destroy; override;
    procedure Terminate; overload;
    procedure AddEvent(AEvent: ITileDownloaderEvent);
    procedure OnCancelEvent(Sender: TObject);
    property OnTTL: TThreadTTLEvent write FOnTTLEvent default nil;
    property Busy: Boolean read FBusy default False;
    property TileRequestBuilder: ITileRequestBuilder write FTileRequestBuilder default nil;
    property TileDownloaderConfig: ITileDownloaderConfig write FTileDownloaderConfig default nil;
    property Semaphore: THandle read FParentSemaphore write FParentSemaphore;
  end;

const
  CThreadTTL = 30000; // ms, врем€, в течении которого поток ожидает нового запроса

implementation

uses
  i_DownloadResult,
  u_NotifyEventListener;

{ TTileDownloaderBaseThread }

constructor TTileDownloaderBaseThread.Create(AAntiBan: IAntiBan);
begin
  FCancelEvent := TEvent.Create;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  FIsCanceled := False;
  FSessionCS := TCriticalSection.Create;
  FSemaphore := CreateSemaphore(nil, 0, 1, nil);
  FHttpDownloader := TTileDownloaderHttp.Create(AAntiBan);
  FWasConnectError := False;
  FOnTTLEvent := nil;
  FLastUsedTime := GetTickCount;
  FreeOnTerminate := False;
  inherited Create(False);
end;

destructor TTileDownloaderBaseThread.Destroy;
begin
  try
    FreeAndNil(FHttpDownloader);
    CloseHandle(FSemaphore);
    FreeAndNil(FCancelEvent);
    FreeAndNil(FSessionCS);
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderBaseThread.Terminate;
begin
  try
    ReleaseSemaphore(FSemaphore, 1, nil);
  finally
    inherited Terminate;
  end;
end;

procedure TTileDownloaderBaseThread.AddEvent(AEvent: ITileDownloaderEvent);
begin
  FBusy := True;
  FEvent := AEvent;
  ReleaseSemaphore(FSemaphore, 1, nil);
end;

procedure TTileDownloaderBaseThread.DoRequest;

  procedure SleepIfConnectErrorOrWaitInterval(
    ATileDownloaderConfigStatic: ITileDownloaderConfigStatic
  );
  var
    VNow: Cardinal;
    VTimeFromLastDownload: Cardinal;
    VSleepTime: Cardinal;
    VInetConfig: IInetConfigStatic;
  begin
    VInetConfig := ATileDownloaderConfigStatic.InetConfigStatic;
    VNow := GetTickCount;
    if VNow >= FLastDownloadTime then begin
      VTimeFromLastDownload := VNow - FLastDownloadTime;
    end else begin
      VTimeFromLastDownload := MaxInt;
    end;
    if FWasConnectError then begin
      if VTimeFromLastDownload < VInetConfig.SleepOnResetConnection then begin
        VSleepTime := VInetConfig.SleepOnResetConnection - VTimeFromLastDownload;
        SleepCancelable(VSleepTime);
      end;
    end else begin
      if VTimeFromLastDownload < ATileDownloaderConfigStatic.WaitInterval then begin
        VSleepTime := ATileDownloaderConfigStatic.WaitInterval - VTimeFromLastDownload;
        //SleepCancelable(VSleepTime); отключено в цел€х упрощени€ отладки
      end;
    end;
  end;

var
  VCount: Integer;
  VTryCount: Integer;
  VTileDownloaderConfigStatic: ITileDownloaderConfigStatic;
begin
  VTileDownloaderConfigStatic := FTileDownloaderConfig.GetStatic;
  SetNotCanceled;
  try
    try
      if (VTileDownloaderConfigStatic <> nil) and (FTileRequestBuilder <> nil) then begin
        try
          if FEvent.CancelNotifier <> nil then begin
            FEvent.CancelNotifier.AddListener(FCancelListener);
          end;
          if FEvent.DownloadResult = nil then begin
            VCount := 0;
            VTryCount := VTileDownloaderConfigStatic.InetConfigStatic.DownloadTryCount;
            FWasConnectError := False;
            FLastDownloadTime := MaxInt;
            repeat
              if IsCanceled then begin
                FEvent.DownloadResult := FHttpDownloader.Cancel(FEvent.Request);
                Break;
              end;
              SleepIfConnectErrorOrWaitInterval(VTileDownloaderConfigStatic);
              FEvent.Request := FTileRequestBuilder.BuildRequest(
                FEvent.TileXY,
                FEvent.TileZoom,
                FEvent.VersionInfo,
                FEvent.LastResponseInfo
              );
              FEvent.DownloadResult := FHttpDownloader.Get(
                FEvent.Request,
                VTileDownloaderConfigStatic,
                FEvent.CheckTileSize,
                FEvent.OldTileSize
              );
              Inc(VCount);
              FLastDownloadTime := GetTickCount;
              if FEvent.DownloadResult <> nil then begin
                FWasConnectError := not FEvent.DownloadResult.IsServerExists;
              end;
            until (not FWasConnectError) or (VCount >= VTryCount);
          end;
        finally
          if FEvent.CancelNotifier <> nil then begin
            FEvent.CancelNotifier.RemoveListener(FCancelListener);
          end;
        end;
      end;
    finally
      FEvent.ProcessEvent;
    end;
  finally
    FEvent := nil;
    FBusy := False;
    if FParentSemaphore <> 0 then begin
      ReleaseSemaphore(FParentSemaphore, 1, nil);
    end;
  end;
end;

procedure TTileDownloaderBaseThread.Execute;
begin
  repeat
    if Terminated then begin
      Break;
    end;
    repeat
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0  then begin
        Break
      end else if Terminated then begin
        Break;
      end else if (GetTickCount - FLastUsedTime) > CThreadTTL then begin
        if Addr(FOnTTLEvent) <> nil then begin
          FOnTTLEvent(Self, Self.ThreadID)
        end;
      end;
    until False;
    if Assigned(FEvent) then begin
      try
        DoRequest;
      finally
        FLastUsedTime := GetTickCount;
      end;
    end;
  until False;
end;

procedure TTileDownloaderBaseThread.OnCancelEvent(Sender: TObject);
begin
  if Assigned(FHttpDownloader) then begin
    FHttpDownloader.Disconnect;
  end;
  SetIsCanceled;
end;

function TTileDownloaderBaseThread.IsCanceled: Boolean;
begin
  FSessionCS.Acquire;
  try
    Result := FIsCanceled;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SetIsCanceled;
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.SetEvent;
    FIsCanceled := True;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SetNotCanceled;
begin
  FSessionCS.Acquire;
  try
    FCancelEvent.ResetEvent;
    FIsCanceled := False;
  finally
    FSessionCS.Release;
  end;
end;

procedure TTileDownloaderBaseThread.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

end.
