{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
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

unit u_TileDownloaderEvent;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  Types,
  i_JclNotify,
  i_OperationNotifier,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_TileDownloader,
  i_TileError,
  i_DownloadResult,
  i_DownloadInfoSimple,
  i_TileDownloadRequest,
  u_MapType;

type
  TEventStatus = class
  private
    FCancelNotifier: IOperationNotifier;
    FCancelListener: IJclListener;
    FThreadSafeCS: TCriticalSection;
    FCancelled: Boolean;
    FOperationID: Integer;
    function GetIsCanselled: Boolean;
    function GetNotifier: IOperationNotifier;
    procedure OnCancelEvent(Sender: TObject);
  public
    constructor Create(ACancelNotifier: IOperationNotifier; AOperationID: Integer);
    destructor Destroy; override;
    property IsCanceled: Boolean read GetIsCanselled;
    property Notifier: IOperationNotifier read GetNotifier;
  end;

  TTileDownloaderEvent = class(TInterfacedObject, ITileDownloaderEvent)
  private
    FProcessed: Boolean;
    FEventStatus: TEventStatus;
    FCallBackList: TList;
    FRES_TileDownloadUnexpectedError: string;
    FDownloadInfo: IDownloadInfoSimple;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;
    FDownloadResult: IDownloadResult;
    FRequest: ITileDownloadRequest;
    FLastResponseInfo: ILastResponseInfo;
    FVersionInfo: IMapVersionInfo;
    FTileXY: TPoint;
    FTileZoom: Byte;
    FCheckTileSize: Boolean;
    FOldTileSize: Cardinal;
    FErrorString: string;
    procedure GuiSync;
  public
    constructor Create(
      ADownloadInfo: IDownloadInfoSimple;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger;
      AMapType: TMapType;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    );
    destructor Destroy; override;

    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);
    procedure ExecCallBackList;

    function  GetRequest: ITileDownloadRequest;
    procedure SetRequest(AValue: ITileDownloadRequest);
    function  GetLastResponseInfo: ILastResponseInfo;
    procedure SetLastResponseInfo(AValue: ILastResponseInfo);
    function  GetVersionInfo: IMapVersionInfo;
    procedure SetVersionInfo(AValue: IMapVersionInfo);
    function  GetTileXY: TPoint;
    procedure SetTileXY(AValue: TPoint);
    function  GetTileZoom: Byte;
    procedure SetTileZoom(AValue: Byte);
    function  GetCheckTileSize: Boolean;
    procedure SetCheckTileSize(AValue: Boolean);
    function  GetOldTileSize: Cardinal;
    procedure SetOldTileSize(AValue: Cardinal);
    function  GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(AValue: IDownloadResult);
    function  GetCancelNotifier: IOperationNotifier;

    property Request: ITileDownloadRequest read GetRequest write SetRequest;
    property LastResponseInfo: ILastResponseInfo read GetLastResponseInfo write SetLastResponseInfo;
    property VersionInfo: IMapVersionInfo read GetVersionInfo write SetVersionInfo;
    property TileXY: TPoint read GetTileXY write SetTileXY;
    property TileZoom: Byte read GetTileZoom write SetTileZoom;
    property CheckTileSize: Boolean read GetCheckTileSize write SetCheckTileSize;
    property OldTileSize: Cardinal read GetOldTileSize write SetOldTileSize;
    property DownloadResult: IDownloadResult read GetDownloadResult write SetDownloadResult;
    property CancelNotifier: IOperationNotifier read GetCancelNotifier;
  end;

implementation

uses
  u_TileErrorInfo,
  u_ResStrings,
  u_NotifyEventListener;

{ TEventStatus }

constructor TEventStatus.Create(
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  inherited Create;
  FCancelled := False;
  FOperationID := AOperationID;
  FThreadSafeCS := TCriticalSection.Create;
  FCancelNotifier := ACancelNotifier;
  FCancelListener := TNotifyEventListener.Create(Self.OnCancelEvent);
  if FCancelNotifier <> nil then begin
    FCancelNotifier.AddListener(FCancelListener);
  end;
end;

destructor TEventStatus.Destroy;
begin
  try
    if FCancelNotifier <> nil then begin
      FCancelNotifier.RemoveListener(FCancelListener);
    end;
    FreeAndNil(FThreadSafeCS);
  finally
    inherited;
  end;
end;

procedure TEventStatus.OnCancelEvent(Sender: TObject);
begin
  FThreadSafeCS.Acquire;
  try
    FCancelled := True;
  finally
    FThreadSafeCS.Release;
  end;
end;

function TEventStatus.GetIsCanselled: Boolean;
begin
  FThreadSafeCS.Acquire;
  try
    Result := FCancelled or FCancelNotifier.IsOperationCanceled(FOperationID);
  finally
    FThreadSafeCS.Release;
  end;
end;

function TEventStatus.GetNotifier: IOperationNotifier;
begin
  FThreadSafeCS.Acquire;
  try
    Result := FCancelNotifier;
  finally
    FThreadSafeCS.Release;
  end;
end;

{ TTileDownloaderEvent }

constructor TTileDownloaderEvent.Create(
  ADownloadInfo: IDownloadInfoSimple;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger;
  AMapType: TMapType;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  inherited Create;
  FEventStatus := TEventStatus.Create(ACancelNotifier, AOperationID);
  FProcessed := False;
  FDownloadInfo := ADownloadInfo;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;
  FDownloadResult := nil;
  FRequest := nil;
  FLastResponseInfo := nil;
  FVersionInfo := nil;
  FTileXY := Types.Point(0,0);
  FTileZoom := 0;
  FCheckTileSize := False;
  FOldTileSize := 0;
  FErrorString := '';
  FCallBackList := TList.Create;
  FRES_TileDownloadUnexpectedError := SAS_ERR_TileDownloadUnexpectedError;
end;

destructor TTileDownloaderEvent.Destroy;
begin
  try
    try
      if Assigned(FEventStatus) and
         not FEventStatus.IsCanceled and
         not FProcessed then
      begin
        ProcessEvent;
      end;
      FCallBackList.Clear;
      FreeAndNil(FCallBackList);
    finally
      FreeAndNil(FEventStatus);
    end; 
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderEvent.GuiSync;
begin
  if not FEventStatus.IsCanceled and (Addr(FMapTileUpdateEvent) <> nil) then begin
    FMapTileUpdateEvent(FMapType, FTileZoom, FTileXY);
  end;
end;

procedure TTileDownloaderEvent.ProcessEvent;
const
  CErrorStrBufLength = 40;
var
  VErrorString: string;
  VResultOk: IDownloadResultOk;
  VResultDownloadError: IDownloadResultError;
  VResultNotNecessary: IDownloadResultNotNecessary;
begin
  try
    VErrorString := '';
    try
      ExecCallBackList;
      if Supports(FDownloadResult, IDownloadResultOk, VResultOk) then begin
        if not FEventStatus.IsCanceled and (FDownloadInfo <> nil) then begin
          FDownloadInfo.Add(1, VResultOk.Size);
        end;
      end else if Supports(FDownloadResult, IDownloadResultError, VResultDownloadError) then begin
        VErrorString := VResultDownloadError.ErrorText;
      end else if Supports(FDownloadResult, IDownloadResultNotNecessary, VResultNotNecessary) then begin
        VErrorString := VResultNotNecessary.ReasonText;
      end else begin
        VErrorString := FRES_TileDownloadUnexpectedError;
      end;
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end
      else begin
        VErrorString := FRES_TileDownloadUnexpectedError;
      end;
    end;
    if VErrorString <> '' then begin
      if (FErrorLogger <> nil) and (not FEventStatus.IsCanceled) then begin
        VErrorString := 'Error: ' + VErrorString;
        if Length(VErrorString) > CErrorStrBufLength then begin
          SetLength(VErrorString, CErrorStrBufLength);
          VErrorString := VErrorString + '..';
        end;
        FErrorLogger.LogError(
          TTileErrorInfo.Create(
            FMapType,
            FTileZoom,
            FTileXY,
            VErrorString
          )
        );
      end;
    end else begin
      if not FEventStatus.IsCanceled then begin
        TThread.Synchronize(nil, GuiSync);
      end;
    end;
  finally
    FProcessed := True;
  end;
end;

procedure TTileDownloaderEvent.AddToCallBackList(ACallBack: TOnDownloadCallBack);
var
  VCallBack: POnDownloadCallBack;
begin
  if Assigned(ACallBack) then begin
    if not Assigned(FCallBackList) then begin
      FCallBackList := TList.Create;
    end;
    New(VCallBack);
    TOnDownloadCallBack(VCallBack^) := ACallBack;
    FCallBackList.Add(VCallBack);
  end;
end;

procedure TTileDownloaderEvent.ExecCallBackList;
var
  i: Integer;
  VCallBack: POnDownloadCallBack;
begin
  if Assigned(FCallBackList) then
  try
    for i := FCallBackList.Count - 1 downto 0 do // !!! FILO
    try
      VCallBack := FCallBackList.Items[i];
      if Assigned(VCallBack) then
      try
        if not FEventStatus.IsCanceled then begin
          VCallBack^(Self);
        end;
      finally
        Dispose(VCallBack);
      end;
    except
      // ignore all
    end;
  finally
    FCallBackList.Clear;
  end;
end;

procedure TTileDownloaderEvent.SetRequest(AValue: ITileDownloadRequest);
begin
  FRequest := AValue;
end;

function TTileDownloaderEvent.GetRequest: ITileDownloadRequest;
begin
  Result := FRequest;
end;

procedure TTileDownloaderEvent.SetLastResponseInfo(AValue: ILastResponseInfo);
begin
  FLastResponseInfo := AValue;
end;

function TTileDownloaderEvent.GetLastResponseInfo: ILastResponseInfo;
begin
  Result := FLastResponseInfo;
end;

procedure TTileDownloaderEvent.SetVersionInfo(AValue: IMapVersionInfo);
begin
  FVersionInfo := AValue;
end;

function TTileDownloaderEvent.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

procedure TTileDownloaderEvent.SetTileXY(AValue: TPoint);
begin
  FTileXY := AValue;
end;

function TTileDownloaderEvent.GetTileXY: TPoint;
begin
  Result := FTileXY;
end;

procedure TTileDownloaderEvent.SetTileZoom(AValue: Byte);
begin
  FTileZoom := AValue;
end;

function TTileDownloaderEvent.GetTileZoom: Byte;
begin
  Result := FTileZoom;
end;

procedure TTileDownloaderEvent.SetCheckTileSize(AValue: Boolean);
begin
  FCheckTileSize := AValue;
end;

function TTileDownloaderEvent.GetCheckTileSize: Boolean;
begin
  Result := FCheckTileSize;
end;

procedure TTileDownloaderEvent.SetOldTileSize(AValue: Cardinal);
begin
  FOldTileSize := AValue;
end;

function TTileDownloaderEvent.GetOldTileSize: Cardinal;
begin
  Result := FOldTileSize;
end;

procedure TTileDownloaderEvent.SetDownloadResult(AValue: IDownloadResult);
begin
  FDownloadResult := AValue;
end;

function TTileDownloaderEvent.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

function TTileDownloaderEvent.GetCancelNotifier: IOperationNotifier;
begin
  Result := FEventStatus.Notifier;
end;

end.
