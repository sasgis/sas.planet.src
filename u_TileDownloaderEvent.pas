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
  i_LastResponseInfo,
  i_TileDownloader,
  i_TileError,
  i_TileRequest,
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
    FProcessed: Boolean;
    FOperationID: Integer;
    function GetIsCanselled: Boolean;
    function GetIsProcessed: Boolean;
    procedure SetIsProcessed(AValue: Boolean);
    function GetNotifier: IOperationNotifier;
    procedure OnCancelEvent(Sender: TObject);
    function GetOperationID: Integer;
  public
    constructor Create(ACancelNotifier: IOperationNotifier; AOperationID: Integer);
    destructor Destroy; override;
    property IsCanceled: Boolean read GetIsCanselled;
    property IsProcessed: Boolean read GetIsProcessed write SetIsProcessed;
    property Notifier: IOperationNotifier read GetNotifier;
  end;

  TTileDownloaderEvent = class(TInterfacedObject, ITileDownloaderEvent)
  private
    FEventStatus: TEventStatus;
    FCallBackList: TList;
    FRES_TileDownloadUnexpectedError: string;
    FDownloadInfo: IDownloadInfoSimple;
    FErrorLogger: ITileErrorLogger;
    FMapType: TMapType;
    FDownloadResult: IDownloadResult;
    FDownloadRequest: ITileDownloadRequest;
    FLastResponseInfo: ILastResponseInfo;
    FRequest: ITileRequest;
    FErrorString: string;
    procedure ExecCallBackList;
  public
    constructor Create(
      ADownloadInfo: IDownloadInfoSimple;
      AErrorLogger: ITileErrorLogger;
      AMapType: TMapType;
      ARequest: ITileRequest
    );
    destructor Destroy; override;

    procedure ProcessEvent;

    procedure AddToCallBackList(ACallBack: TOnDownloadCallBack);

    function  GetRequest: ITileRequest;
    function  GetDownloadRequest: ITileDownloadRequest;
    procedure SetDownloadRequest(AValue: ITileDownloadRequest);
    function  GetDownloadResult: IDownloadResult;
    procedure SetDownloadResult(AValue: IDownloadResult);
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
  FProcessed := False;
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
    FCancelNotifier := nil;
    FCancelListener := nil;
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

function TEventStatus.GetIsProcessed: Boolean;
begin
  FThreadSafeCS.Acquire;
  try
    Result := FProcessed;
  finally
    FThreadSafeCS.Release;
  end;
end;

procedure TEventStatus.SetIsProcessed(AValue: Boolean);
begin
  FThreadSafeCS.Acquire;
  try
    FProcessed := AValue;
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

function TEventStatus.GetOperationID: Integer;
begin
  Result := FOperationID;
end;

{ TTileDownloaderEvent }

constructor TTileDownloaderEvent.Create(
  ADownloadInfo: IDownloadInfoSimple;
  AErrorLogger: ITileErrorLogger;
  AMapType: TMapType;
  ARequest: ITileRequest
);
begin
  inherited Create;
  FEventStatus := TEventStatus.Create(ARequest.CancelNotifier, ARequest.OperationID);
  FDownloadInfo := ADownloadInfo;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;
  FDownloadResult := nil;
  FRequest := ARequest;
  FDownloadRequest := nil;
  FLastResponseInfo := nil;
  FErrorString := '';
  FCallBackList := TList.Create;
  FRES_TileDownloadUnexpectedError := SAS_ERR_TileDownloadUnexpectedError;
end;

destructor TTileDownloaderEvent.Destroy;
begin
  try
    try
      ProcessEvent;
      FCallBackList.Clear;
      FreeAndNil(FCallBackList);
    finally
      FreeAndNil(FEventStatus);
      FDownloadInfo := nil;
      FErrorLogger := nil;
      FDownloadResult := nil;
      FRequest := nil;
      FLastResponseInfo := nil;
    end;
  finally
    inherited Destroy;
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
  if not FEventStatus.IsProcessed then begin
    FEventStatus.IsProcessed := True;
    VErrorString := '';
    try
      ExecCallBackList; // Обрабатываем все колбэки, даже если загрузка отменена
    except
      on E: Exception do begin
        VErrorString := E.Message;
      end;
    end;
    if not FEventStatus.IsCanceled then begin
      if Supports(FDownloadResult, IDownloadResultOk, VResultOk) then begin
        if FDownloadInfo <> nil then begin
          FDownloadInfo.Add(1, VResultOk.Size);
        end;
      end else if Supports(FDownloadResult, IDownloadResultError, VResultDownloadError) then begin
        VErrorString := VResultDownloadError.ErrorText;
      end else if Supports(FDownloadResult, IDownloadResultNotNecessary, VResultNotNecessary) then begin
        VErrorString := VResultNotNecessary.ReasonText;
      end else begin
        VErrorString := FRES_TileDownloadUnexpectedError;
      end;
      if VErrorString <> '' then begin
        if FErrorLogger <> nil then begin
          VErrorString := 'Error: ' + VErrorString;
          if Length(VErrorString) > CErrorStrBufLength then begin
            SetLength(VErrorString, CErrorStrBufLength);
            VErrorString := VErrorString + '..';
          end;
          FErrorLogger.LogError(
            TTileErrorInfo.Create(
              FMapType,
              FRequest.Zoom,
              FRequest.Tile,
              VErrorString
            )
          );
        end;
      end;
    end;
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

function TTileDownloaderEvent.GetRequest: ITileRequest;
begin
  Result := FRequest;
end;

procedure TTileDownloaderEvent.SetDownloadRequest(AValue: ITileDownloadRequest);
begin
  FDownloadRequest := AValue;
end;

procedure TTileDownloaderEvent.SetDownloadResult(AValue: IDownloadResult);
begin
  FDownloadResult := AValue;
end;

function TTileDownloaderEvent.GetDownloadRequest: ITileDownloadRequest;
begin
  Result := FDownloadRequest;
end;

function TTileDownloaderEvent.GetDownloadResult: IDownloadResult;
begin
  Result := FDownloadResult;
end;

end.
