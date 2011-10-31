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

unit u_TileDownloaderThread;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  i_DownloadInfoSimple,
  i_OperationNotifier,
  u_OperationNotifier,
  i_TileError,
  i_TileDownloader,
  i_TileDownloadRequest,
  i_TileRequest,
  u_MapType,
  u_TileDownloaderEvent;

type
  PTileRec = ^TTileRec;
  TTileRec = record
    MapGUID: TGUID;
    Tile: TPoint;
    Zoom: Byte;
  end;

  TTileDownloaderThread = class (TThread)
  private
    FCancelNotifierInternal: IOperationNotifierInternal;
  protected
    FMapType: TMapType;
    FErrorLogger: ITileErrorLogger;
    FDownloadInfo: IDownloadInfoSimple;
    FMapTileUpdateEvent: TMapTileUpdateEvent;
    FProcessList: TList;
    FCancelEvent: TEvent;
    FCancelNotifier: IOperationNotifier;
    FMaxRequestCount: Integer;
    FSemaphore: THandle;
    FCS: TCriticalSection;
    function CreateNewTileDownloaderEvent(
      ATile: TPoint;
      AZoom: Byte;
      ACallBack: TOnDownloadCallBack;
      ACheckExistsTileSize: Boolean;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    ): ITileDownloaderEvent;
    procedure Download(
      ATile: TPoint;
      AZoom: Byte;
      ACallBack: TOnDownloadCallBack;
      ACheckExistsTileSize: Boolean;
      ACancelNotifier: IOperationNotifier;
      AOperationID: Integer
    );
    procedure SleepCancelable(ATime: Cardinal);
    procedure AddTileToProcessList(
      AMapGUID: TGUID;
      ATile: TPoint;
      AZoom: Byte
    );
    procedure RemoveTileFromProcessList(
      AMapGUID: TGUID;
      ATile: TPoint;
      AZoom: Byte
    );
    function TileExistsInProcessList(
      AMapGUID: TGUID;
      ATile: TPoint;
      AZoom: Byte
    ): Boolean;
    procedure ClearProcessList;
  public
    constructor Create(
      ACreateSuspended: Boolean;
      ADownloadInfo: IDownloadInfoSimple;
      AMapTileUpdateEvent: TMapTileUpdateEvent;
      AErrorLogger: ITileErrorLogger;
      AMaxRequestCount: Integer
    );
    destructor Destroy; override;
    procedure OnTileDownload(AEvent: ITileDownloaderEvent); virtual;
    procedure Terminate; reintroduce;
  end;

implementation

uses
  SysUtils;

{ TTileDownloaderThread }

constructor TTileDownloaderThread.Create(
  ACreateSuspended: Boolean;
  ADownloadInfo: IDownloadInfoSimple;
  AMapTileUpdateEvent: TMapTileUpdateEvent;
  AErrorLogger: ITileErrorLogger;
  AMaxRequestCount: Integer);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create(ACreateSuspended);
  FCS := TCriticalSection.Create;
  FCancelEvent := TEvent.Create;
  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
  FMapType := nil;
  FDownloadInfo := ADownloadInfo;
  FMapTileUpdateEvent := AMapTileUpdateEvent;
  FErrorLogger := AErrorLogger;
  FMaxRequestCount := AMaxRequestCount;
  FSemaphore := CreateSemaphore(nil, FMaxRequestCount, FMaxRequestCount, nil);
  FProcessList := TList.Create;
end;

destructor TTileDownloaderThread.Destroy;
begin
  try
    Terminate;
    FreeAndNil(FCancelEvent);
    CloseHandle(FSemaphore);
    ClearProcessList;
    FreeAndNil(FProcessList);
    FreeAndNil(FCS);
  finally
    inherited Destroy;
  end;
end;

procedure TTileDownloaderThread.Terminate;
begin
  inherited;
  FCancelEvent.SetEvent;
  FCancelNotifierInternal.NextOperation;
end;

procedure TTileDownloaderThread.SleepCancelable(ATime: Cardinal);
begin
  if ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderThread.OnTileDownload(AEvent: ITileDownloaderEvent);
var
  VRequest: ITileRequest;
begin
  ReleaseSemaphore(FSemaphore, 1, nil);
  VRequest := AEvent.Request;
  if VRequest <> nil then begin
    RemoveTileFromProcessList(
      VRequest.Zmp.GUID,
      VRequest.Tile,
      VRequest.Zoom
    );
  end;                                 
end;

function TTileDownloaderThread.CreateNewTileDownloaderEvent(
  ATile: TPoint;
  AZoom: Byte;
  ACallBack: TOnDownloadCallBack;
  ACheckExistsTileSize: Boolean;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
): ITileDownloaderEvent;
begin
  Result := TTileDownloaderEvent.Create(
              FDownloadInfo,
              FMapTileUpdateEvent,
              FErrorLogger,
              FMapType,
              FMapType.GetRequest(ATile, AZoom, ACheckExistsTileSize),
              ACheckExistsTileSize,
              ACancelNotifier,
              AOperationID
            );
  Result.AddToCallBackList(ACallBack);
end;

procedure TTileDownloaderThread.Download(
  ATile: TPoint;
  AZoom: Byte;
  ACallBack: TOnDownloadCallBack;
  ACheckExistsTileSize: Boolean;
  ACancelNotifier: IOperationNotifier;
  AOperationID: Integer
);
begin
  repeat // Стартуем закачку
    if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0 then begin

      AddTileToProcessList(FMapType.Zmp.GUID, ATile, AZoom);

      FMapType.DownloadTile(
        CreateNewTileDownloaderEvent(
          ATile,
          AZoom,
          ACallBack,
          ACheckExistsTileSize,
          ACancelNotifier,
          AOperationID
        )
      );
      Break;
    end else if Terminated then begin
      Break;
    end;
  until False;
  if not Terminated then begin
    repeat // Ждём освобождения потока(ов)
      if WaitForSingleObject(FSemaphore, 300) = WAIT_OBJECT_0 then begin
        ReleaseSemaphore(FSemaphore, 1, nil);
        Break;
      end else if Terminated then begin
        Break;
      end;
    until False;
  end;
end;

procedure TTileDownloaderThread.AddTileToProcessList(
  AMapGUID: TGUID;
  ATile: TPoint;
  AZoom: Byte
);
var
  VTileRec: PTileRec;
begin
  if not TileExistsInProcessList(AMapGUID, ATile, AZoom) then begin
    New(VTileRec);
    VTileRec.MapGUID := AMapGUID;
    VTileRec.Tile := ATile;
    VTileRec.Zoom := AZoom;
    FCS.Acquire;
    try
      FProcessList.Add(VTileRec);
    finally
      FCS.Release;
    end;
  end;
end;

procedure TTileDownloaderThread.RemoveTileFromProcessList(
  AMapGUID: TGUID;
  ATile: TPoint;
  AZoom: Byte
);
var
  I: Integer;
  VTileRec: PTileRec;
begin
  FCS.Acquire;
  try
    for I := 0 to FProcessList.Count - 1 do begin
      VTileRec := FProcessList.Items[I];
      if Assigned(VTileRec) then begin
        if (VTileRec.Zoom = AZoom) and
           (VTileRec.Tile.X = ATile.X) and
           (VTileRec.Tile.Y = ATile.Y) and
            IsEqualGUID(VTileRec.MapGUID, AMapGUID) then
        begin
          FProcessList.Delete(I);
          Dispose(VTileRec);
          Break;
        end;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

function TTileDownloaderThread.TileExistsInProcessList(
  AMapGUID: TGUID;
  ATile: TPoint;
  AZoom: Byte
): Boolean;
var
  I: Integer;
  VTileRec: PTileRec;
begin
  Result := False;
  FCS.Acquire;
  try
    for I := 0 to FProcessList.Count - 1 do begin
      VTileRec := FProcessList.Items[I];
      if Assigned(VTileRec) then begin
        if (VTileRec.Zoom = AZoom) and
           (VTileRec.Tile.X = ATile.X) and
           (VTileRec.Tile.Y = ATile.Y) and
            IsEqualGUID(VTileRec.MapGUID, AMapGUID) then
        begin
          Result := True;
          Break;
        end;
      end;
    end;
  finally
    FCS.Release;
  end;
end;

procedure TTileDownloaderThread.ClearProcessList;
var
  I: Integer;
  VTileRec: PTileRec;
begin
  FCS.Acquire;
  try
    for I := 0 to FProcessList.Count - 1 do begin
      VTileRec := FProcessList.Items[I];
      if Assigned(VTileRec) then begin
        Dispose(VTileRec);
      end;
    end;
    FProcessList.Clear;
  finally
    FCS.Release;
  end;
end;

end.
