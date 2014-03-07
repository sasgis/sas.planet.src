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

unit u_UiTileRequestManager;

interface

uses
  Windows,
  SyncObjs,
  i_Listener,
  i_TileIterator,
  i_MapVersionInfo;

type
  TTaskState = (
    tsNone = 0,
    tsActive,
    tsActiveWaiting,
    tsMissed
  );

  TTaskArrayItem = record
    State: TTaskState;
    SessionID: Integer;
    Tile: TPoint;
    Zoom: Byte;
    Version: IMapVersionInfo
  end;

  TUiTileRequestManager = class(TObject)
  private
    FConcurrentCoutLimiter: THandle;

    FCancelEventHandle: THandle;
    FCancelListener: IListener;

    FSessionID: Integer;
    FSessionZoom: Byte;
    FSessionRect: TRect;
    FSessionVersionInfo: IMapVersionInfo;
    FSessionTileIterator: ITileIterator;
    FSessionLock: TCriticalSection;

    FTaskArray: array of TTaskArrayItem;
    FTaskArrayLock: TCriticalSection;

    FMissedTaskCount: Integer;
    FMissedTaskEventHandle: THandle;

    FActiveWaitingTaskCount: Integer;
  private
    procedure OnCancelSession;

    function IsValidSession(const ASessionID: Integer): Boolean; inline;

    function AcquireByIterator(
      out ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const AIterator: ITileIterator;
      const ARect: TRect;
      const ASessionID: Integer
    ): Boolean;

    function AcquireByMissedTasks(
      out ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ASessionID: Integer
    ): Boolean;
  public
    function Acquire(out ATile: TPoint): Boolean;

    procedure Release(
      const ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo;
      const ACanceled: Boolean
    );

    procedure InitSession(
      const AZoom: Byte;
      const ARect: TRect;
      const AVersionInfo: IMapVersionInfo
    );

    property SessionCancelListener: IListener read FCancelListener;
  public
    constructor Create(const AMaxConcurrentRequestCount: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_TileIteratorSpiralByRect;

function IsEqualVersionInfo(const A, B: IMapVersionInfo): Boolean; inline;
begin
  if Assigned(A) then begin
    Result := A.IsSame(B);
  end else begin
    Result := not Assigned(B);
  end;
end;

function IsPointInRect(const APoint: TPoint; const ARect: TRect): Boolean; inline;
begin
  Result :=
    (APoint.X <= ARect.Right) and (APoint.X >= ARect.Left) and
    (APoint.Y >= ARect.Top)   and (APoint.Y <= ARect.Bottom);
end;

{ TUiTileRequestManager }

constructor TUiTileRequestManager.Create(const AMaxConcurrentRequestCount: Integer);
begin
  inherited Create;

  FSessionID := 0;
  FSessionTileIterator := nil;

  FConcurrentCoutLimiter := CreateSemaphore(nil, AMaxConcurrentRequestCount, AMaxConcurrentRequestCount, nil);
  FCancelEventHandle := CreateEvent(nil, TRUE, FALSE, nil);

  FMissedTaskCount := 0;
  FMissedTaskEventHandle := CreateEvent(nil, TRUE, FALSE, nil);

  FActiveWaitingTaskCount := 0;

  FCancelListener := TNotifyNoMmgEventListener.Create(Self.OnCancelSession);

  SetLength(FTaskArray, AMaxConcurrentRequestCount * 2); // (!)
  FillChar(FTaskArray[0], Length(FTaskArray) * SizeOf(FTaskArray[0]), 0);

  FSessionLock := TCriticalSection.Create;
  FTaskArrayLock := TCriticalSection.Create;
end;

destructor TUiTileRequestManager.Destroy;
var
  I: Integer;
begin
  Self.OnCancelSession;

  FTaskArrayLock.Acquire;
  try
    for I := 0 to Length(FTaskArray) - 1 do begin
      FTaskArray[I].Version := nil;
    end;
    SetLength(FTaskArray, 0);
  finally
    FTaskArrayLock.Release;
  end;

  CloseHandle(FConcurrentCoutLimiter);
  CloseHandle(FCancelEventHandle);
  CloseHandle(FMissedTaskEventHandle);

  FCancelListener := nil;

  FSessionLock.Free;
  FTaskArrayLock.Free;

  inherited;
end;

procedure TUiTileRequestManager.OnCancelSession;
begin
  FSessionLock.Acquire;
  try
    InterlockedIncrement(FSessionID);
    FSessionTileIterator := nil;
  finally
    FSessionLock.Release;
  end;
  SetEvent(FCancelEventHandle);
end;

function TUiTileRequestManager.IsValidSession(const ASessionID: Integer): Boolean;
begin
  Result := InterlockedCompareExchange(FSessionID, 0, 0) = ASessionID;
end;

procedure TUiTileRequestManager.InitSession(
  const AZoom: Byte;
  const ARect: TRect;
  const AVersionInfo: IMapVersionInfo
);
var
  I: Integer;
  VSessionID: Integer;
begin
  FSessionLock.Acquire;
  try
    VSessionID := InterlockedIncrement(FSessionID);
    FSessionZoom := AZoom;
    FSessionRect := ARect;
    FSessionVersionInfo := AVersionInfo;
    FSessionTileIterator := TTileIteratorSpiralByRect.Create(FSessionRect);

    FTaskArrayLock.Acquire;
    try
      for I := 0 to Length(FTaskArray) - 1 do begin
        case FTaskArray[I].State of
          tsMissed: begin
            FTaskArray[I].State := tsNone;
            FTaskArray[I].Version := nil;
            Dec(FMissedTaskCount);
          end;
          tsActive: begin
            if (FTaskArray[I].Zoom = FSessionZoom) and
               IsEqualVersionInfo(FTaskArray[I].Version, FSessionVersionInfo) and
               IsPointInRect(FTaskArray[I].Tile, ARect) then
            begin
              FTaskArray[I].State := tsActiveWaiting;
              FTaskArray[I].SessionID := VSessionID;
              Inc(FActiveWaitingTaskCount);
            end;
          end;
          tsActiveWaiting: begin
            if (FTaskArray[I].Zoom = FSessionZoom) and
               IsEqualVersionInfo(FTaskArray[I].Version, FSessionVersionInfo) and
               IsPointInRect(FTaskArray[I].Tile, ARect) then
            begin
              FTaskArray[I].SessionID := VSessionID;
            end else begin
              FTaskArray[I].State := tsActive;
              Dec(FActiveWaitingTaskCount);
            end;
          end;
        end;
      end;

      Assert(FMissedTaskCount = 0);

      if FActiveWaitingTaskCount > 0 then begin
        ResetEvent(FMissedTaskEventHandle);
      end else begin
        SetEvent(FMissedTaskEventHandle);
      end;
    finally
      FTaskArrayLock.Release;
    end;

  finally
    FSessionLock.Release;
  end;

  ResetEvent(FCancelEventHandle);
end;

function TUiTileRequestManager.AcquireByIterator(
  out ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const AIterator: ITileIterator;
  const ARect: TRect;
  const ASessionID: Integer
): Boolean;
var
  I: Integer;
  VState: TTaskState;
  VEmptyItemIndex: Integer;
begin
  Result := False;

  while IsValidSession(ASessionID) and AIterator.Next(ATile) do begin

    VState := tsNone;
    VEmptyItemIndex := -1;

    FTaskArrayLock.Acquire;
    try
      for I := 0 to Length(FTaskArray) - 1 do begin
        if not IsValidSession(ASessionID) then begin
          Exit;
        end;

        if FTaskArray[I].State = tsNone then
        begin
          if VEmptyItemIndex = -1 then begin
            VEmptyItemIndex := I;
          end;
        end
        else
        if (FTaskArray[I].Zoom = AZoom) and
           (FTaskArray[I].Tile.X = ATile.X) and
           (FTaskArray[I].Tile.Y = ATile.Y) and
           IsEqualVersionInfo(FTaskArray[I].Version, AVersionInfo) then
        begin
          case FTaskArray[I].State of
            tsActive: begin
              Assert(False);
            end;
            tsActiveWaiting: begin
              Assert(FTaskArray[I].SessionID = ASessionID);
            end;
            tsMissed: begin
              Assert(FTaskArray[I].SessionID = ASessionID);
              FTaskArray[I].State := tsActive;
              Dec(FMissedTaskCount);
            end;
          end;
          VState := FTaskArray[I].State;
          Break;
        end;
      end; // for

      if (VState = tsNone) and IsValidSession(ASessionID) then begin
        I := VEmptyItemIndex;
        Assert(I <> -1);

        FTaskArray[I].State := tsActive;
        FTaskArray[I].SessionID := ASessionID;
        FTaskArray[I].Zoom := AZoom;
        FTaskArray[I].Tile.X := ATile.X;
        FTaskArray[I].Tile.Y := ATile.Y;
        FTaskArray[I].Version := AVersionInfo;

        VState := FTaskArray[I].State;
      end;
    finally
      FTaskArrayLock.Release;
    end;

    if VState = tsActive then begin
      Result := True;
      Break;
    end;
  end; // while
end;

function TUiTileRequestManager.AcquireByMissedTasks(
  out ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ASessionID: Integer
): Boolean;
var
  I: Integer;
  VWaitResult: DWORD;
  VHandles: array [0..1] of THandle;
begin
  Result := False;

  if not IsValidSession(ASessionID) then begin
    Exit;
  end;

  VHandles[0] := FMissedTaskEventHandle;
  VHandles[1] := FCancelEventHandle;

  VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);

  case VWaitResult of
    WAIT_OBJECT_0: begin
      FTaskArrayLock.Acquire;
      try
        if FMissedTaskCount = 0 then begin
          Exit;
        end;

        for I := 0 to Length(FTaskArray) - 1 do begin
          if not IsValidSession(ASessionID) then begin
            Exit;
          end;
          if FTaskArray[I].State = tsMissed then begin
            Assert(FTaskArray[I].SessionID = ASessionID);
            Assert(FTaskArray[I].Zoom = AZoom);
            Assert(IsEqualVersionInfo(FTaskArray[I].Version, AVersionInfo) );

            ATile := FTaskArray[I].Tile;
            FTaskArray[I].State := tsActive;

            Dec(FMissedTaskCount);

            Result := True;
            Break;
          end;
        end;

        SetEvent(FMissedTaskEventHandle);

        {$IFDEF DEBUG}
        if not Result then begin
          for I := 0 to Length(FTaskArray) - 1 do begin
            Assert(FTaskArray[I].State in [tsNone, tsActive]);
          end;
        end;
        {$ENDIF}

      finally
        FTaskArrayLock.Release;
      end;
    end;
  end;
end;

function TUiTileRequestManager.Acquire(out ATile: TPoint): Boolean;
var
  VIterator: ITileIterator;
  VSessionID: Integer;
  VZoom: Byte;
  VRect: TRect;
  VVersion: IMapVersionInfo;
  VWaitResult: DWORD;
  VHandles: array [0..1] of THandle;
begin
  Result := False;

  VHandles[0] := FConcurrentCoutLimiter;
  VHandles[1] := FCancelEventHandle;

  VWaitResult := WaitForMultipleObjects(Length(VHandles), @VHandles[0], False, INFINITE);

  case VWaitResult of
    WAIT_OBJECT_0:
    try
      FSessionLock.Acquire;
      try
        VSessionID := InterlockedCompareExchange(FSessionID, 0, 0);
        VZoom := FSessionZoom;
        VRect := FSessionRect;
        VVersion := FSessionVersionInfo;
        VIterator := FSessionTileIterator;
      finally
        FSessionLock.Release;
      end;

      if Assigned(VIterator) and IsValidSession(VSessionID) then begin
        Result := AcquireByIterator(ATile, VZoom, VVersion, VIterator, VRect, VSessionID);
        if not Result then begin
          Result := AcquireByMissedTasks(ATile, VZoom, VVersion, VSessionID);
        end;
      end;
    finally
      if not Result then begin
        ReleaseSemaphore(FConcurrentCoutLimiter, 1, nil);
      end;
    end;
  end;
end;

procedure TUiTileRequestManager.Release(
  const ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo;
  const ACanceled: Boolean
);
var
  I: Integer;
  VResult: Boolean;
begin
  try
    VResult := False;

    FTaskArrayLock.Acquire;
    try
      for I := 0 to Length(FTaskArray) - 1 do begin
        if
          (FTaskArray[I].State in [tsActive, tsActiveWaiting]) and
          (FTaskArray[I].Zoom = AZoom) and
          (FTaskArray[I].Tile.X = ATile.X) and
          (FTaskArray[I].Tile.Y = ATile.Y) and
          IsEqualVersionInfo(FTaskArray[I].Version, AVersionInfo) then
        begin
          if FTaskArray[I].State = tsActiveWaiting then begin
            if ACanceled then begin
              FTaskArray[I].State := tsMissed;
              Inc(FMissedTaskCount);
            end;
            Dec(FActiveWaitingTaskCount);
          end;

          if FTaskArray[I].State <> tsMissed then begin
            FTaskArray[I].State := tsNone;
            FTaskArray[I].Version := nil;
          end;

          if FActiveWaitingTaskCount = 0 then begin
            SetEvent(FMissedTaskEventHandle);
          end;

          VResult := True;
          Break;
        end;
      end;
    finally
      FTaskArrayLock.Release;
    end;

    Assert(VResult);
  finally
    ReleaseSemaphore(FConcurrentCoutLimiter, 1, nil);
  end;
end;

end.
