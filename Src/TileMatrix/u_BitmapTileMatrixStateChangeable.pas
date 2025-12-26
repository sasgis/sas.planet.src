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

unit u_BitmapTileMatrixStateChangeable;

interface

{$I DebugLog.inc}

uses
  SysUtils,
  i_Listener,
  i_BitmapTileMatrixChangeable,
  u_ChangeableBase;

type
  TBitmapTileMatrixStateChangeableArray = array of IBitmapTileMatrixStateChangeable;

  TBitmapTileMatrixStateChangeable = class(
    TChangeableBase,
    IBitmapTileMatrixStateChangeable,
    IBitmapTileMatrixStateChangeableInternal
  )
  private
    FState: TBitmapTileMatrixPrepareState;
    FLock: IReadWriteSync;
    FDebugName: string;
    FSourceState: TBitmapTileMatrixStateChangeableArray;
    FIsComposite: Boolean;
    FListener: IListener;
    function IsAllSourcesComplete: Boolean;
    procedure OnSourceStateChange;
  private
    function GetState: TBitmapTileMatrixPrepareState;
    procedure SetState(AState: TBitmapTileMatrixPrepareState);
  public
    constructor Create(
      const ADebugName: string;
      const ASourceState: TBitmapTileMatrixStateChangeableArray = nil
    );
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
  Rtti,
  u_DebugLogger,
  {$ENDIF}
  u_ListenerByEvent,
  u_Synchronizer;

{ TBitmapTileMatrixStateChangeable }

constructor TBitmapTileMatrixStateChangeable.Create(
  const ADebugName: string;
  const ASourceState: TBitmapTileMatrixStateChangeableArray
);
var
  I: Integer;
begin
  inherited Create(GSync.SyncStd.Make(Self.ClassName + 'Notifiers'));
  FLock := GSync.SyncVariable.Make(Self.ClassName + 'Lock');
  FState := psNone;
  FDebugName := ADebugName;
  FSourceState := ASourceState;
  FIsComposite := Length(FSourceState) > 0;

  if FIsComposite then begin
    FListener := TNotifyNoMmgEventListener.Create(Self.OnSourceStateChange);
    for I := 0 to Length(FSourceState) - 1 do begin
      FSourceState[I].ChangeNotifier.Add(FListener);
    end;
  end;
end;

destructor TBitmapTileMatrixStateChangeable.Destroy;
var
  I: Integer;
begin
  if FListener <> nil then begin
    for I := 0 to Length(FSourceState) - 1 do begin
      FSourceState[I].ChangeNotifier.Remove(FListener);
    end;
    FListener := nil;
  end;
  inherited Destroy;
end;

function TBitmapTileMatrixStateChangeable.IsAllSourcesComplete: Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(FSourceState) - 1 do begin
    if not (FSourceState[I].State in [psNone, psComplete]) then begin
      Result := False;
      Exit;
    end;
  end;
  Result := True;
end;

procedure TBitmapTileMatrixStateChangeable.OnSourceStateChange;
var
  VDoNotify: Boolean;
begin
  VDoNotify := False;

  FLock.BeginWrite;
  try
    if (FState = psWaiting) and IsAllSourcesComplete then begin
      FState := psComplete;
      VDoNotify := True;
      {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
      GLog.Write(Self, '%s: State: %s', [FDebugName, TRttiEnumerationType.GetName(FState)]);
      {$ENDIF}
    end;
  finally
    FLock.EndWrite;
  end;

  if VDoNotify then begin
    DoChangeNotify;
  end;
end;

function TBitmapTileMatrixStateChangeable.GetState: TBitmapTileMatrixPrepareState;
begin
  FLock.BeginRead;
  try
    Result := FState;
  finally
    FLock.EndRead;
  end;
end;

procedure TBitmapTileMatrixStateChangeable.SetState(AState: TBitmapTileMatrixPrepareState);
var
  VDoNotify: Boolean;
begin
  FLock.BeginWrite;
  try
    if FIsComposite and (AState = psComplete) and not IsAllSourcesComplete then begin
      AState := psWaiting;
    end;

    VDoNotify := AState <> FState;

    if VDoNotify then begin
      FState := AState;
      {$IFDEF ENABLE_BITMAP_TILE_MATRIX_LOGGING}
      GLog.Write(Self, '%s: State: %s', [FDebugName, TRttiEnumerationType.GetName(FState)]);
      {$ENDIF}
    end;
  finally
    FLock.EndWrite;
  end;

  if VDoNotify then begin
    DoChangeNotify;
  end;
end;

end.
