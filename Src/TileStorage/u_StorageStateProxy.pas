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

unit u_StorageStateProxy;

interface

uses
  t_CommonTypes,
  i_Listener,
  i_StorageState,
  i_StorageStateProxy,
  u_ConfigDataElementBase;

type
  TStorageStateProxy = class(TConfigDataElementBaseEmptySaveLoad, IStorageStateProxy, IStorageStateChangeble)
  private
    FTarget: IStorageStateChangeble;
    FTargetChangeListener: IListener;
    FDisableStatic: IStorageStateStatic;

    FLastStatic: IStorageStateStatic;
    procedure OnTargetChange;
  private
    function GetTarget: IStorageStateChangeble;
    procedure SetTarget(const AValue: IStorageStateChangeble);

    function GetStatic: IStorageStateStatic;
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_StorageStateStatic;

{ TStorageStateProxy }

constructor TStorageStateProxy.Create;
begin
  inherited Create;
  FTarget := nil;
  FDisableStatic :=
    TStorageStateStatic.Create(
      asDisabled,
      asDisabled,
      asDisabled,
      asDisabled,
      asDisabled
    );
  FLastStatic := FDisableStatic;
  FTargetChangeListener := TNotifyNoMmgEventListener.Create(Self.OnTargetChange);
end;

destructor TStorageStateProxy.Destroy;
begin
  LockWrite;
  try
    if Assigned(FTarget) and Assigned(FTargetChangeListener) then begin
      FTarget.ChangeNotifier.Remove(FTargetChangeListener);
      FTarget := nil;
      FTargetChangeListener := nil;
    end;
  finally
    UnlockWrite;
  end;

  inherited;
end;

function TStorageStateProxy.GetStatic: IStorageStateStatic;
begin
  LockRead;
  try
    Result := FLastStatic;
  finally
    UnlockRead;
  end;
end;

function TStorageStateProxy.GetTarget: IStorageStateChangeble;
begin
  LockRead;
  try
    Result := FTarget;
  finally
    UnlockRead;
  end;
end;

procedure TStorageStateProxy.OnTargetChange;
var
  VState: IStorageStateStatic;
begin
  LockWrite;
  try
    VState := nil;
    if FTarget <> nil then begin
      VState := FTarget.GetStatic;
    end;
    if VState = nil then begin
      VState := FDisableStatic;
    end;
    if not FLastStatic.IsSame(VState) then begin
      FLastStatic := VState;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TStorageStateProxy.SetTarget(const AValue: IStorageStateChangeble);
begin
  LockWrite;
  try
    if FTarget <> AValue then begin
      if FTarget <> nil then begin
        FTarget.ChangeNotifier.Remove(FTargetChangeListener);
      end;
      FTarget := AValue;
      if FTarget <> nil then begin
        FTarget.ChangeNotifier.Add(FTargetChangeListener);
      end;
      OnTargetChange;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
