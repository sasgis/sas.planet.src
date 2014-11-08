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

unit u_ReadWriteStateInternalByOther;

interface

uses
  i_ReadWriteState,
  i_Listener,
  u_ConfigDataElementBase;

type
  IReadWriteStateInternalByOther = interface(IReadWriteStateChangeble)
    procedure SetOther(const AState: IReadWriteStateChangeble);
  end;

  TReadWriteStateInternalByOther = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IReadWriteStateChangeble, IReadWriteStateInternalByOther)
  private
    FOtherState: IReadWriteStateChangeble;
    FDefault: IReadWriteStateStatic;
    FOtherStateListener: IListener;
    procedure OnOtherStateChange;
  protected
    function CreateStatic: IInterface; override;
  private
    procedure SetOther(const AState: IReadWriteStateChangeble);
  private
    function GetStatic: IReadWriteStateStatic;
  public
    constructor Create();
    destructor Destroy; override;
  end;

implementation

uses
  t_CommonTypes,
  u_ListenerByEvent,
  u_ReadWriteStateStatic;

{ TReadWriteStateInternalByOther }

constructor TReadWriteStateInternalByOther.Create;
begin
  inherited Create;
  FDefault := TReadWriteStateStatic.Create(asDisabled, asDisabled);
  FOtherStateListener := TNotifyNoMmgEventListener.Create(Self.OnOtherStateChange);
  FOtherState := nil;
end;

destructor TReadWriteStateInternalByOther.Destroy;
begin
  if Assigned(FOtherState) and Assigned(FOtherStateListener) then begin
    FOtherState.ChangeNotifier.Remove(FOtherStateListener);
    FOtherState := nil;
  end;
  inherited;
end;

function TReadWriteStateInternalByOther.CreateStatic: IInterface;
begin
  LockRead;
  try
    if FOtherState <> nil then begin
      Result := FOtherState.GetStatic;
    end else begin
      Result := FDefault;
    end;
  finally
    UnlockRead;
  end;
end;

function TReadWriteStateInternalByOther.GetStatic: IReadWriteStateStatic;
begin
  Result := IReadWriteStateStatic(GetStaticInternal);
end;

procedure TReadWriteStateInternalByOther.OnOtherStateChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TReadWriteStateInternalByOther.SetOther(
  const AState: IReadWriteStateChangeble);
begin
  LockWrite;
  try
    if FOtherState <> AState then begin
      if FOtherState <> nil then begin
        FOtherState.ChangeNotifier.Remove(FOtherStateListener);
      end;
      FOtherState := AState;
      if FOtherState <> nil then begin
        FOtherState.ChangeNotifier.Add(FOtherStateListener);
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
