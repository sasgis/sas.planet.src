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

unit u_Notifier;

interface

uses
  Classes,
  SysUtils,
  i_Notifier,
  i_Listener,
  u_BaseInterfacedObject;

type
  TNotifierBase = class (TBaseInterfacedObject, INotifier, INotifierInternal)
  private
    FListeners: TList;
    FSync: IReadWriteSync;
  private
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  private
    procedure Notify(const AMsg: IInterface);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TNotifierFaked = class (TBaseInterfacedObject, INotifier, INotifierInternal)
  private
    procedure Add(const AListener: IListener);
    procedure Remove(const AListener: IListener);
  private
    procedure Notify(const AMsg: IInterface);
  end;

implementation

uses
  u_Synchronizer;

{ TNotifierBase }

constructor TNotifierBase.Create;
begin
  inherited Create;
  FListeners := TList.Create;
  FSync := GSync.SyncVariable.Make(Self.ClassName);
end;

destructor TNotifierBase.Destroy;
var
  i: integer;
begin
  if Assigned(FListeners) then begin
    for i := 0 to FListeners.Count - 1 do begin
      IInterface(FListeners.Items[i])._Release;
    end;
  end;
  FreeAndNil(FListeners);
  inherited;
end;

procedure TNotifierBase.Add(const AListener: IListener);
begin
  FSync.BeginWrite;
  try
    AListener._AddRef;
    FListeners.Add(Pointer(AListener));
  finally
    FSync.EndWrite;
  end;
end;

procedure TNotifierBase.Notify(const AMsg: IInterface);
var
  idx: Integer;
  VList: array of IListener;
begin
  FSync.BeginRead;
  try
    SetLength(VList, FListeners.Count);
    for idx := 0 to FListeners.Count - 1 do
      VList[idx] := IListener(Pointer(FListeners[idx]));
  finally
    FSync.EndRead;
  end;
  for idx := 0 to Length(VList) - 1 do begin
    VList[idx].Notification(AMsg);
    VList[idx] := nil;
  end;
  VList := nil;
end;

procedure TNotifierBase.Remove(const AListener: IListener);
var
  idx: Integer;
  VLastIndex: Integer;
begin
  FSync.BeginWrite;
  try
    idx := FListeners.IndexOf(Pointer(AListener));
    if idx >= 0 then begin
      VLastIndex := FListeners.Count - 1;
      if idx < VLastIndex then begin
        FListeners[idx] :=  FListeners[VLastIndex];
      end;
      FListeners.Delete(VLastIndex);
      AListener._Release;
    end;
  finally
    FSync.EndWrite;
  end;
end;

{ TNotifierFaked }

procedure TNotifierFaked.Add(const AListener: IListener);
begin
  // do nothing;
end;

procedure TNotifierFaked.Notify(const AMsg: IInterface);
begin
  // do nothing;
end;

procedure TNotifierFaked.Remove(const AListener: IListener);
begin
  // do nothing;
end;

end.







