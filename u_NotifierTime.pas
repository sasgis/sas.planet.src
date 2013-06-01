{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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

unit u_NotifierTime;

interface

uses
  Classes,
  SysUtils,
  i_ListenerTime,
  i_NotifierTime,
  u_BaseInterfacedObject;

type
  TNotifierTime = class(TBaseInterfacedObject, INotifierTime, INotifierTimeInternal)
  private
    FListeners: TList;
    FSync: IReadWriteSync;
  private
    procedure Add(const AListener: IListenerTime);
    procedure Remove(const AListener: IListenerTime);
    procedure Notify(const ANow: Cardinal);
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer;

{ TNotifierTime }

constructor TNotifierTime.Create;
begin
  inherited Create;
  FSync := MakeSyncRW_Big(Self, False);
  FListeners := TList.Create;
end;

destructor TNotifierTime.Destroy;
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

procedure TNotifierTime.Add(const AListener: IListenerTime);
begin
  FSync.BeginWrite;
  try
    AListener._AddRef;
    FListeners.Add(Pointer(AListener));
  finally
    FSync.EndWrite;
  end;
end;

procedure TNotifierTime.Notify(const ANow: Cardinal);
var
  i: integer;
  VList: array of IListenerTime;
begin
  FSync.BeginRead;
  try
    SetLength(VList, FListeners.Count);
    for i := 0 to FListeners.Count - 1 do begin
      VList[i] := IListenerTime(Pointer(FListeners[i]));

    end;
  finally
    FSync.EndRead;
  end;
  for i := 0 to Length(VList) - 1 do begin
    VList[i].Notification(ANow);
  end;
end;

procedure TNotifierTime.Remove(const AListener: IListenerTime);
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

end.

