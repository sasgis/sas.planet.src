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

unit u_NotifyEventListener;

interface

uses
  Windows,
  Classes,
  i_JclNotify,
  u_JclNotify;

type
  TNotifyListenerNoMmgEvent = procedure of object;
  TNotifyListenerEvent = procedure(const AMsg: IInterface) of object;

  TNotifyEventListener = class(TJclBaseListener, IJclListenerDisconnectable)
  private
    FDisconnectCount: Integer;
    FEvent: TNotifyListenerEvent;
  protected
    procedure Notification(const AMsg: IInterface); override;
  protected
    procedure Disconnect; stdcall;
  public
    constructor Create(AEvent: TNotifyListenerEvent);
  end;

  TNotifyNoMmgEventListener = class(TJclBaseListener)
  private
    FEvent: TNotifyListenerNoMmgEvent;
  protected
    procedure Notification(const AMsg: IInterface); override;
  public
    constructor Create(AEvent: TNotifyListenerNoMmgEvent);
  end;

  TNotifyEventListenerSync = class(TJclBaseListener)
  private
    FEvent: TNotifyListenerNoMmgEvent;
    procedure DoEvent;
  protected
    procedure Notification(const AMsg: IInterface); override;
  public
    constructor Create(AEvent: TNotifyListenerNoMmgEvent);
  end;

implementation

{ TSimpleEventListener }

constructor TNotifyEventListener.Create(AEvent: TNotifyListenerEvent);
begin
  inherited Create;
  FEvent := AEvent;
  FDisconnectCount := 0;
  Assert(Assigned(FEvent));
end;

procedure TNotifyEventListener.Disconnect;
begin
  InterlockedIncrement(FDisconnectCount);
end;

procedure TNotifyEventListener.Notification(const AMsg: IInterface);
begin
  inherited;
  if InterlockedCompareExchange(FDisconnectCount, 0, 0) = 0 then begin
    FEvent(AMsg);
  end;
end;

{ TNotifyEventListenerSync }

constructor TNotifyEventListenerSync.Create(AEvent: TNotifyListenerNoMmgEvent);
begin
  inherited Create;
  FEvent := AEvent;
  Assert(Assigned(FEvent));
end;

procedure TNotifyEventListenerSync.DoEvent;
begin
  FEvent;
end;

procedure TNotifyEventListenerSync.Notification(const AMsg: IInterface);
begin
  inherited;
  TThread.Synchronize(nil, DoEvent);
end;

{ TNotifyNoMmgEventListener }

constructor TNotifyNoMmgEventListener.Create(AEvent: TNotifyListenerNoMmgEvent);
begin
  inherited Create;
  FEvent := AEvent;
  Assert(Assigned(FEvent));
end;

procedure TNotifyNoMmgEventListener.Notification(const AMsg: IInterface);
begin
  FEvent;
end;

end.
