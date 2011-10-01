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

unit u_NotifyEventListener;

interface

uses
  Classes,
  i_JclNotify,
  u_JclNotify;

type
  TNotifyEventListenerBase = class(TJclBaseListener)
  private
    FEvent: TNotifyEvent;
    FSender: TObject;
  protected
    procedure DoEvent; virtual;
  public
    constructor Create(AEvent: TNotifyEvent; ASender: TObject = nil);
  end;

  TNotifyEventListener = class(TNotifyEventListenerBase)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

  TNotifyEventListenerSync = class(TNotifyEventListenerBase)
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  end;

implementation

{ TSimpleEventListenerBase }

constructor TNotifyEventListenerBase.Create(AEvent: TNotifyEvent; ASender: TObject);
begin
  FSender := ASender;
  FEvent := AEvent;
end;

procedure TNotifyEventListenerBase.DoEvent;
begin
  if Assigned(FEvent) then begin
    FEvent(FSender);
  end;
end;

{ TSimpleEventListener }

procedure TNotifyEventListener.Notification(msg: IJclNotificationMessage);
begin
  inherited;
  DoEvent;
end;

{ TNotifyEventListenerSync }

procedure TNotifyEventListenerSync.Notification(msg: IJclNotificationMessage);
begin
  inherited;
  TThread.Synchronize(nil, DoEvent);
end;

end.
