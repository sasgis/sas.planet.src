{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
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
