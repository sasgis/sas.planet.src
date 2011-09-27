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

unit u_NotifyWithGUIDEvent;

interface

uses
  i_JclNotify,
  u_JclNotify;

type
  TNotifyWithGUIDEvent = procedure(const AGUID: TGUID) of object;

type
  INotificationMessageWithGUID = interface(IJclNotificationMessage)
    ['{1C88C4F8-5990-42DA-A841-CB9597C97AB6}']
    function GetGUID: TGUID;
  end;

type
  TNotificationMessageWithGUID = class(TJclBaseNotificationMessage, INotificationMessageWithGUID)
  private
    FGUID: TGUID;
  protected
    function GetGUID: TGUID;
  public
    constructor Create(AGUID: TGUID);
  end;

type
  TNotifyWithGUIDEventListener =  class(TJclBaseListener)
  private
    FEvent: TNotifyWithGUIDEvent;
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TNotifyWithGUIDEvent);
  end;

type
  INotifierWithGUID = interface(IJclNotifier)
    ['{7160ECC8-5A85-445C-8655-5E5574E60C88}']
    procedure NotifyByGUID(const AGUID: TGUID); stdcall;
  end;

type
  TNotifierWithGUID = class (TJclBaseNotifier, INotifierWithGUID)
  protected
    procedure NotifyByGUID(const AGUID: TGUID); stdcall;
  end;

implementation

{ TNotificationMessageWithGUID }

constructor TNotificationMessageWithGUID.Create(AGUID: TGUID);
begin
  FGUID := AGUID;
end;

function TNotificationMessageWithGUID.GetGUID: TGUID;
begin
  Result := FGUID;
end;

{ TNotifyWithGUIDEventListener }

constructor TNotifyWithGUIDEventListener.Create(AEvent: TNotifyWithGUIDEvent);
begin
  FEvent := AEvent;
end;

procedure TNotifyWithGUIDEventListener.Notification(
  msg: IJclNotificationMessage);
begin
  FEvent(INotificationMessageWithGUID(msg).GetGUID);
end;

{ TNotifierWithGUID }

procedure TNotifierWithGUID.NotifyByGUID(const AGUID: TGUID);
var
  msg: INotificationMessageWithGUID;
begin
  msg := TNotificationMessageWithGUID.Create(AGUID);
  Notify(msg);
end;

end.
