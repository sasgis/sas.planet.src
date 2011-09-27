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

unit u_NotifyEventPosChangeListener;

interface

uses
  i_JclNotify,
  u_JclNotify,
  i_LocalCoordConverter,
  i_PosChangeMessage;

type
  TPosChangeNotifyEvent = procedure(ANewConverter: ILocalCoordConverter) of object;

  TPosChangeNotifyEventListener = class(TJclBaseListener)
  private
    FEvent: TPosChangeNotifyEvent;
  protected
    procedure DoEvent(AMessage: IPosChangeMessage); virtual;
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TPosChangeNotifyEvent);
  end;

implementation

{ TPosChangeNotifyEventListener }

constructor TPosChangeNotifyEventListener.Create(AEvent: TPosChangeNotifyEvent);
begin
  FEvent := AEvent;
end;

procedure TPosChangeNotifyEventListener.DoEvent(AMessage: IPosChangeMessage);
begin
  if Assigned(FEvent) then begin
    FEvent(AMessage.GetVisualCoordConverter);
  end;
end;

procedure TPosChangeNotifyEventListener.Notification(
  msg: IJclNotificationMessage);
begin
  inherited;
  DoEvent(IPosChangeMessage(msg));
end;

end.
