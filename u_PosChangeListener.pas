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

unit u_PosChangeListener;

interface

uses
  i_JclNotify,
  u_JclNotify,
  i_PosChangeMessage;

type
  TPosChangeEvent = procedure(AMessage: IPosChangeMessage) of object; 

type
  TPosChangeListener = class(TJclBaseListener)
  private
    FEvent: TPosChangeEvent;
  protected
    procedure Notification(msg: IJclNotificationMessage); override;
  public
    constructor Create(AEvent: TPosChangeEvent);
  end;

implementation

{ TPosChangeListener }

constructor TPosChangeListener.Create(AEvent: TPosChangeEvent);
begin
  FEvent := AEvent;
end;

procedure TPosChangeListener.Notification(msg: IJclNotificationMessage);
var
  VMessage: IPosChangeMessage;
begin
  if Assigned(FEvent) then begin
    if msg.QueryInterface(IPosChangeMessage, VMessage) = S_OK then begin
      FEvent(VMessage);
    end;
  end;
end;

end.
