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

unit u_PosChangeMessage;

interface

uses
  Types,
  u_JclNotify,
  i_LocalCoordConverter,
  i_PosChangeMessage;

type
  TPosChangeMessage = class(TJclBaseNotificationMessage, IPosChangeMessage)
  private
    FVisualCoordConverter: ILocalCoordConverter;
    function GetVisualCoordConverter: ILocalCoordConverter; stdcall;
  public
    constructor Create(
      AVisuzlCoordConverter: ILocalCoordConverter
    );
  end;

implementation

{ TPosChangeMessage }

constructor TPosChangeMessage.Create(
  AVisuzlCoordConverter: ILocalCoordConverter
);
begin
  FVisualCoordConverter := AVisuzlCoordConverter;
end;

function TPosChangeMessage.GetVisualCoordConverter: ILocalCoordConverter;
begin
  Result := FVisualCoordConverter;
end;

end.
 