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

unit u_EnumUnknownEmpty;

interface

uses
  ActiveX;

type
  TEmptyEnum = class(TInterfacedObject, IEnumUnknown)
    function Next(celt: Longint; out elt;
      pceltFetched: PLongint): HResult; stdcall;
    function Skip(celt: Longint): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out enm: IEnumUnknown): HResult; stdcall;
  end;

implementation

{ TEmptyEnum }

function TEmptyEnum.Clone(out enm: IEnumUnknown): HResult;
begin
  enm := Self;
  Result := S_OK;
end;

function TEmptyEnum.Next(celt: Integer; out elt;
  pceltFetched: PLongint): HResult;
begin
  pceltFetched^ := 0;
  Result := S_FALSE;
end;

function TEmptyEnum.Reset: HResult;
begin
  Result := S_OK;
end;

function TEmptyEnum.Skip(celt: Integer): HResult;
begin
  Result := S_FALSE;
end;

end.
