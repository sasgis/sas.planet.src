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

unit u_TileStorageTypeListItem;

interface

uses
  i_TileStorageType,
  i_TileStorageTypeListItem;

type
  TTileStorageTypeListItem = class(TInterfacedObject, ITileStorageTypeListItem)
  private
    FGUID: TGUID;
    FStorageType: ITileStorageType;
    FCanUseAsDefault: Boolean;
  protected
    function GetGUID: TGUID;
    function GetStorageType: ITileStorageType;
    function GetCanUseAsDefault: Boolean;
  public
    constructor Create(
      AGUID: TGUID;
      AStorageType: ITileStorageType;
      ACanUseAsDefault: Boolean
    );
  end;

implementation

{ TTileStorageTypeListItem }

constructor TTileStorageTypeListItem.Create(
  AGUID: TGUID;
  AStorageType: ITileStorageType;
  ACanUseAsDefault: Boolean
);
begin
  FGUID := AGUID;
  FStorageType := AStorageType;
  FCanUseAsDefault := ACanUseAsDefault;
end;

function TTileStorageTypeListItem.GetCanUseAsDefault: Boolean;
begin
  Result := FCanUseAsDefault;
end;

function TTileStorageTypeListItem.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeListItem.GetStorageType: ITileStorageType;
begin
  Result := FStorageType;
end;

end.
