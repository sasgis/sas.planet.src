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

unit u_TileStorageTypeBase;

interface

uses
  i_TileStorageTypeInfo,
  i_TileStorageTypeConfig,
  i_TileStorage,
  i_TileStorageType;

type
  TTileStorageTypeBase = class(TInterfacedObject, ITileStorageType)
  private
    FGUID: TGUID;
    FCaption: string;
    FInfo: ITileStorageTypeInfo;
    FConfig: ITileStorageTypeConfig;
  protected
    function GetGUID: TGUID;
    function GetInfo: ITileStorageTypeInfo;
    function GetConfig: ITileStorageTypeConfig;
    function BuildStorage(APath: string): ITileStorage; virtual; abstract;
    function GetCaption: string;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: string;
      AInfo: ITileStorageTypeInfo;
      AConfig: ITileStorageTypeConfig
    );
  end;

implementation

{ TTileStorageTypeBase }

constructor TTileStorageTypeBase.Create(
  AGUID: TGUID;
  ACaption: string;
  AInfo: ITileStorageTypeInfo;
  AConfig: ITileStorageTypeConfig
);
begin
  FGUID := AGUID;
  FCaption := ACaption;
  FInfo := AInfo;
  FConfig := AConfig;
end;

function TTileStorageTypeBase.GetCaption: string;
begin
  Result := FCaption;
end;

function TTileStorageTypeBase.GetConfig: ITileStorageTypeConfig;
begin
  Result := FConfig;
end;

function TTileStorageTypeBase.GetGUID: TGUID;
begin
  Result := FGUID;
end;

function TTileStorageTypeBase.GetInfo: ITileStorageTypeInfo;
begin
  Result := FInfo;
end;

end.
