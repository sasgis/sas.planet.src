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

unit u_TileStorageTypeInfo;

interface

uses
  i_TileStorageTypeInfo;

type
  TTileStorageTypeInfoFieFolder = class(TInterfacedObject, ITileStorageTypeInfo)
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;

  TTileStorageTypeGE = class(TInterfacedObject, ITileStorageTypeInfo)
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;

implementation

{ TTileStorageTypeInfoFieFolder }

function TTileStorageTypeInfoFieFolder.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowMultiRead: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowMultiWrite: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowSave: boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetIsFileCache: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetIsReadOnly: boolean;
begin
  Result := False;
end;

{ TTileStorageTypeGE }

function TTileStorageTypeGE.GetAllowDelete: boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowMultiRead: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowMultiWrite: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowSave: boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetIsFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetIsReadOnly: boolean;
begin
  Result := True;
end;

end.
