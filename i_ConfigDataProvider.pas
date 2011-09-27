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

unit i_ConfigDataProvider;

interface

uses
  Classes;

type
  IConfigDataProvider = interface
    ['{FB657238-6D8F-463D-B56F-3FB4C74EE352}']
    function GetSubItem(const AIdent: string): IConfigDataProvider;
    function ReadBinaryStream(const AIdent: string; AValue: TStream): Integer;
    function ReadString(const AIdent: string; const ADefault: string): string;
    function ReadInteger(const AIdent: string; const ADefault: Longint): Longint;
    function ReadBool(const AIdent: string; const ADefault: Boolean): Boolean;
    function ReadDate(const AIdent: string; const ADefault: TDateTime): TDateTime;
    function ReadDateTime(const AIdent: string; const ADefault: TDateTime): TDateTime;
    function ReadFloat(const AIdent: string; const ADefault: Double): Double;
    function ReadTime(const AIdent: string; const ADefault: TDateTime): TDateTime;

    procedure ReadSubItemsList(AList: TStrings);
    procedure ReadValuesList(AList: TStrings);
  end;

implementation

end.
