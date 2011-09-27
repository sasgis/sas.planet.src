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

unit i_DownloadUIConfig;

interface

uses
  t_CommonTypes,
  i_ConfigDataElement;

type
  IDownloadUIConfig = interface(IConfigDataElement)
    ['{CED08DA4-F287-49A5-9FF2-C7959F1712F5}']
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(const AValue: TTileSource);
    property UseDownload: TTileSource read GetUseDownload write SetUseDownload;

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(const AValue: TDateTime);
    property TileMaxAgeInInternet: TDateTime read GetTileMaxAgeInInternet write SetTileMaxAgeInInternet;

    function GetTilesOut: Integer;
    procedure SetTilesOut(const AValue: Integer);
    property TilesOut: Integer read GetTilesOut write SetTilesOut;
  end;


implementation

end.
