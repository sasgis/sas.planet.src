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

unit i_GlobalDownloadConfig;

interface

uses
  i_ConfigDataElement;

type
  IGlobalDownloadConfig = interface(IConfigDataElement)
    ['{66442801-51C5-43DF-AB59-747E058A3567}']
    // Переходить к следующему тайлу если произошла ошибка закачки
    function GetIsGoNextTileIfDownloadError: Boolean;
    procedure SetIsGoNextTileIfDownloadError(AValue: Boolean);
    property IsGoNextTileIfDownloadError: Boolean read GetIsGoNextTileIfDownloadError write SetIsGoNextTileIfDownloadError;

    //Начать сохраненную сессию загрузки с последнего удачно загруженного тайла
    function GetIsUseSessionLastSuccess: Boolean;
    procedure SetIsUseSessionLastSuccess(AValue: Boolean);
    property IsUseSessionLastSuccess: Boolean read GetIsUseSessionLastSuccess write SetIsUseSessionLastSuccess;

    //Записывать информацию о тайлах отсутствующих на сервере
    function GetIsSaveTileNotExists: Boolean;
    procedure SetIsSaveTileNotExists(AValue: Boolean);
    property IsSaveTileNotExists: Boolean read GetIsSaveTileNotExists write SetIsSaveTileNotExists;
  end;

implementation

end.
