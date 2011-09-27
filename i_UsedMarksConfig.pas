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

unit i_UsedMarksConfig;

interface

uses
  i_ConfigDataElement;

type
  IUsedMarksConfigStatic = interface
  ['{77A54AD4-2B5B-48CE-BD5F-1F4B89763FF2}']
    function GetIsUseMarks: Boolean;
    property IsUseMarks: Boolean read GetIsUseMarks;
    function GetIgnoreCategoriesVisible: Boolean;
    property IgnoreCategoriesVisible: Boolean read GetIgnoreCategoriesVisible;
    function GetIgnoreMarksVisible: Boolean;
    property IgnoreMarksVisible: Boolean read GetIgnoreMarksVisible;
  end;

  IUsedMarksConfig = interface(IConfigDataElement)
    ['{5E07CEB8-C461-4994-A2CD-3A38269060F9}']
    function GetIsUseMarks: Boolean;
    procedure SetIsUseMarks(AValue: Boolean);
    property IsUseMarks: Boolean read GetIsUseMarks write SetIsUseMarks;

    function GetIgnoreCategoriesVisible: Boolean;
    procedure SetIgnoreCategoriesVisible(AValue: Boolean);
    property IgnoreCategoriesVisible: Boolean read GetIgnoreCategoriesVisible write SetIgnoreCategoriesVisible;

    function GetIgnoreMarksVisible: Boolean;
    procedure SetIgnoreMarksVisible(AValue: Boolean);
    property IgnoreMarksVisible: Boolean read GetIgnoreMarksVisible write SetIgnoreMarksVisible;

    function GetStatic: IUsedMarksConfigStatic;
  end;

implementation

end.
