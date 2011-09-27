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

unit i_Sensor;

interface

uses
  GR32,
  i_JclNotify,
  i_ConfigDataElement;

type
  ISensor = interface
    ['{EFD30054-5F65-49DF-8EB9-A4EF816D05D2}']
    function CanReset: Boolean;
    procedure Reset;
    function GetDataUpdateNotifier: IJclNotifier;
  end;

  ISensorText = interface(ISensor)
  ['{9FBEF687-7C1E-4BA6-85D7-ECD16E2F1A7A}']
    function GetText: string;
  end;

  ISensorBitmap = interface(ISensor)
  ['{6A1BB26A-13DE-4533-BA3F-188769BF71D6}']
    procedure GetBitmap(ATarget: TCustomBitmap32);
  end;

  ISensorViewConfig = interface(IConfigDataElement)
    ['{ABA124E3-376F-495E-982C-F3D27F48F610}']
    function GetVisible: Boolean;
    procedure SetVisible(AValue: Boolean);
    property Visible: Boolean read GetVisible write SetVisible;
  end;

  ISensorView = interface
    ['{3D7823AF-17D9-495E-901C-BF6435E5C0E1}']
    function GetConfig: ISensorViewConfig;
    function GetSensor: ISensor;
  end;

implementation

end.
