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

unit i_MarksFactoryConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataElement,
  i_MarkPicture,
  i_MarkCategory,
  i_MarkNameGenerator,
  i_MarkTemplate;

type
  IMarkPointTemplateConfig = interface(IConfigDataElement)
    ['{B796934A-83FE-4E8A-B69D-11237690AA23}']
    function CreateTemplate(
      APic: IMarkPicture;
      ACategory: ICategory;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkTemplatePoint;

    function GetMarkPictureList: IMarkPictureList;
    property MarkPictureList: IMarkPictureList read GetMarkPictureList;

    function GetDefaultTemplate: IMarkTemplatePoint;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoint);
    property DefaultTemplate: IMarkTemplatePoint read GetDefaultTemplate write SetDefaultTemplate;

    function GetNameGenerator: IMarkNameGenerator;
  end;

  IMarkLineTemplateConfig = interface(IConfigDataElement)
    ['{0F7596F4-1BA2-4581-9509-77627F50B1AF}']
    function CreateTemplate(
      ACategory: ICategory;
      AColor1: TColor32;
      AScale1: Integer
    ): IMarkTemplateLine;

    function GetDefaultTemplate: IMarkTemplateLine;
    procedure SetDefaultTemplate(AValue: IMarkTemplateLine);
    property DefaultTemplate: IMarkTemplateLine read GetDefaultTemplate write SetDefaultTemplate;

    function GetNameGenerator: IMarkNameGenerator;
  end;

  IMarkPolyTemplateConfig = interface(IConfigDataElement)
    ['{149D8DC1-7848-4D34-ABCA-2B7F8D3A22EF}']
    function CreateTemplate(
      ACategory: ICategory;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer
    ): IMarkTemplatePoly;

    function GetDefaultTemplate: IMarkTemplatePoly;
    procedure SetDefaultTemplate(AValue: IMarkTemplatePoly);
    property TemplateDefault: IMarkTemplatePoly read GetDefaultTemplate write SetDefaultTemplate;

    function GetNameGenerator: IMarkNameGenerator;
  end;


  IMarksFactoryConfig = interface(IConfigDataElement)
    ['{9CC0FDE0-44B2-443D-8856-ED7263F0F8BF}']
    function GetPointTemplateConfig: IMarkPointTemplateConfig;
    property PointTemplateConfig: IMarkPointTemplateConfig read GetPointTemplateConfig;

    function GetLineTemplateConfig: IMarkLineTemplateConfig;
    property LineTemplateConfig: IMarkLineTemplateConfig read GetLineTemplateConfig;

    function GetPolyTemplateConfig: IMarkPolyTemplateConfig;
    property PolyTemplateConfig: IMarkPolyTemplateConfig read GetPolyTemplateConfig;
  end;

implementation

end.
