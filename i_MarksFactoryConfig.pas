unit i_MarksFactoryConfig;

interface

uses
  Classes,
  GR32,
  i_ConfigDataElement,
  i_MarkPicture,
  i_MarkCategory,
  i_MarkNameGenerator,
  i_MarksSimple;

type
  IMarkPointTemplateConfig = interface(IConfigDataElement)
    ['{B796934A-83FE-4E8A-B69D-11237690AA23}']
    function CreateTemplate(
      APicName: string;
      APic: IMarkPicture;
      ACategory: IMarkCategory;
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
      ACategory: IMarkCategory;
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
      ACategory: IMarkCategory;
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
