unit i_MarkFactory;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkCategory,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarkTemplate,
  i_MarksSimple;

type
  IMarkFactory = interface
    ['{725CB1AC-1393-4889-B621-64C3B4348331}']
    function CreateNewPoint(
      APoint: TDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoint = nil
    ): IMarkFull;
    function CreateNewLine(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplateLine = nil
    ): IMarkFull;
    function CreateNewPoly(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoly = nil
    ): IMarkFull;

    function ModifyPoint(
      ASource: IMarkFull;
      AName: string;
      AVisible: Boolean;
      APic: IMarkPicture;
      ACategory: IMarkCategory;
      ADesc: string;
      APoint: TDoublePoint;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
    ): IMarkFull;
    function ModifyLine(
      ASource: IMarkFull;
      AName: string;
      AVisible: Boolean;
      ACategory: IMarkCategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkFull;
    function ModifyPoly(
      ASource: IMarkFull;
      AName: string;
      AVisible: Boolean;
      ACategory: IMarkCategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkFull;

    function SimpleModifyLine(
      ASource: IMarkFull;
      APoints: TArrayOfDoublePoint;
      ADesc: string
    ): IMarkFull;
    function SimpleModifyPoly(
      ASource: IMarkFull;
      APoints: TArrayOfDoublePoint
    ): IMarkFull;

    function GetConfig: IMarksFactoryConfig;
    property Config: IMarksFactoryConfig read GetConfig;
  end;

implementation

end.
