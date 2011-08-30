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
    ): IMarkPoint;
    function CreateNewLine(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplateLine = nil
    ): IMarkLine;
    function CreateNewPoly(
      APoints: TArrayOfDoublePoint;
      AName: string;
      ADesc: string;
      ATemplate: IMarkTemplatePoly = nil
    ): IMarkPoly;

    function ModifyPoint(
      ASource: IMarkPoint;
      AName: string;
      AVisible: Boolean;
      APic: IMarkPicture;
      ACategory: ICategory;
      ADesc: string;
      APoint: TDoublePoint;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
    ): IMarkPoint;
    function ModifyLine(
      ASource: IMarkLine;
      AName: string;
      AVisible: Boolean;
      ACategory: ICategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ALineColor: TColor32;
      ALineWidth: Integer
    ): IMarkLine;
    function ModifyPoly(
      ASource: IMarkPoly;
      AName: string;
      AVisible: Boolean;
      ACategory: ICategory;
      ADesc: string;
      APoints: TArrayOfDoublePoint;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    ): IMarkPoly;

    function SimpleModifyLine(
      ASource: IMarkLine;
      APoints: TArrayOfDoublePoint;
      ADesc: string
    ): IMarkLine;
    function SimpleModifyPoly(
      ASource: IMarkPoly;
      APoints: TArrayOfDoublePoint
    ): IMarkPoly;

    function GetConfig: IMarksFactoryConfig;
    property Config: IMarksFactoryConfig read GetConfig;
  end;

implementation

end.
