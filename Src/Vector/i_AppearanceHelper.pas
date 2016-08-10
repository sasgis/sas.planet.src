unit i_AppearanceHelper;

interface

uses
  t_MarkAppearance,
  i_ImportConfig,
  i_MarkPicture,
  i_Appearance,
  i_AppearanceOfMarkFactory;

type
  IAppearanceHelper = interface
    ['{75208191-C955-44A0-B9A0-A9215BC6712C}']
    function GetAppearanceOfMarkFactory: IAppearanceOfMarkFactory;
    property AppearanceOfMarkFactory: IAppearanceOfMarkFactory read GetAppearanceOfMarkFactory;

    function GetMarkPictureList: IMarkPictureList;
    property MarkPictureList: IMarkPictureList read GetMarkPictureList;

    function LoadPointAppearance(
      const AAppearance: PPointAppearanceData;
      const APicName: string;
      const APicOptional: IMarkPicture
    ): IAppearance;

    function LoadPolylineAppearance(
      const AAppearance: PPolylineAppearanceData
    ): IAppearance;

    function LoadPolygonAppearance(
      const AAppearance: PPolygonAppearanceData
    ): IAppearance;

    function RedefinePointAppearance(
      const APointParams: IImportPointParams;
      const ARedefinitions: TAppearanceRedefinitions;
      const ARedefinePic: IMarkPicture
    ): IAppearance;

    function RedefineLineAppearance(
      const ALineParams: IImportLineParams;
      const ARedefinitions: TAppearanceRedefinitions
    ): IAppearance;

    function RedefinePolygonAppearance(
      const APolygonParams: IImportPolyParams;
      const ARedefinitions: TAppearanceRedefinitions
    ): IAppearance;
  end;
  
implementation

end.
