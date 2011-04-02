unit i_MarkFactoryDbInternal;

interface

uses
  GR32,
  t_GeoTypes,
  i_MarkPicture,
  i_MarksFactoryConfig,
  i_MarksSimple;

type
  IMarkFactoryDbInternal = interface
    ['{0D5A67D8-585A-4DA8-9047-CB3CB76A600E}']
    function CreateMark(
      AID: Integer;
      AName: string;
      AVisible: Boolean;
      APicName: string;
      ACategoryId: Integer;
      ADesc: string;
      ARect: TDoubleRect;
      APoints: TArrayOfDoublePoint;
      AColor1: TColor32;
      AColor2: TColor32;
      AScale1: Integer;
      AScale2: Integer
    ): IMarkFull;
    function CreateMarkId(
      AName: string;
      AId: Integer;
      AVisible: Boolean
    ): IMarkID;
  end;

implementation

end.
