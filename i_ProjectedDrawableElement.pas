unit i_ProjectedDrawableElement;

interface

uses
  GR32,
  i_ProjectionInfo,
  i_LocalCoordConverter;

type
  IProjectedDrawableElement = interface
    ['{999BACCC-135B-439A-A646-A827398B316E}']
    function GetProjectionInfo: IProjectionInfo;
    property ProjectionInfo: IProjectionInfo read GetProjectionInfo;

    procedure Draw(
      ABitmap: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter
    );
  end;

implementation

end.
