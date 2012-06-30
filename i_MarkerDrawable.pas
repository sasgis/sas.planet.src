unit i_MarkerDrawable;

interface

uses
  GR32,
  t_GeoTypes;

type
  IMarkerDrawable = interface
    ['{91E8968F-8563-4ED0-8774-AF844F8CA8B9}']
    procedure DrawToBitmap(
      ABitmap: TCustomBitmap32;
      ACombineMode: TCombineMode;
      const APosition: TDoublePoint
    );
  end;

  IMarkerDrawableWithDirection = interface
    ['{76C743DE-86B4-4EF2-9451-DB22A90B8628}']
    procedure DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      ACombineMode: TCombineMode;
      const APosition: TDoublePoint;
      const AAngle: Double
    );
  end;

  IMarkerDrawableProvider = interface
    ['{E4DEDD31-B1E3-4776-98DC-C0E3B99537FB}']
    function GetMarker: IMarkerDrawable;
  end;

  IMarkerDrawableProviderBySize = interface
    ['{4B34C9BF-3F37-4782-9B3E-CB8BDFEE82B1}']
    function GetMarker(ASize: Integer): IMarkerDrawable;
  end;

  IMarkerDrawableProviderByDirection = interface
    ['{A38DF7EA-58A3-4893-9E9D-BDD651F7BD96}']
    function GetMarker(const AAngle: Double): IMarkerDrawable;
  end;

  IMarkerDrawableProviderBySizeAbdDirection = interface
    ['{FF051FA9-D992-4383-8321-DC5CC2DBB6F1}']
    function GetMarker(
      const AAngle: Double;
      ASize: Integer
    ): IMarkerDrawable;
  end;

implementation

end.
