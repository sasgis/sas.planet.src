unit i_MarkerDrawable;

interface

uses
  GR32,
  t_GeoTypes,
  i_Changeable;

type
  IMarkerDrawable = interface
    ['{91E8968F-8563-4ED0-8774-AF844F8CA8B9}']
    function GetBoundsForPosition(const APosition: TDoublePoint): TRect;
    function DrawToBitmap(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint
    ): Boolean;
  end;

  IMarkerDrawableChangeable = interface(IChangeable)
    ['{0982D5D9-DE1F-4C35-84DD-E461A332D38C}']
    function GetStatic: IMarkerDrawable;
  end;

  IMarkerDrawableWithDirection = interface
    ['{76C743DE-86B4-4EF2-9451-DB22A90B8628}']
    function DrawToBitmapWithDirection(
      ABitmap: TCustomBitmap32;
      const APosition: TDoublePoint;
      const AAngle: Double
    ): Boolean;
  end;

  IMarkerDrawableWithDirectionChangeable = interface(IChangeable)
    ['{AFFC6BF0-734C-4D86-9057-EC49F4F3045B}']
    function GetStatic: IMarkerDrawableWithDirection;
  end;

implementation

end.
