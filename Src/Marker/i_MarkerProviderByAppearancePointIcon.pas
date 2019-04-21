unit i_MarkerProviderByAppearancePointIcon;

interface

uses
  i_AppearanceOfVectorItem,
  i_BitmapMarker;

type
  IMarkerProviderByAppearancePointIcon = interface
    ['{BA053828-9E7F-42E8-A692-2088BEA70AAF}']
    function GetMarker(
      const AAppearance: IAppearancePointIcon
    ): IBitmapMarker;
  end;

implementation

end.
