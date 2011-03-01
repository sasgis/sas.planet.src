unit i_ISatellitesInViewMapDraw;

interface

uses
  GR32,
  i_GPS;

type
  ISatellitesInViewMapDraw = interface
    ['{31DF984C-0BA5-4B05-8AEA-36D68A60C323}']
    procedure Draw(ABitmap: TBitmap32; ASatellites: IGPSSatellitesInView);
  end;

implementation

end.
