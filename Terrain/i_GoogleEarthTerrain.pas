unit i_GoogleEarthTerrain;

interface

const
  IID_IGoogleEarthTerrain: TGUID = '{A045855E-05DD-4BFD-9775-39567DD68444}';

type
  IGoogleEarthTerrain = interface (IInterface)
    ['{25229FC3-C973-462C-BF08-9ED5CC74E695}']
    procedure Open(const ATileData: PByte; const ATileSize: Integer); safecall;
    function Elevation(const ALon: Double; const ALat: Double): Single; safecall;
  end;

implementation

end.
