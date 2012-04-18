unit i_LonLatRectUpdateNotifier;

interface

uses
  t_GeoTypes,
  i_JclNotify,
  i_LonLatRect;

type
  ILonLatRectUpdateNotifier = interface
    ['{BE8DACC7-D94E-408A-A202-985D6DAC682A}']
    procedure Add(
      const AListener: IJclListener;
      const ATileRect: TDoubleRect
    ); stdcall;
    procedure Remove(const AListener: IJclListener); stdcall;
  end;

  ILonLatRectUpdateNotifierInternal = interface
    ['{4B62C3A6-959C-4B6F-84B9-C4AF2B9AC87D}']
    procedure TileUpdateNotify(const ARect: ILonLatRect); stdcall; overload;
    procedure TileUpdateNotify(const ARect: TDoubleRect); stdcall; overload;
  end;

implementation

end.
