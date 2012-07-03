unit i_NotifierLonLatRectUpdate;

interface

uses
  t_GeoTypes,
  i_Listener,
  i_LonLatRect;

type
  INotifierLonLatRectUpdate = interface
    ['{BE8DACC7-D94E-408A-A202-985D6DAC682A}']
    procedure Add(
      const AListener: IListener;
      const ARect: ILonLatRect
    ); stdcall;
    procedure Remove(const AListener: IListener); stdcall;
  end;

  INotifierLonLatRectUpdateInternal = interface
    procedure RectUpdateNotify(const ARect: ILonLatRect); stdcall; overload;
    procedure RectUpdateNotify(const ARect: TDoubleRect); stdcall; overload;
  end;

implementation

end.
