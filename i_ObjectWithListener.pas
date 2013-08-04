unit i_ObjectWithListener;

interface

uses
  i_Listener,
  i_LocalCoordConverter;

type
  IObjectWithListener = interface
    ['{95B7E0FF-1FD5-4239-BBDA-1535BF03A965}']
    procedure SetListener(
      const AListener: IListener;
      const ALocalConverter: ILocalCoordConverter
    );
    procedure RemoveListener;
  end;

implementation

end.


