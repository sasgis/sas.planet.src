unit i_LocalCoordConverterChangeable;

interface

uses
  i_Changeable,
  i_LocalCoordConverter;

type
  ILocalCoordConverterChangeable = interface(IChangeable)
    ['{ED080B29-6C15-4C8E-9233-522F651004E8}']
    function GetStatic: ILocalCoordConverter;
  end;

  ILocalCoordConverterChangeableInternal = interface(ILocalCoordConverterChangeable)
    procedure SetConverter(const AValue: ILocalCoordConverter);
    procedure StopNotify;
    procedure StartNotify;
  end;

implementation

end.
