unit i_IBackgroundTaskLayerDraw;

interface

uses
  Types,
  GR32,
  i_ILocalCoordConverter,
  i_IBackgroundTask;

type
  IBackgroundTaskLayerDraw = interface(IBackgroundTask)
    ['{DFC6C5B8-C3A1-4463-9B88-D7C89E031077}']
    procedure ChangePos(AConverter: ILocalCoordConverter);
    procedure ChangeSize(ANewSize: TPoint);
  end;

  IBackgroundTaskLayerDrawFactory = interface
    ['{F6147704-4B34-4798-A697-8D7B59421945}']
    function GetTask(ABitmap: TCustomBitmap32): IBackgroundTaskLayerDraw;
  end;

implementation

end.
