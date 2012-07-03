unit i_ImageLineProvider;

interface

uses
  i_NotifierOperation,
  i_LocalCoordConverter;

type
  IImageLineProvider = interface
    ['{86177BB4-DACC-4DF7-A5F9-577B5D4B8C4F}']
    function GetLocalConverter: ILocalCoordConverter;
    property LocalConverter: ILocalCoordConverter read GetLocalConverter;

    function GetBytesPerPixel: Integer;
    property BytesPerPixel: Integer read GetBytesPerPixel;

    function GetLine(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      ALine: Integer
    ): Pointer;
  end;

implementation

end.
