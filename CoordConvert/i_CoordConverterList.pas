unit i_CoordConverterList;

interface

uses
  i_CoordConverter;

type
  ICoordConverterList = interface
    ['{CC888F5D-5DDA-427F-8127-93B0F1BD8CA5}']
    function Count: Integer;

    function Get(AIndex: Integer): ICoordConverter;
    property Items[Index: Integer]: ICoordConverter read Get; default;

    function GetCaption(AIndex: Integer): string;
    property Captions[Index: Integer]: string read GetCaption;
  end;

implementation

end.
