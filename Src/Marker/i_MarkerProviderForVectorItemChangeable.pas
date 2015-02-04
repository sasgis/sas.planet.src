unit i_MarkerProviderForVectorItemChangeable;

interface

uses
  i_MarkerProviderForVectorItem,
  i_Changeable;

type
  IMarkerProviderForVectorItemChangeable = interface
    ['{1CF6921E-25EB-4266-926A-E57DF113199B}']
    function GetStatic: IMarkerProviderForVectorItem;
  end;

implementation

end.
