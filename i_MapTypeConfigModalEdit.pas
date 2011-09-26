unit i_MapTypeConfigModalEdit;

interface

uses
  u_MapType;

type
  IMapTypeConfigModalEdit = interface
    ['{E6C024D3-4392-4AB3-86A3-9CB9FA33ED87}']
    function EditMap(AMapType: TMapType): Boolean;
  end;

implementation

end.
