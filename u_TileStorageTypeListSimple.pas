unit u_TileStorageTypeListSimple;

interface

uses
  i_ITileStorageTypeConfig,
  i_ITileStorageType,
  i_ITileStorageTypeListItem,
  u_TileStorageTypeList;

type
  TTileStorageTypeListSimple = class(TTileStorageTypeList)
  public
    constructor Create;
  end;

implementation

uses
  u_TileStorageTypeListItem;

{ TTileStorageTypeListSimple }

constructor TTileStorageTypeListSimple.Create;
var
  VItem: ITileStorageTypeListItem;
begin
//  VItem := TTileStorageTypeListItem.Create(
//    ['{C66C56C5-43AC-490A-B3A1-34898B13BD79}'],
//    
//  );
  inherited Create(VItem);
end;

end.
