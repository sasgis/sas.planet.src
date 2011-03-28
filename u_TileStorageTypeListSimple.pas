unit u_TileStorageTypeListSimple;

interface

uses
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeListItem,
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
