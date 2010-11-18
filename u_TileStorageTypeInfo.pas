unit u_TileStorageTypeInfo;

interface

uses
  i_ITileStorageTypeInfo;

type
  TTileStorageTypeInfo = class(TInterfacedObject, ITileStorageTypeInfo)
  private
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;
implementation

end.
