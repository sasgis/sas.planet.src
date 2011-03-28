unit u_TileStorageTypeInfo;

interface

uses
  i_TileStorageTypeInfo;

type
  TTileStorageTypeInfoFieFolder = class(TInterfacedObject, ITileStorageTypeInfo)
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;

  TTileStorageTypeGE = class(TInterfacedObject, ITileStorageTypeInfo)
  protected
    function GetIsFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetAllowMultiWrite: Boolean;
    function GetAllowMultiRead: Boolean;
  end;

implementation

{ TTileStorageTypeInfoFieFolder }

function TTileStorageTypeInfoFieFolder.GetAllowDelete: boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowMultiRead: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowMultiWrite: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetAllowSave: boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetIsFileCache: Boolean;
begin
  Result := True;
end;

function TTileStorageTypeInfoFieFolder.GetIsReadOnly: boolean;
begin
  Result := False;
end;

{ TTileStorageTypeGE }

function TTileStorageTypeGE.GetAllowDelete: boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowMultiRead: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowMultiWrite: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetAllowSave: boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetIsFileCache: Boolean;
begin
  Result := False;
end;

function TTileStorageTypeGE.GetIsReadOnly: boolean;
begin
  Result := True;
end;

end.
