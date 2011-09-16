unit u_SimpleTileStorageConfigStatic;

interface

uses
  i_SimpleTileStorageConfig;

type
  TSimpleTileStorageConfigStatic = class(TInterfacedObject, ISimpleTileStorageConfigStatic)
  private
    FCacheTypeCode: Integer;
    FNameInCache: string;
    FTileFileExt: string;
    FIsStoreFileCache: Boolean;
    FIsReadOnly: boolean;
    FAllowDelete: boolean;
    FAllowAdd: boolean;
    FAllowReplace: boolean;
  protected
    function GetCacheTypeCode: Integer;
    function GetNameInCache: string;
    function GetTileFileExt: string;
    function GetIsStoreFileCache: Boolean;
    function GetIsReadOnly: boolean;
    function GetAllowDelete: boolean;
    function GetAllowAdd: boolean;
    function GetAllowReplace: boolean;
  public
    constructor Create(
      ACacheTypeCode: Integer;
      ANameInCache: string;
      ATileFileExt: string;
      AIsStoreFileCache: Boolean;
      AIsReadOnly: boolean;
      AAllowDelete: boolean;
      AAllowAdd: boolean;
      AAllowReplace: boolean
    );
  end;

implementation

{ TSimpleTileStorageConfigStatic }

constructor TSimpleTileStorageConfigStatic.Create(
  ACacheTypeCode: Integer;
  ANameInCache: string;
  ATileFileExt: string;
  AIsStoreFileCache, AIsReadOnly, AAllowDelete, AAllowAdd, AAllowReplace: boolean
);
begin
  FCacheTypeCode := ACacheTypeCode;
  FNameInCache := ANameInCache;
  FTileFileExt := ATileFileExt;
  FIsStoreFileCache := AIsStoreFileCache;
  FIsReadOnly := AIsReadOnly;
  FAllowDelete := AAllowDelete;
  FAllowAdd := AAllowAdd;
  FAllowReplace := AAllowReplace;
end;

function TSimpleTileStorageConfigStatic.GetAllowAdd: boolean;
begin
  Result := FAllowAdd;
end;

function TSimpleTileStorageConfigStatic.GetAllowDelete: boolean;
begin
  Result := FAllowDelete;
end;

function TSimpleTileStorageConfigStatic.GetAllowReplace: boolean;
begin
  Result := FAllowReplace;
end;

function TSimpleTileStorageConfigStatic.GetCacheTypeCode: Integer;
begin
  Result := FCacheTypeCode;
end;

function TSimpleTileStorageConfigStatic.GetIsReadOnly: boolean;
begin
  Result := FIsReadOnly;
end;

function TSimpleTileStorageConfigStatic.GetIsStoreFileCache: Boolean;
begin
  Result := FIsStoreFileCache;
end;

function TSimpleTileStorageConfigStatic.GetNameInCache: string;
begin
  Result := FNameInCache;
end;

function TSimpleTileStorageConfigStatic.GetTileFileExt: string;
begin
  Result := FTileFileExt;
end;

end.
