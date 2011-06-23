unit u_TileStorageInfo;

interface

uses
  i_CoordConverter,
  i_ContentTypeInfo,
  i_TileStorageTypeInfo,
  i_TileStorageInfo;

type
  TTileStorageInfo = class(TInterfacedObject, ITileStorageInfo)
  private
    FTypeInfo: ITileStorageTypeInfo;
    FMainContentType: IContentTypeInfoBasic;
    FAllowDifferentContentTypes: Boolean;

    FAllowDelete: boolean;
    FAllowSave: boolean;
    FIsReadOnly: boolean;
    FCoordConverter: ICoordConverter;
  protected
    function GetTypeInfo: ITileStorageTypeInfo;
    function GetMainContentType: IContentTypeInfoBasic;
    function GetAllowDifferentContentTypes: Boolean;

    function GetAllowDelete: boolean;
    function GetAllowSave: boolean;
    function GetIsReadOnly: boolean;
    function GetCoordConverter: ICoordConverter;
  public
    constructor Create(
      ATypeInfo: ITileStorageTypeInfo;
      AMainContentType: IContentTypeInfoBasic;
      AAllowDifferentContentTypes: Boolean;
      AAllowDelete: boolean;
      AAllowSave: boolean;
      AIsReadOnly: boolean;
      ACoordConverter: ICoordConverter
    );
  end;

implementation

{ TTileStorageInfo }

constructor TTileStorageInfo.Create(
  ATypeInfo: ITileStorageTypeInfo;
  AMainContentType: IContentTypeInfoBasic; AAllowDifferentContentTypes,
  AAllowDelete, AAllowSave, AIsReadOnly: boolean;
  ACoordConverter: ICoordConverter
);
begin
  FTypeInfo := ATypeInfo;
  FMainContentType := AMainContentType;
  FAllowDifferentContentTypes := AAllowDifferentContentTypes;
  FAllowDelete := AAllowDelete;
  FAllowSave := AAllowSave;
end;

function TTileStorageInfo.GetAllowDelete: boolean;
begin
  Result := FAllowDelete;
end;

function TTileStorageInfo.GetAllowDifferentContentTypes: Boolean;
begin
  Result := FAllowDifferentContentTypes;
end;

function TTileStorageInfo.GetAllowSave: boolean;
begin
  Result := FAllowSave;
end;

function TTileStorageInfo.GetCoordConverter: ICoordConverter;
begin
  Result := FCoordConverter;
end;

function TTileStorageInfo.GetIsReadOnly: boolean;
begin
  Result := FIsReadOnly;
end;

function TTileStorageInfo.GetMainContentType: IContentTypeInfoBasic;
begin
  Result := FMainContentType;
end;

function TTileStorageInfo.GetTypeInfo: ITileStorageTypeInfo;
begin
  Result := FTypeInfo;
end;

end.
