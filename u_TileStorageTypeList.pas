unit u_TileStorageTypeList;

interface

uses
  ActiveX,
  i_IGUIDList,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IConfigDataElement,
  i_ITileStorageTypeConfig,
  i_ITileStorageType,
  i_ITileStorageTypeList,
  u_ConfigDataElementBase;

type
  IInternalTileStorageTypeInternal = interface
    ['{103AA969-E2D0-4E66-8B8D-F78E6D442E7D}']
    function GetGUID: TGUID;
    property GUID: TGUID read GetGUID;

    function GetStorageType: ITileStorageType;
    property StorageType: ITileStorageType read GetStorageType;

    function GetCanUseAsDefault: Boolean;
    property CanUseAsDefault: Boolean read GetCanUseAsDefault;

    function GetConfig: ITileStorageTypeConfig;
    property Config: ITileStorageTypeConfig read GetConfig;
  end;

  TTileStorageTypeList = class(TConfigDataElementBase, ITileStorageTypeList)
  private
    FList: IGUIDInterfaceList;
    FDefault: ITileStorageType;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetDefault: ITileStorageType;
    procedure SetDefaultByGUID(AGUID: TGUID);
    function Get(AGUID: TGUID): ITileStorageType;
    function GetCanUseAsDefault(AGUID: TGUID): Boolean;
    function GetConfig(AGUID: TGUID): ITileStorageTypeConfig;
    function GetEnum: IEnumGUID;
  protected
    procedure Add(AValue: IInternalTileStorageTypeInternal);
  public
    constructor Create(AFirstType: IInternalTileStorageTypeInternal);
  end;

implementation

uses
  SysUtils;

{ TTileStorageTypeList }

constructor TTileStorageTypeList.Create(
  AFirstType: IInternalTileStorageTypeInternal);
begin
  inherited Create;
  Assert(AFirstType.CanUseAsDefault);
  Add(AFirstType);
  FDefault := AFirstType.StorageType;
end;

procedure TTileStorageTypeList.Add(AValue: IInternalTileStorageTypeInternal);
begin
  FList.Add(AValue.GUID, AValue);
end;

procedure TTileStorageTypeList.DoReadConfig(AConfigData: IConfigDataProvider);
var
  i: Cardinal;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VConfigData: IConfigDataProvider;
  VItem: IInternalTileStorageTypeInternal;
  VConfig: ITileStorageTypeConfig;
begin
  inherited;
  if AConfigData <> nil then begin
    VEnum := FList.GetGUIDEnum;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := IInternalTileStorageTypeInternal(FList.GetByGUID(VGUID));
      VConfig := VItem.Config;
      if VConfig <> nil then begin
        VConfigData := AConfigData.GetSubItem(GUIDToString(VGUID));
        VConfig.ReadConfig(VConfigData);
      end;
    end;
  end;
end;

procedure TTileStorageTypeList.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
var
  i: Cardinal;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VConfigData: IConfigDataProvider;
  VItem: IInternalTileStorageTypeInternal;
  VConfig: ITileStorageTypeConfig;
begin
  inherited;
  VEnum := FList.GetGUIDEnum;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VItem := IInternalTileStorageTypeInternal(FList.GetByGUID(VGUID));
    VConfig := VItem.Config;
    if VConfig <> nil then begin
      VConfigData := AConfigData.GetSubItem(GUIDToString(VGUID));
      VConfig.ReadConfig(VConfigData);
    end;
  end;
end;

function TTileStorageTypeList.Get(AGUID: TGUID): ITileStorageType;
begin

end;

function TTileStorageTypeList.GetCanUseAsDefault(AGUID: TGUID): Boolean;
begin

end;

function TTileStorageTypeList.GetConfig(AGUID: TGUID): ITileStorageTypeConfig;
begin

end;

function TTileStorageTypeList.GetDefault: ITileStorageType;
begin

end;

function TTileStorageTypeList.GetEnum: IEnumGUID;
begin

end;

procedure TTileStorageTypeList.SetDefaultByGUID(AGUID: TGUID);
begin

end;

end.
