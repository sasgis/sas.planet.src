unit u_TileStorageTypeList;

interface

uses
  ActiveX,
  i_GUIDList,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement,
  i_TileStorageTypeConfig,
  i_TileStorageType,
  i_TileStorageTypeList,
  i_TileStorageTypeListItem,
  u_ConfigDataElementBase;

type
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
    procedure Add(AValue: ITileStorageTypeListItem);
  public
    constructor Create(AFirstType: ITileStorageTypeListItem);
  end;

implementation

uses
  SysUtils;

{ TTileStorageTypeList }

constructor TTileStorageTypeList.Create(
  AFirstType: ITileStorageTypeListItem);
begin
  inherited Create;
  Assert(AFirstType.CanUseAsDefault);
  Add(AFirstType);
  FDefault := AFirstType.StorageType;
end;

procedure TTileStorageTypeList.Add(AValue: ITileStorageTypeListItem);
begin
  FList.Add(AValue.GUID, AValue);
end;

procedure TTileStorageTypeList.DoReadConfig(AConfigData: IConfigDataProvider);
var
  i: Cardinal;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VConfigData: IConfigDataProvider;
  VItem: ITileStorageTypeListItem;
  VConfig: ITileStorageTypeConfig;
begin
  inherited;
  if AConfigData <> nil then begin
    VEnum := FList.GetGUIDEnum;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := ITileStorageTypeListItem(FList.GetByGUID(VGUID));
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
  VItem: ITileStorageTypeListItem;
  VConfig: ITileStorageTypeConfig;
begin
  inherited;
  VEnum := FList.GetGUIDEnum;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VItem := ITileStorageTypeListItem(FList.GetByGUID(VGUID));
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
