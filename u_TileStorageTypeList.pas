{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_TileStorageTypeList;

interface

uses
  ActiveX,
  i_GUIDSet,
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
    FList: IGUIDInterfaceSet;
    FDefault: ITileStorageTypeListItem;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetDefault: ITileStorageType;
    procedure SetDefaultByGUID(const AGUID: TGUID);
    function Get(const AGUID: TGUID): ITileStorageType;
    function GetCanUseAsDefault(const AGUID: TGUID): Boolean;
    function GetEnum: IEnumGUID;
  protected
    procedure Add(const AValue: ITileStorageTypeListItem);
  public
    constructor Create(const AFirstType: ITileStorageTypeListItem);
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_GUIDInterfaceSet;

{ TTileStorageTypeList }

constructor TTileStorageTypeList.Create(
  const AFirstType: ITileStorageTypeListItem
);
begin
  inherited Create;
  Assert(AFirstType.CanUseAsDefault);
  FList := TGUIDInterfaceSet.Create(False);
  Add(AFirstType);
  FDefault := AFirstType;
end;

procedure TTileStorageTypeList.Add(const AValue: ITileStorageTypeListItem);
begin
  FList.Add(AValue.GUID, AValue);
end;

procedure TTileStorageTypeList.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  i: Cardinal;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VConfigData: IConfigDataProvider;
  VItem: ITileStorageTypeListItem;
  VConfig: ITileStorageTypeConfig;
  VGUIDString: string;
begin
  inherited;
  if AConfigData <> nil then begin
    VEnum := FList.GetGUIDEnum;
    while VEnum.Next(1, VGUID, i) = S_OK do begin
      VItem := ITileStorageTypeListItem(FList.GetByGUID(VGUID));
      VConfig := VItem.StorageType.Config;
      if VConfig <> nil then begin
        VConfigData := AConfigData.GetSubItem(GUIDToString(VGUID));
        VConfig.ReadConfig(VConfigData);
      end;
    end;
    VGUID := CGUID_Zero;
    VGUIDString := AConfigData.ReadString('DefaultTypeGUID', '');
    if VGUIDString <> '' then begin
      try
        VGUID := StringToGUID(VGUIDString);
      except
        VGUID := CGUID_Zero;
      end;
    end;
    if not IsEqualGUID(VGUID, CGUID_Zero) then begin
      SetDefaultByGUID(VGUID);
    end;
  end;
end;

procedure TTileStorageTypeList.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  i: Cardinal;
  VGUID: TGUID;
  VEnum: IEnumGUID;
  VConfigData: IConfigDataWriteProvider;
  VItem: ITileStorageTypeListItem;
  VConfig: ITileStorageTypeConfig;
begin
  inherited;
  AConfigData.WriteString('DefaultTypeGUID', GUIDToString(FDefault.GUID));
  AConfigData.WriteString('DefaultTypeName', FDefault.Caption);

  VEnum := FList.GetGUIDEnum;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VItem := ITileStorageTypeListItem(FList.GetByGUID(VGUID));
    VConfig := VItem.StorageType.Config;
    if VConfig <> nil then begin
      VConfigData := AConfigData.GetOrCreateSubItem(GUIDToString(VGUID));
      VConfigData.WriteString('Name', VItem.Caption);
      VConfig.WriteConfig(VConfigData);
    end;
  end;
end;

function TTileStorageTypeList.Get(const AGUID: TGUID): ITileStorageType;
var
  VResult: ITileStorageTypeListItem;
begin
  Result := nil;
  VResult := ITileStorageTypeListItem(FList.GetByGUID(AGUID));
  if VResult <> nil then begin
    Result := VResult.StorageType;
  end;
end;

function TTileStorageTypeList.GetCanUseAsDefault(const AGUID: TGUID): Boolean;
var
  VResult: ITileStorageTypeListItem;
begin
  Result := False;
  VResult := ITileStorageTypeListItem(FList.GetByGUID(AGUID));
  if VResult <> nil then begin
    Result := VResult.CanUseAsDefault;
  end;
end;

function TTileStorageTypeList.GetDefault: ITileStorageType;
begin
  Result := FDefault.StorageType;
end;

function TTileStorageTypeList.GetEnum: IEnumGUID;
begin
  Result := FList.GetGUIDEnum;
end;

procedure TTileStorageTypeList.SetDefaultByGUID(const AGUID: TGUID);
var
  VResult: ITileStorageTypeListItem;
begin
  VResult := ITileStorageTypeListItem(FList.GetByGUID(AGUID));
  if VResult <> nil then begin
    VResult.StorageType;
  end;
end;

end.
