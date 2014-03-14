{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_ActivMapWithLayers;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapTypes,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_MapTypeSetChangeable,
  i_ActiveMapsConfig,
  u_NotifyWithGUIDEvent,
  u_MainActiveMap;

type
  TActivMapWithLayers = class(TMainActiveMap, IActivMapWithLayers)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FLayerSetSelectNotyfier: INotifierWithGUID;
    FLayerSetUnselectNotyfier: INotifierWithGUID;

    FLayersSet: IMapTypeSet;
    FAllMapsSet: IMapTypeSet;

    FActiveLayersSet: IMapTypeSetChangeable;
    FAllActiveMapsSet: IMapTypeSetChangeable;
  protected
    property LayerSetSelectNotyfier: INotifierWithGUID read FLayerSetSelectNotyfier;
    property LayerSetUnselectNotyfier: INotifierWithGUID read FLayerSetUnselectNotyfier;
  protected
    procedure InvertLayerSelectionByGUID(const AMapGUID: TGUID);
    procedure SelectLayerByGUID(const AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(const AMapGUID: TGUID);

    function GetLayersSet: IMapTypeSet;
    function GetAllMapsSet: IMapTypeSet;

    function GetActiveLayersSet: IMapTypeSetChangeable;
    function GetAllActiveMapsSet: IMapTypeSetChangeable;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(
      const AIsAllowNilMap: Boolean;
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMapsSet, ALayersSet: IMapTypeSet
    );
  end;

implementation

uses
  SysUtils,
  StrUtils,
  ActiveX,
  c_ZeroGUID,
  i_StringListStatic,
  u_ActiveMapsSet;

const
  CKeyNameLayer = 'Layer';

{ TActivMapWithLayers }

constructor TActivMapWithLayers.Create(
  const AIsAllowNilMap: Boolean;
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMapsSet, ALayersSet: IMapTypeSet
);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VAllMapsList: IMapTypeSetBuilder;
  VMainMapChangeNotyfier: INotifierWithGUID;
begin
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  VMainMapChangeNotyfier := TNotifierWithGUID.Create;
  FLayerSetSelectNotyfier := TNotifierWithGUID.Create;
  FLayerSetUnselectNotyfier := TNotifierWithGUID.Create;
  FLayersSet := ALayersSet;

  VAllMapsList := FMapTypeSetBuilderFactory.Build(True);
  VAllMapsList.Capacity := AMapsSet.Count + ALayersSet.Count;

  VEnun := AMapsSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := AMapsSet.GetMapTypeByGUID(VGUID);
    VAllMapsList.Add(VMapType);
  end;

  VEnun := FLayersSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := FLayersSet.GetMapTypeByGUID(VGUID);
    VAllMapsList.Add(VMapType);
  end;
  inherited Create(AIsAllowNilMap, AMapsSet, VMainMapChangeNotyfier);

  FAllMapsSet := VAllMapsList.MakeAndClear;

  FActiveLayersSet := TLayerSetChangeable.Create(
    FMapTypeSetBuilderFactory,
    FLayersSet,
    FLayerSetSelectNotyfier,
    FLayerSetUnselectNotyfier
  );

  FAllActiveMapsSet :=
    TMapsSetChangeableByMainMapAndLayersSet.Create(
      FMapTypeSetBuilderFactory,
      GetActiveMap,
      FActiveLayersSet
    );
end;

procedure TActivMapWithLayers.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VList: IStringListStatic;
  i: Integer;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
  VMap: IMapType;
begin
  inherited;
  if AConfigData <> nil then begin
    VList := AConfigData.ReadValuesList;
    for i := 0 to VList.Count - 1 do begin
      VKeyName := VList.Items[i];
      if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
        VGUIDString := AConfigData.ReadString(VKeyName, '');
        if VGUIDString <> '' then begin
          try
            VGUID := StringToGUID(VGUIDString);
          except
            VGUID := CGUID_Zero;
          end;
        end else begin
          VGUID := CGUID_Zero;
        end;
        if not IsEqualGUID(VGUID, CGUID_Zero) then begin
          VMap := FLayersSet.GetMapTypeByGUID(VGUID);
          if VMap <> nil then begin
            SelectLayerByGUID(VGUID);
          end;
        end;
      end;
    end;
  end;
end;

procedure TActivMapWithLayers.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VList: IStringListStatic;
  i: Cardinal;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
  VIndex: Integer;
  VEnum: IEnumGUID;
begin
  inherited;
  VList := AConfigData.ReadValuesList;
  for i := 0 to VList.Count - 1 do begin
    VKeyName := VList.Items[i];
    if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
      AConfigData.DeleteValue(VKeyName);
    end;
  end;

  VIndex := 0;
  VEnum := FActiveLayersSet.GetStatic.GetIterator;
  while VEnum.Next(1, VGUID, i) = S_OK do begin
    VGUIDString := GUIDToString(VGUID);
    AConfigData.WriteString(CKeyNameLayer + IntToStr(VIndex), VGUIDString);
    Inc(VIndex);
  end;
end;

function TActivMapWithLayers.GetAllActiveMapsSet: IMapTypeSetChangeable;
begin
  Result := FAllActiveMapsSet;
end;

function TActivMapWithLayers.GetAllMapsSet: IMapTypeSet;
begin
  Result := FAllMapsSet;
end;

function TActivMapWithLayers.GetLayersSet: IMapTypeSet;
begin
  Result := FLayersSet;
end;

procedure TActivMapWithLayers.InvertLayerSelectionByGUID(const AMapGUID: TGUID);
begin
  LockWrite;
  try
    if FActiveLayersSet.GetStatic.GetMapTypeByGUID(AMapGUID) = nil then begin
      FLayerSetSelectNotyfier.NotifyByGUID(AMapGUID);
    end else begin
      FLayerSetUnselectNotyfier.NotifyByGUID(AMapGUID);
    end;
  finally
    UnlockWrite;
  end;
end;

function TActivMapWithLayers.GetActiveLayersSet: IMapTypeSetChangeable;
begin
  Result := FActiveLayersSet;
end;

procedure TActivMapWithLayers.SelectLayerByGUID(const AMapGUID: TGUID);
begin
  LockWrite;
  try
    FLayerSetSelectNotyfier.NotifyByGUID(AMapGUID);
  finally
    UnlockWrite;
  end;
end;

procedure TActivMapWithLayers.UnSelectLayerByGUID(const AMapGUID: TGUID);
begin
  LockWrite;
  try
    FLayerSetUnselectNotyfier.NotifyByGUID(AMapGUID);
  finally
    UnlockWrite;
  end;
end;

end.
