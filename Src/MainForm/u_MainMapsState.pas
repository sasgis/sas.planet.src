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

unit u_MainMapsState;

interface

uses
  i_ActiveMapsConfig,
  i_MapType,
  i_MapTypeListChangeable,
  i_MapTypeListBuilder,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_MapTypeSetChangeable,
  i_MainMapsState,
  u_BaseInterfacedObject;

type
  TMainMapsState = class(TBaseInterfacedObject, IMainMapsState)
  private
    FMainMapConfig: IActiveMapConfig;
    FMainLayersConfig: IActiveLayersConfig;
    FMiniMapConfig: IActiveMapConfig;
    FMiniLayersConfig: IActiveLayersConfig;
    FFillingMapConfig: IActiveMapConfig;
    FFirstMainMapGUID: TGUID;

    FMapsSet: IMapTypeSet;
    FActiveMap: IMapTypeChangeable;

    FLayersSet: IMapTypeSet;
    FActiveLayersSet: IMapTypeSetChangeable;

    FAllMapsSet: IMapTypeSet;
    FAllActiveMapsSet: IMapTypeSetChangeable;
    FActiveMapsSetLicenseNotEmpty: IMapTypeSetChangeable;

    FActiveBitmapMapsSet: IMapTypeSetChangeable;

    FBitmapLayersSet: IMapTypeSet;
    FActiveBitmapLayersSet: IMapTypeSetChangeable;
    FActiveBitmapLayersList: IMapTypeListChangeable;

    FKmlLayersSet: IMapTypeSet;
    FActiveKmlLayersSet: IMapTypeSetChangeable;

    FMiniMapMapsSet: IMapTypeSet;
    FMiniMapActiveMap: IMapTypeChangeable;

    FMiniMapLayersSet: IMapTypeSet;
    FMiniMapActiveLayersSet: IMapTypeSetChangeable;
    FMiniMapActiveBitmapLayersList: IMapTypeListChangeable;

    FFillingMapActiveMap: IMapTypeChangeable;
  protected
    function GetMapsSet: IMapTypeSet;
    function GetLayersSet: IMapTypeSet;
    function GetAllMapsSet: IMapTypeSet;

    function GetActiveMap: IMapTypeChangeable;
    function GetActiveLayersSet: IMapTypeSetChangeable;
    function GetAllActiveMapsSet: IMapTypeSetChangeable;
    function GetActiveMapsSetLicenseNotEmpty: IMapTypeSetChangeable;

    function GetActiveBitmapMapsSet: IMapTypeSetChangeable;
    function GetActiveBitmapLayersSet: IMapTypeSetChangeable;
    function GetActiveBitmapLayersList: IMapTypeListChangeable;
    function GetActiveKmlLayersSet: IMapTypeSetChangeable;

    function GetMiniMapMapsSet: IMapTypeSet;
    function GetMiniMapLayersSet: IMapTypeSet;

    function GetMiniMapActiveMap: IMapTypeChangeable;
    function GetMiniMapActiveLayersSet: IMapTypeSetChangeable;
    function GetMiniMapActiveBitmapLayersList: IMapTypeListChangeable;

    function GetFillingMapActiveMap: IMapTypeChangeable;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
      const AMapsSet, ALayersSet: IMapTypeSet;
      const AFirstMainMapGUID: TGUID;
      const AMainMapConfig: IActiveMapConfig;
      const AMainLayersConfig: IActiveLayersConfig;
      const AMiniMapConfig: IActiveMapConfig;
      const AMiniLayersConfig: IActiveLayersConfig;
      const AFillingMapConfig: IActiveMapConfig
    );
  end;

implementation

uses
  c_ZeroGUID,
  u_MapTypeChangeableByConfig,
  u_MapTypeSetChangeable,
  u_MapTypeSetChangeableBySourceSetWithFilter,
  u_MapTypeListChangeableActiveBitmapLayers,
  u_MapTypeChangeableWithDefault;

{ TMainMapsConfig }

constructor TMainMapsState.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMapTypeListBuilderFactory: IMapTypeListBuilderFactory;
  const AMapsSet, ALayersSet: IMapTypeSet;
  const AFirstMainMapGUID: TGUID;
  const AMainMapConfig: IActiveMapConfig;
  const AMainLayersConfig: IActiveLayersConfig;
  const AMiniMapConfig: IActiveMapConfig;
  const AMiniLayersConfig: IActiveLayersConfig;
  const AFillingMapConfig: IActiveMapConfig
);
var
  VMapType: IMapType;
  VAllMapsSet: IMapTypeSetBuilder;
  VBitmapLayersSet: IMapTypeSetBuilder;
  VKmlLayersSet: IMapTypeSetBuilder;
  VMiniMapMapsSet: IMapTypeSetBuilder;
  VMiniMapLayersSet: IMapTypeSetBuilder;
  i: Integer;
  VGUID: TGUID;
begin
  Assert(Assigned(AMapTypeSetBuilderFactory));
  Assert(Assigned(AMapTypeListBuilderFactory));
  Assert(Assigned(AMapsSet));
  Assert(Assigned(AMainMapConfig));
  Assert(Assigned(AMainLayersConfig));
  Assert(Assigned(AMiniMapConfig));
  Assert(Assigned(AMiniLayersConfig));
  Assert(Assigned(AFillingMapConfig));
  inherited Create;
  FMapsSet := AMapsSet;
  FLayersSet := ALayersSet;
  FFirstMainMapGUID := AFirstMainMapGUID;
  FMainMapConfig := AMainMapConfig;
  FMainLayersConfig := AMainLayersConfig;
  FMiniMapConfig := AMiniMapConfig;
  FMiniLayersConfig := AMiniLayersConfig;
  FFillingMapConfig := AFillingMapConfig;

  VAllMapsSet := AMapTypeSetBuilderFactory.Build(False);
  VKmlLayersSet := AMapTypeSetBuilderFactory.Build(False);
  VBitmapLayersSet := AMapTypeSetBuilderFactory.Build(False);

  VMiniMapMapsSet := AMapTypeSetBuilderFactory.Build(False);
  VMiniMapLayersSet := AMapTypeSetBuilderFactory.Build(False);
  for i := 0 to AMapsSet.Count - 1 do begin
    VMapType := AMapsSet.Items[i];
    VAllMapsSet.Add(VMapType);
    if VMapType.Abilities.IsShowOnSmMap and VMapType.IsBitmapTiles then begin
      VMiniMapMapsSet.Add(VMapType);
    end;
  end;
  if Assigned(ALayersSet) then begin
    for i := 0 to ALayersSet.Count - 1 do begin
      VMapType := ALayersSet.Items[i];
      VAllMapsSet.Add(VMapType);
      if VMapType.IsBitmapTiles then begin
        if VMapType.Abilities.IsShowOnSmMap then begin
          VMiniMapLayersSet.Add(VMapType);
        end;
        VBitmapLayersSet.Add(VMapType);
      end else if VMapType.IsKmlTiles then begin
        VKmlLayersSet.Add(VMapType);
      end;
    end;
  end;
  FAllMapsSet := VAllMapsSet.MakeAndClear;
  FBitmapLayersSet := VBitmapLayersSet.MakeAndClear;
  FKmlLayersSet := VKmlLayersSet.MakeAndClear;


  VGUID := FMainMapConfig.MainMapGUID;
  if not FMapsSet.IsExists(VGUID) then begin
    FMainMapConfig.MainMapGUID := FFirstMainMapGUID;
  end;
  FActiveMap :=
    TMapTypeChangeableByConfig.Create(
      FMainMapConfig,
      FMapsSet
    );
  FActiveLayersSet :=
    TLayerSetChangeableByConfig.Create(
      AMapTypeSetBuilderFactory,
      FLayersSet,
      FMainLayersConfig
    );
  FAllActiveMapsSet :=
    TMapsSetChangeableMainAndLayers.Create(
      AMapTypeSetBuilderFactory,
      FActiveMap,
      FActiveLayersSet
    );

  FActiveMapsSetLicenseNotEmpty :=
    TMapTypeSetChangeableBySourceSetWithFilterLicenseNotEmpty.Create(
      AMapTypeSetBuilderFactory,
      FAllActiveMapsSet
    );

  FActiveBitmapLayersSet :=
    TMapTypeSetChangeableBySourceSetWithFilterBitmap.Create(
      AMapTypeSetBuilderFactory,
      FActiveLayersSet
    );

  FActiveBitmapMapsSet :=
    TMapsSetChangeableMainAndLayers.Create(
      AMapTypeSetBuilderFactory,
      FActiveMap,
      FActiveBitmapLayersSet
    );
  FActiveBitmapLayersList :=
    TMapTypeListChangeableByActiveMapsSet.Create(
      AMapTypeListBuilderFactory,
      FActiveBitmapLayersSet
    );

  FActiveKmlLayersSet :=
    TMapTypeSetChangeableBySourceSetWithFilterVector.Create(
      AMapTypeSetBuilderFactory,
      FActiveLayersSet
    );

  FMiniMapMapsSet := VMiniMapMapsSet.MakeAndClear;
  FMiniMapLayersSet := VMiniMapLayersSet.MakeAndClear;

  if Assigned(VMiniMapMapsSet) then begin
    VGUID := FMiniMapConfig.MainMapGUID;
    if not FMiniMapMapsSet.IsExists(VGUID) then begin
      FMiniMapConfig.MainMapGUID := CGUID_Zero;
    end;
    FMiniMapActiveMap :=
      TMapTypeChangeableWithDefault.Create(
        FMiniMapMapsSet,
        FActiveMap,
        FMiniMapConfig
      );
  end else begin
    FMiniMapActiveMap := FActiveMap;
  end;

  FMiniMapActiveLayersSet :=
    TLayerSetChangeableByConfig.Create(
      AMapTypeSetBuilderFactory,
      FMiniMapLayersSet,
      FMiniLayersConfig
    );
  FMiniMapActiveBitmapLayersList :=
    TMapTypeListChangeableByActiveMapsSet.Create(
      AMapTypeListBuilderFactory,
      FMiniMapActiveLayersSet
    );

  VGUID := FFillingMapConfig.MainMapGUID;
  if not FAllMapsSet.IsExists(VGUID) then begin
    FFillingMapConfig.MainMapGUID := CGUID_Zero;
  end;
  FFillingMapActiveMap :=
    TMapTypeChangeableWithDefault.Create(
      FAllMapsSet,
      FActiveMap,
      FFillingMapConfig
    );
end;

function TMainMapsState.GetActiveBitmapLayersList: IMapTypeListChangeable;
begin
  Result := FActiveBitmapLayersList;
end;

function TMainMapsState.GetActiveBitmapLayersSet: IMapTypeSetChangeable;
begin
  Result := FActiveBitmapLayersSet;
end;

function TMainMapsState.GetActiveBitmapMapsSet: IMapTypeSetChangeable;
begin
  Result := FActiveBitmapMapsSet;
end;

function TMainMapsState.GetActiveKmlLayersSet: IMapTypeSetChangeable;
begin
  Result := FActiveKmlLayersSet;
end;

function TMainMapsState.GetActiveLayersSet: IMapTypeSetChangeable;
begin
  Result := FActiveLayersSet;
end;

function TMainMapsState.GetActiveMap: IMapTypeChangeable;
begin
  Result := FActiveMap;
end;

function TMainMapsState.GetActiveMapsSetLicenseNotEmpty: IMapTypeSetChangeable;
begin
  Result := FActiveMapsSetLicenseNotEmpty;
end;

function TMainMapsState.GetAllActiveMapsSet: IMapTypeSetChangeable;
begin
  Result := FAllActiveMapsSet;
end;

function TMainMapsState.GetAllMapsSet: IMapTypeSet;
begin
  Result := FAllMapsSet;
end;

function TMainMapsState.GetFillingMapActiveMap: IMapTypeChangeable;
begin
  Result := FFillingMapActiveMap;
end;

function TMainMapsState.GetLayersSet: IMapTypeSet;
begin
  Result := FLayersSet;
end;

function TMainMapsState.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

function TMainMapsState.GetMiniMapActiveBitmapLayersList: IMapTypeListChangeable;
begin
  Result := FMiniMapActiveBitmapLayersList;
end;

function TMainMapsState.GetMiniMapActiveLayersSet: IMapTypeSetChangeable;
begin
  Result := FMiniMapActiveLayersSet;
end;

function TMainMapsState.GetMiniMapActiveMap: IMapTypeChangeable;
begin
  Result := FMiniMapActiveMap;
end;

function TMainMapsState.GetMiniMapLayersSet: IMapTypeSet;
begin
  Result := FMiniMapLayersSet;
end;

function TMainMapsState.GetMiniMapMapsSet: IMapTypeSet;
begin
  Result := FMiniMapMapsSet;
end;

end.
