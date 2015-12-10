{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_FavoriteMapSetHelper;

interface

uses
  i_MapTypeSet,
  i_ViewPortState,
  i_ActiveMapsConfig,
  i_FavoriteMapSetHelper,
  i_FavoriteMapSetItemStatic,
  u_BaseInterfacedObject;

type
  TFavoriteMapSetHelper = class(TBaseInterfacedObject, IFavoriteMapSetHelper)
  private
    FMapsSet: IMapTypeSet;
    FMainMapConfig: IActiveMapConfig;
    FMapLayersConfig: IActiveLayersConfig;
    FViewPortState: IViewPortState;
  private
    function TrySwitchOn(
      const AItem: IFavoriteMapSetItemStatic;
      out AErrMsg: string
    ): Boolean;
  public
    constructor Create(
      const AMapsSet: IMapTypeSet;
      const AMainMapConfig: IActiveMapConfig;
      const AMapLayersConfig: IActiveLayersConfig;
      const AViewPortState: IViewPortState
    );
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  i_GUIDListStatic;

{ TFavoriteMapSetHelper }

constructor TFavoriteMapSetHelper.Create(
  const AMapsSet: IMapTypeSet;
  const AMainMapConfig: IActiveMapConfig;
  const AMapLayersConfig: IActiveLayersConfig;
  const AViewPortState: IViewPortState
);
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FMainMapConfig := AMainMapConfig;
  FMapLayersConfig := AMapLayersConfig;
  FViewPortState := AViewPortState;
end;

function TFavoriteMapSetHelper.TrySwitchOn(
  const AItem: IFavoriteMapSetItemStatic;
  out AErrMsg: string
): Boolean;
var
  I: Integer;
  VLayers: IGUIDSetStatic;
begin
  AErrMsg := '';
  Result := False;

  if not IsEqualGUID(AItem.BaseMap, CGUID_Zero) then begin
    if FMapsSet.IsExists(AItem.BaseMap) then begin
      FMainMapConfig.MainMapGUID := AItem.BaseMap;
    end else begin
      AErrMsg := 'Can''t switch Map - unknown GUID: ' + GUIDToString(AItem.BaseMap);
      Exit;
    end;
  end;

  VLayers := AItem.Layers;
  if Assigned(VLayers) then begin
    for I := 0 to VLayers.Count - 1 do begin
      if not FMapsSet.IsExists(VLayers.Items[I]) then begin
        AErrMsg := 'Can''t switch Layers - unknown GUID: ' + GUIDToString(VLayers.Items[I]);
        Exit;
      end;
    end;
  end;

  if not AItem.MergeLayers then begin
    FMapLayersConfig.LayerGuids := VLayers;
  end else if Assigned(VLayers) then begin
    for I := 0 to VLayers.Count - 1 do begin
      FMapLayersConfig.SelectLayerByGUID(VLayers.Items[I]);
    end;
  end;

  if AItem.Zoom >= 0 then begin
    FViewPortState.ChangeZoomWithFreezeAtCenter(AItem.Zoom);
  end;

  Result := True;
end;

end.
