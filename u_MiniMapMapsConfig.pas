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

unit u_MiniMapMapsConfig;

interface

uses
  i_Notifier,
  i_Listener,
  i_ActiveMapsConfig,
  i_MapTypes,
  i_MiniMapLayerConfig,
  u_ActivMapWithLayers;

type
  TMiniMapMapsConfig = class(TActivMapWithLayers, IMiniMapMapsConfig)
  private
    FActiveMiniMap: IMapType;
    FSelectedMapChangeListener: IListener;
    FMainMapsConfig: IMainMapsConfig;
    FMainMapChangeListener: IListener;
    function CreateMiniMapMapsSet: IMapTypeSet;
    function CreateMiniMapLayersSet: IMapTypeSet;
    procedure OnMainMapChange;
    procedure OnSelectedChange(const AGUID: TGUID);
    procedure SetActiveMiniMap(const AValue: IMapType);
  protected
    function GetActiveMiniMap: IMapType;
  public
    constructor Create(const AMapsConfig: IMainMapsConfig);
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  c_ZeroGUID,
  u_NotifyEventListener,
  u_NotifyWithGUIDEvent,
  u_MapTypeBasic,
  u_MapTypeSet;

{ TMiniMapMapsConfig }

constructor TMiniMapMapsConfig.Create(const AMapsConfig: IMainMapsConfig);
begin
  FMainMapsConfig := AMapsConfig;
  inherited Create(CreateMiniMapMapsSet, CreateMiniMapLayersSet);

  FMainMapChangeListener := TNotifyNoMmgEventListener.Create(Self.OnMainMapChange);
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Add(FMainMapChangeListener);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  OnSelectedChange(GetActiveMap.GetSelectedGUID);
end;

destructor TMiniMapMapsConfig.Destroy;
begin
  FMainMapsConfig.GetActiveMap.GetChangeNotifier.Remove(FMainMapChangeListener);
  FMainMapChangeListener := nil;

  MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
  FSelectedMapChangeListener := nil;

  FMainMapsConfig := nil;
  inherited;
end;

function TMiniMapMapsConfig.GetActiveMiniMap: IMapType;
begin
  LockRead;
  try
    Result := FActiveMiniMap;
  finally
    UnlockRead;
  end;
end;

function TMiniMapMapsConfig.CreateMiniMapLayersSet: IMapTypeSet;
var
  VSourceSet: IMapTypeSet;
  VMap: IMapType;
  VList: TMapTypeSet;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VSourceSet := FMainMapsConfig.GetActiveBitmapLayersSet.GetMapsSet;
  VList := TMapTypeSet.Create(True);
  Result := VList;
  VEnun := VSourceSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := VSourceSet.GetMapTypeByGUID(VGUID);
    if VMap.MapType.Abilities.IsShowOnSmMap then begin
      VList.Add(VMap);
    end;
  end;
end;

function TMiniMapMapsConfig.CreateMiniMapMapsSet: IMapTypeSet;
var
  VSourceSet: IMapTypeSet;
  VMap: IMapType;
  VList: TMapTypeSet;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VSourceSet := FMainMapsConfig.GetActiveMap.GetMapsSet;
  VList := TMapTypeSet.Create(True);
  Result := VList;
  VList.Add(TMapTypeBasic.Create(nil));
  VEnun := VSourceSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := VSourceSet.GetMapTypeByGUID(VGUID);
    if VMap.MapType.Abilities.IsShowOnSmMap then begin
      VList.Add(VMap);
    end;
  end;
end;

procedure TMiniMapMapsConfig.OnMainMapChange;
var
  VGUID: TGUID;
begin
  VGUID := GetActiveMap.GetSelectedGUID;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    SetActiveMiniMap(FMainMapsConfig.GetSelectedMapType);
  end;
end;

procedure TMiniMapMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  if IsEqualGUID(AGUID, CGUID_Zero) then begin
    SetActiveMiniMap(FMainMapsConfig.GetSelectedMapType);
  end else begin
    SetActiveMiniMap(GetActiveMap.GetMapsSet.GetMapTypeByGUID(AGUID));
  end;
end;

procedure TMiniMapMapsConfig.SetActiveMiniMap(const AValue: IMapType);
begin
  LockWrite;
  try
    if FActiveMiniMap <> AValue then begin
      FActiveMiniMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
