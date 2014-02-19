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
  i_MapTypes,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  u_ActivMapWithLayers;

type
  TMiniMapMapsConfig = class(TActivMapWithLayers, IMapTypeChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FStatic: IMapType;
    FSelectedMapChangeListener: IListener;
    FMainMap: IMapTypeChangeable;
    FMainMapChangeListener: IListener;
    function CreateMiniMapMapsSet(const ASourceMapsSet: IMapTypeSet): IMapTypeSet;
    function CreateMiniMapLayersSet(const ASourceLayersSet: IMapTypeSet): IMapTypeSet;
    procedure OnMainMapChange;
    procedure OnSelectedChange(const AGUID: TGUID);
    procedure SetActiveMiniMap(const AValue: IMapType);
  private
    function GetStatic: IMapType;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMainMap: IMapTypeChangeable;
      const AMapsSet: IMapTypeSet;
      const ALayersSet: IMapTypeSet
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  c_ZeroGUID,
  u_ListenerByEvent,
  u_NotifyWithGUIDEvent;

{ TMiniMapMapsConfig }

constructor TMiniMapMapsConfig.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMainMap: IMapTypeChangeable;
  const AMapsSet: IMapTypeSet;
  const ALayersSet: IMapTypeSet
);
begin
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FMainMap := AMainMap;
  inherited Create(True, AMapTypeSetBuilderFactory, CreateMiniMapMapsSet(AMapsSet), CreateMiniMapLayersSet(ALayersSet));

  FMainMapChangeListener := TNotifyNoMmgEventListener.Create(Self.OnMainMapChange);
  FMainMap.ChangeNotifier.Add(FMainMapChangeListener);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  OnSelectedChange(CGUID_Zero);
end;

destructor TMiniMapMapsConfig.Destroy;
begin
  if Assigned(FMainMap) and Assigned(FMainMapChangeListener) then begin
    FMainMap.ChangeNotifier.Remove(FMainMapChangeListener);
    FMainMapChangeListener := nil;
  end;

  if Assigned(MainMapChangeNotyfier) and Assigned(FSelectedMapChangeListener) then begin
    MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
    FSelectedMapChangeListener := nil;
  end;

  FMainMap := nil;
  inherited;
end;

function TMiniMapMapsConfig.GetStatic: IMapType;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

function TMiniMapMapsConfig.CreateMiniMapLayersSet(const ASourceLayersSet: IMapTypeSet): IMapTypeSet;
var
  VMap: IMapType;
  VList: IMapTypeSetBuilder;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VList := FMapTypeSetBuilderFactory.Build(True);
  VEnun := ASourceLayersSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := ASourceLayersSet.GetMapTypeByGUID(VGUID);
    if VMap.Abilities.IsShowOnSmMap and VMap.IsBitmapTiles then begin
      VList.Add(VMap);
    end;
  end;
  Result := VList.MakeAndClear;
end;

function TMiniMapMapsConfig.CreateMiniMapMapsSet(const ASourceMapsSet: IMapTypeSet): IMapTypeSet;
var
  VMap: IMapType;
  VList: IMapTypeSetBuilder;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VList := FMapTypeSetBuilderFactory.Build(True);
  VEnun := ASourceMapsSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMap := ASourceMapsSet.GetMapTypeByGUID(VGUID);
    if VMap.Abilities.IsShowOnSmMap and VMap.IsBitmapTiles then begin
      VList.Add(VMap);
    end;
  end;
  Result := VList.MakeAndClear;
end;

procedure TMiniMapMapsConfig.OnMainMapChange;
var
  VMapType: IMapType;
begin
  VMapType := GetActiveMap.GetStatic;
  if not Assigned(VMapType) then begin
    SetActiveMiniMap(FMainMap.GetStatic);
  end;
end;

procedure TMiniMapMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  if IsEqualGUID(AGUID, CGUID_Zero) then begin
    SetActiveMiniMap(FMainMap.GetStatic);
  end else begin
    SetActiveMiniMap(GetMapsSet.GetMapTypeByGUID(AGUID));
  end;
end;

procedure TMiniMapMapsConfig.SetActiveMiniMap(const AValue: IMapType);
begin
  Assert(Assigned(AValue));
  LockWrite;
  try
    if FStatic <> AValue then begin
      FStatic := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
