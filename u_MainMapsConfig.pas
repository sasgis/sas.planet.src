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

unit u_MainMapsConfig;

interface

uses
  i_JclNotify,
  i_ActiveMapsConfig,
  i_MapTypes,
  u_ActivMapWithLayers;

type
  TMainMapsConfig = class(TActivMapWithLayers, IMainMapsConfig)
  private
    FDefaultMapGUID: TGUID;
    FSelectedMapType: IMapType;
    FActiveBitmapLayersSet: IActiveMapsSet;
    FActiveKmlLayersSet: IActiveMapsSet;
    FSelectedMapChangeListener: IJclListener;
    procedure OnSelectedChange(const AGUID: TGUID);
  protected
    function GetSelectedMapType: IMapType;
    function GetActiveBitmapLayersSet: IActiveMapsSet;
    function GetActiveKmlLayersSet: IActiveMapsSet;
  public
    constructor Create(
      const AMapsSet, ALayersSet: IMapTypeSet;
      const ADefaultMapGUID: TGUID
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_NotifyWithGUIDEvent,
  u_MapTypeSet,
  u_ActiveMapsSet;

{ TMainMapsConfig }

constructor TMainMapsConfig.Create(
  const AMapsSet, ALayersSet: IMapTypeSet;
  const ADefaultMapGUID: TGUID
);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VBitmapLayersList: TMapTypeSet;
  VKmlLayersList: TMapTypeSet;
begin
  FDefaultMapGUID := ADefaultMapGUID;
  inherited Create(AMapsSet, ALayersSet);

  VBitmapLayersList := TMapTypeSet.Create(True);
  VKmlLayersList := TMapTypeSet.Create(True);

  VEnun := ALayersSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := ALayersSet.GetMapTypeByGUID(VGUID);
    if VMapType.MapType.IsBitmapTiles then begin
      VBitmapLayersList.Add(VMapType);
    end;
    if VMapType.MapType.IsKmlTiles then begin
      VKmlLayersList.Add(VMapType);
    end;
  end;

  FActiveBitmapLayersSet := TActiveMapsSet.Create(
    VBitmapLayersList,
    AllMapsSingleList,
    nil,
    LayerSetSelectNotyfier,
    LayerSetUnselectNotyfier
  );
  Add(FActiveBitmapLayersSet, nil);

  FActiveKmlLayersSet := TActiveMapsSet.Create(
    VKmlLayersList,
    AllMapsSingleList,
    nil,
    LayerSetSelectNotyfier,
    LayerSetUnselectNotyfier
  );
  Add(FActiveKmlLayersSet, nil);

  FSelectedMapChangeListener := TNotifyWithGUIDEventListener.Create(Self.OnSelectedChange);
  MainMapChangeNotyfier.Add(FSelectedMapChangeListener);

  SelectMainByGUID(FDefaultMapGUID);
  FDefaultMapGUID := GetActiveMap.GetSelectedGUID;
  OnSelectedChange(FDefaultMapGUID);
end;

destructor TMainMapsConfig.Destroy;
begin
  MainMapChangeNotyfier.Remove(FSelectedMapChangeListener);
  FSelectedMapChangeListener := nil;

  FActiveBitmapLayersSet := nil;
  FActiveKmlLayersSet := nil;
  inherited;
end;

function TMainMapsConfig.GetActiveBitmapLayersSet: IActiveMapsSet;
begin
  Result := FActiveBitmapLayersSet;
end;

function TMainMapsConfig.GetActiveKmlLayersSet: IActiveMapsSet;
begin
  Result := FActiveKmlLayersSet;
end;

function TMainMapsConfig.GetSelectedMapType: IMapType;
begin
  LockRead;
  try
    Result := FSelectedMapType;
  finally
    UnlockRead;
  end;
end;

procedure TMainMapsConfig.OnSelectedChange(const AGUID: TGUID);
begin
  LockWrite;
  try
    FSelectedMapType := GetActiveMap.GetMapsSet.GetMapTypeByGUID(AGUID);
    Assert(FSelectedMapType <> nil);
  finally
    UnlockWrite;
  end;
end;

end.
