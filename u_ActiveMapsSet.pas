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

unit u_ActiveMapsSet;

interface

uses
  i_JclNotify,
  i_GUIDSet,
  i_MapTypes,
  i_ActiveMapsConfig,
  u_ConfigDataElementBase;

type
  TActiveMapsSet = class(TConfigDataElementBaseEmptySaveLoad, IActiveMapsSet)
  private
    FMapsSet: IMapTypeSet;

    FSingeMapsSet: IGUIDInterfaceSet;
    FSelectedMapsList: IMapTypeSet;

    FMainMapChangeNotyfier: IJclNotifier;
    FMainMapListener: IJclListener;

    FLayerSetSelectNotyfier: IJclNotifier;
    FLayerSetUnselectNotyfier: IJclNotifier;

    FLayerSetSelectListener: IJclListener;
    FLayerSetUnselectListener: IJclListener;

    procedure OnMainMapChange(const AGUID: TGUID);
    procedure OnLayerSetSelectChange(const AGUID: TGUID);
    procedure OnLayerSetUnselectChange(const AGUID: TGUID);
  protected
    function IsGUIDSelected(const AMapGUID: TGUID): Boolean;
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
    function GetSelectedMapsSet: IMapTypeSet;
    function GetMapsSet: IMapTypeSet;
  public
    constructor Create(
      AMapsSet: IMapTypeSet;
      ASingeMapsList: IGUIDInterfaceSet;
      AMainMapChangeNotyfier: IJclNotifier;
      ALayerSetSelectNotyfier: IJclNotifier;
      ALayerSetUnselectNotyfier: IJclNotifier
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_MapTypeSet,
  u_NotifyWithGUIDEvent;

{ TActiveMapsSet }

constructor TActiveMapsSet.Create(AMapsSet: IMapTypeSet;
  ASingeMapsList: IGUIDInterfaceSet; AMainMapChangeNotyfier,
  ALayerSetSelectNotyfier, ALayerSetUnselectNotyfier: IJclNotifier);
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FSingeMapsSet := ASingeMapsList;
  FSelectedMapsList := TMapTypeSet.Create(True);

  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  if FMainMapChangeNotyfier <> nil then begin
    FMainMapListener := TNotifyWithGUIDEventListener.Create(Self.OnMainMapChange);
    FMainMapChangeNotyfier.Add(FMainMapListener);
  end;

  FLayerSetSelectNotyfier := ALayerSetSelectNotyfier;
  if FLayerSetSelectNotyfier <> nil then begin
    FLayerSetSelectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetSelectChange);
    FLayerSetSelectNotyfier.Add(FLayerSetSelectListener);
  end;

  FLayerSetUnselectNotyfier := ALayerSetUnselectNotyfier;
  if FLayerSetUnselectNotyfier <> nil then begin
    FLayerSetUnselectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetUnselectChange);
    FLayerSetUnselectNotyfier.Add(FLayerSetUnselectListener);
  end;
end;

destructor TActiveMapsSet.Destroy;
begin
  if FMainMapChangeNotyfier <> nil then begin
    FMainMapChangeNotyfier.Remove(FMainMapListener);
  end;
  FMainMapListener := nil;
  FMainMapChangeNotyfier := nil;

  if FLayerSetSelectNotyfier <> nil then begin
    FLayerSetSelectNotyfier.Remove(FLayerSetSelectListener);
  end;
  FLayerSetSelectListener := nil;
  FLayerSetSelectNotyfier := nil;

  if FLayerSetUnselectNotyfier <> nil then begin
    FLayerSetUnselectNotyfier.Remove(FLayerSetUnselectListener);
  end;
  FLayerSetUnselectListener := nil;
  FLayerSetUnselectNotyfier := nil;

  inherited;
end;

function TActiveMapsSet.GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
begin
  if FMapsSet.GetMapTypeByGUID(AMapGUID) <> nil then begin
    Result := IActiveMapSingle(FSingeMapsSet.GetByGUID(AMapGUID));
  end;
end;

function TActiveMapsSet.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

function TActiveMapsSet.GetSelectedMapsSet: IMapTypeSet;
begin
  LockRead;
  try
    Result := FSelectedMapsList;
  finally
    UnlockRead;
  end;
end;

function TActiveMapsSet.IsGUIDSelected(const AMapGUID: TGUID): Boolean;
begin
  LockRead;
  try
    Result := FSelectedMapsList.GetMapTypeByGUID(AMapGUID) <> nil;
  finally
    UnlockRead;
  end;
end;

procedure TActiveMapsSet.OnLayerSetSelectChange(const AGUID: TGUID);
var
  VMapType: IMapType;
  VList: TMapTypeSet;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VMapType := FMapsSet.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    LockWrite;
    try
      if FSelectedMapsList.GetMapTypeByGUID(AGUID) = nil then begin
        VList := TMapTypeSet.Create(True);
        VEnun := FSelectedMapsList.GetIterator;
        while VEnun.Next(1, VGUID, i) = S_OK do begin
          VList.Add(FMapsSet.GetMapTypeByGUID(VGUID));
        end;
        VList.Add(VMapType);
        FSelectedMapsList := VList;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TActiveMapsSet.OnLayerSetUnselectChange(const AGUID: TGUID);
var
  VMapType: IMapType;
  VList: TMapTypeSet;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VMapType := FMapsSet.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    LockWrite;
    try
      if FSelectedMapsList.GetMapTypeByGUID(AGUID) <> nil then begin
        VList := TMapTypeSet.Create(True);
        VEnun := FSelectedMapsList.GetIterator;
        while VEnun.Next(1, VGUID, i) = S_OK do begin
          if not IsEqualGUID(VGUID, AGUID) then begin
            VList.Add(FMapsSet.GetMapTypeByGUID(VGUID));
          end;
        end;
        FSelectedMapsList := VList;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TActiveMapsSet.OnMainMapChange(const AGUID: TGUID);
var
  VMapSingle: IActiveMapSingle;
  VList: TMapTypeSet;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  LockWrite;
  try
    VList := TMapTypeSet.Create(True);
    VEnun := FMapsSet.GetIterator;
    while VEnun.Next(1, VGUID, i) = S_OK do begin
      VMapSingle := IActiveMapSingle(FSingeMapsSet.GetByGUID(VGUID));
      if VMapSingle.GetIsActive then begin
        VList.Add(VMapSingle.GetMapType);
      end;
    end;
    FSelectedMapsList := VList;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
