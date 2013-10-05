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
  i_Notifier,
  i_Listener,
  i_MapTypes,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_MapTypeSetChangeable,
  u_ConfigDataElementBase;

type
  TLayerSetChangeable = class(TConfigDataElementBaseEmptySaveLoad, IMapTypeSetChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FMapsSet: IMapTypeSet;

    FStatic: IMapTypeSet;

    FLayerSetSelectNotyfier: INotifier;
    FLayerSetUnselectNotyfier: INotifier;

    FLayerSetSelectListener: IListener;
    FLayerSetUnselectListener: IListener;

    procedure OnLayerSetSelect(const AGUID: TGUID);
    procedure OnLayerSetUnselect(const AGUID: TGUID);
  private
    function GetStatic: IMapTypeSet;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMapsSet: IMapTypeSet;
      const ALayerSetSelectNotyfier: INotifier;
      const ALayerSetUnselectNotyfier: INotifier
    );
    destructor Destroy; override;
  end;

  TMapsSetChangeableByMainMapAndLayersSet = class(TConfigDataElementWithStaticBaseEmptySaveLoad, IMapTypeSetChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FMainMap: IMapTypeChangeable;
    FLayersSet: IMapTypeSetChangeable;

    FMainMapListener: IListener;
    FLayerSetListener: IListener;

    procedure OnMainMapChange;
    procedure OnLayersSetChange;
  protected
    function CreateStatic: IInterface; override;
  private
    function GetStatic: IMapTypeSet;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const AMainMap: IMapTypeChangeable;
      const ALayersSet: IMapTypeSetChangeable
    );
    destructor Destroy; override;
  end;

implementation

uses
  ActiveX,
  u_ListenerByEvent,
  u_NotifyWithGUIDEvent;

{ TActiveMapsSet }

constructor TLayerSetChangeable.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMapsSet: IMapTypeSet;
  const ALayerSetSelectNotyfier, ALayerSetUnselectNotyfier: INotifier
);
var
  VMapTypeSetBuilder: IMapTypeSetBuilder;
begin
  Assert(AMapsSet <> nil);
  Assert(ALayerSetSelectNotyfier <> nil);
  Assert(ALayerSetUnselectNotyfier <> nil);
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FMapsSet := AMapsSet;
  VMapTypeSetBuilder := FMapTypeSetBuilderFactory.Build(True);
  FStatic := VMapTypeSetBuilder.MakeAndClear;

  FLayerSetSelectNotyfier := ALayerSetSelectNotyfier;
  if FLayerSetSelectNotyfier <> nil then begin
    FLayerSetSelectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetSelect);
    FLayerSetSelectNotyfier.Add(FLayerSetSelectListener);
  end;

  FLayerSetUnselectNotyfier := ALayerSetUnselectNotyfier;
  if FLayerSetUnselectNotyfier <> nil then begin
    FLayerSetUnselectListener := TNotifyWithGUIDEventListener.Create(Self.OnLayerSetUnselect);
    FLayerSetUnselectNotyfier.Add(FLayerSetUnselectListener);
  end;
end;

destructor TLayerSetChangeable.Destroy;
begin
  if Assigned(FLayerSetSelectNotyfier) and Assigned(FLayerSetSelectListener) then begin
    FLayerSetSelectNotyfier.Remove(FLayerSetSelectListener);
    FLayerSetSelectListener := nil;
    FLayerSetSelectNotyfier := nil;
  end;

  if Assigned(FLayerSetUnselectNotyfier) and Assigned(FLayerSetUnselectListener) then begin
    FLayerSetUnselectNotyfier.Remove(FLayerSetUnselectListener);
    FLayerSetUnselectListener := nil;
    FLayerSetUnselectNotyfier := nil;
  end;

  inherited;
end;

function TLayerSetChangeable.GetStatic: IMapTypeSet;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

procedure TLayerSetChangeable.OnLayerSetSelect(const AGUID: TGUID);
var
  VMapType: IMapType;
  VList: IMapTypeSetBuilder;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VMapType := FMapsSet.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    LockWrite;
    try
      if FStatic.GetMapTypeByGUID(AGUID) = nil then begin
        VList := FMapTypeSetBuilderFactory.Build(True);
        VList.Capacity := FStatic.Count + 1;
        VEnun := FStatic.GetIterator;
        while VEnun.Next(1, VGUID, i) = S_OK do begin
          VList.Add(FMapsSet.GetMapTypeByGUID(VGUID));
        end;
        VList.Add(VMapType);
        FStatic := VList.MakeAndClear;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

procedure TLayerSetChangeable.OnLayerSetUnselect(const AGUID: TGUID);
var
  VMapType: IMapType;
  VList: IMapTypeSetBuilder;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
begin
  VMapType := FMapsSet.GetMapTypeByGUID(AGUID);
  if VMapType <> nil then begin
    LockWrite;
    try
      if FStatic.GetMapTypeByGUID(AGUID) <> nil then begin
        VList := FMapTypeSetBuilderFactory.Build(True);
        if FStatic.Count > 0 then begin
          VList.Capacity := FStatic.Count - 1;
        end;
        VEnun := FStatic.GetIterator;
        while VEnun.Next(1, VGUID, i) = S_OK do begin
          if not IsEqualGUID(VGUID, AGUID) then begin
            VList.Add(FMapsSet.GetMapTypeByGUID(VGUID));
          end;
        end;
        FStatic := VList.MakeAndClear;
        SetChanged;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

{ TMapsSetChangeableByMainMapAndLayersSet }

constructor TMapsSetChangeableByMainMapAndLayersSet.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMainMap: IMapTypeChangeable;
  const ALayersSet: IMapTypeSetChangeable
);
begin
  Assert(AMainMap <> nil);
  Assert(ALayersSet <> nil);
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FMainMap := AMainMap;
  FLayersSet := ALayersSet;

  FMainMapListener := TNotifyNoMmgEventListener.Create(Self.OnMainMapChange);
  FMainMap.ChangeNotifier.Add(FMainMapListener);

  FLayerSetListener := TNotifyNoMmgEventListener.Create(Self.OnLayersSetChange);
  FLayersSet.ChangeNotifier.Add(FLayerSetListener);
end;

function TMapsSetChangeableByMainMapAndLayersSet.CreateStatic: IInterface;
var
  VLayersSet: IMapTypeSet;
  VList: IMapTypeSetBuilder;
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VStatic: IMapTypeSet;
begin
  VLayersSet := FLayersSet.GetStatic;
  VList := FMapTypeSetBuilderFactory.Build(True);
  VList.Capacity := VLayersSet.Count + 1;
  VEnun := VLayersSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VList.Add(VLayersSet.GetMapTypeByGUID(VGUID));
  end;
  VList.Add(FMainMap.GetStatic);
  VStatic := VList.MakeAndClear;
  Result := VStatic;
end;

destructor TMapsSetChangeableByMainMapAndLayersSet.Destroy;
begin
  if Assigned(FMainMap) and Assigned(FMainMapListener) then begin
    FMainMap.ChangeNotifier.Remove(FMainMapListener);
    FMainMap := nil;
    FMainMapListener := nil;
  end;

  if Assigned(FLayersSet) and Assigned(FLayerSetListener) then begin
    FLayersSet.ChangeNotifier.Remove(FLayerSetListener);
    FLayersSet := nil;
    FLayerSetListener := nil;
  end;

  inherited;
end;

function TMapsSetChangeableByMainMapAndLayersSet.GetStatic: IMapTypeSet;
begin
  Result := IMapTypeSet(GetStaticInternal);
end;

procedure TMapsSetChangeableByMainMapAndLayersSet.OnLayersSetChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMapsSetChangeableByMainMapAndLayersSet.OnMainMapChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
