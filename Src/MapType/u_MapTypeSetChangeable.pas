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

unit u_MapTypeSetChangeable;

interface

uses
  SysUtils,
  i_Notifier,
  i_Listener,
  i_MapType,
  i_MapTypeSet,
  i_MapTypeSetBuilder,
  i_ActiveMapsConfig,
  i_MapTypeSetChangeable,
  u_ChangeableBase;

type
  TLayerSetChangeableByConfig = class(TChangeableWithSimpleLockBase, IMapTypeSetChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FLayersSet: IMapTypeSet;
    FConfig: IActiveLayersConfig;

    FStatic: IMapTypeSet;

    FListener: IListener;

    procedure OnConfigChange;
  private
    function GetStatic: IMapTypeSet;
  public
    constructor Create(
      const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
      const ALayersSet: IMapTypeSet;
      const AConfig: IActiveLayersConfig
    );
    destructor Destroy; override;
  end;

  TMapsSetChangeableMainAndLayers = class(TChangeableWithSimpleLockBase, IMapTypeSetChangeable)
  private
    FMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
    FMainMap: IMapTypeChangeable;
    FLayersSet: IMapTypeSetChangeable;

    FStatic: IMapTypeSet;

    FListener: IListener;

    procedure OnChange;
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
  i_GUIDListStatic,
  u_ListenerByEvent;

{ TLayerSetChangeableByConfig }

constructor TLayerSetChangeableByConfig.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const ALayersSet: IMapTypeSet;
  const AConfig: IActiveLayersConfig
);
begin
  Assert(Assigned(AMapTypeSetBuilderFactory));
  Assert(Assigned(AConfig));
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FLayersSet := ALayersSet;
  FConfig := AConfig;
  if Assigned(FLayersSet) then begin
    FListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
    FConfig.ChangeNotifier.Add(FListener);
    OnConfigChange;
  end;
end;

destructor TLayerSetChangeableByConfig.Destroy;
begin
  if Assigned(FConfig) and Assigned(FListener) then begin
    FConfig.ChangeNotifier.Remove(FListener);
    FListener := nil;
    FConfig := nil;
  end;
  inherited;
end;

function TLayerSetChangeableByConfig.GetStatic: IMapTypeSet;
begin
  Result := nil;
  if Assigned(CS) then begin
    CS.BeginRead;
    try
      Result := FStatic;
    finally
      CS.EndRead;
    end;
  end;
end;

procedure TLayerSetChangeableByConfig.OnConfigChange;
var
  VGuids: IGUIDSetStatic;
  i: Integer;
  VResultBuilder: IMapTypeSetBuilder;
  VResult: IMapTypeSet;
  VGUID: TGUID;
  VMapType: IMapType;
  VChanged: Boolean;
begin
  VGuids := FConfig.LayerGuids;
  if Assigned(VGuids) then begin
    VResultBuilder := FMapTypeSetBuilderFactory.Build(False);
    for i := 0 to VGuids.Count - 1 do begin
      VGUID := VGuids.Items[i];
      VMapType := FLayersSet.GetMapTypeByGUID(VGUID);
      if Assigned(VMapType) then begin
        VResultBuilder.Add(VMapType);
      end;
    end;
    VResult := VResultBuilder.MakeAndClear;
  end else begin
    VResult := nil;
  end;
  VChanged := False;
  CS.BeginWrite;
  try
    if Assigned(VResult) then begin
      if not VResult.IsEqual(FStatic) then begin
        FStatic := VResult;
        VChanged := True;
      end;
    end else begin
      if Assigned(FStatic) then begin
        FStatic := nil;
        VChanged := True;
      end;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;


{ TMapsSetChangeableMainAndLayers }

constructor TMapsSetChangeableMainAndLayers.Create(
  const AMapTypeSetBuilderFactory: IMapTypeSetBuilderFactory;
  const AMainMap: IMapTypeChangeable;
  const ALayersSet: IMapTypeSetChangeable
);
begin
  Assert(Assigned(AMapTypeSetBuilderFactory));
  Assert(Assigned(AMainMap));
  Assert(Assigned(ALayersSet));
  inherited Create;
  FMapTypeSetBuilderFactory := AMapTypeSetBuilderFactory;
  FMainMap := AMainMap;
  FLayersSet := ALayersSet;
  FListener := TNotifyNoMmgEventListener.Create(Self.OnChange);
  FMainMap.ChangeNotifier.Add(FListener);
  FLayersSet.ChangeNotifier.Add(FListener);
  OnChange;
end;

destructor TMapsSetChangeableMainAndLayers.Destroy;
begin
  if Assigned(FMainMap) and Assigned(FListener) then begin
    FMainMap.ChangeNotifier.Remove(FListener);
    FMainMap := nil;
  end;
  if Assigned(FLayersSet) and Assigned(FListener) then begin
    FLayersSet.ChangeNotifier.Remove(FListener);
    FLayersSet := nil;
  end;
  FListener := nil;

  inherited;
end;

function TMapsSetChangeableMainAndLayers.GetStatic: IMapTypeSet;
begin
  CS.BeginRead;
  try
    Result := FStatic;
  finally
    CS.EndRead;
  end;
end;

procedure TMapsSetChangeableMainAndLayers.OnChange;
var
  VResultBuilder: IMapTypeSetBuilder;
  VResult: IMapTypeSet;
  VMapType: IMapType;
  VLayers: IMapTypeSet;
  i: Integer;
  VChanged: Boolean;
begin
  VResultBuilder := FMapTypeSetBuilderFactory.Build(False);
  VLayers := FLayersSet.GetStatic;
  if Assigned(VLayers) then begin
    for i := 0 to VLayers.Count - 1 do begin
      VResultBuilder.Add(VLayers.Items[i]);
    end;
  end;
  VMapType := FMainMap.GetStatic;
  Assert(Assigned(VMapType));
  VResultBuilder.Add(VMapType);
  VResult := VResultBuilder.MakeAndClear;
  VChanged := False;
  CS.BeginWrite;
  try
    if not VResult.IsEqual(FStatic) then begin
      FStatic := VResult;
      VChanged := True;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.
