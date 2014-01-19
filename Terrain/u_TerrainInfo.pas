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

unit u_TerrainInfo;

interface

uses
  SyncObjs,
  t_GeoTypes,
  i_Listener,
  i_TerrainInfo,
  i_TerrainConfig,
  i_TerrainProvider,
  i_TerrainProviderList,
  u_BaseInterfacedObject;

type
  TTerrainInfo = class(TBaseInterfacedObject, ITerrainInfo)
  private
    FTerrainConfig: ITerrainConfig;
    FPrimaryTerrainProviderGUID: TGUID;
    FPrimaryTerrainProvider: ITerrainProvider;
    FTerrainProviderList: ITerrainProviderList;
    FLastPoint: TDoublePoint;
    FLastZoom: Byte;
    FLastElevation: Integer;
    FProviderStateListner: IListener;
    FConfigSync: TCriticalSection;
    function GetElevationInfo(
      const APoint: TDoublePoint;
      const AZoom: Byte
    ): Integer;
    procedure OnProviderStateChange;
  public
    constructor Create(
      const ATerrainConfig: ITerrainConfig;
      const ATerrainProviderList: ITerrainProviderList
    );
    destructor Destroy; override;
    function GetElevationInfoStr(
      const APoint: TDoublePoint;
      const AZoom: Byte
    ): string;
  end;

implementation

uses
  ActiveX,
  SysUtils,
  Math,
  i_Notifier,
  i_TerrainProviderListElement,
  c_TerrainProvider,
  u_GeoFunc,
  u_ListenerByEvent,
  u_ResStrings;

const
  cUndefinedPointValue: TDoublePoint = (X: NAN; Y: NAN);
  cUndefinedZoomValue = $FF;

{ TTerrainInfo }

constructor TTerrainInfo.Create(
  const ATerrainConfig: ITerrainConfig;
  const ATerrainProviderList: ITerrainProviderList
);
var
  VGUID: TGUID;
  VTmp: Cardinal;
  VEnum: IEnumGUID;
  VElement: ITerrainProviderListElement;
  VProvider: ITerrainProvider;
  VNotifier: INotifier;
begin
  inherited Create;
  FConfigSync := TCriticalSection.Create;
  FTerrainConfig := ATerrainConfig;
  FTerrainProviderList := ATerrainProviderList;
  FPrimaryTerrainProviderGUID := FTerrainConfig.ElevationPrimaryProvider;

  VElement := FTerrainProviderList.Get(FPrimaryTerrainProviderGUID);
  if VElement <> nil then begin
    FPrimaryTerrainProvider := VElement.Provider;
    if FPrimaryTerrainProvider <> nil then begin
      FTerrainConfig.ElevationInfoAvailable := FPrimaryTerrainProvider.Available;
    end;
  end;

  FProviderStateListner := TNotifyNoMmgEventListener.Create(Self.OnProviderStateChange);
  FTerrainConfig.ChangeNotifier.Add(FProviderStateListner);

  FConfigSync.Acquire;
  try
    VEnum := FTerrainProviderList.GetGUIDEnum;
    while VEnum.Next(1, VGUID, VTmp) = S_OK do begin
      VElement := FTerrainProviderList.Get(VGUID);
      if VElement <> nil then begin
        VProvider := VElement.Provider;
        if VProvider <> nil then begin
          if FTerrainConfig.TrySecondaryElevationProviders then begin
            FTerrainConfig.ElevationInfoAvailable :=
              FTerrainConfig.ElevationInfoAvailable or VProvider.Available;
          end;
          VNotifier := VProvider.StateChangeNotifier;
          if VNotifier <> nil then begin
            VNotifier.Add(FProviderStateListner);
          end;
        end;
      end;
    end;
  finally
    FConfigSync.Release;
  end;

  FLastPoint := cUndefinedPointValue;
  FLastZoom := cUndefinedZoomValue;
  FLastElevation := cUndefinedElevationValue;
end;

destructor TTerrainInfo.Destroy;
var
  VGUID: TGUID;
  VTmp: Cardinal;
  VEnum: IEnumGUID;
  VElement: ITerrainProviderListElement;
  VProvider: ITerrainProvider;
  VNotifier: INotifier;
begin
  if (FTerrainProviderList <> nil) and (FProviderStateListner <> nil) then begin
    VEnum := FTerrainProviderList.GetGUIDEnum;
    while VEnum.Next(1, VGUID, VTmp) = S_OK do begin
      VElement := FTerrainProviderList.Get(VGUID);
      if VElement <> nil then begin
        VProvider := VElement.Provider;
        if VProvider <> nil then begin
          VNotifier := VProvider.StateChangeNotifier;
          if VNotifier <> nil then begin
            VNotifier.Remove(FProviderStateListner);
          end;
        end;
      end;
    end;
  end;

  if (FTerrainConfig <> nil) and (FProviderStateListner <> nil) then begin
    FTerrainConfig.ChangeNotifier.Remove(FProviderStateListner);
  end;

  FProviderStateListner := nil;
  FTerrainProviderList := nil;
  FTerrainConfig := nil;

  FreeAndNil(FConfigSync);

  inherited;
end;

procedure TTerrainInfo.OnProviderStateChange;
var
  VGUID: TGUID;
  VTmp: Cardinal;
  VEnum: IEnumGUID;
  VElement: ITerrainProviderListElement;
  VProvider: ITerrainProvider;
  VInfoAvail: Boolean;
  VFirstAvailProvider: ITerrainProvider;
  VFirstAvailProviderGUID: TGUID;
begin
  FConfigSync.Acquire;
  try
    FPrimaryTerrainProviderGUID := FTerrainConfig.ElevationPrimaryProvider;

    if (FTerrainProviderList <> nil) then begin
      VInfoAvail := False;
      VFirstAvailProvider := nil;
      VEnum := FTerrainProviderList.GetGUIDEnum;
      while VEnum.Next(1, VGUID, VTmp) = S_OK do begin
        VElement := FTerrainProviderList.Get(VGUID);
        if VElement <> nil then begin
          if IsEqualGUID(VElement.GUID, FPrimaryTerrainProviderGUID) then begin
            FPrimaryTerrainProvider := VElement.Provider;
            VInfoAvail := VInfoAvail or FPrimaryTerrainProvider.Available;
          end;

          VProvider := VElement.Provider;
          if VProvider <> nil then begin
            if (VProvider.Available) and (VFirstAvailProvider = nil) then begin
              VFirstAvailProvider := VProvider;
              VFirstAvailProviderGUID := VElement.GUID;
            end;
            if FTerrainConfig.TrySecondaryElevationProviders and
               not IsEqualGUID(VElement.GUID, FPrimaryTerrainProviderGUID)
            then begin
              VInfoAvail := VInfoAvail or VProvider.Available;
            end;
          end;
        end;
      end; // while

      // if external provider selected and then removed
      if ((FPrimaryTerrainProvider=nil) or (not FPrimaryTerrainProvider.Available)) and
        (VFirstAvailProvider <> nil) and
        FTerrainConfig.TrySecondaryElevationProviders
      then begin
        FPrimaryTerrainProvider := VFirstAvailProvider;
        FPrimaryTerrainProviderGUID := VFirstAvailProviderGUID;
      end;

      FTerrainConfig.ElevationInfoAvailable := VInfoAvail;
      FTerrainConfig.ElevationPrimaryProvider := FPrimaryTerrainProviderGUID;
    end;
  finally
    FConfigSync.Release;
  end;
end;

function TTerrainInfo.GetElevationInfo(
  const APoint: TDoublePoint;
  const AZoom: Byte
): Integer;
var
  VGUID: TGUID;
  VTmp: Cardinal;
  VEnum: IEnumGUID;
  VItem: ITerrainProviderListElement;
  VProvider: ITerrainProvider;
begin
  FConfigSync.Acquire;
  try
    if not DoublePointsEqual(FLastPoint, cUndefinedPointValue) and
       DoublePointsEqual(FLastPoint, APoint) and (FLastZoom = AZoom) and
       (FLastElevation <> cUndefinedElevationValue)
    then begin
      Result := FLastElevation;
      Exit;
    end;

    Result := Round(FPrimaryTerrainProvider.GetPointElevation(APoint, AZoom));
    if (Result = cUndefinedElevationValue) and FTerrainConfig.TrySecondaryElevationProviders then begin
      VEnum := FTerrainProviderList.GetGUIDEnum;
      while VEnum.Next(1, VGUID, VTmp) = S_OK do begin
        VItem := FTerrainProviderList.Get(VGUID);
        if VItem <> nil then begin
          VProvider := VItem.Provider;
          Result := Round(VProvider.GetPointElevation(APoint, AZoom));
          if Result <> cUndefinedElevationValue then begin
            FTerrainConfig.LastActualProviderWithElevationData := VItem.GUID;
            Break;
          end;
        end;
      end;
    end else begin
      FTerrainConfig.LastActualProviderWithElevationData := FPrimaryTerrainProviderGUID;
    end;

    if Result = cUndefinedElevationValue then begin
      Result := 0;
    end;

    FLastPoint := APoint;
    FLastZoom := AZoom;
    FLastElevation := Result;
  finally
    FConfigSync.Release;
  end;
end;

function TTerrainInfo.GetElevationInfoStr(
  const APoint: TDoublePoint;
  const AZoom: Byte
): string;
begin
  Result := IntToStr(GetElevationInfo(APoint, AZoom)) + ' ' + SAS_UNITS_m;
end;

end.
