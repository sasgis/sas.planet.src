{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ElevationProfilePresenterOnPanel;

interface

uses
  SysUtils,
  Controls,
  TB2Item,
  t_GeoTypes,
  i_GeoCalc,
  i_GeometryLonLat,
  i_ElevationMetaWriter,
  i_ElevationProfileConfig,
  i_ElevationProfilePresenter,
  i_LanguageManager,
  i_MapViewGoto,
  i_VectorDataItemSimple,
  i_TerrainConfig,
  i_Listener,
  u_BaseInterfacedObject,
  fr_ElevationProfile;

type
  TElevationProfilePresenterOnPanel = class(TBaseInterfacedObject, IElevationProfilePresenter)
  private
    FItemCached: IVectorDataItem;
    FLonLatLocation: TDoublePoint;

    FDrawParent: TWinControl;
    FVisibilityToggleItem: TTBCustomItem;
    FConfig: IElevationProfileConfig;
    FLanguageManager: ILanguageManager;
    FMapGoTo: IMapViewGoto;

    FGeoCalc: IGeoCalcChangeable;
    FGeoCalcListener: IListener;

    FTerrainConfig: ITerrainConfig;
    FTerrainConfigListener: IListener;

    FfrElevationProfile: TfrElevationProfile;
    FElevationMetaWriter: IElevationMetaWriter;

    procedure AddChangeListeners;
    procedure RemoveChangeListeners;

    procedure ShowProfileInternal(
      const ALines: TArrayOfGeometryLonLatSingleLine
    );

    procedure OnElevationMetaWrite(const ALine: IGeometryLonLatLine);

    procedure HideParent;
    procedure RefreshParent;
  private
    { IElevationProfilePresenter }
    procedure ShowProfile(
      const AItem: IVectorDataItem;
      const ALonLatLocation: PDoublePoint = nil
    );
  public
    constructor Create(
      const ADrawParent: TWinControl;
      const AVisibilityToggleItem: TTBCustomItem;
      const AConfig: IElevationProfileConfig;
      const ATerrainConfig: ITerrainConfig;
      const ALanguageManager: ILanguageManager;
      const AGeoCalc: IGeoCalcChangeable;
      const AMapGoTo: IMapViewGoto;
      const AElevationMetaWriter: IElevationMetaWriter
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_GeoFunc,
  u_GeometryFunc;

{ TElevationProfilePresenterOnPanel }

constructor TElevationProfilePresenterOnPanel.Create(
  const ADrawParent: TWinControl;
  const AVisibilityToggleItem: TTBCustomItem;
  const AConfig: IElevationProfileConfig;
  const ATerrainConfig: ITerrainConfig;
  const ALanguageManager: ILanguageManager;
  const AGeoCalc: IGeoCalcChangeable;
  const AMapGoTo: IMapViewGoto;
  const AElevationMetaWriter: IElevationMetaWriter
);
begin
  inherited Create;

  FDrawParent := ADrawParent;
  FVisibilityToggleItem := AVisibilityToggleItem;
  FConfig := AConfig;
  FTerrainConfig := ATerrainConfig;
  FLanguageManager := ALanguageManager;
  FGeoCalc := AGeoCalc;
  FMapGoTo := AMapGoTo;
  FElevationMetaWriter := AElevationMetaWriter;

  FfrElevationProfile := nil;

  FGeoCalcListener := nil;
  FTerrainConfigListener := nil;

  HideParent;
end;

destructor TElevationProfilePresenterOnPanel.Destroy;
begin
  RemoveChangeListeners;
  FfrElevationProfile := nil; // will be destroyed by its parent
  inherited Destroy;
end;

procedure TElevationProfilePresenterOnPanel.AddChangeListeners;
begin
  if FGeoCalcListener = nil then begin
    FGeoCalcListener := TNotifyNoMmgEventListener.Create(Self.RefreshParent);
    FGeoCalc.ChangeNotifier.Add(FGeoCalcListener);
  end;
  if (FTerrainConfigListener = nil) and (FTerrainConfig <> nil) then begin
    FTerrainConfigListener := TNotifyNoMmgEventListener.Create(Self.RefreshParent);
    FTerrainConfig.ChangeNotifier.Add(FTerrainConfigListener);
  end;
end;

procedure TElevationProfilePresenterOnPanel.RemoveChangeListeners;
begin
  if FGeoCalcListener <> nil  then begin
    FGeoCalc.ChangeNotifier.Remove(FGeoCalcListener);
    FGeoCalcListener := nil;
  end;
  if (FTerrainConfig <> nil) and (FTerrainConfigListener <> nil) then begin
    FTerrainConfig.ChangeNotifier.Remove(FTerrainConfigListener);
    FTerrainConfigListener := nil;
  end;
end;

procedure TElevationProfilePresenterOnPanel.HideParent;
begin
  RemoveChangeListeners;

  FItemCached := nil;
  FLonLatLocation := CEmptyDoublePoint;

  FDrawParent.Visible := False;
  FVisibilityToggleItem.Enabled := False;

  if FfrElevationProfile <> nil then begin
    FfrElevationProfile.Visible := False;
  end;
end;

procedure TElevationProfilePresenterOnPanel.RefreshParent;
var
  VItem: IVectorDataItem;
begin
  // initialize force redraw elevation profile
  VItem := FItemCached;
  FItemCached := nil;

  ShowProfile(VItem);
end;

procedure TElevationProfilePresenterOnPanel.ShowProfile(
  const AItem: IVectorDataItem;
  const ALonLatLocation: PDoublePoint
);
var
  VLines: TArrayOfGeometryLonLatSingleLine;
begin
  Assert(Supports(AItem.Geometry, IGeometryLonLatLine));

  if ALonLatLocation <> nil then begin
    FLonLatLocation := ALonLatLocation^;
  end else begin
    FLonLatLocation := CEmptyDoublePoint;
  end;

  if (FItemCached <> nil) and FItemCached.Geometry.IsSameGeometry(AItem.Geometry) then begin
    FfrElevationProfile.SetLocation(FLonLatLocation);
    Exit;
  end;

  RemoveChangeListeners;

  FItemCached := AItem;

  if FfrElevationProfile = nil then begin
    FfrElevationProfile :=
      TfrElevationProfile.Create(
        FDrawParent,
        Self.HideParent,
        Self.RefreshParent,
        FConfig,
        FLanguageManager,
        FMapGoTo
      );
  end;

  case FConfig.ElevationSource of
    esTrackMetadata: begin
      VLines := GeometryLonLatLineToArray(AItem.Geometry as IGeometryLonLatLine);
      ShowProfileInternal(VLines);
    end;
    esTerrainProvider: begin
      FElevationMetaWriter.ProcessLineAsync(
        AItem.Geometry as IGeometryLonLatLine,
        Self.OnElevationMetaWrite,
        (FConfig.MaxDistanceForIntermediatePoint > 0),
        FConfig.MaxDistanceForIntermediatePoint
      );
    end;
  else
    Assert(False);
  end;
end;

procedure TElevationProfilePresenterOnPanel.ShowProfileInternal(
  const ALines: TArrayOfGeometryLonLatSingleLine
);
begin
  FfrElevationProfile.ShowProfile(FGeoCalc.Datum, ALines);
  FfrElevationProfile.Visible := True;

  FDrawParent.Visible := True;
  if FDrawParent.Height < 200 then begin
    FDrawParent.Height := 200;
  end;

  FVisibilityToggleItem.Enabled := True;

  FfrElevationProfile.SetFocusOnChart;
  FfrElevationProfile.SetLocation(FLonLatLocation);

  AddChangeListeners;
end;

procedure TElevationProfilePresenterOnPanel.OnElevationMetaWrite(const ALine: IGeometryLonLatLine);
var
  VLines: TArrayOfGeometryLonLatSingleLine;
begin
  VLines := GeometryLonLatLineToArray(ALine);
  ShowProfileInternal(VLines);
end;

end.
