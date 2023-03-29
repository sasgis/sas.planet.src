{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
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
  i_Datum,
  i_GeometryLonLat,
  i_ElevationMetaWriter,
  i_ElevationProfileConfig,
  i_ElevationProfilePresenter,
  i_LanguageManager,
  i_MapViewGoto,
  i_VectorDataItemSimple,
  u_BaseInterfacedObject,
  fr_ElevationProfile;

type
  TElevationProfilePresenterOnPanel = class(TBaseInterfacedObject, IElevationProfilePresenter)
  private
    FItemCached: IVectorDataItem;

    FDrawParent: TWinControl;
    FVisibilityToggleItem: TTBCustomItem;
    FConfig: IElevationProfileConfig;
    FLanguageManager: ILanguageManager;
    FDatum: IDatum;
    FMapGoTo: IMapViewGoto;

    FfrElevationProfile: TfrElevationProfile;
    FElevationMetaWriter: IElevationMetaWriter;

    procedure ShowProfileInternal(
      const AItem: IVectorDataItem;
      const ALines: TArrayOfGeometryLonLatSingleLine
    );

    procedure OnElevationMetaWrite(const AItem: IVectorDataItem);

    procedure HideParent;
    procedure RefreshParent;
  private
    { IElevationProfilePresenter }
    procedure ShowProfile(const AItem: IVectorDataItem);
  public
    constructor Create(
      const ADrawParent: TWinControl;
      const AVisibilityToggleItem: TTBCustomItem;
      const AConfig: IElevationProfileConfig;
      const ALanguageManager: ILanguageManager;
      const ADatum: IDatum;
      const AMapGoTo: IMapViewGoto;
      const AElevationMetaWriter: IElevationMetaWriter
    );
    destructor Destroy; override;
  end;

implementation

uses
  t_GeoTypes,
  u_GeometryFunc;

{ TElevationProfilePresenterOnPanel }

constructor TElevationProfilePresenterOnPanel.Create(
  const ADrawParent: TWinControl;
  const AVisibilityToggleItem: TTBCustomItem;
  const AConfig: IElevationProfileConfig;
  const ALanguageManager: ILanguageManager;
  const ADatum: IDatum;
  const AMapGoTo: IMapViewGoto;
  const AElevationMetaWriter: IElevationMetaWriter
);
begin
  inherited Create;

  FDrawParent := ADrawParent;
  FVisibilityToggleItem := AVisibilityToggleItem;
  FConfig := AConfig;
  FLanguageManager := ALanguageManager;
  FDatum := ADatum;
  FMapGoTo := AMapGoTo;
  FElevationMetaWriter := AElevationMetaWriter;

  FfrElevationProfile := nil;

  HideParent;
end;

destructor TElevationProfilePresenterOnPanel.Destroy;
begin
  //FreeAndNil(FfrElevationProfile); // ???
  inherited Destroy;
end;

procedure TElevationProfilePresenterOnPanel.HideParent;
begin
  FItemCached := nil;

  FDrawParent.Visible := False;
  FVisibilityToggleItem.Enabled := False;

  if FfrElevationProfile <> nil then begin
    FfrElevationProfile.Visible := False;
  end;
end;

procedure TElevationProfilePresenterOnPanel.RefreshParent;
begin
  ShowProfile(FItemCached);
end;

procedure TElevationProfilePresenterOnPanel.ShowProfile(
  const AItem: IVectorDataItem
);
var
  VLines: TArrayOfGeometryLonLatSingleLine;
begin
  Assert(Supports(AItem.Geometry, IGeometryLonLatLine));

  FItemCached := AItem;

  if FfrElevationProfile = nil then begin
    FfrElevationProfile := TfrElevationProfile.Create(
      FDrawParent,
      Self.HideParent,
      Self.RefreshParent,
      FConfig,
      FLanguageManager,
      FDatum,
      FMapGoTo
    );
  end;

  VLines := GeometryLonLatLineToArray(AItem.Geometry as IGeometryLonLatLine);

  case FConfig.ElevationSource of
    esTrackMetadata: begin
      ShowProfileInternal(AItem, VLines);
    end;
    esDEM: begin
      FElevationMetaWriter.ProcessItemAsync(
        AItem,
        Self.OnElevationMetaWrite
      );
    end;
    esBoth: begin
      Assert(False, 'ToDo');
    end;
  else
    Assert(False);
  end;
end;

procedure TElevationProfilePresenterOnPanel.ShowProfileInternal(
  const AItem: IVectorDataItem;
  const ALines: TArrayOfGeometryLonLatSingleLine
);
begin
  FfrElevationProfile.ShowProfile(ALines);
  FfrElevationProfile.Visible := True;

  FDrawParent.Visible := True;
  if FDrawParent.Height < 200 then begin
    FDrawParent.Height := 200;
  end;

  FVisibilityToggleItem.Enabled := True;

  FfrElevationProfile.SetFocusOnChart;
end;

procedure TElevationProfilePresenterOnPanel.OnElevationMetaWrite(const AItem: IVectorDataItem);
var
  VLines: TArrayOfGeometryLonLatSingleLine;
begin
  VLines := GeometryLonLatLineToArray(AItem.Geometry as IGeometryLonLatLine);
  ShowProfileInternal(AItem, VLines);
end;

end.
