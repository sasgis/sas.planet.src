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
  FDrawParent.Visible := False;
  FVisibilityToggleItem.Enabled := False;

  if FfrElevationProfile <> nil then begin
    FfrElevationProfile.Visible := False;
  end;
end;

procedure TElevationProfilePresenterOnPanel.ShowProfile(
  const AItem: IVectorDataItem
);

  function IsElevationMetaPresent(
    const ALines: TArrayOfGeometryLonLatSingleLine
  ): Boolean;
  var
    I: Integer;
    VMeta: PDoublePointsMeta;
  begin
    Result := False;
    for I := 0 to Length(ALines) - 1 do begin
      VMeta := ALines[I].Meta;
      if (VMeta <> nil) and (VMeta.Elevation <> nil) then begin
        Result := True;
        Exit;
      end;
    end;
  end;

var
  VLines: TArrayOfGeometryLonLatSingleLine;
begin
  Assert(Supports(AItem.Geometry, IGeometryLonLatLine));

  if FfrElevationProfile = nil then begin
    FfrElevationProfile := TfrElevationProfile.Create(
      FDrawParent,
      Self.HideParent,
      FConfig,
      FLanguageManager,
      FDatum,
      FMapGoTo
    );
  end;

  VLines := GeometryLonLatLineToArray(AItem.Geometry as IGeometryLonLatLine);

  if IsElevationMetaPresent(VLines) then begin
    ShowProfileInternal(AItem, VLines);
  end else begin
    FElevationMetaWriter.ProcessItemAsync(
      AItem,
      Self.OnElevationMetaWrite
    );
  end;
end;

procedure TElevationProfilePresenterOnPanel.ShowProfileInternal(
  const AItem: IVectorDataItem;
  const ALines: TArrayOfGeometryLonLatSingleLine
);
begin
  if FDrawParent.Height < 200 then begin
    FDrawParent.Height := 200;
  end;

  FfrElevationProfile.ShowProfile(ALines);
  FfrElevationProfile.Visible := True;

  FDrawParent.Visible := True;
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
