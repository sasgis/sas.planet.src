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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_BitmapLayerProviderChangeableForGrids;

interface

uses
  i_MapLayerGridsConfig,
  i_Bitmap32BufferFactory,
  i_ValueToStringConverter,
  i_BitmapLayerProvider,
  i_ListenerNotifierLinksList,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForGrids = class(TBitmapLayerProviderChangeableBase)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FConfig: IMapLayerGridsConfig;
    FValueToStringConverter: IValueToStringConverterChangeable;

    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AValueToStringConverter: IValueToStringConverterChangeable;
      const AConfig: IMapLayerGridsConfig
    );
  end;

implementation

uses
  GR32,
  u_ListenerByEvent,
  u_BitmapLayerProviderComplex,
  u_BitmapLayerProviderGridGenshtab,
  u_BitmapLayerProviderGridDegree,
  u_BitmapLayerProviderGridTiles;

{ TBitmapLayerProviderChangeableForGrids }

constructor TBitmapLayerProviderChangeableForGrids.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AValueToStringConverter: IValueToStringConverterChangeable;
  const AConfig: IMapLayerGridsConfig
);
begin
  Assert(Assigned(AValueToStringConverter));
  Assert(Assigned(AConfig));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FConfig := AConfig;
  FValueToStringConverter := AValueToStringConverter;

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.TileGrid.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.GenShtabGrid.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.DegreeGrid.ChangeNotifier
  );
  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FValueToStringConverter.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForGrids.CreateStatic: IInterface;
var
  VVisible: Boolean;
  VColor: TColor32;
  VUseRelativeZoom: Boolean;
  VZoom: Integer;
  VShowText: Boolean;
  VShowLines: Boolean;
  VScale: Integer;
  VScaleDegree: Double;
  VProvider: IBitmapLayerProvider;
  VResult: IBitmapLayerProvider;
begin
  VResult := nil;
  FConfig.TileGrid.LockRead;
  try
    VVisible := FConfig.TileGrid.Visible;
    VColor := FConfig.TileGrid.GridColor;
    VUseRelativeZoom := FConfig.TileGrid.UseRelativeZoom;
    VZoom := FConfig.TileGrid.Zoom;
    VShowText := FConfig.TileGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.TileGrid.UnlockRead;
  end;
  if VVisible then begin
    VResult :=
      TBitmapLayerProviderGridTiles.Create(
        FBitmap32StaticFactory,
        VColor,
        VUseRelativeZoom,
        VZoom,
        VShowText,
        VShowLines
      );
  end;
  FConfig.GenShtabGrid.LockRead;
  try
    VVisible := FConfig.GenShtabGrid.Visible;
    VColor := FConfig.GenShtabGrid.GridColor;
    VScale := FConfig.GenShtabGrid.Scale;
    VShowText := FConfig.GenShtabGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.GenShtabGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridGenshtab.Create(
        FBitmap32StaticFactory,
        VColor,
        VScale,
        VShowText,
        VShowLines
      );

    if VResult <> nil then begin
      VResult :=
        TBitmapLayerProviderComplex.Create(
          FBitmap32StaticFactory,
          VResult,
          VProvider
        );
    end else begin
      VResult := VProvider;
    end;
  end;
  FConfig.DegreeGrid.LockRead;
  try
    VVisible := FConfig.DegreeGrid.Visible;
    VColor := FConfig.DegreeGrid.GridColor;
    VScaleDegree := FConfig.DegreeGrid.Scale;
    VShowText := FConfig.DegreeGrid.ShowText;
    VShowLines := True;
  finally
    FConfig.DegreeGrid.UnlockRead;
  end;
  if VVisible then begin
    VProvider :=
      TBitmapLayerProviderGridDegree.Create(
        FBitmap32StaticFactory,
        VColor,
        VScaleDegree,
        VShowText,
        VShowLines,
        FValueToStringConverter.GetStatic
      );
    if VResult <> nil then begin
      VResult :=
        TBitmapLayerProviderComplex.Create(
          FBitmap32StaticFactory,
          VResult,
          VProvider
        );
    end else begin
      VResult := VProvider;
    end;
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForGrids.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
