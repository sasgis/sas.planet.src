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

unit u_BitmapLayerProviderChangeableForFillingMap;

interface

uses
  i_Listener,
  i_FillingMapLayerConfig,
  i_MapTypes,
  i_Bitmap32StaticFactory,
  i_BitmapLayerProvider,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForFillingMap = class(TBitmapLayerProviderChangeableBase)
  private
    FBitmapFactory: IBitmap32BufferFactory;
    FConfig: IFillingMapLayerConfig;

    FVersionListener: IListener;
    FSourceMapLast: IMapType;
    procedure OnMapVersionChange;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32BufferFactory;
      const AConfig: IFillingMapLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_BitmapLayerProviderFillingMap;

{ TBitmapLayerProviderChangeableForFillingMap }

constructor TBitmapLayerProviderChangeableForFillingMap.Create(
  const ABitmapFactory: IBitmap32BufferFactory;
  const AConfig: IFillingMapLayerConfig
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FConfig := AConfig;

  FVersionListener := TNotifyNoMmgEventListener.Create(Self.OnMapVersionChange);

  LinksList.Add(
    TNotifyNoMmgEventListener.Create(OnConfigChange),
    FConfig.ChangeNotifier
  );
end;

destructor TBitmapLayerProviderChangeableForFillingMap.Destroy;
begin
  if Assigned(FSourceMapLast) and Assigned(FVersionListener) then begin
    FSourceMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
    FSourceMapLast := nil;
    FVersionListener := nil;
  end;
  inherited;
end;

function TBitmapLayerProviderChangeableForFillingMap.CreateStatic: IInterface;
var
  VConfig: IFillingMapLayerConfigStatic;
  VResult: IBitmapLayerProvider;
  VMap: IMapType;
begin
  VResult := nil;
  VConfig := FConfig.GetStatic;
  if VConfig.Visible then begin
    VMap := VConfig.SourceMap;
    if FSourceMapLast <> VMap then begin
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.VersionRequestConfig.ChangeNotifier.Remove(FVersionListener);
      end;
      FSourceMapLast := VMap;
      if Assigned(FSourceMapLast) then begin
        FSourceMapLast.VersionRequestConfig.ChangeNotifier.Add(FVersionListener);
      end;
    end;
    VResult :=
      TBitmapLayerProviderFillingMap.Create(
        FBitmapFactory,
        VMap.TileStorage,
        VMap.VersionRequestConfig.GetStatic,
        VConfig.UseRelativeZoom,
        VConfig.Zoom,
        VConfig.Colorer
      );
  end;
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForFillingMap.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TBitmapLayerProviderChangeableForFillingMap.OnMapVersionChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
