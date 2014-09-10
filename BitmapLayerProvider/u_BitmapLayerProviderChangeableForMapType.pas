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

unit u_BitmapLayerProviderChangeableForMapType;

interface

uses
  i_TileError,
  i_TileStorage,
  i_BitmapLayerProvider,
  i_UseTilePrevZoomConfig,
  i_ImageResamplerFactoryChangeable,
  i_Bitmap32BufferFactory,
  i_MapVersionRequestConfig,
  i_TileObjCache,
  i_ListenerNotifierLinksList,
  u_BitmapLayerProviderChangeableBase;

type
  TBitmapLayerProviderChangeableForMapType = class(TBitmapLayerProviderChangeableBase)
  private
    FGuid: TGUID;
    FIsLayer: Boolean;
    FLoadPrevMaxZoomDelta: Integer;
    FTileStorage: ITileStorage;
    FCache: ITileObjCacheBitmap;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FErrorLogger: ITileErrorLogger;

    FResamplerChangeProjection: IImageResamplerFactoryChangeable;
    FResamplerGetPrev: IImageResamplerFactoryChangeable;
    FResamplerLoad: IImageResamplerFactoryChangeable;
    FVersion: IMapVersionRequestConfig;
    FUseTilePrevZoomConfig: IUseTilePrevZoomConfig;
    procedure OnConfigChange;
  protected
    function CreateStatic: IInterface; override;
  public
    constructor Create(
      const AGuid: TGUID;
      const AIsLayer: Boolean;
      const ALoadPrevMaxZoomDelta: Integer;
      const ATileStorage: ITileStorage;
      const ACache: ITileObjCacheBitmap;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AErrorLogger: ITileErrorLogger;
      const AResamplerChangeProjection: IImageResamplerFactoryChangeable;
      const AResamplerGetPrev: IImageResamplerFactoryChangeable;
      const AResamplerLoad: IImageResamplerFactoryChangeable;
      const AVersion: IMapVersionRequestConfig;
      const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig
    );
  end;

implementation

uses
  i_Listener,
  u_ListenerByEvent,
  u_BitmapLayerProviderByMapType;

{ TBitmapLayerProviderChangeableForMapType }

constructor TBitmapLayerProviderChangeableForMapType.Create(
  const AGuid: TGUID;
  const AIsLayer: Boolean;
  const ALoadPrevMaxZoomDelta: Integer;
  const ATileStorage: ITileStorage;
  const ACache: ITileObjCacheBitmap;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AErrorLogger: ITileErrorLogger;
  const AResamplerChangeProjection, AResamplerGetPrev, AResamplerLoad: IImageResamplerFactoryChangeable;
  const AVersion: IMapVersionRequestConfig;
  const AUseTilePrevZoomConfig: IUseTilePrevZoomConfig
);
var
  VListener: IListener;
begin
  Assert(Assigned(FTileStorage));
  Assert(Assigned(FCache));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(FErrorLogger));
  Assert(Assigned(FResamplerChangeProjection));
  Assert(Assigned(FResamplerGetPrev));
  Assert(Assigned(FResamplerLoad));
  Assert(Assigned(FVersion));
  Assert(Assigned(FUseTilePrevZoomConfig));
  inherited Create;
  FGuid := AGuid;
  FIsLayer := AIsLayer;
  FLoadPrevMaxZoomDelta := ALoadPrevMaxZoomDelta;
  FTileStorage := ATileStorage;
  FCache := ACache;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FErrorLogger := AErrorLogger;

  FResamplerChangeProjection := AResamplerChangeProjection;
  FResamplerGetPrev := AResamplerGetPrev;
  FResamplerLoad := AResamplerLoad;
  FVersion := AVersion;
  FUseTilePrevZoomConfig := AUseTilePrevZoomConfig;

  VListener := TNotifyNoMmgEventListener.Create(Self.OnConfigChange);
  LinksList.Add(
    VListener,
    FResamplerChangeProjection.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FResamplerGetPrev.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FResamplerLoad.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FVersion.ChangeNotifier
  );
  LinksList.Add(
    VListener,
    FUseTilePrevZoomConfig.ChangeNotifier
  );
end;

function TBitmapLayerProviderChangeableForMapType.CreateStatic: IInterface;
var
  VResult: IBitmapLayerProvider;
  VUsePrev: Boolean;
begin
  if FIsLayer then begin
    VUsePrev := FUseTilePrevZoomConfig.GetStatic.UsePrevZoomAtLayer;
  end else begin
    VUsePrev := FUseTilePrevZoomConfig.GetStatic.UsePrevZoomAtMap;
  end;
  VResult :=
    TBitmapLayerProviderByMapType.Create(
      FGuid,
      FErrorLogger,
      FTileStorage,
      FResamplerChangeProjection.GetStatic,
      FResamplerGetPrev.GetStatic,
      FResamplerLoad.GetStatic,
      FBitmap32StaticFactory,
      FLoadPrevMaxZoomDelta,
      FVersion.GetStatic,
      FCache,
      VUsePrev
    );
  Result := VResult;
end;

procedure TBitmapLayerProviderChangeableForMapType.OnConfigChange;
begin
  LockWrite;
  try
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
