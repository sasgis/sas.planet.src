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

unit u_TileProviderByStorage;

interface

uses
  Types,
  SysUtils,
  i_NotifierTilePyramidUpdate,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_MapVersionConfig,
  i_BitmapTileSaveLoad,
  i_VectorItemSubset,
  i_ProjectionInfo,
  i_TileProvider,
  i_VectorDataLoader,
  i_ImageResamplerFactoryChangeable,
  i_VectorDataFactory,
  i_TileStorage,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByStorage = class(TBaseInterfacedObject, IBitmapTileProviderWithNotifier)
  private
    FProjectionInfo: IProjectionInfo;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IBitmapTileLoader;
    FBitmapFactory: IBitmap32BufferFactory;
    FStorage: ITileStorage;
    FIsIgnoreError: Boolean;
    FImageResampler: IImageResamplerFactoryChangeable;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(const ATile: TPoint): IBitmap32Static;
    function GetChangeNotifier: INotifierTilePyramidUpdate;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmapFactory: IBitmap32BufferFactory;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IBitmapTileLoader;
      const AProjectionInfo: IProjectionInfo;
      const AStorage: ITileStorage
    );
  end;

  TVectorTileProviderByStorage = class(TBaseInterfacedObject, IVectorTileProviderWithNotifier)
  private
    FProjectionInfo: IProjectionInfo;
    FVersionConfig: IMapVersionConfig;
    FLoaderFromStorage: IVectorDataLoader;
    FStorage: ITileStorage;
    FVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
    FIsIgnoreError: Boolean;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(const ATile: TPoint): IVectorItemSubset;
    function GetChangeNotifier: INotifierTilePyramidUpdate;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
      const AVersionConfig: IMapVersionConfig;
      const ALoaderFromStorage: IVectorDataLoader;
      const AProjectionInfo: IProjectionInfo;
      const AStorage: ITileStorage
    );
  end;

implementation

uses
  GR32,
  i_TileInfoBasic,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapTileProviderByStorage }

constructor TBitmapTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmapFactory: IBitmap32BufferFactory;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IBitmapTileLoader;
  const AProjectionInfo: IProjectionInfo;
  const AStorage: ITileStorage
);
begin
  Assert(AImageResampler <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  Assert(AProjectionInfo <> nil);
  Assert(AStorage.CoordConverter.IsSameConverter(AProjectionInfo.GeoConverter));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FImageResampler := AImageResampler;
  FStorage := AStorage;
  FBitmapFactory := ABitmapFactory;
  FProjectionInfo := AProjectionInfo;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TBitmapTileProviderByStorage.GetChangeNotifier: INotifierTilePyramidUpdate;
begin
  Result := FStorage.TileNotifier;
end;

function TBitmapTileProviderByStorage.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

function TBitmapTileProviderByStorage.GetTile(const ATile: TPoint): IBitmap32Static;
var
  VTileInfo: ITileInfoWithData;
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
  VZoom: Byte;
begin
  Result := nil;
  try
    VZoom := FProjectionInfo.Zoom;
    if Supports(FStorage.GetTileInfo(ATile, VZoom, FVersionConfig.Version, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData);
    end;
    if Result <> nil then begin
      VRect := FProjectionInfo.GeoConverter.TilePos2PixelRect(ATile, VZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Size.X <> VSize.X) or
        (Result.Size.Y <> VSize.Y) then begin
        VResampler := FImageResampler.GetStatic.CreateResampler;
        try
          VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            StretchTransferFull(
              VBitmap,
              VBitmap.BoundsRect,
              Result,
              VResampler,
              dmOpaque
            );
            Result := VBitmap.MakeAndClear;
          finally
            VBitmap.Free;
          end;
        finally
          VResampler.Free;
        end;
      end;
    end;
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

{ TVectorTileProviderByStorage }

constructor TVectorTileProviderByStorage.Create(
  const AIsIgnoreError: Boolean;
  const AVectorDataItemMainInfoFactory: IVectorDataItemMainInfoFactory;
  const AVersionConfig: IMapVersionConfig;
  const ALoaderFromStorage: IVectorDataLoader;
  const AProjectionInfo: IProjectionInfo;
  const AStorage: ITileStorage);
begin
  Assert(AVectorDataItemMainInfoFactory <> nil);
  Assert(AVersionConfig <> nil);
  Assert(ALoaderFromStorage <> nil);
  Assert(AStorage <> nil);
  Assert(AProjectionInfo <> nil);
  Assert(AStorage.CoordConverter.IsSameConverter(AProjectionInfo.GeoConverter));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FVectorDataItemMainInfoFactory := AVectorDataItemMainInfoFactory;
  FStorage := AStorage;
  FProjectionInfo := AProjectionInfo;
  FVersionConfig := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TVectorTileProviderByStorage.GetChangeNotifier: INotifierTilePyramidUpdate;
begin
  Result := FStorage.TileNotifier;
end;

function TVectorTileProviderByStorage.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

function TVectorTileProviderByStorage.GetTile(const ATile: TPoint): IVectorItemSubset;
var
  VTileInfo: ITileInfoWithData;
  VZoom: Byte;
begin
  Result := nil;
  try
    VZoom := FProjectionInfo.Zoom;
    if Supports(FStorage.GetTileInfo(ATile, VZoom, FVersionConfig.Version, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData, nil, FVectorDataItemMainInfoFactory);
    end;
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

end.
