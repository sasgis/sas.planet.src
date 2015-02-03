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

unit u_BitmapTileProviderByStorage;

interface

uses
  Types,
  SysUtils,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_MapVersionRequest,
  i_BitmapTileSaveLoad,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  i_ImageResamplerFactoryChangeable,
  i_TileStorage,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByStorage = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FProjectionInfo: IProjectionInfo;
    FVersion: IMapVersionRequest;
    FLoaderFromStorage: IBitmapTileLoader;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FStorage: ITileStorage;
    FIsIgnoreError: Boolean;
    FImageResampler: IImageResamplerFactoryChangeable;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AVersionConfig: IMapVersionRequest;
      const ALoaderFromStorage: IBitmapTileLoader;
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
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AVersionConfig: IMapVersionRequest;
  const ALoaderFromStorage: IBitmapTileLoader;
  const AProjectionInfo: IProjectionInfo;
  const AStorage: ITileStorage
);
begin
  Assert(Assigned(AImageResampler));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AVersionConfig));
  Assert(Assigned(ALoaderFromStorage));
  Assert(Assigned(AStorage));
  Assert(Assigned(AProjectionInfo));
  Assert(AStorage.CoordConverter.IsSameConverter(AProjectionInfo.GeoConverter));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FImageResampler := AImageResampler;
  FStorage := AStorage;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjectionInfo := AProjectionInfo;
  FVersion := AVersionConfig;
  FLoaderFromStorage := ALoaderFromStorage;
end;

function TBitmapTileProviderByStorage.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjectionInfo;
end;

function TBitmapTileProviderByStorage.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
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
    if Supports(FStorage.GetTileInfoEx(ATile, VZoom, FVersion, gtimWithData), ITileInfoWithData, VTileInfo) then begin
      Result := FLoaderFromStorage.Load(VTileInfo.TileData);
    end;
    if Result <> nil then begin
      VRect := FProjectionInfo.GeoConverter.TilePos2PixelRect(ATile, VZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Size.X <> VSize.X) or
        (Result.Size.Y <> VSize.Y) then begin
        VResampler := FImageResampler.GetStatic.CreateResampler;
        try
          VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
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

end.
