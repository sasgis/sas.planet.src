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

unit u_BitmapTileProviderByInfoTileProvider;

interface

uses
  Types,
  SysUtils,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_Projection,
  i_InfoTileProvider,
  i_BitmapTileProvider,
  i_ImageResamplerFactoryChangeable,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderByInfoTileProvider = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FProjection: IProjection;
    FSource: IInfoTileProvider;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FIsIgnoreError: Boolean;
    FIsResizeTile: Boolean;
    FImageResampler: IImageResamplerFactoryChangeable;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const AIsIgnoreError: Boolean;
      const AIsResizeTile: Boolean;
      const AImageResampler: IImageResamplerFactoryChangeable;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASource: IInfoTileProvider
    );
  end;

type
  EBadContentTypeError = type Exception;

implementation

uses
  GR32,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  i_BitmapTileSaveLoad,
  i_BinaryData,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapTileProviderByInfoTileProvider }

constructor TBitmapTileProviderByInfoTileProvider.Create(
  const AIsIgnoreError: Boolean;
  const AIsResizeTile: Boolean;
  const AImageResampler: IImageResamplerFactoryChangeable;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASource: IInfoTileProvider
);
begin
  Assert(Assigned(AImageResampler));
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(ASource));
  inherited Create;
  FIsIgnoreError := AIsIgnoreError;
  FIsResizeTile := AIsResizeTile;
  FImageResampler := AImageResampler;
  FSource := ASource;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProjection := FSource.Projection;
end;

function TBitmapTileProviderByInfoTileProvider.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TBitmapTileProviderByInfoTileProvider.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VTileInfo: ITileInfoBasic;
  VTileInfoWithData: ITileInfoWithData;
  VSize: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VContentType: IContentTypeInfoBitmap;
  VResampler: TCustomResampler;
  VLoader: IBitmapTileLoader;
  VBinary: IBinaryData;
begin
  Result := nil;
  try
    VTileInfo := FSource.GetTile(AOperationID, ACancelNotifier, ATile);
  except
    if not FIsIgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
  if Supports(VTileInfo, ITileInfoWithData, VTileInfoWithData) then begin
    VBinary := VTileInfoWithData.TileData;
    if Assigned(VBinary) then begin
      if Supports(VTileInfo.ContentType, IContentTypeInfoBitmap, VContentType) then begin
        VLoader := VContentType.GetLoader;
        if Assigned(VLoader) then begin
          try
            Result := VLoader.Load(VBinary);
          except
            if not FIsIgnoreError then begin
              raise;
            end else begin
              Result := nil;
            end;
          end;
        end else begin
          if not FIsIgnoreError then begin
            raise EBadContentTypeError.Create('No loader for this bitmap type');
          end;
        end;
      end else begin
        if not FIsIgnoreError then begin
          raise EBadContentTypeError.Create('Tile is not bitmap');
        end;
      end;
    end;
  end;
  if Result <> nil then begin
    VSize := FProjection.GetTileSize(ATile);
    if (Result.Size.X <> VSize.X) or
      (Result.Size.Y <> VSize.Y) then begin
      if FIsResizeTile then begin
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
      end else begin
        if (Result.Size.X > VSize.X) or
          (Result.Size.Y > VSize.Y) then begin
          VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
          try
            VBitmap.SetSize(VSize.X, VSize.Y);
            BlockTransferFull(
              VBitmap,
              0, 0,
              Result,
              dmOpaque
            );
            Result := VBitmap.MakeAndClear;
          finally
            VBitmap.Free;
          end;
        end;
      end;
    end;
  end;
end;

end.
