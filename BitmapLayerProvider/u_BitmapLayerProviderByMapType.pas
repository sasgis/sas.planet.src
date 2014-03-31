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

unit u_BitmapLayerProviderByMapType;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_CoordConverter,
  i_LocalCoordConverter,
  i_TileObjCache,
  i_BitmapLayerProvider,
  i_ImageResamplerFactory,
  i_TileStorage,
  i_TileError,
  i_Bitmap32BufferFactory,
  i_MapVersionRequest,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByMapType = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FGuid: TGUID;
    FTileStorage: ITileStorage;
    FResamplerChangeProjection: IImageResamplerFactory;
    FResamplerGetPrev: IImageResamplerFactory;
    FResamplerLoad: IImageResamplerFactory;
    FBitmapFactory: IBitmap32BufferFactory;
    FLoadPrevMaxZoomDelta: Integer;
    FErrorLogger: ITileErrorLogger;
    FVersion: IMapVersionRequest;
    FCache: ITileObjCacheBitmap;
    FUsePrevZoom: Boolean;
    function LoadBitmapTileFromStorage(
      const AXY: TPoint;
      const AZoom: Byte
    ): IBitmap32Static;
    function LoadTile(
      const AXY: TPoint;
      const AZoom: byte;
      IgnoreError: Boolean
    ): IBitmap32Static;
    function LoadTileFromPreZ(
      const AXY: TPoint;
      const AZoom: byte;
      IgnoreError: Boolean
    ): IBitmap32Static;
    function LoadTileOrPreZ(
      const AXY: TPoint;
      const AZoom: byte;
      IgnoreError: Boolean
    ): IBitmap32Static;
    function LoadBitmap(
      const APixelRectTarget: TRect;
      const AZoom: byte;
      AAllowPartial, IgnoreError: Boolean
    ): IBitmap32Static;
    function LoadBitmapUni(
      const APixelRectTarget: TRect;
      const AZoom: byte;
      const ACoordConverterTarget: ICoordConverter;
      AAllowPartial, IgnoreError: Boolean
    ): IBitmap32Static;
    function LoadTileUni(
      const AXY: TPoint;
      const AZoom: byte;
      const ACoordConverterTarget: ICoordConverter;
      AAllowPartial, IgnoreError: Boolean
    ): IBitmap32Static;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const AGuid: TGUID;
      const AErrorLogger: ITileErrorLogger;
      const ATileStorage: ITileStorage;
      const AResamplerChangeProjection: IImageResamplerFactory;
      const AResamplerGetPrev: IImageResamplerFactory;
      const AResamplerLoad: IImageResamplerFactory;
      const ABitmapFactory: IBitmap32BufferFactory;
      const ALoadPrevMaxZoomDelta: Integer;
      const AVersion: IMapVersionRequest;
      const ACache: ITileObjCacheBitmap;
      AUsePrevZoom: Boolean
    );
  end;

implementation

uses
  SysUtils,
  GR32,
  t_GeoTypes,
  i_TileInfoBasic,
  i_ContentTypeInfo,
  u_Bitmap32ByStaticBitmap,
  u_TileErrorInfo,
  u_BitmapFunc,
  u_GeoFunc;

{ TBitmapLayerProviderByMapType }

constructor TBitmapLayerProviderByMapType.Create(
  const AGuid: TGUID;
  const AErrorLogger: ITileErrorLogger;
  const ATileStorage: ITileStorage;
  const AResamplerChangeProjection: IImageResamplerFactory;
  const AResamplerGetPrev: IImageResamplerFactory;
  const AResamplerLoad: IImageResamplerFactory;
  const ABitmapFactory: IBitmap32BufferFactory;
  const ALoadPrevMaxZoomDelta: Integer;
  const AVersion: IMapVersionRequest;
  const ACache: ITileObjCacheBitmap;
  AUsePrevZoom: Boolean
);
begin
  Assert(Assigned(ATileStorage));
  Assert(Assigned(AResamplerChangeProjection));
  Assert(Assigned(AResamplerGetPrev));
  Assert(Assigned(AResamplerLoad));
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(AVersion));
  inherited Create;
  FGuid := AGuid;
  FErrorLogger := AErrorLogger;
  FTileStorage := ATileStorage;
  FResamplerChangeProjection := AResamplerChangeProjection;
  FResamplerGetPrev := AResamplerGetPrev;
  FResamplerLoad := AResamplerLoad;
  FBitmapFactory := ABitmapFactory;
  FLoadPrevMaxZoomDelta := ALoadPrevMaxZoomDelta;
  FVersion := AVersion;
  FCache := ACache;
  FUsePrevZoom := AUsePrevZoom;
end;

function TBitmapLayerProviderByMapType.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTile: TPoint;
  Vzoom: byte;
  VCoordConverter: ICoordConverter;
  VPixelRect: TRect;
  VError: ITileErrorInfo;
begin
  Vzoom := ALocalConverter.Zoom;
  VCoordConverter := ALocalConverter.GeoConverter;
  VPixelRect := ALocalConverter.GetRectInMapPixel;
  VTile := VCoordConverter.PixelRect2TileRect(VPixelRect, Vzoom).TopLeft;
  Assert(EqualRect(VPixelRect, VCoordConverter.TilePos2PixelRect(VTile, Vzoom)));

  try
    Result :=
      LoadTileUni(
        VTile,
        Vzoom,
        VCoordConverter,
        True,
        Assigned(FErrorLogger)
      );
  except
    on E: Exception do begin
      if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            FGuid,
            Vzoom,
            VTile,
            E.Message
          );
        FErrorLogger.LogError(VError);
      end else begin
        raise;
      end;
    end;
    else if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            FGuid,
            VZoom,
            VTile,
            'Unexpected read tile error'
          );
        FErrorLogger.LogError(VError);
      end else begin
        raise;
      end;
  end;
end;

function TBitmapLayerProviderByMapType.LoadBitmap(
  const APixelRectTarget: TRect;
  const AZoom: byte;
  AAllowPartial, IgnoreError: Boolean
): IBitmap32Static;
var
  VZoom: Byte;
  VPixelRectTarget: TRect;
  VTileRect: TRect;
  VTargetImageSize: TPoint;
  VPixelRectCurrTile: TRect;
  i, j: Integer;
  VTile: TPoint;
  VSpr: IBitmap32Static;
  VSourceBounds: TRect;
  VTargetBounds: TRect;
  VBitmap: TBitmap32ByStaticBitmap;
  VCoordConverterSource: ICoordConverter;
begin
  Result := nil;

  VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
  VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

  VPixelRectTarget := APixelRectTarget;
  VZoom := AZoom;
  VCoordConverterSource := FTileStorage.CoordConverter;
  VCoordConverterSource.CheckPixelRect(VPixelRectTarget, VZoom);
  VTileRect := VCoordConverterSource.PixelRect2TileRect(VPixelRectTarget, VZoom);
  if (VTileRect.Left = VTileRect.Right - 1) and
    (VTileRect.Top = VTileRect.Bottom - 1) then begin
    VPixelRectCurrTile := VCoordConverterSource.TilePos2PixelRect(VTileRect.TopLeft, VZoom);
    if Types.EqualRect(VPixelRectCurrTile, APixelRectTarget) then begin
      Result := LoadTileOrPreZ(VTileRect.TopLeft, VZoom, IgnoreError);
      Exit;
    end;
  end;
  VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
  try
    VBitmap.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
    VBitmap.Clear(0);

    for i := VTileRect.Top to VTileRect.Bottom - 1 do begin
      VTile.Y := i;
      for j := VTileRect.Left to VTileRect.Right - 1 do begin
        VTile.X := j;
        VSpr := LoadTileOrPreZ(VTile, VZoom, IgnoreError);
        if VSpr <> nil then begin
          VPixelRectCurrTile := VCoordConverterSource.TilePos2PixelRect(VTile, VZoom);

          if VPixelRectCurrTile.Top < APixelRectTarget.Top then begin
            VSourceBounds.Top := APixelRectTarget.Top - VPixelRectCurrTile.Top;
          end else begin
            VSourceBounds.Top := 0;
          end;

          if VPixelRectCurrTile.Left < APixelRectTarget.Left then begin
            VSourceBounds.Left := APixelRectTarget.Left - VPixelRectCurrTile.Left;
          end else begin
            VSourceBounds.Left := 0;
          end;

          if VPixelRectCurrTile.Bottom < APixelRectTarget.Bottom then begin
            VSourceBounds.Bottom := VPixelRectCurrTile.Bottom - VPixelRectCurrTile.Top;
          end else begin
            VSourceBounds.Bottom := APixelRectTarget.Bottom - VPixelRectCurrTile.Top;
          end;

          if VPixelRectCurrTile.Right < APixelRectTarget.Right then begin
            VSourceBounds.Right := VPixelRectCurrTile.Right - VPixelRectCurrTile.Left;
          end else begin
            VSourceBounds.Right := APixelRectTarget.Right - VPixelRectCurrTile.Left;
          end;

          if VPixelRectCurrTile.Top < APixelRectTarget.Top then begin
            VTargetBounds.Top := 0;
          end else begin
            VTargetBounds.Top := VPixelRectCurrTile.Top - APixelRectTarget.Top;
          end;

          if VPixelRectCurrTile.Left < APixelRectTarget.Left then begin
            VTargetBounds.Left := 0;
          end else begin
            VTargetBounds.Left := VPixelRectCurrTile.Left - APixelRectTarget.Left;
          end;

          if VPixelRectCurrTile.Bottom < APixelRectTarget.Bottom then begin
            VTargetBounds.Bottom := VPixelRectCurrTile.Bottom - APixelRectTarget.Top;
          end else begin
            VTargetBounds.Bottom := APixelRectTarget.Bottom - APixelRectTarget.Top;
          end;

          if VPixelRectCurrTile.Right < APixelRectTarget.Right then begin
            VTargetBounds.Right := VPixelRectCurrTile.Right - APixelRectTarget.Left;
          end else begin
            VTargetBounds.Right := APixelRectTarget.Right - APixelRectTarget.Left;
          end;

          BlockTransfer(
            VBitmap,
            VTargetBounds.Left,
            VTargetBounds.Top,
            VSpr,
            VSourceBounds,
            dmOpaque
          );
        end else begin
          if not AAllowPartial then begin
            Exit;
          end;
        end;
      end;
    end;
    Result := VBitmap.MakeAndClear;
  finally
    VBitmap.Free;
  end;
end;

function TBitmapLayerProviderByMapType.LoadBitmapTileFromStorage(
  const AXY: TPoint;
  const AZoom: Byte
): IBitmap32Static;
var
  VTileInfoWithData: ITileInfoWithData;
  VContentType: IContentTypeInfoBitmap;
begin
  Result := nil;
  if Supports(FTileStorage.GetTileInfoEx(AXY, AZoom, FVersion, gtimWithData), ITileInfoWithData, VTileInfoWithData) then begin
    if Supports(VTileInfoWithData.ContentType, IContentTypeInfoBitmap, VContentType) then begin
      Result := VContentType.GetLoader.Load(VTileInfoWithData.TileData);
    end;
  end;
end;

function TBitmapLayerProviderByMapType.LoadBitmapUni(
  const APixelRectTarget: TRect;
  const AZoom: byte;
  const ACoordConverterTarget: ICoordConverter;
  AAllowPartial, IgnoreError: Boolean
): IBitmap32Static;
var
  VPixelRectTarget: TRect;
  VZoom: Byte;
  VLonLatRectTarget: TDoubleRect;
  VTileRectInSource: TRect;
  VPixelRectOfTargetPixelRectInSource: TRect;
  VSpr: IBitmap32Static;
  VTargetImageSize: TPoint;
  VResampler: TCustomResampler;
  VBitmap: TBitmap32ByStaticBitmap;
  VCoordConverterSource: ICoordConverter;
begin
  Result := nil;
  VCoordConverterSource := FTileStorage.CoordConverter;
  if VCoordConverterSource.IsSameConverter(ACoordConverterTarget) then begin
    Result := LoadBitmap(APixelRectTarget, AZoom, AAllowPartial, IgnoreError);
  end else begin
    VZoom := AZoom;
    VTargetImageSize.X := APixelRectTarget.Right - APixelRectTarget.Left;
    VTargetImageSize.Y := APixelRectTarget.Bottom - APixelRectTarget.Top;

    VPixelRectTarget := APixelRectTarget;
    ACoordConverterTarget.CheckPixelRect(VPixelRectTarget, VZoom);
    VLonLatRectTarget := ACoordConverterTarget.PixelRect2LonLatRect(VPixelRectTarget, VZoom);
    VCoordConverterSource.CheckLonLatRect(VLonLatRectTarget);
    VPixelRectOfTargetPixelRectInSource :=
      RectFromDoubleRect(
        VCoordConverterSource.LonLatRect2PixelRectFloat(VLonLatRectTarget, VZoom),
        rrToTopLeft
      );
    VTileRectInSource := VCoordConverterSource.PixelRect2TileRect(VPixelRectOfTargetPixelRectInSource, VZoom);
    VSpr := LoadBitmap(VPixelRectOfTargetPixelRectInSource, VZoom, AAllowPartial, IgnoreError);
    if VSpr <> nil then begin
      VResampler := FResamplerChangeProjection.CreateResampler;
      try
        VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
        try
          VBitmap.SetSize(VTargetImageSize.X, VTargetImageSize.Y);
          VBitmap.Clear(0);
          StretchTransferFull(
            VBitmap,
            VBitmap.BoundsRect,
            VSpr,
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
end;

function TBitmapLayerProviderByMapType.LoadTile(
  const AXY: TPoint;
  const AZoom: byte;
  IgnoreError: Boolean
): IBitmap32Static;
var
  VRect: TRect;
  VSize: TPoint;
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
begin
  try
    Result := nil;
    if FCache = nil then begin
      Result := LoadBitmapTileFromStorage(AXY, AZoom);
    end else begin
      Result := FCache.TryLoadTileFromCache(AXY, AZoom);
      if Result = nil then begin
        Result := LoadBitmapTileFromStorage(AXY, AZoom);
        if Result <> nil then begin
          FCache.AddTileToCache(Result, AXY, AZoom);
        end;
      end;
    end;
    if Result <> nil then begin
      VRect := FTileStorage.CoordConverter.TilePos2PixelRect(AXY, AZoom);
      VSize := Types.Point(VRect.Right - VRect.Left, VRect.Bottom - VRect.Top);
      if (Result.Size.X <> VSize.X) or
        (Result.Size.Y <> VSize.Y) then begin
        VResampler := FResamplerLoad.CreateResampler;
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
    if not IgnoreError then begin
      raise;
    end else begin
      Result := nil;
    end;
  end;
end;

function TBitmapLayerProviderByMapType.LoadTileFromPreZ(
  const AXY: TPoint;
  const AZoom: byte;
  IgnoreError: Boolean
): IBitmap32Static;
var
  i: integer;
  VBmp: IBitmap32Static;
  VTileTargetBounds: TRect;
  VTileSourceBounds: TRect;
  VTileParent: TPoint;
  VTargetTilePixelRect: TRect;
  VSourceTilePixelRect: TRect;
  VRelative: TDoublePoint;
  VRelativeRect: TDoubleRect;
  VParentZoom: Byte;
  VMinZoom: Integer;
  VBitmap: TBitmap32ByStaticBitmap;
  VResampler: TCustomResampler;
  VCoordConverterSource: ICoordConverter;
begin
  Result := nil;
  VCoordConverterSource := FTileStorage.CoordConverter;
  VRelative := VCoordConverterSource.TilePos2Relative(AXY, AZoom);
  VMinZoom := AZoom - FLoadPrevMaxZoomDelta;
  if VMinZoom < 0 then begin
    VMinZoom := 0;
  end;
  if AZoom - 1 > VMinZoom then begin
    for i := AZoom - 1 downto VMinZoom do begin
      VParentZoom := i;
      VTileParent := PointFromDoublePoint(VCoordConverterSource.Relative2TilePosFloat(VRelative, i), prToTopLeft);
      VBmp := LoadTile(VTileParent, VParentZoom, IgnoreError);
      if VBmp <> nil then begin
        VTargetTilePixelRect := VCoordConverterSource.TilePos2PixelRect(AXY, AZoom);
        VRelativeRect := VCoordConverterSource.PixelRect2RelativeRect(VTargetTilePixelRect, AZoom);
        VTileTargetBounds.Left := 0;
        VTileTargetBounds.Top := 0;
        VTileTargetBounds.Right := VTargetTilePixelRect.Right - VTargetTilePixelRect.Left;
        VTileTargetBounds.Bottom := VTargetTilePixelRect.Bottom - VTargetTilePixelRect.Top;

        VSourceTilePixelRect := VCoordConverterSource.TilePos2PixelRect(VTileParent, VParentZoom);
        VTargetTilePixelRect :=
          RectFromDoubleRect(
            VCoordConverterSource.RelativeRect2PixelRectFloat(VRelativeRect, VParentZoom),
            rrToTopLeft
          );
        VTileSourceBounds.Left := VTargetTilePixelRect.Left - VSourceTilePixelRect.Left;
        VTileSourceBounds.Top := VTargetTilePixelRect.Top - VSourceTilePixelRect.Top;
        VTileSourceBounds.Right := VTargetTilePixelRect.Right - VSourceTilePixelRect.Left;
        VTileSourceBounds.Bottom := VTargetTilePixelRect.Bottom - VSourceTilePixelRect.Top;
        VResampler := FResamplerGetPrev.CreateResampler;
        try
          try
            VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
            try
              VBitmap.SetSize(VTileTargetBounds.Right, VTileTargetBounds.Bottom);
              StretchTransfer(
                VBitmap,
                VTileTargetBounds,
                VBmp,
                VTileSourceBounds,
                VResampler,
                dmOpaque
              );
              Result := VBitmap.MakeAndClear;
            finally
              VBitmap.Free;
            end;
            Break;
          except
            if not IgnoreError then begin
              raise;
            end;
          end;
        finally
          VResampler.Free;
        end;
      end;
    end;
  end;
end;

function TBitmapLayerProviderByMapType.LoadTileOrPreZ(
  const AXY: TPoint;
  const AZoom: byte;
  IgnoreError: Boolean
): IBitmap32Static;
begin
  Result := LoadTile(AXY, AZoom, IgnoreError);
  if Result = nil then begin
    if FUsePrevZoom then begin
      Result := LoadTileFromPreZ(AXY, AZoom, IgnoreError);
    end;
  end;
end;

function TBitmapLayerProviderByMapType.LoadTileUni(
  const AXY: TPoint;
  const AZoom: byte;
  const ACoordConverterTarget: ICoordConverter;
  AAllowPartial, IgnoreError: Boolean
): IBitmap32Static;
var
  VPixelRect: TRect;
begin
  VPixelRect := ACoordConverterTarget.TilePos2PixelRect(AXY, AZoom);
  Result := LoadBitmapUni(VPixelRect, AZoom, ACoordConverterTarget, AAllowPartial, IgnoreError);
end;

end.
