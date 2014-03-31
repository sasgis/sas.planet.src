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

unit u_BitmapLayerProviderMapWithLayer;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_MapVersionRequest,
  i_MapTypes,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderMapWithLayer = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32BufferFactory;
    FMapTypeMain: IMapType;
    FMapTypeMainVersion: IMapVersionRequest;
    FMapTypeHybr: IMapType;
    FMapTypeHybrVersion: IMapVersionRequest;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32BufferFactory;
      const AMapTypeMain: IMapType;
      const AMapTypeMainVersion: IMapVersionRequest;
      const AMapTypeHybr: IMapType;
      const AMapTypeHybrVersion: IMapVersionRequest;
      AUsePrevZoomAtMap: Boolean;
      AUsePrevZoomAtLayer: Boolean
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderMapWithLayer }

constructor TBitmapLayerProviderMapWithLayer.Create(
  const ABitmapFactory: IBitmap32BufferFactory;
  const AMapTypeMain: IMapType;
  const AMapTypeMainVersion: IMapVersionRequest;
  const AMapTypeHybr: IMapType;
  const AMapTypeHybrVersion: IMapVersionRequest;
  AUsePrevZoomAtMap, AUsePrevZoomAtLayer: Boolean
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FMapTypeMain := AMapTypeMain;
  FMapTypeMainVersion := AMapTypeMainVersion;
  FMapTypeHybr := AMapTypeHybr;
  FMapTypeHybrVersion := AMapTypeHybrVersion;
  FUsePrevZoomAtMap := AUsePrevZoomAtMap;
  FUsePrevZoomAtLayer := AUsePrevZoomAtLayer;
end;

function TBitmapLayerProviderMapWithLayer.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;
  VLayer := nil;
  if FMapTypeMain <> nil then begin
    Result :=
      FMapTypeMain.LoadBitmapUni(
        ALocalConverter.GetRectInMapPixel,
        ALocalConverter.GetZoom,
        FMapTypeMainVersion,
        ALocalConverter.GetGeoConverter,
        FUsePrevZoomAtMap,
        True,
        True
      );
  end;

  if FMapTypeHybr <> nil then begin
    VLayer :=
      FMapTypeHybr.LoadBitmapUni(
        ALocalConverter.GetRectInMapPixel,
        ALocalConverter.GetZoom,
        FMapTypeHybrVersion,
        ALocalConverter.GetGeoConverter,
        FUsePrevZoomAtLayer,
        True,
        True
      );
  end;

  if Result <> nil then begin
    if VLayer <> nil then begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
      try
        AssignStaticToBitmap32(VBitmap, Result);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VLayer,
          dmBlend
        );
        Result := VBitmap.MakeAndClear;
      finally
        VBitmap.Free;
      end;
    end;
  end else begin
    Result := VLayer;
  end;
end;

end.
