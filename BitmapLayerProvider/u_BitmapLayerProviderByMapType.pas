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

unit u_BitmapLayerProviderByMapType;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_LocalCoordConverter,
  i_TileObjCache,
  i_BitmapLayerProvider,
  i_TileError,
  i_MapTypes,
  i_MapVersionRequest,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByMapType = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FErrorLogger: ITileErrorLogger;
    FMapType: IMapType;
    FVersion: IMapVersionRequest;
    FCache: ITileObjCacheBitmap;
    FUsePrevZoom: Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const AErrorLogger: ITileErrorLogger;
      const AMapType: IMapType;
      const AVersion: IMapVersionRequest;
      const ACache: ITileObjCacheBitmap;
      AUsePrevZoom: Boolean
    );
  end;

implementation

uses
  Types,
  SysUtils,
  i_CoordConverter,
  u_TileErrorInfo;

{ TBitmapLayerProviderByMapType }

constructor TBitmapLayerProviderByMapType.Create(
  const AErrorLogger: ITileErrorLogger;
  const AMapType: IMapType;
  const AVersion: IMapVersionRequest;
  const ACache: ITileObjCacheBitmap;
  AUsePrevZoom: Boolean
);
begin
  Assert(Assigned(AMapType));
  Assert(Assigned(AVersion));
  inherited Create;
  FErrorLogger := AErrorLogger;
  FMapType := AMapType;
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
      FMapType.LoadTileUni(
        VTile,
        Vzoom,
        FVersion,
        VCoordConverter,
        FUsePrevZoom,
        True,
        Assigned(FErrorLogger),
        FCache
      );
  except
    on E: Exception do begin
      if FErrorLogger <> nil then begin
        VError :=
          TTileErrorInfo.Create(
            FMapType.Zmp.GUID,
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
            FMapType.Zmp.GUID,
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

end.
