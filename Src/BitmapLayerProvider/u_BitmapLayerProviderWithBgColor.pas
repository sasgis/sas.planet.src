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

unit u_BitmapLayerProviderWithBgColor;

interface

uses
  Types,
  t_Bitmap32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderWithBGColor = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FSourceProvider: IBitmapTileUniProvider;
    FBackGroundColor: TColor32;
  private
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      ABackGroundColor: TColor32;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASourceProvider: IBitmapTileUniProvider
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderWithBGColor }

constructor TBitmapLayerProviderWithBGColor.Create(
  ABackGroundColor: TColor32;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASourceProvider: IBitmapTileUniProvider
);
begin
  Assert(Assigned(ASourceProvider));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FSourceProvider := ASourceProvider;
  FBackGroundColor := ABackGroundColor;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  Assert(FSourceProvider <> nil);
end;

function TBitmapLayerProviderWithBGColor.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IBitmap32Static;
var
  VPixelRect: TRect;
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
begin
  Result :=
    FSourceProvider.GetTile(
      AOperationID,
      ACancelNotifier,
      AProjectionInfo,
      ATile
    );
  if Result <> nil then begin
    VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VPixelRect :=  AProjectionInfo.GeoConverter.TilePos2PixelRect(ATile, AProjectionInfo.Zoom);
      VTileSize := Point(VPixelRect.Right - VPixelRect.Left, VPixelRect.Bottom - VPixelRect.Top);
      VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
      VTargetBmp.Clear(FBackGroundColor);
      BlockTransferFull(
        VTargetBmp,
        0,
        0,
        Result,
        dmBlend
      );
      Result := VTargetBmp.MakeAndClear;
    finally
      VTargetBmp.Free;
    end;
  end;
end;

end.
