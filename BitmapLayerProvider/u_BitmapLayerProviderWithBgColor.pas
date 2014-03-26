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
  t_Bitmap32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderWithBGColor = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FSourceProvider: IBitmapLayerProvider;
    FBackGroundColor: TColor32;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      ABackGroundColor: TColor32;
      const ABitmapFactory: IBitmap32StaticFactory;
      const ASourceProvider: IBitmapLayerProvider
    );
  end;

implementation

uses
  Types,
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderWithBGColor }

constructor TBitmapLayerProviderWithBGColor.Create(
  ABackGroundColor: TColor32;
  const ABitmapFactory: IBitmap32StaticFactory;
  const ASourceProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FSourceProvider := ASourceProvider;
  FBackGroundColor := ABackGroundColor;
  FBitmapFactory := ABitmapFactory;
  Assert(FSourceProvider <> nil);
end;

function TBitmapLayerProviderWithBGColor.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
begin
  Result :=
    FSourceProvider.GetBitmapRect(
      AOperationID,
      ACancelNotifier,
      ALocalConverter
    );
  if Result <> nil then begin
    VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
    try
      VTileSize := ALocalConverter.GetLocalRectSize;
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
