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

unit u_BitmapLayerProviderComplex;

interface

uses
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderComplex = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FBitmapFactory: IBitmap32StaticFactory;
    FProviderFrist: IBitmapLayerProvider;
    FProviderSecond: IBitmapLayerProvider;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProviderFrist: IBitmapLayerProvider;
      const AProviderSecond: IBitmapLayerProvider
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderComplex }

constructor TBitmapLayerProviderComplex.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProviderFrist, AProviderSecond: IBitmapLayerProvider
);
begin
  Assert(ABitmapFactory <> nil);
  Assert(AProviderFrist <> nil);
  Assert(AProviderSecond <> nil);
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FProviderFrist := AProviderFrist;
  FProviderSecond := AProviderSecond;
end;

function TBitmapLayerProviderComplex.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VResultFirst: IBitmap32Static;
  VResultSecond: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  VResultFirst := FProviderFrist.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  VResultSecond := FProviderSecond.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  if VResultFirst = nil then begin
    Result := VResultSecond;
  end else begin
    if VResultSecond = nil then begin
      Result := VResultFirst;
    end else begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmapFactory);
      try
        AssignStaticToBitmap32(VBitmap, VResultFirst);
        BlockTransferFull(
          VBitmap,
          0, 0,
          VResultSecond,
          dmBlend,
          cmMerge
        );
        Result := VBitmap.MakeAndClear;
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

end.
