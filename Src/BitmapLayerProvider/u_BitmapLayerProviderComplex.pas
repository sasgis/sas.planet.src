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
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderComplex = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FProviderFrist: IBitmapTileUniProvider;
    FProviderSecond: IBitmapTileUniProvider;
  private
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjectionInfo;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProviderFrist: IBitmapTileUniProvider;
      const AProviderSecond: IBitmapTileUniProvider
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderComplex }

constructor TBitmapLayerProviderComplex.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProviderFrist, AProviderSecond: IBitmapTileUniProvider
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(AProviderFrist <> nil);
  Assert(AProviderSecond <> nil);
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProviderFrist := AProviderFrist;
  FProviderSecond := AProviderSecond;
end;

function TBitmapLayerProviderComplex.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IBitmap32Static;
var
  VResultFirst: IBitmap32Static;
  VResultSecond: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  VResultFirst := FProviderFrist.GetTile(AOperationID, ACancelNotifier, AProjectionInfo, ATile);
  VResultSecond := FProviderSecond.GetTile(AOperationID, ACancelNotifier, AProjectionInfo, ATile);
  if VResultFirst = nil then begin
    Result := VResultSecond;
  end else begin
    if VResultSecond = nil then begin
      Result := VResultFirst;
    end else begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
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
