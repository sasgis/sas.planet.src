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

unit u_BitmapLayerProviderSimpleForCombine;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_BitmapLayerProvider,
  i_BitmapPostProcessing,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderSimpleForCombine = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FRecolorConfig: IBitmapPostProcessing;
    FSourceProvider: IBitmapTileUniProvider;
    FMarksImageProvider: IBitmapTileUniProvider;
    FBitmap32StaticFactory: IBitmap32StaticFactory;
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
      const ARecolorConfig: IBitmapPostProcessing;
      const ASourceProvider: IBitmapTileUniProvider;
      const AMarksImageProvider: IBitmapTileUniProvider
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderSimpleForCombine }

constructor TBitmapLayerProviderSimpleForCombine.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ARecolorConfig: IBitmapPostProcessing;
  const ASourceProvider: IBitmapTileUniProvider;
  const AMarksImageProvider: IBitmapTileUniProvider
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(ASourceProvider));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FSourceProvider := ASourceProvider;
  FMarksImageProvider := AMarksImageProvider;
  FRecolorConfig := ARecolorConfig;
end;

function TBitmapLayerProviderSimpleForCombine.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjectionInfo;
  const ATile: TPoint
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := FSourceProvider.GetTile(AOperationID, ACancelNotifier, AProjectionInfo, ATile);
  if Result <> nil then begin
    if FRecolorConfig <> nil then begin
      Result := FRecolorConfig.Process(Result);
    end;
  end;
  if FMarksImageProvider <> nil then begin
    VLayer := FMarksImageProvider.GetTile(AOperationID, ACancelNotifier, AProjectionInfo, ATile);
  end;
  if Result <> nil then begin
    if VLayer <> nil then begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
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
