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
  GR32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32StaticFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_BitmapPostProcessing,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderSimpleForCombine = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FRecolorConfig: IBitmapPostProcessing;
    FSourceProvider: IBitmapLayerProvider;
    FMarksImageProvider: IBitmapLayerProvider;
    FBitmapFactory: IBitmap32StaticFactory;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const ARecolorConfig: IBitmapPostProcessing;
      const ASourceProvider: IBitmapLayerProvider;
      const AMarksImageProvider: IBitmapLayerProvider
    );
  end;

implementation

uses
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapLayerProviderSimpleForCombine }

constructor TBitmapLayerProviderSimpleForCombine.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const ARecolorConfig: IBitmapPostProcessing;
  const ASourceProvider: IBitmapLayerProvider;
  const AMarksImageProvider: IBitmapLayerProvider
);
begin
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FSourceProvider := ASourceProvider;
  FMarksImageProvider := AMarksImageProvider;
  FRecolorConfig := ARecolorConfig;
end;

function TBitmapLayerProviderSimpleForCombine.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VLayer: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := FSourceProvider.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
  if Result <> nil then begin
    if FRecolorConfig <> nil then begin
      Result := FRecolorConfig.Process(Result);
    end;
  end;
  if FMarksImageProvider <> nil then begin
    VLayer := FMarksImageProvider.GetBitmapRect(AOperationID, ACancelNotifier, ALocalConverter);
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
