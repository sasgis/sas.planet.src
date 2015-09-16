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

unit u_BitmapTileProviderComplex;

interface

uses
  Types,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderComplex = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FProviderFrist: IBitmapTileProvider;
    FProviderSecond: IBitmapTileProvider;
  private
    function GetProjection: IProjection;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AProviderFrist: IBitmapTileProvider;
      const AProviderSecond: IBitmapTileProvider
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapTileProviderComplex }

constructor TBitmapTileProviderComplex.Create(
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AProviderFrist, AProviderSecond: IBitmapTileProvider
);
begin
  Assert(Assigned(ABitmap32StaticFactory));
  Assert(Assigned(AProviderFrist));
  Assert(Assigned(AProviderSecond));
  Assert(AProviderFrist.Projection.GetIsSameProjectionInfo(AProviderSecond.Projection));
  inherited Create;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  FProviderFrist := AProviderFrist;
  FProviderSecond := AProviderSecond;
end;

function TBitmapTileProviderComplex.GetProjection: IProjection;
begin
  Result := FProviderFrist.Projection;
end;

function TBitmapTileProviderComplex.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VResultFirst: IBitmap32Static;
  VResultSecond: IBitmap32Static;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  VResultFirst := FProviderFrist.GetTile(AOperationID, ACancelNotifier, ATile);
  VResultSecond := FProviderSecond.GetTile(AOperationID, ACancelNotifier, ATile);
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
