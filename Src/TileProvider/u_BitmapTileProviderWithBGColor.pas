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

unit u_BitmapTileProviderWithBGColor;

interface

uses
  Types,
  t_Bitmap32,
  i_NotifierOperation,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_ProjectionInfo,
  i_BitmapTileProvider,
  u_BaseInterfacedObject;

type
  TBitmapTileProviderWithBGColor = class(TBaseInterfacedObject, IBitmapTileProvider)
  private
    FBitmap32StaticFactory: IBitmap32StaticFactory;
    FSourceProvider: IBitmapTileProvider;
    FEmptyTile: IBitmap32Static;
    FBackGroundColor: TColor32;
  private
    function GetProjectionInfo: IProjectionInfo;
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      ABackGroundColor: TColor32;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const ASourceProvider: IBitmapTileProvider
    );
  end;

implementation

uses
  GR32,
  u_BitmapFunc,
  u_GeoFunc,
  u_Bitmap32ByStaticBitmap;

{ TBitmapTileProviderWithBGColor }

constructor TBitmapTileProviderWithBGColor.Create(
  ABackGroundColor: TColor32;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const ASourceProvider: IBitmapTileProvider
);
var
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
begin
  Assert(Assigned(ASourceProvider));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FSourceProvider := ASourceProvider;
  FBackGroundColor := ABackGroundColor;
  FBitmap32StaticFactory := ABitmap32StaticFactory;

  VTileSize := ASourceProvider.ProjectionInfo.GeoConverter.GetTileSize(Point(0, 0), ASourceProvider.ProjectionInfo.Zoom);
  VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
  try
    VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
    VTargetBmp.Clear(FBackGroundColor);
    FEmptyTile := VTargetBmp.MakeAndClear;
  finally
    VTargetBmp.Free;
  end;

  Assert(FSourceProvider <> nil);
end;

function TBitmapTileProviderWithBGColor.GetProjectionInfo: IProjectionInfo;
begin
  Result := FSourceProvider.ProjectionInfo;
end;

function TBitmapTileProviderWithBGColor.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ATile: TPoint
): IBitmap32Static;
var
  VTileSize: TPoint;
  VTargetBmp: TBitmap32ByStaticBitmap;
begin
  Result :=
    FSourceProvider.GetTile(
      AOperationID,
      ACancelNotifier,
      ATile
    );
  if Result <> nil then begin
    VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
    try
      VTileSize :=  FSourceProvider.ProjectionInfo.GeoConverter.GetTileSize(ATile, FSourceProvider.ProjectionInfo.Zoom);
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
  end else begin
    VTileSize :=  FSourceProvider.ProjectionInfo.GeoConverter.GetTileSize(ATile, FSourceProvider.ProjectionInfo.Zoom);
    if IsPointsEqual(VTileSize, FEmptyTile.Size) then begin
      Result := FEmptyTile;
    end else begin
      VTargetBmp := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
      try
        VTargetBmp.SetSize(VTileSize.X, VTileSize.Y);
        VTargetBmp.Clear(FBackGroundColor);
        Result := VTargetBmp.MakeAndClear;
      finally
        VTargetBmp.Free;
      end;
    end;
  end;
end;

end.
