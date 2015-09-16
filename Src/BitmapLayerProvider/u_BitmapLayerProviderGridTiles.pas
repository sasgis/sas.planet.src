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

unit u_BitmapLayerProviderGridTiles;

interface

uses
  Types,
  SysUtils,
  GR32,
  i_SimpleFlag,
  i_NotifierOperation,
  i_ProjectionInfo,
  i_ProjectionSet,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderGridTiles = class(TBaseInterfacedObject, IBitmapTileUniProvider)
  private
    FColor: TColor32;
    FUseRelativeZoom: Boolean;
    FZoom: Integer;
    FShowText: Boolean;
    FShowLines: Boolean;
    FProjectionSet: IProjectionSet;
    FBitmapFactory: IBitmap32StaticFactory;
    FCS: IReadWriteSync;
    FBitmap: TBitmap32;
    FBitmapChangeFlag: ISimpleFlag;
    procedure OnBitmapChange(Sender: TObject);
    procedure InitBitmap(const ASize: TPoint);
    function GetActualProjection(
      const AProjection: IProjection
    ): IProjection;
    procedure DrawLines(
      const AGridProjection: IProjection;
      const AProjection: IProjection;
      const AMapRect: TRect
    );
    procedure DrawCaptions(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AGridProjection: IProjection;
      const AProjection: IProjection;
      const AMapRect: TRect
    );
  private
    function GetTile(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const AProjectionInfo: IProjection;
      const ATile: TPoint
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32StaticFactory;
      const AProjectionSet: IProjectionSet;
      AColor: TColor32;
      AUseRelativeZoom: Boolean;
      AZoom: Integer;
      AShowText: Boolean;
      AShowLines: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  t_GeoTypes,
  u_SimpleFlagWithInterlock,
  u_TileIteratorByRect,
  u_GeoFunc,
  u_Synchronizer;

{ TBitmapLayerProviderGridTiles }

constructor TBitmapLayerProviderGridTiles.Create(
  const ABitmapFactory: IBitmap32StaticFactory;
  const AProjectionSet: IProjectionSet;
  AColor: TColor32;
  AUseRelativeZoom: Boolean;
  AZoom: Integer;
  AShowText, AShowLines: Boolean
);
begin
  Assert(Assigned(ABitmapFactory));
  Assert(Assigned(AProjectionSet));
  inherited Create;
  FBitmapFactory := ABitmapFactory;
  FProjectionSet := AProjectionSet;
  FColor := AColor;
  FUseRelativeZoom := AUseRelativeZoom;
  FZoom := AZoom;
  FShowText := AShowText;
  FShowLines := AShowLines;

  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FBitmapChangeFlag := TSimpleFlagWithInterlock.Create;
  FBitmap := TBitmap32.Create;
  FBitmap.SetSize(256, 256);
  FBitmap.OnChange := Self.OnBitmapChange;
end;

destructor TBitmapLayerProviderGridTiles.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TBitmapLayerProviderGridTiles.DrawCaptions(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AGridProjection: IProjection;
  const AProjection: IProjection;
  const AMapRect: TRect
);
var
  VLoadedRelativeRect: TDoubleRect;
  VTilesRect: TRect;
  VIterator: TTileIteratorByRectRecord;
  VTileIndex: TPoint;
  VTileRelativeRect: TDoubleRect;
  VPixelRectOfTile: TDoubleRect;
  VLocalRectOfTile: TDoubleRect;
  VTileSize: TDoublePoint;
  VTileCenter: TDoublePoint;
  textoutx: string;
  textouty: string;
  Sz1, Sz2: TSize;
  VOutPoint: TPoint;
begin
  VLoadedRelativeRect := AProjection.PixelRect2RelativeRect(AMapRect);
  VTilesRect :=
    RectFromDoubleRect(
      AGridProjection.RelativeRect2TileRectFloat(VLoadedRelativeRect),
      rrOutside
    );
  VIterator.Init(VTilesRect);
  while VIterator.Next(VTileIndex) do begin
      VTileRelativeRect := AGridProjection.TilePos2RelativeRect(VTileIndex);
      VPixelRectOfTile := AProjection.RelativeRect2PixelRectFloat(VTileRelativeRect);
      VLocalRectOfTile.Left := VPixelRectOfTile.Left - AMapRect.Left;
      VLocalRectOfTile.Top := VPixelRectOfTile.Top - AMapRect.Top;
      VLocalRectOfTile.Right := VPixelRectOfTile.Right - AMapRect.Left;
      VLocalRectOfTile.Bottom := VPixelRectOfTile.Bottom - AMapRect.Top;
      VTileSize.X := VPixelRectOfTile.Right - VPixelRectOfTile.Left;
      VTileSize.Y := VPixelRectOfTile.Bottom - VPixelRectOfTile.Top;
      VTileCenter.X := VLocalRectOfTile.Left + VTileSize.X / 2;
      VTileCenter.Y := VLocalRectOfTile.Top + VTileSize.Y / 2;
      textoutx := 'x=' + inttostr(VTileIndex.X);
      textouty := 'y=' + inttostr(VTileIndex.Y);
      Sz1 := FBitmap.TextExtent(textoutx);
      Sz2 := FBitmap.TextExtent(textouty);
      if (Sz1.cx < VTileSize.X) and (Sz2.cx < VTileSize.X) then begin
        VOutPoint := Types.Point(Trunc(VTileCenter.X - Sz1.cx / 2), Trunc(VTileCenter.Y - Sz1.cy));
        FBitmap.RenderText(VOutPoint.X, VOutPoint.Y, textoutx, 0, FColor);
        VOutPoint := Types.Point(Trunc(VTileCenter.X - Sz2.cx / 2), Trunc(VTileCenter.Y));
        FBitmap.RenderText(VOutPoint.X, VOutPoint.Y, textouty, 0, FColor);
      end;
  end;
end;

procedure TBitmapLayerProviderGridTiles.DrawLines(
  const AGridProjection: IProjection;
  const AProjection: IProjection;
  const AMapRect: TRect
);
var
  VLocalRect: TRect;
  VRelativeRect: TDoubleRect;
  VTilesRect: TRect;
  VTilesLineRect: TRect;
  i, j: integer;
  VRelativeRectOfTilesLine: TDoubleRect;
  VMapPixelRectOfTilesLine: TDoubleRect;
  VLocalRectOfTilesLine: TRect;
begin
  VLocalRect := Rect(0, 0, AMapRect.Right - AMapRect.Left, AMapRect.Bottom - AMapRect.Top);

  VRelativeRect := AProjection.PixelRect2RelativeRect(AMapRect);
  VTilesRect :=
    RectFromDoubleRect(
      AGridProjection.RelativeRect2TileRectFloat(VRelativeRect),
      rrOutside
    );

  VTilesLineRect.Left := VTilesRect.Left;
  VTilesLineRect.Right := VTilesRect.Right;
  for i := VTilesRect.Top to VTilesRect.Bottom do begin
    VTilesLineRect.Top := i;
    VTilesLineRect.Bottom := i;

    VRelativeRectOfTilesLine := AGridProjection.TileRect2RelativeRect(VTilesLineRect);
    VMapPixelRectOfTilesLine := AProjection.RelativeRect2PixelRectFloat(VRelativeRectOfTilesLine);

    VLocalRectOfTilesLine.Left := VLocalRect.Left;
    VLocalRectOfTilesLine.Top := Trunc(VMapPixelRectOfTilesLine.Top - AMapRect.Top);

    VLocalRectOfTilesLine.Right := VLocalRect.Right;
    VLocalRectOfTilesLine.Bottom := Trunc(VMapPixelRectOfTilesLine.Bottom - AMapRect.Top);

    if (VLocalRectOfTilesLine.Top >= VLocalRect.Top) and
      (VLocalRectOfTilesLine.Top < VLocalRect.Bottom) then begin
      FBitmap.HorzLineTS(
        VLocalRectOfTilesLine.Left,
        VLocalRectOfTilesLine.Top,
        VLocalRectOfTilesLine.Right,
        FColor
      );
    end;
  end;

  VTilesLineRect.Top := VTilesRect.Top;
  VTilesLineRect.Bottom := VTilesRect.Bottom;
  for j := VTilesRect.Left to VTilesRect.Right do begin
    VTilesLineRect.Left := j;
    VTilesLineRect.Right := j;

    VRelativeRectOfTilesLine := AGridProjection.TileRect2RelativeRect(VTilesLineRect);
    VMapPixelRectOfTilesLine := AProjection.RelativeRect2PixelRectFloat(VRelativeRectOfTilesLine);

    VLocalRectOfTilesLine.Left := Trunc(VMapPixelRectOfTilesLine.Left - AMapRect.Left);
    VLocalRectOfTilesLine.Top := VLocalRect.Top;

    VLocalRectOfTilesLine.Right := Trunc(VMapPixelRectOfTilesLine.Right - AMapRect.Left);
    VLocalRectOfTilesLine.Bottom := VLocalRect.Bottom;

    if (VLocalRectOfTilesLine.Left >= VLocalRect.Left) and
      (VLocalRectOfTilesLine.Left < VLocalRect.Right) then begin
      FBitmap.VertLineTS(
        VLocalRectOfTilesLine.Left,
        VLocalRectOfTilesLine.Top,
        VLocalRectOfTilesLine.Bottom,
        FColor
      );
    end;
  end;
end;

function TBitmapLayerProviderGridTiles.GetActualProjection(
  const AProjection: IProjection
): IProjection;
var
  VZoom: Integer;
  VResultZoom: Byte;
  VCurrentZoom: Byte;
  VProjection: IProjection;
begin
  Result := nil;
  VProjection := FProjectionSet.GetSuitableProjection(AProjection);
  VCurrentZoom := VProjection.Zoom;
  VZoom := FZoom;
  if FUseRelativeZoom then begin
    VZoom := VZoom + VCurrentZoom;
  end;
  if VZoom < 0 then begin
    VResultZoom := 0;
  end else begin
    VResultZoom := VZoom;
    FProjectionSet.ValidateZoom(VResultZoom);
  end;
  if VResultZoom <= VCurrentZoom + 5 then begin
    Result := FProjectionSet.Zooms[VResultZoom];
  end;
end;

function TBitmapLayerProviderGridTiles.GetTile(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const AProjectionInfo: IProjection;
  const ATile: TPoint
): IBitmap32Static;
var
  VMapRect: TRect;
  VGridProjection: IProjection;
begin
  Result := nil;
  VGridProjection := GetActualProjection(AProjectionInfo);
  if not Assigned(VGridProjection) then begin
    Exit;
  end;
  VMapRect := AProjectionInfo.TilePos2PixelRect(ATile);

  FCS.BeginWrite;
  try
    InitBitmap(RectSize(VMapRect));
    FBitmapChangeFlag.CheckFlagAndReset;
    if FShowLines then begin
      DrawLines(VGridProjection, AProjectionInfo, VMapRect);
    end;

    if FShowText then begin
      if (VGridProjection.Zoom >= AProjectionInfo.Zoom - 2) and (VGridProjection.Zoom <= AProjectionInfo.Zoom + 3) then begin
        DrawCaptions(AOperationID, ACancelNotifier, VGridProjection, AProjectionInfo, VMapRect);
      end;
    end;
    if FBitmapChangeFlag.CheckFlagAndReset then begin
      Result := FBitmapFactory.Build(Types.Point(FBitmap.Width, FBitmap.Height), FBitmap.Bits);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TBitmapLayerProviderGridTiles.InitBitmap(
  const ASize: TPoint
);
begin
  FBitmap.SetSize(ASize.X, ASize.Y);
  FBitmap.Clear(0);
end;

procedure TBitmapLayerProviderGridTiles.OnBitmapChange(Sender: TObject);
begin
  FBitmapChangeFlag.SetFlag;
end;

end.
