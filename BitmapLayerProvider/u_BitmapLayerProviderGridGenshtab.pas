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

unit u_BitmapLayerProviderGridGenshtab;

interface

uses
  Types,
  SysUtils,
  GR32,
  i_SimpleFlag,
  i_NotifierOperation,
  i_LocalCoordConverter,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_BitmapLayerProvider,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderGridGenshtab = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FColor: TColor32;
    FShowText: Boolean;
    FShowLines: Boolean;
    FScale: Integer;
    FBitmapFactory: IBitmap32BufferFactory;

    FCS: IReadWriteSync;
    FBitmap: TBitmap32;
    FBitmapChangeFlag: ISimpleFlag;
    procedure OnBitmapChange(Sender: TObject);
    procedure InitBitmap(const ALocalConverter: ILocalCoordConverter);
    procedure DrawLines(
      const ALocalConverter: ILocalCoordConverter
    );
    procedure DrawCaptions(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    );
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      const ABitmapFactory: IBitmap32BufferFactory;
      AColor: TColor32;
      AScale: Integer;
      AShowText: Boolean;
      AShowLines: Boolean
    );
    destructor Destroy; override;
  end;

implementation

uses
  Math,
  WGS84_SK42,
  t_GeoTypes,
  i_CoordConverter,
  u_SimpleFlagWithInterlock,
  u_GeoFunc,
  u_GeoToStrFunc,
  u_Synchronizer;

function WGS84_To_SK42(const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := SK42_WGS84_Long(APoint.Y, APoint.X, 0);
  Result.Y := SK42_WGS84_Lat(APoint.Y, APoint.X, 0);
end;

{ TBitmapLayerProviderGridGenshtab }

constructor TBitmapLayerProviderGridGenshtab.Create(
  const ABitmapFactory: IBitmap32BufferFactory;
  AColor: TColor32;
  AScale: Integer;
  AShowText, AShowLines: Boolean
);
begin
  inherited Create;
  FColor := AColor;
  FScale := AScale;
  FShowText := AShowText;
  FShowLines := AShowLines;
  FBitmapFactory := ABitmapFactory;

  FCS := GSync.SyncVariable.Make(Self.ClassName);
  FBitmapChangeFlag := TSimpleFlagWithInterlock.Create;
  FBitmap := TBitmap32.Create;
  FBitmap.SetSize(256, 256);
  FBitmap.OnChange := Self.OnBitmapChange;
end;

destructor TBitmapLayerProviderGridGenshtab.Destroy;
begin
  FreeAndNil(FBitmap);
  inherited;
end;

procedure TBitmapLayerProviderGridGenshtab.DrawCaptions(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
);
var
  VLocalRect: TRect;
  z: TDoublePoint;
  VZoom: Byte;
  VLoadedRect: TDoubleRect;
  VLoadedLonLatRect: TDoubleRect;
  VGridLonLatRect: TDoubleRect;
  VLonLatRectOfCell: TDoubleRect;
  VLocalRectOfCell: TDoubleRect;
  VGeoConvert: ICoordConverter;
  VGridRect: TRect;
  i, j: Integer;
  VTextSize: TSize;
  VListName: String;
  VLonLatCenter: TDoublePoint;
  VLocalCellCenter: TDoublePoint;
  VOutPoint: TPoint;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  z := GetGhBordersStepByScale(FScale, VZoom);
  VLocalRect := ALocalConverter.GetLocalRect;
  VLoadedRect := ALocalConverter.GetRectInMapPixelFloat;

  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);

  VLoadedLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VLoadedRect, VZoom);
  if VLoadedLonLatRect.Top > 90 then begin
    VLoadedLonLatRect.Top := 90;
  end;
  if VLoadedLonLatRect.Bottom < -90 then begin
    VLoadedLonLatRect.Bottom := -90;
  end;

  VGridRect.Left := Floor(VLoadedLonLatRect.Left / z.X);
  VGridRect.Top := Ceil(VLoadedLonLatRect.Top / z.Y);
  VGridRect.Right := Ceil(VLoadedLonLatRect.Right / z.X);
  VGridRect.Bottom := Floor(VLoadedLonLatRect.Bottom / z.Y);

  VGridLonLatRect.Left := VGridRect.Left * z.X;
  VGridLonLatRect.Top := VGridRect.Top * z.Y;
  VGridLonLatRect.Right := VGridRect.Right * z.X;
  VGridLonLatRect.Bottom := VGridRect.Bottom * z.Y;

  VLonLatRectOfCell.TopLeft := VGridLonLatRect.TopLeft;
  VLonLatRectOfCell.BottomRight := DoublePoint(VGridLonLatRect.Left + z.X, VGridLonLatRect.Top - z.Y);
  VGeoConvert.CheckLonLatRect(VLonLatRectOfCell);
  VLocalRectOfCell := ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCell);
  if abs(VLocalRectOfCell.Right - VLocalRectOfCell.Left) < 30 then begin
    exit;
  end;
  VLonLatRectOfCell.Right := VGridLonLatRect.Left;
  for i := VGridRect.Left + 1 to VGridRect.Right do begin
    VLonLatRectOfCell.Left := VLonLatRectOfCell.Right;
    VLonLatRectOfCell.Right := i * z.X;
    VLonLatRectOfCell.Top := VGridLonLatRect.Bottom;
    for j := VGridRect.Bottom + 1 to VGridRect.Top do begin
      VLonLatRectOfCell.Bottom := VLonLatRectOfCell.Top;
      VLonLatRectOfCell.Top := j * z.Y;
      VGeoConvert.CheckLonLatRect(VLonLatRectOfCell);

      VLonLatCenter := RectCenter(VLonLatRectOfCell);
      VListName := LonLat2GShListName(VLonLatCenter, GetActualGshSCale(FScale,ALocalConverter.getzoom), 100000000);

      VTextSize := FBitmap.TextExtent(VListName);

      VLocalRectOfCell := ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCell);
      VLocalCellCenter := RectCenter(VLocalRectOfCell);
      VOutPoint := Types.Point(Trunc(VLocalCellCenter.X - VTextSize.cx / 2), Trunc(VLocalCellCenter.Y - VTextSize.cy));
      FBitmap.RenderText(VOutPoint.X, VOutPoint.Y, VListName, 0, FColor);
    end;
  end;
end;

procedure TBitmapLayerProviderGridGenshtab.DrawLines(
  const ALocalConverter: ILocalCoordConverter
);
var
  VLocalRect: TRect;
  VGridStep: TDoublePoint;
  VZoom: Byte;
  VLoadedRect: TDoubleRect;
  VLoadedLonLatRect: TDoubleRect;
  VGridLonLatRect: TDoubleRect;
  VLonLatRectOfCellsLine: TDoubleRect;
  VLocalRectOfCellsLine: TRect;
  VGeoConvert: ICoordConverter;
  VGridRect: TRect;
  I: Integer;
begin
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VZoom := ALocalConverter.GetZoom;
  VGridStep := GetGhBordersStepByScale(FScale, VZoom);
  VLocalRect := ALocalConverter.GetLocalRect;
  VLoadedRect := ALocalConverter.GetRectInMapPixelFloat;

  VGeoConvert.CheckPixelRectFloat(VLoadedRect, VZoom);

  VLoadedLonLatRect := VGeoConvert.PixelRectFloat2LonLatRect(VLoadedRect, VZoom);
  if VLoadedLonLatRect.Top > 90 then begin
    VLoadedLonLatRect.Top := 90;
  end;
  if VLoadedLonLatRect.Bottom < -90 then begin
    VLoadedLonLatRect.Bottom := -90;
  end;

  VGridRect.Left := Floor(VLoadedLonLatRect.Left / VGridStep.X);
  VGridRect.Top := Ceil(VLoadedLonLatRect.Top / VGridStep.Y);
  VGridRect.Right := Ceil(VLoadedLonLatRect.Right / VGridStep.X);
  VGridRect.Bottom := Floor(VLoadedLonLatRect.Bottom / VGridStep.Y);

  VGridLonLatRect.Left := VGridRect.Left * VGridStep.X;
  VGridLonLatRect.Top := VGridRect.Top * VGridStep.Y;
  VGridLonLatRect.Right := VGridRect.Right * VGridStep.X;
  VGridLonLatRect.Bottom := VGridRect.Bottom * VGridStep.Y;

  VLonLatRectOfCellsLine.TopLeft := WGS84_To_SK42(VGridLonLatRect.TopLeft);
  VLonLatRectOfCellsLine.BottomRight := WGS84_To_SK42(
    DoublePoint(VGridLonLatRect.Left + VGridStep.X, VGridLonLatRect.Top - VGridStep.Y)
  );

  VGeoConvert.CheckLonLatRect(VLonLatRectOfCellsLine);
  VLocalRectOfCellsLine :=
    RectFromDoubleRect(
      ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCellsLine),
      rrToTopLeft
    );
  if Abs(VLocalRectOfCellsLine.Right - VLocalRectOfCellsLine.Left) < 4 then begin
    Exit;
  end;

  for I := VGridRect.Left to VGridRect.Right do begin
    VLonLatRectOfCellsLine.Left := I * VGridStep.X;
    VLonLatRectOfCellsLine.Top := VGridLonLatRect.Top;
    VLonLatRectOfCellsLine.Right := VLonLatRectOfCellsLine.Left;
    VLonLatRectOfCellsLine.Bottom := VGridLonLatRect.Bottom;

    VLonLatRectOfCellsLine.TopLeft := WGS84_To_SK42(VLonLatRectOfCellsLine.TopLeft);
    VLonLatRectOfCellsLine.BottomRight := WGS84_To_SK42(VLonLatRectOfCellsLine.BottomRight);

    VGeoConvert.CheckLonLatRect(VLonLatRectOfCellsLine);
    VLocalRectOfCellsLine :=
      RectFromDoubleRect(
        ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCellsLine),
        rrToTopLeft
      );

    if (VLocalRectOfCellsLine.Left >= VLocalRect.Left) and
      (VLocalRectOfCellsLine.Left < VLocalRect.Right) then begin

      if VLocalRectOfCellsLine.Top > VLocalRect.Top then begin
        VLocalRectOfCellsLine.Top := VLocalRect.Top;
      end;
      if VLocalRectOfCellsLine.Bottom < VLocalRect.Bottom then begin
        VLocalRectOfCellsLine.Bottom := VLocalRect.Bottom
      end;

      FBitmap.LineTS(
        VLocalRectOfCellsLine.Left,
        VLocalRectOfCellsLine.Top,
        VLocalRectOfCellsLine.Right,
        VLocalRectOfCellsLine.Bottom,
        FColor
      );
    end;
  end;

  for I := VGridRect.Bottom to VGridRect.Top do begin
    VLonLatRectOfCellsLine.Left := VGridLonLatRect.Left;
    VLonLatRectOfCellsLine.Top := I * VGridStep.Y;
    VLonLatRectOfCellsLine.Right := VGridLonLatRect.Right;
    VLonLatRectOfCellsLine.Bottom := VLonLatRectOfCellsLine.Top;

    VLonLatRectOfCellsLine.TopLeft := WGS84_To_SK42(VLonLatRectOfCellsLine.TopLeft);
    VLonLatRectOfCellsLine.BottomRight := WGS84_To_SK42(VLonLatRectOfCellsLine.BottomRight);

    VGeoConvert.CheckLonLatRect(VLonLatRectOfCellsLine);
    VLocalRectOfCellsLine :=
      RectFromDoubleRect(
        ALocalConverter.LonLatRect2LocalRectFloat(VLonLatRectOfCellsLine),
        rrToTopLeft
      );

    if (VLocalRectOfCellsLine.Top >= VLocalRect.Top) and
      (VLocalRectOfCellsLine.Bottom < VLocalRect.Bottom) then begin

      if VLocalRectOfCellsLine.Left > VLocalRect.Left then begin
        VLocalRectOfCellsLine.Left := VLocalRect.Left
      end;
      if VLocalRectOfCellsLine.Right < VLocalRect.Right then begin
        VLocalRectOfCellsLine.Right := VLocalRect.Right
      end;

      FBitmap.LineTS(
        VLocalRectOfCellsLine.Left,
        VLocalRectOfCellsLine.Top,
        VLocalRectOfCellsLine.Right,
        VLocalRectOfCellsLine.Bottom,
        FColor
      );
    end;
  end;
end;

function TBitmapLayerProviderGridGenshtab.GetBitmapRect(AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter): IBitmap32Static;
begin
  Result := nil;
  FCS.BeginWrite;
  try
    InitBitmap(ALocalConverter);
    FBitmapChangeFlag.CheckFlagAndReset;
    if FShowLines then begin
      DrawLines(ALocalConverter);
    end;

    if FShowText then begin
      DrawCaptions(AOperationID, ACancelNotifier, ALocalConverter);
    end;
    if FBitmapChangeFlag.CheckFlagAndReset then begin
      Result := FBitmapFactory.Build(Types.Point(FBitmap.Width, FBitmap.Height), FBitmap.Bits);
    end;
  finally
    FCS.EndWrite;
  end;
end;

procedure TBitmapLayerProviderGridGenshtab.InitBitmap(
  const ALocalConverter: ILocalCoordConverter);
var
  VSize: TPoint;
begin
  VSize := ALocalConverter.GetLocalRectSize;
  FBitmap.SetSize(VSize.X, VSize.Y);
  FBitmap.Clear(0);
end;

procedure TBitmapLayerProviderGridGenshtab.OnBitmapChange(Sender: TObject);
begin
  FBitmapChangeFlag.SetFlag;
end;

end.
