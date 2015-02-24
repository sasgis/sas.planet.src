{******************************************************************************}
{* SAS.Planet (SAS.џырэх±р)                                                   *}
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

unit u_BitmapLayerProviderByTrackPath;

interface

uses
  GR32,
  GR32_Polygons,
  t_GeoTypes,
  i_NotifierOperation,
  i_ProjectionInfo,
  i_Bitmap32Static,
  i_Bitmap32BufferFactory,
  i_LocalCoordConverter,
  i_BitmapLayerProvider,
  i_GPSRecorder,
  i_MapLayerGPSTrackConfig,
  u_BaseInterfacedObject;

type
  TBitmapLayerProviderByTrackPath = class(TBaseInterfacedObject, IBitmapLayerProvider)
  private
    FLineWidth: Double;
    FTrackColorer: ITrackColorerStatic;
    FBitmap32StaticFactory: IBitmap32StaticFactory;

    FRectIsEmpty: Boolean;
    FLonLatRect: TDoubleRect;

    FPointsLonLat: array of TGPSTrackPoint;
    FPointsLonLatCount: Integer;

    FPreparedProjection: IProjectionInfo;
    FPointsProjected: array of TGPSTrackPoint;
    FPointsProjectedCount: Integer;

    FPolygon: TPolygon32;
    procedure PrepareLonLatPointsByEnum(
      AMaxPointsCount: Integer;
      const AEnum: IEnumGPSTrackPoint
    );
    procedure PrepareProjectedPoints(
      const AProjection: IProjectionInfo
    );
    procedure InitBitmap(
      ATargetBmp: TCustomBitmap32;
      const ASize: TPoint
    );
    procedure DrawSection(
      ATargetBmp: TCustomBitmap32;
      const ATrackColorer: ITrackColorerStatic;
      const ALineWidth: Double;
      const APointPrev, APointCurr: TDoublePoint;
      const ASpeed: Double
    );
    function DrawPath(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      ATargetBmp: TCustomBitmap32;
      const ALocalConverter: ILocalCoordConverter;
      const ATrackColorer: ITrackColorerStatic;
      const ALineWidth: Double;
      APointsCount: Integer
    ): Boolean;
  private
    function GetBitmapRect(
      AOperationID: Integer;
      const ACancelNotifier: INotifierOperation;
      const ALocalConverter: ILocalCoordConverter
    ): IBitmap32Static;
  public
    constructor Create(
      AMaxPointsCount: Integer;
      const ALineWidth: Double;
      const ATrackColorer: ITrackColorerStatic;
      const ABitmap32StaticFactory: IBitmap32StaticFactory;
      const AEnum: IEnumGPSTrackPoint
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_CoordConverter,
  u_Bitmap32ByStaticBitmap,
  u_GeoFunc;

{ TBitmapLayerProviderByTrackPath }

constructor TBitmapLayerProviderByTrackPath.Create(
  AMaxPointsCount: Integer;
  const ALineWidth: Double;
  const ATrackColorer: ITrackColorerStatic;
  const ABitmap32StaticFactory: IBitmap32StaticFactory;
  const AEnum: IEnumGPSTrackPoint
);
begin
  Assert(Assigned(ATrackColorer));
  Assert(Assigned(AEnum));
  Assert(Assigned(ABitmap32StaticFactory));
  inherited Create;
  FLineWidth := ALineWidth;
  FTrackColorer := ATrackColorer;
  FBitmap32StaticFactory := ABitmap32StaticFactory;
  Assert(FLineWidth >= 0);
  Assert(FTrackColorer <> nil);
  SetLength(FPointsLonLat, AMaxPointsCount);
  SetLength(FPointsProjected, AMaxPointsCount);

  PrepareLonLatPointsByEnum(AMaxPointsCount, AEnum);

  FPolygon := TPolygon32.Create;
  FPolygon.Antialiased := true;
  FPolygon.AntialiasMode := am4times;
  FPolygon.Closed := false;
end;

destructor TBitmapLayerProviderByTrackPath.Destroy;
begin
  FreeAndNil(FPolygon);
  inherited;
end;

function TBitmapLayerProviderByTrackPath.DrawPath(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  ATargetBmp: TCustomBitmap32;
  const ALocalConverter: ILocalCoordConverter;
  const ATrackColorer: ITrackColorerStatic;
  const ALineWidth: Double;
  APointsCount: Integer
): Boolean;
  function GetCode(
  const AMapRect: TDoubleRect;
  const APoint: TDoublePoint
  ): Byte;
    //  Смысл разрядов кода:

    // 1 рр = 1 - точка над верхним краем окна;

    // 2 рр = 1 - точка под нижним краем окна;

    // 3 рр = 1 - точка справа от правого края окна;

    // 4 рр = 1 - точка слева от левого края окна.
  begin
    Result := 0;
    if AMapRect.Top > APoint.Y then begin
      Result := 1;
    end else if AMapRect.Bottom < APoint.Y then begin
      Result := 2;
    end;

    if AMapRect.Left > APoint.X then begin
      Result := Result or 8;
    end else if AMapRect.Right < APoint.X then begin
      Result := Result or 4;
    end;
  end;

var
  VPointPrev: TDoublePoint;
  VPointPrevIsEmpty: Boolean;
  VPointPrevCode: Byte;
  VPointPrevLocal: TDoublePoint;
  i: Integer;
  VPointCurr: TDoublePoint;
  VPointCurrIsEmpty: Boolean;
  VPointCurrCode: Byte;
  VPointCurrLocal: TDoublePoint;

  VGeoConvert: ICoordConverter;
  VMapPixelRect: TDoubleRect;
begin
  Result := False;
  VGeoConvert := ALocalConverter.GetGeoConverter;
  VMapPixelRect := ALocalConverter.GetRectInMapPixelFloat;

  VPointCurrCode := 0;
  VPointPrevCode := 0;
  VPointPrev := FPointsProjected[APointsCount - 1].Point;
  VPointPrevIsEmpty := PointIsEmpty(VPointPrev);
  if not VPointPrevIsEmpty then begin
    VPointPrevCode := GetCode(VMapPixelRect, VPointPrev);
    VPointPrevLocal := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPointPrev);
  end;
  for i := APointsCount - 2 downto 0 do begin
    if ACancelNotifier.IsOperationCanceled(AOperationID) then begin
      Break;
    end;
    VPointCurr := FPointsProjected[i].Point;
    VPointCurrIsEmpty := PointIsEmpty(VPointCurr);
    if not VPointCurrIsEmpty then begin
      VPointCurrCode := GetCode(VMapPixelRect, VPointCurr);
      VPointCurrLocal := ALocalConverter.MapPixelFloat2LocalPixelFloat(VPointCurr);
      if not VPointPrevIsEmpty then begin
        if (VPointPrevCode and VPointCurrCode) = 0 then begin
          if not Result then begin
            InitBitmap(ATargetBmp, ALocalConverter.GetLocalRectSize);
            Result := True;
          end;
          DrawSection(ATargetBmp, ATrackColorer, ALineWidth, VPointPrevLocal, VPointCurrLocal, FPointsProjected[i].Speed);
        end;
      end;
    end;
    VPointPrev := VPointCurr;
    VPointPrevLocal := VPointCurrLocal;
    VPointPrevIsEmpty := VPointCurrIsEmpty;
    VPointPrevCode := VPointCurrCode;
  end;
end;

procedure TBitmapLayerProviderByTrackPath.DrawSection(
  ATargetBmp: TCustomBitmap32;
  const ATrackColorer: ITrackColorerStatic;
  const ALineWidth: Double;
  const APointPrev, APointCurr: TDoublePoint;
  const ASpeed: Double
);
var
  VFixedPointsPair: array [0..10] of TFixedPoint;
  VSegmentColor: TColor32;
begin
  if (APointPrev.x < 32767) and (APointPrev.x > -32767) and (APointPrev.y < 32767) and (APointPrev.y > -32767) then begin
    VFixedPointsPair[0] := FixedPoint(APointPrev.X, APointPrev.Y);
    VFixedPointsPair[1] := FixedPoint(APointCurr.X, APointCurr.Y);
    FPolygon.Clear;
    FPolygon.AddPoints(VFixedPointsPair[0], 2);
    with FPolygon.Outline do begin
      try
        with Grow(Fixed(ALineWidth / 2), 0.5) do begin
          try
            VSegmentColor := ATrackColorer.GetColorForSpeed(ASpeed);
            DrawFill(ATargetBmp, VSegmentColor);
          finally
            free;
          end;
        end;
      finally
        free;
      end;
    end;
    FPolygon.Clear;
  end;
end;

function TBitmapLayerProviderByTrackPath.GetBitmapRect(
  AOperationID: Integer;
  const ACancelNotifier: INotifierOperation;
  const ALocalConverter: ILocalCoordConverter
): IBitmap32Static;
var
  VTargetRect: TDoubleRect;
  VLonLatRect: TDoubleRect;
  VConverter: ICoordConverter;
  VZoom: Byte;
  VBitmap: TBitmap32ByStaticBitmap;
begin
  Result := nil;
  if not FRectIsEmpty then begin
    VZoom := ALocalConverter.GetZoom;
    VConverter := ALocalConverter.GetGeoConverter;
    VTargetRect := ALocalConverter.GetRectInMapPixelFloat;
    VConverter.ValidatePixelRectFloat(VTargetRect, VZoom);
    VLonLatRect := VConverter.PixelRectFloat2LonLatRect(VTargetRect, VZoom);
    if IsIntersecLonLatRect(FLonLatRect, VLonLatRect) then begin
      VBitmap := TBitmap32ByStaticBitmap.Create(FBitmap32StaticFactory);
      try
        if not ALocalConverter.ProjectionInfo.GetIsSameProjectionInfo(FPreparedProjection) then begin
          PrepareProjectedPoints(ALocalConverter.ProjectionInfo);
        end;
        if
          DrawPath(
            AOperationID,
            ACancelNotifier,
            VBitmap,
            ALocalConverter,
            FTrackColorer,
            FLineWidth,
            FPointsProjectedCount
          )
        then begin
          Result := VBitmap.MakeAndClear;
        end;
      finally
        VBitmap.Free;
      end;
    end;
  end;
end;

procedure TBitmapLayerProviderByTrackPath.InitBitmap(
  ATargetBmp: TCustomBitmap32;
  const ASize: TPoint
);
begin
  ATargetBmp.SetSize(ASize.X, ASize.Y);
  ATargetBmp.Clear(0);
  ATargetBmp.CombineMode := cmMerge;
end;

procedure TBitmapLayerProviderByTrackPath.PrepareLonLatPointsByEnum(
  AMaxPointsCount: Integer;
  const AEnum: IEnumGPSTrackPoint
);
var
  i: Integer;
  VPoint: TGPSTrackPoint;
begin
  FRectIsEmpty := True;
  FPointsLonLatCount := 0;
  i := 0;
  while (i < AMaxPointsCount) and AEnum.Next(VPoint) do begin
    if not PointIsEmpty(VPoint.Point) then begin
      if FRectIsEmpty then begin
        FLonLatRect.TopLeft := VPoint.Point;
        FLonLatRect.BottomRight := VPoint.Point;
        FRectIsEmpty := False;
      end else begin
        if FLonLatRect.Left > VPoint.Point.X then begin
          FLonLatRect.Left := VPoint.Point.X;
        end;
        if FLonLatRect.Top < VPoint.Point.Y then begin
          FLonLatRect.Top := VPoint.Point.Y;
        end;
        if FLonLatRect.Right < VPoint.Point.X then begin
          FLonLatRect.Right := VPoint.Point.X;
        end;
        if FLonLatRect.Bottom > VPoint.Point.Y then begin
          FLonLatRect.Bottom := VPoint.Point.Y;
        end;
      end;
    end;
    FPointsLonLat[i] := VPoint;
    Inc(i);
  end;
  FPointsLonLatCount := i;
end;

procedure TBitmapLayerProviderByTrackPath.PrepareProjectedPoints(
  const AProjection: IProjectionInfo
);
var
  i: Integer;
  VIndex: Integer;
  VPoint: TGPSTrackPoint;
  VGeoConverter: ICoordConverter;
  VZoom: Byte;
  VCurrPointIsEmpty: Boolean;
  VPrevPointIsEmpty: Boolean;
  VCurrPoint: TDoublePoint;
  VPrevPoint: TDoublePoint;
begin
  FPointsProjectedCount := 0;
  FPreparedProjection := AProjection;
  VGeoConverter := AProjection.GeoConverter;
  VZoom := AProjection.Zoom;
  i := 0;
  VIndex := 0;
  VPrevPointIsEmpty := True;
  while (i < FPointsLonLatCount) do begin
    VPoint := FPointsLonLat[i];
    VCurrPointIsEmpty := PointIsEmpty(VPoint.Point);
    if not VCurrPointIsEmpty then begin
      VGeoConverter.ValidateLonLatPos(VPoint.Point);
      if FRectIsEmpty then begin
        FLonLatRect.TopLeft := VPoint.Point;
        FLonLatRect.BottomRight := VPoint.Point;
        FRectIsEmpty := False;
      end else begin
        if FLonLatRect.Left > VPoint.Point.X then begin
          FLonLatRect.Left := VPoint.Point.X;
        end;
        if FLonLatRect.Top < VPoint.Point.Y then begin
          FLonLatRect.Top := VPoint.Point.Y;
        end;
        if FLonLatRect.Right < VPoint.Point.X then begin
          FLonLatRect.Right := VPoint.Point.X;
        end;
        if FLonLatRect.Bottom > VPoint.Point.Y then begin
          FLonLatRect.Bottom := VPoint.Point.Y;
        end;
      end;
      VPoint.Point := VGeoConverter.LonLat2PixelPosFloat(VPoint.Point, VZoom);
    end;

    VCurrPoint := VPoint.Point;
    if VCurrPointIsEmpty then begin
      if not VPrevPointIsEmpty then begin
        FPointsProjected[VIndex] := VPoint;
        Inc(VIndex);
        VPrevPointIsEmpty := VCurrPointIsEmpty;
        VPrevPoint := VCurrPoint;
      end;
    end else begin
      if VPrevPointIsEmpty then begin
        FPointsProjected[VIndex] := VPoint;
        Inc(VIndex);
        VPrevPointIsEmpty := VCurrPointIsEmpty;
        VPrevPoint := VCurrPoint;
      end else begin
        if (abs(VPrevPoint.X - VCurrPoint.X) > 2) or
          (abs(VPrevPoint.Y - VCurrPoint.Y) > 2) then begin
          FPointsProjected[VIndex] := VPoint;
          Inc(VIndex);
          VPrevPointIsEmpty := VCurrPointIsEmpty;
          VPrevPoint := VCurrPoint;
        end;
      end;
    end;
    Inc(i);
  end;
  FPointsProjectedCount := VIndex;
end;

end.
