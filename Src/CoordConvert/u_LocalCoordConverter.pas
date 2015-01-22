{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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

unit u_LocalCoordConverter;

interface

uses
  Types,
  t_Hash,
  t_GeoTypes,
  i_CoordConverter,
  i_ProjectionInfo,
  i_LocalCoordConverter,
  u_BaseInterfacedObject;

type
  TLocalCoordConverterBase = class(TBaseInterfacedObject, ILocalCoordConverter)
  private
    FHash: THashValue;
    FLocalRect: TRect;
    FLocalSize: TPoint;
    FLocalCenter: TDoublePoint;
    FRectInMapPixel: TRect;
    FRectInMapPixelFloat: TDoubleRect;
    FMapPixelCenter: TDoublePoint;
    FProjection: IProjectionInfo;
    FZoom: Byte;
    FGeoConverter: ICoordConverter;
  protected
    function GetHash: THashValue;
    function GetIsSameConverter(const AConverter: ILocalCoordConverter): Boolean; virtual;

    function GetScale: Double; virtual; abstract;
    function GetLocalRect: TRect;
    function GetLocalRectSize: TPoint;

    function GetProjectionInfo: IProjectionInfo;
    function GetZoom: Byte;
    function GetGeoConverter: ICoordConverter;

    function LocalPixel2MapPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; virtual; abstract;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint; virtual; abstract;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint; virtual; abstract;
    function MapPixel2LocalPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; virtual; abstract;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint; virtual; abstract;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint; virtual; abstract;

    function LocalRect2MapRect(
      const ARect: TRect;
      ARounding: TRectRounding
    ): TRect;
    function LocalRect2MapRectFloat(const ARect: TRect): TDoubleRect;
    function LocalRectFloat2MapRectFloat(const ARect: TDoubleRect): TDoubleRect;
    function MapRect2LocalRect(
      const ARect: TRect;
      ARounding: TRectRounding
    ): TRect;
    function MapRect2LocalRectFloat(const ARect: TRect): TDoubleRect;
    function MapRectFloat2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;

    function LonLat2LocalPixel(
      const APoint: TDoublePoint;
      ARounding: TPointRounding
    ): TPoint;
    function LonLat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint;
    function LonLatRect2LocalRectFloat(const ARect: TDoubleRect): TDoubleRect;

    function GetCenterMapPixelFloat: TDoublePoint;
    function GetCenterLonLat: TDoublePoint;
    function GetRectInMapPixel: TRect;
    function GetRectInMapPixelFloat: TDoubleRect;
  public
    constructor Create(
      const AHash: THashValue;
      const ALocalRect: TRect;
      const ARectInMapPixel: TRect;
      const ARectInMapPixelFloat: TDoubleRect;
      const AMapPixelCenter: TDoublePoint;
      const AProjection: IProjectionInfo
    );
  end;


  TLocalCoordConverter = class(TLocalCoordConverterBase)
  private
    FMapPixelAtLocalZero: TDoublePoint;
    FMapScale: Double;
  protected
    function GetIsSameConverter(const AConverter: ILocalCoordConverter): Boolean; override;
    function GetScale: Double; override;
    function LocalPixel2MapPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; override;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint; override;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint; override;
    function MapPixel2LocalPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; override;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint; override;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(
      const AHash: THashValue;
      const ALocalRect: TRect;
      const ARectInMapPixel: TRect;
      const ARectInMapPixelFloat: TDoubleRect;
      const AMapPixelCenter: TDoublePoint;
      const AProjection: IProjectionInfo;
      const AMapScale: Double;
      const AMapPixelAtLocalZero: TDoublePoint
    );
  end;

  TLocalCoordConverterNoScaleIntDelta = class(TLocalCoordConverterBase)
  private
    FMapPixelAtLocalZero: TPoint;
  protected
    function GetIsSameConverter(const AConverter: ILocalCoordConverter): Boolean; override;
    function GetScale: Double; override;
    function LocalPixel2MapPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; override;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint; override;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint; override;
    function MapPixel2LocalPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; override;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint; override;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(
      const AHash: THashValue;
      const ALocalRect: TRect;
      const ARectInMapPixel: TRect;
      const ARectInMapPixelFloat: TDoubleRect;
      const AMapPixelCenter: TDoublePoint;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TPoint
    );
  end;

  TLocalCoordConverterNoScale = class(TLocalCoordConverterBase)
  private
    FMapPixelAtLocalZero: TDoublePoint;
  protected
    function GetIsSameConverter(const AConverter: ILocalCoordConverter): Boolean; override;
    function GetScale: Double; override;
    function LocalPixel2MapPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; override;
    function LocalPixel2MapPixelFloat(const APoint: TPoint): TDoublePoint; override;
    function LocalPixelFloat2MapPixelFloat(const APoint: TDoublePoint): TDoublePoint; override;
    function MapPixel2LocalPixel(
      const APoint: TPoint;
      ARounding: TPointRounding
    ): TPoint; override;
    function MapPixel2LocalPixelFloat(const APoint: TPoint): TDoublePoint; override;
    function MapPixelFloat2LocalPixelFloat(const APoint: TDoublePoint): TDoublePoint; override;
  public
    constructor Create(
      const AHash: THashValue;
      const ALocalRect: TRect;
      const ARectInMapPixel: TRect;
      const ARectInMapPixelFloat: TDoubleRect;
      const AMapPixelCenter: TDoublePoint;
      const AProjection: IProjectionInfo;
      const AMapPixelAtLocalZero: TDoublePoint
    );
  end;

implementation

uses
  u_GeoFunc;

constructor TLocalCoordConverterBase.Create(
  const AHash: THashValue;
  const ALocalRect: TRect;
  const ARectInMapPixel: TRect;
  const ARectInMapPixelFloat: TDoubleRect;
  const AMapPixelCenter: TDoublePoint;
  const AProjection: IProjectionInfo
);
begin
  Assert(Assigned(AProjection));
  Assert(ALocalRect.Left <= ALocalRect.Right);
  Assert(ALocalRect.Top <= ALocalRect.Bottom);
  Assert(ARectInMapPixelFloat.Left <= ARectInMapPixelFloat.Right);
  Assert(ARectInMapPixelFloat.Top <= ARectInMapPixelFloat.Bottom);
  Assert(ARectInMapPixel.Left <= ARectInMapPixel.Right);
  Assert(ARectInMapPixel.Top <= ARectInMapPixel.Bottom);
  Assert(abs(ARectInMapPixelFloat.Left - ARectInMapPixel.Left) < 2);
  Assert(abs(ARectInMapPixelFloat.Top - ARectInMapPixel.Top) < 2);
  Assert(abs(ARectInMapPixelFloat.Right - ARectInMapPixel.Right) < 2);
  Assert(abs(ARectInMapPixelFloat.Bottom - ARectInMapPixel.Bottom) < 2);
  Assert(AProjection.GeoConverter.CheckPixelRect(ARectInMapPixel, AProjection.Zoom));
  Assert(AProjection.GeoConverter.CheckPixelRectFloat(ARectInMapPixelFloat, AProjection.Zoom));
  inherited Create;
  FHash := AHash;
  FLocalRect := ALocalRect;
  FLocalSize.X := FLocalRect.Right - FLocalRect.Left;
  FLocalSize.Y := FLocalRect.Bottom - FLocalRect.Top;
  FLocalCenter.X := FLocalRect.Left + FLocalSize.X / 2;
  FLocalCenter.Y := FLocalRect.Left + FLocalSize.Y / 2;
  FRectInMapPixel := ARectInMapPixel;
  FRectInMapPixelFloat := ARectInMapPixelFloat;
  FMapPixelCenter := AMapPixelCenter;
  FProjection := AProjection;
  FZoom := FProjection.Zoom;
  FGeoConverter := FProjection.GeoConverter;
end;

function TLocalCoordConverterBase.GetCenterLonLat: TDoublePoint;
begin
  Result := FGeoConverter.PixelPosFloat2LonLat(GetCenterMapPixelFloat, FZoom);
end;

function TLocalCoordConverterBase.GetCenterMapPixelFloat: TDoublePoint;
begin
  Result := FMapPixelCenter;
end;

function TLocalCoordConverterBase.GetGeoConverter: ICoordConverter;
begin
  Result := FGeoConverter;
end;

function TLocalCoordConverterBase.GetHash: THashValue;
begin
  Result := FHash;
end;

function TLocalCoordConverterBase.GetIsSameConverter(
  const AConverter: ILocalCoordConverter
): Boolean;
begin
  if ILocalCoordConverter(Self) = AConverter then begin
    Result := True;
  end else if AConverter = nil then begin
    Result := False;
  end else if (FHash <> 0) and (AConverter.Hash <> 0) and (FHash <> AConverter.Hash) then begin
    Result := False;
  end else begin
    Result := False;
    if FProjection.GetIsSameProjectionInfo(AConverter.ProjectionInfo) then begin
      if EqualRect(FLocalRect, AConverter.GetLocalRect) then begin
        Result := True;
      end;
    end;
  end;
end;

function TLocalCoordConverterBase.GetLocalRect: TRect;
begin
  Result := FLocalRect;
end;

function TLocalCoordConverterBase.GetLocalRectSize: TPoint;
begin
  Result := FLocalSize;
end;

function TLocalCoordConverterBase.GetProjectionInfo: IProjectionInfo;
begin
  Result := FProjection;
end;

function TLocalCoordConverterBase.GetRectInMapPixel: TRect;
begin
  Result := FRectInMapPixel;
end;

function TLocalCoordConverterBase.GetRectInMapPixelFloat: TDoubleRect;
begin
  Result := FRectInMapPixelFloat;
end;

function TLocalCoordConverterBase.GetZoom: Byte;
begin
  Result := FZoom;
end;

function TLocalCoordConverterBase.LocalRect2MapRect(
  const ARect: TRect;
  ARounding: TRectRounding
): TRect;
var
  VTopLeftRound: TPointRounding;
  VBottomRightRound: TPointRounding;
begin
  VTopLeftRound := prToTopLeft;
  VBottomRightRound := prToTopLeft;
  case ARounding of
    rrClosest: begin
      VTopLeftRound := prClosest;
      VBottomRightRound := prClosest;
    end;
    rrOutside: begin
      VTopLeftRound := prToTopLeft;
      VBottomRightRound := prToBottomRight;
    end;
    rrInside: begin
      VTopLeftRound := prToBottomRight;
      VBottomRightRound := prToTopLeft;
    end;
    rrToTopLeft: begin
      VTopLeftRound := prToTopLeft;
      VBottomRightRound := prToTopLeft;
    end;
  end;
  Result.TopLeft := LocalPixel2MapPixel(ARect.TopLeft, VTopLeftRound);
  Result.BottomRight := LocalPixel2MapPixel(ARect.BottomRight, VBottomRightRound);
end;

function TLocalCoordConverterBase.LocalRect2MapRectFloat(
  const ARect: TRect): TDoubleRect;
begin
  Result.TopLeft := LocalPixel2MapPixelFloat(ARect.TopLeft);
  Result.BottomRight := LocalPixel2MapPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverterBase.LocalRectFloat2MapRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := LocalPixelFloat2MapPixelFloat(ARect.TopLeft);
  Result.BottomRight := LocalPixelFloat2MapPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverterBase.LonLat2LocalPixel(
  const APoint: TDoublePoint;
  ARounding: TPointRounding
): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := LonLat2LocalPixelFloat(APoint);
  Result := PointFromDoublePoint(VResultPoint, ARounding);
end;

function TLocalCoordConverterBase.LonLat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result :=
    MapPixelFloat2LocalPixelFloat(
      FGeoConverter.LonLat2PixelPosFloat(APoint, FZoom)
    );
end;

function TLocalCoordConverterBase.LonLatRect2LocalRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result :=
    MapRectFloat2LocalRectFloat(
      FGeoConverter.LonLatRect2PixelRectFloat(ARect, FZoom)
    );
end;

function TLocalCoordConverterBase.MapRect2LocalRect(
  const ARect: TRect;
  ARounding: TRectRounding
): TRect;
var
  VTopLeftRound: TPointRounding;
  VBottomRightRound: TPointRounding;
begin
  VTopLeftRound := prToTopLeft;
  VBottomRightRound := prToTopLeft;
  case ARounding of
    rrClosest: begin
      VTopLeftRound := prClosest;
      VBottomRightRound := prClosest;
    end;
    rrOutside: begin
      VTopLeftRound := prToTopLeft;
      VBottomRightRound := prToBottomRight;
    end;
    rrInside: begin
      VTopLeftRound := prToBottomRight;
      VBottomRightRound := prToTopLeft;
    end;
    rrToTopLeft: begin
      VTopLeftRound := prToTopLeft;
      VBottomRightRound := prToTopLeft;
    end;
  end;
  Result.TopLeft := MapPixel2LocalPixel(ARect.TopLeft, VTopLeftRound);
  Result.BottomRight := MapPixel2LocalPixel(ARect.BottomRight, VBottomRightRound);
end;

function TLocalCoordConverterBase.MapRect2LocalRectFloat(
  const ARect: TRect): TDoubleRect;
begin
  Result.TopLeft := MapPixel2LocalPixelFloat(ARect.TopLeft);
  Result.BottomRight := MapPixel2LocalPixelFloat(ARect.BottomRight);
end;

function TLocalCoordConverterBase.MapRectFloat2LocalRectFloat(
  const ARect: TDoubleRect): TDoubleRect;
begin
  Result.TopLeft := MapPixelFloat2LocalPixelFloat(ARect.TopLeft);
  Result.BottomRight := MapPixelFloat2LocalPixelFloat(ARect.BottomRight);
end;

{ TLocalCoordConverter }

constructor TLocalCoordConverter.Create(
  const AHash: THashValue;
  const ALocalRect: TRect;
  const ARectInMapPixel: TRect;
  const ARectInMapPixelFloat: TDoubleRect;
  const AMapPixelCenter: TDoublePoint;
  const AProjection: IProjectionInfo;
  const AMapScale: Double;
  const AMapPixelAtLocalZero: TDoublePoint
);
begin
  inherited Create(
    AHash,
    ALocalRect,
    ARectInMapPixel,
    ARectInMapPixelFloat,
    AMapPixelCenter,
    AProjection
  );
  FMapPixelAtLocalZero := AMapPixelAtLocalZero;
  FMapScale := AMapScale;
end;

function TLocalCoordConverter.GetIsSameConverter(
  const AConverter: ILocalCoordConverter
): Boolean;
begin
  Result := inherited GetIsSameConverter(AConverter);
  if Result then begin
    Result := False;
    if Abs(FMapScale - AConverter.GetScale) < 0.001 then begin
      Result :=
        DoublePointsEqual(
          FMapPixelAtLocalZero,
          AConverter.LocalPixel2MapPixelFloat(Point(0, 0))
        );
    end;
  end;
end;

function TLocalCoordConverter.GetScale: Double;
begin
  Result := FMapScale;
end;

function TLocalCoordConverter.LocalPixel2MapPixel(
  const APoint: TPoint;
  ARounding: TPointRounding
): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := LocalPixel2MapPixelFloat(APoint);
  Result := PointFromDoublePoint(VResultPoint, ARounding);
end;

function TLocalCoordConverter.LocalPixel2MapPixelFloat(
  const APoint: TPoint): TDoublePoint;
var
  VSourcePoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  Result := LocalPixelFloat2MapPixelFloat(VSourcePoint);
end;

function TLocalCoordConverter.LocalPixelFloat2MapPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X / FMapScale + FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y / FMapScale + FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverter.MapPixel2LocalPixel(
  const APoint: TPoint;
  ARounding: TPointRounding
): TPoint;
var
  VResultPoint: TDoublePoint;
begin
  VResultPoint := MapPixel2LocalPixelFloat(APoint);
  Result := PointFromDoublePoint(VResultPoint, ARounding);
end;

function TLocalCoordConverter.MapPixel2LocalPixelFloat(
  const APoint: TPoint): TDoublePoint;
var
  VSourcePoint: TDoublePoint;
begin
  VSourcePoint.X := APoint.X;
  VSourcePoint.Y := APoint.Y;
  Result := MapPixelFloat2LocalPixelFloat(VSourcePoint);
end;

function TLocalCoordConverter.MapPixelFloat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := (APoint.X - FMapPixelAtLocalZero.X) * FMapScale;
  Result.Y := (APoint.Y - FMapPixelAtLocalZero.Y) * FMapScale;
end;

{ TLocalCoordConverterNoScale }

constructor TLocalCoordConverterNoScaleIntDelta.Create(
  const AHash: THashValue;
  const ALocalRect: TRect;
  const ARectInMapPixel: TRect;
  const ARectInMapPixelFloat: TDoubleRect;
  const AMapPixelCenter: TDoublePoint;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TPoint
);
begin
  inherited Create(
    AHash,
    ALocalRect,
    ARectInMapPixel,
    ARectInMapPixelFloat,
    AMapPixelCenter,
    AProjection
  );
  FMapPixelAtLocalZero := AMapPixelAtLocalZero;
end;

function TLocalCoordConverterNoScaleIntDelta.GetIsSameConverter(
  const AConverter: ILocalCoordConverter
): Boolean;
var
  VMapPixelAtLocalZero: TPoint;
begin
  Result := inherited GetIsSameConverter(AConverter);
  if Result then begin
    Result := False;
    if Abs(AConverter.GetScale - 1) < 0.001 then begin
      VMapPixelAtLocalZero := AConverter.LocalPixel2MapPixel(Point(0, 0), prClosest);
      Result :=
        (FMapPixelAtLocalZero.X = VMapPixelAtLocalZero.X) and
        (FMapPixelAtLocalZero.Y = VMapPixelAtLocalZero.Y);
    end;
  end;
end;

function TLocalCoordConverterNoScaleIntDelta.GetScale: Double;
begin
  Result := 1;
end;

function TLocalCoordConverterNoScaleIntDelta.LocalPixel2MapPixel(
  const APoint: TPoint;
  ARounding: TPointRounding
): TPoint;
begin
  Result.X := APoint.X + FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y + FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScaleIntDelta.LocalPixel2MapPixelFloat(
  const APoint: TPoint): TDoublePoint;
begin
  Result := DoublePoint(APoint);
  Result.X := Result.X + FMapPixelAtLocalZero.X;
  Result.Y := Result.Y + FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScaleIntDelta.LocalPixelFloat2MapPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X + FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y + FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScaleIntDelta.MapPixel2LocalPixel(
  const APoint: TPoint;
  ARounding: TPointRounding
): TPoint;
begin
  Result.X := APoint.X - FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y - FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScaleIntDelta.MapPixel2LocalPixelFloat(
  const APoint: TPoint): TDoublePoint;
begin
  Result := DoublePoint(APoint);
  Result.X := Result.X - FMapPixelAtLocalZero.X;
  Result.Y := Result.Y - FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScaleIntDelta.MapPixelFloat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X - FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y - FMapPixelAtLocalZero.Y;
end;

{ TLocalCoordConverterNoScale }

constructor TLocalCoordConverterNoScale.Create(
  const AHash: THashValue;
  const ALocalRect: TRect;
  const ARectInMapPixel: TRect;
  const ARectInMapPixelFloat: TDoubleRect;
  const AMapPixelCenter: TDoublePoint;
  const AProjection: IProjectionInfo;
  const AMapPixelAtLocalZero: TDoublePoint
);
begin
  inherited Create(
    AHash,
    ALocalRect,
    ARectInMapPixel,
    ARectInMapPixelFloat,
    AMapPixelCenter,
    AProjection
  );
  FMapPixelAtLocalZero := AMapPixelAtLocalZero;
end;

function TLocalCoordConverterNoScale.GetIsSameConverter(
  const AConverter: ILocalCoordConverter): Boolean;
begin
  Result := inherited GetIsSameConverter(AConverter);
  if Result then begin
    Result := False;
    if Abs(AConverter.GetScale - 1) < 0.001 then begin
      Result :=
        DoublePointsEqual(
          FMapPixelAtLocalZero,
          AConverter.LocalPixel2MapPixelFloat(Point(0, 0))
        );
    end;
  end;
end;

function TLocalCoordConverterNoScale.GetScale: Double;
begin
  Result := 1;
end;

function TLocalCoordConverterNoScale.LocalPixel2MapPixel(
  const APoint: TPoint;
  ARounding: TPointRounding
): TPoint;
begin
  Result :=
    PointFromDoublePoint(
      DoublePoint(APoint.X + FMapPixelAtLocalZero.X, APoint.Y + FMapPixelAtLocalZero.Y),
      ARounding
    );
end;

function TLocalCoordConverterNoScale.LocalPixel2MapPixelFloat(
  const APoint: TPoint): TDoublePoint;
begin
  Result.X := APoint.X + FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y + FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScale.LocalPixelFloat2MapPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X + FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y + FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScale.MapPixel2LocalPixel(
  const APoint: TPoint;
  ARounding: TPointRounding
): TPoint;
begin
  Result :=
    PointFromDoublePoint(
      DoublePoint(APoint.X - FMapPixelAtLocalZero.X, APoint.Y - FMapPixelAtLocalZero.Y),
      ARounding
    );
end;

function TLocalCoordConverterNoScale.MapPixel2LocalPixelFloat(
  const APoint: TPoint): TDoublePoint;
begin
  Result.X := APoint.X - FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y - FMapPixelAtLocalZero.Y;
end;

function TLocalCoordConverterNoScale.MapPixelFloat2LocalPixelFloat(
  const APoint: TDoublePoint): TDoublePoint;
begin
  Result.X := APoint.X - FMapPixelAtLocalZero.X;
  Result.Y := APoint.Y - FMapPixelAtLocalZero.Y;
end;

end.
