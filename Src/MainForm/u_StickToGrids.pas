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

unit u_StickToGrids;

interface

uses
  t_GeoTypes,
  i_ProjectionInfo,
  i_MapLayerGridsConfig,
  i_StickToGrid,
  u_BaseInterfacedObject;

type
  TStickToGrids = class(TBaseInterfacedObject, IStickToGrid)
  private
    FConfig: IMapLayerGridsConfig;

    FTile: IStickToGrid;
    FDegres: IStickToGrid;
    FGenShtab: IStickToGrid;
  private
    function PointStick(
      const AProjection: IProjectionInfo;
      const ASourceLonLat: TDoublePoint
    ): TDoublePoint;
    function RectStick(
      const AProjection: IProjectionInfo;
      const ASourceRect: TDoubleRect
    ): TDoubleRect;
  public
    constructor Create(const AConfig: IMapLayerGridsConfig);
  end;

implementation

uses
  u_StickToGridTiles,
  u_StickToGridDegree,
  u_StickToGridGenShtab,
  u_GeoFunc;

{ TStickToGrids }

constructor TStickToGrids.Create(const AConfig: IMapLayerGridsConfig);
begin
  Assert(Assigned(AConfig));
  inherited Create;
  FConfig := AConfig;

  FTile := TStickToGridTiles.Create(FConfig.TileGrid);
  FDegres := TStickToGridDegree.Create(FConfig.DegreeGrid);
  FGenShtab := TStickToGridGenShtab.Create(FConfig.GenShtabGrid);
end;

function Dist(const APoint1, APoint2: TDoublePoint): Double; inline;
var
  VDelta: TDoublePoint;
begin
  VDelta.X := APoint1.X - APoint2.X;
  VDelta.Y := APoint1.Y - APoint2.Y;

  Result := VDelta.X * VDelta.X + VDelta.Y *  VDelta.Y;
end;

function TStickToGrids.PointStick(
  const AProjection: IProjectionInfo;
  const ASourceLonLat: TDoublePoint
): TDoublePoint;
var
  VSourceProjected: TDoublePoint;
  VResult: TDoublePoint;
  VResultProjected: TDoublePoint;
  VResultDist: Double;
  VBestExists: Boolean;
  VBestDist: Double;
  VBestResult: TDoublePoint;
  procedure UseStick(
    const AStick: IStickToGrid
  );
  begin
    VResult := AStick.PointStick(AProjection, ASourceLonLat);
    AProjection.ProjectionType.ValidateLonLatPos(VResult);
    VResultProjected := AProjection.LonLat2PixelPosFloat(VResult);
    VResultDist := Dist(VSourceProjected, VResultProjected);
    if VBestExists then begin
      if VBestDist > VResultDist then begin
        VBestDist := VResultDist;
        VBestResult := VResult;
      end;
    end else begin
      VBestExists := True;
      VBestDist := VResultDist;
      VBestResult := VResult;
    end;
  end;
begin
  VBestExists := False;
  VSourceProjected := AProjection.LonLat2PixelPosFloat(ASourceLonLat);
  if FConfig.TileGrid.Visible then begin
    UseStick(FTile);
  end;
  if FConfig.DegreeGrid.Visible then begin
    UseStick(FDegres);
  end;
  if FConfig.GenShtabGrid.Visible then begin
    UseStick(FGenShtab);
  end;
  if VBestExists then begin
    Result := VBestResult;
  end else begin
    Result := ASourceLonLat;
  end;
end;

function TStickToGrids.RectStick(
  const AProjection: IProjectionInfo;
  const ASourceRect: TDoubleRect
): TDoubleRect;
var
  VSourceProjected: TDoubleRect;
  VResult: TDoubleRect;
  VResultProjected: TDoubleRect;
  VResultDistTopLeft: Double;
  VResultDistBottomRight: Double;
  VBestExists: Boolean;
  VBestDistTopLeft: Double;
  VBestDistBottomRight: Double;
  VBestResult: TDoubleRect;
  procedure UseStick(
    const AStick: IStickToGrid
  );
  begin
    VResult := AStick.RectStick(AProjection, ASourceRect);
    AProjection.ProjectionType.ValidateLonLatRect(VResult);
    VResultProjected := AProjection.LonLatRect2PixelRectFloat(VResult);
    VResultDistTopLeft := Dist(VSourceProjected.TopLeft, VResultProjected.TopLeft);
    VResultDistBottomRight := Dist(VSourceProjected.BottomRight, VResultProjected.BottomRight);
    if VBestExists then begin
      if VBestDistTopLeft > VResultDistTopLeft then begin
        VBestDistTopLeft := VResultDistTopLeft;
        VBestResult.TopLeft := VResult.TopLeft;
      end;
      if VBestDistBottomRight > VResultDistBottomRight then begin
        VBestDistBottomRight := VResultDistBottomRight;
        VBestResult.BottomRight := VResult.BottomRight;
      end;
    end else begin
      VBestExists := True;
      VBestDistTopLeft := VResultDistTopLeft;
      VBestDistBottomRight := VResultDistBottomRight;
      VBestResult := VResult;
    end;
  end;
begin
  VBestExists := False;
  VSourceProjected := AProjection.LonLatRect2PixelRectFloat(ASourceRect);
  if FConfig.TileGrid.Visible then begin
    UseStick(FTile);
  end;
  if FConfig.DegreeGrid.Visible then begin
    UseStick(FDegres);
  end;
  if FConfig.GenShtabGrid.Visible then begin
    UseStick(FGenShtab);
  end;
  if VBestExists then begin
    Result := VBestResult;
  end else begin
    Result := ASourceRect;
  end;
end;

end.
