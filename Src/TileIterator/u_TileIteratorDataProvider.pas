{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2016, SAS.Planet development team.                      *}
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

unit u_TileIteratorDataProvider;

interface

uses
  Types,
  SysUtils,
  t_GeoTypes,
  i_Projection,
  i_GeometryProjected,
  i_TileIteratorDataProvider,
  u_BaseInterfacedObject;

type
  TTileIteratorDataProvider = class(TBaseInterfacedObject, ITileIteratorDataProvider)
  private
    FProjection: IProjection;
    FPolygon: IGeometryProjectedPolygon;
    FPartsCount: Integer;
    FTilesTotal: Int64;
    FStartPoints: TArrayOfPoint;
    FTilesCount: TIntegerDynArray;
    FPrepared: Boolean;
    FLock: IReadWriteSync;
    procedure _DoPrepareCount;
    procedure _DoPrepareSplit;
    procedure _CheckIndex(const AIndex: Integer);
  private
    { ITileIteratorDataProvider }
    function GetProjection: IProjection;
    function GetPolygon: IGeometryProjectedPolygon;
    function GetTilesTotal: Int64;
    function GetTilesToProcess(const APartIndex: Integer): Int64;
    function GetStartPoint(const APartIndex: Integer): TPoint;
    function GetPartsCount: Integer;
  public
    constructor Create(
      const AProjection: IProjection;
      const APolygon: IGeometryProjectedPolygon;
      const APartsCount: Integer
    );
  end;

implementation

uses
  Math,
  u_Synchronizer,
  u_GeoFunc,
  u_GeometryFunc;

{ TTileIteratorDataProvider }

constructor TTileIteratorDataProvider.Create(
  const AProjection: IProjection;
  const APolygon: IGeometryProjectedPolygon;
  const APartsCount: Integer
);
begin
  Assert(APartsCount >= 1);
  inherited Create;
  FProjection := AProjection;
  FPolygon := APolygon;
  FPartsCount := APartsCount;
  FPrepared := False;
  FTilesTotal := -1;
  FLock := GSync.SyncStd.Make(Self.ClassName);
end;

function TTileIteratorDataProvider.GetPolygon: IGeometryProjectedPolygon;
begin
  Result := FPolygon;
end;

function TTileIteratorDataProvider.GetProjection: IProjection;
begin
  Result := FProjection;
end;

function TTileIteratorDataProvider.GetTilesTotal: Int64;
begin
  FLock.BeginRead;
  try
    Result := FTilesTotal;
  finally
    FLock.EndRead;
  end;

  if Result = -1 then begin
    FLock.BeginWrite;
    try
      _DoPrepareCount;
      Result := FTilesTotal;
    finally
      FLock.EndWrite;
    end;
  end;
end;

function TTileIteratorDataProvider.GetStartPoint(
  const APartIndex: Integer
): TPoint;
begin
  if not FPrepared then begin
    _DoPrepareSplit;
  end;

  FLock.BeginRead;
  try
    _CheckIndex(APartIndex);
    Result := FStartPoints[APartIndex];
  finally
    FLock.EndRead;
  end;
end;

function TTileIteratorDataProvider.GetTilesToProcess(
  const APartIndex: Integer
): Int64;
begin
  if not FPrepared then begin
    _DoPrepareSplit;
  end;

  FLock.BeginRead;
  try
    _CheckIndex(APartIndex);
    Result := FTilesCount[APartIndex];
  finally
    FLock.EndRead;
  end;
end;

function TTileIteratorDataProvider.GetPartsCount: Integer;
begin
  if not FPrepared then begin
    _DoPrepareSplit;
  end;

  FLock.BeginRead;
  try
    Result := FPartsCount;
  finally
    FLock.EndRead;
  end;
end;

procedure TTileIteratorDataProvider._DoPrepareCount;
begin
  if FTilesTotal = -1 then begin
    FTilesTotal := CalcTileCountInProjectedPolygon(FProjection, FPolygon);
  end;
end;

procedure TTileIteratorDataProvider._DoPrepareSplit;
var
  VTilesRect: TRect;
begin
  FLock.BeginWrite;
  try
    if not FPrepared then begin
      _DoPrepareCount;
      if FPartsCount = 1 then begin
        SetLength(FStartPoints, 1);
        SetLength(FTilesCount, 1);
        VTilesRect :=
          RectFromDoubleRect(
            FProjection.PixelRectFloat2TileRectFloat(FPolygon.Bounds),
            rrOutside
          );
        FStartPoints[0] := VTilesRect.TopLeft;
        FTilesCount[0] := FTilesTotal;
      end else begin
        SplitProjectedPolygon(
          FProjection,
          FPolygon,
          FPartsCount,
          FTilesTotal,
          FStartPoints,
          FTilesCount
        );
        FPartsCount := Length(FStartPoints);
      end;
      FPrepared := True;
    end;
  finally
    FLock.EndWrite;
  end;
end;

procedure TTileIteratorDataProvider._CheckIndex(const AIndex: Integer);
begin
  if AIndex >= FPartsCount then begin
    raise Exception.CreateFmt(
      'Part index "%d" is out of range [0..%d]',
      [AIndex, FPartsCount - 1]
    );
  end;
end;

end.
