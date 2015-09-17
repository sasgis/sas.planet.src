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

unit u_GeometryLonLatPolygonChangeableByPathEdit;

interface

uses
  SysUtils,
  i_Listener,
  i_LineOnMapEdit,
  i_SelectionPolylineLayerConfig,
  i_Projection,
  i_LocalCoordConverterChangeable,
  i_GeometryLonLatFactory,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  u_ChangeableBase;

type
  TGeometryLonLatPolygonChangeableByPathEdit = class(TChangeableWithSimpleLockBase, IGeometryLonLatPolygonChangeable)
  private
    FVectorGeometryLonLatFactory: IGeometryLonLatFactory;
    FConverter: ILocalCoordConverterChangeable;
    FSource: IPathOnMapEdit;
    FConfig: ISelectionPolylineShadowLayerConfig;
    FSourceListener: IListener;

    FLastLine: IGeometryLonLatLine;
    FLastProjection: IProjection;
    FLastRadius: Double;
    FResult: IGeometryLonLatPolygon;
    procedure OnSourceChange;
    function PolygonByLine(
      const ALine: IGeometryLonLatLine;
      const AProjection: IProjection;
      const ARadius: Double
    ): IGeometryLonLatPolygon;
  private
    function GetStatic: IGeometryLonLatPolygon;
  public
    constructor Create(
      const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
      const AConverter: ILocalCoordConverterChangeable;
      const ASource: IPathOnMapEdit;
      const AConfig: ISelectionPolylineShadowLayerConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  i_LocalCoordConverter,
  i_DoublePointFilter,
  u_EnumDoublePointLine2Poly,
  u_ListenerByEvent;

{ TGeometryLonLatLineChangeableByPathEdit }

constructor TGeometryLonLatPolygonChangeableByPathEdit.Create(
  const AVectorGeometryLonLatFactory: IGeometryLonLatFactory;
  const AConverter: ILocalCoordConverterChangeable;
  const ASource: IPathOnMapEdit;
  const AConfig: ISelectionPolylineShadowLayerConfig
);
begin
  Assert(Assigned(AVectorGeometryLonLatFactory));
  Assert(Assigned(AConverter));
  Assert(Assigned(ASource));
  Assert(Assigned(AConfig));
  inherited Create;

  FVectorGeometryLonLatFactory := AVectorGeometryLonLatFactory;
  FConverter := AConverter;
  FSource := ASource;
  FConfig := AConfig;
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FLastLine := nil;
  FLastRadius := 0;

  FConverter.ChangeNotifier.Add(FSourceListener);
  FSource.ChangeNotifier.Add(FSourceListener);
  FConfig.ChangeNotifier.Add(FSourceListener);
end;

destructor TGeometryLonLatPolygonChangeableByPathEdit.Destroy;
begin
  if Assigned(FConverter) and Assigned(FSourceListener) then begin
    FConverter.ChangeNotifier.Remove(FSourceListener);
  end;

  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Remove(FSourceListener);
  end;

  if Assigned(FConfig) and Assigned(FSourceListener) then begin
    FConfig.ChangeNotifier.Remove(FSourceListener);
  end;

  inherited;
end;

function TGeometryLonLatPolygonChangeableByPathEdit.GetStatic: IGeometryLonLatPolygon;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TGeometryLonLatPolygonChangeableByPathEdit.OnSourceChange;
var
  VPath: ILonLatPathWithSelected;
  VLine: IGeometryLonLatLine;
  VRadius: Double;
  VConverter: ILocalCoordConverter;
  VProjection: IProjection;
  VResult: IGeometryLonLatPolygon;
  VChanged: Boolean;
begin
  CS.BeginWrite;
  try
    VResult := nil;
    VPath := FSource.Path;
    if Assigned(VPath) then begin
      VLine := VPath.Geometry;
    end;
    VConverter := FConverter.GetStatic;
    VRadius := FConfig.Radius;
    VChanged := False;
    if Assigned(VLine) and Assigned(VConverter) then begin
      VProjection := VConverter.Projection;
      Assert(Assigned(VProjection));
      if VLine.IsSameGeometry(FLastLine) then begin
        if VProjection.IsSame(FLastProjection) then begin
          if Abs(FLastRadius - VRadius) > 1 then begin
            VResult := PolygonByLine(VLine, VProjection, VRadius);
            FLastRadius := VRadius;
            if Assigned(VResult) then begin
              VChanged := not VResult.IsSameGeometry(FResult);
            end else begin
              VChanged := Assigned(FResult);
            end;
          end;
        end else begin
          VResult := PolygonByLine(VLine, VProjection, VRadius);
          FLastProjection := VProjection;
          FLastRadius := VRadius;
          if Assigned(VResult) then begin
            VChanged := not VResult.IsSameGeometry(FResult);
          end else begin
            VChanged := Assigned(FResult);
          end;
        end;
      end else begin
        VResult := PolygonByLine(VLine, VProjection, VRadius);
        FLastLine := VLine;
        FLastProjection := VProjection;
        FLastRadius := VRadius;
        if Assigned(VResult) then begin
          VChanged := not VResult.IsSameGeometry(FResult);
        end else begin
          VChanged := Assigned(FResult);
        end;
      end;
    end else begin
      VChanged := Assigned(FResult);
      if VChanged then begin
        FLastLine := nil;
      end;
    end;
    if VChanged then begin
      FResult := VResult;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

function TGeometryLonLatPolygonChangeableByPathEdit.PolygonByLine(
  const ALine: IGeometryLonLatLine;
  const AProjection: IProjection;
  const ARadius: Double
): IGeometryLonLatPolygon;
var
  VFilter: ILonLatPointFilter;
begin
  Result := nil;
  if Assigned(ALine) then begin
    VFilter :=
      TLonLatPointFilterLine2Poly.Create(
        ARadius,
        AProjection
      );
    Result :=
      FVectorGeometryLonLatFactory.CreateLonLatPolygonByLonLatPathAndFilter(
        ALine,
        VFilter
      );
  end;
end;

end.
