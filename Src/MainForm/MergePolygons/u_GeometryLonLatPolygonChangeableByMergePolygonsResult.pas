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

unit u_GeometryLonLatPolygonChangeableByMergePolygonsResult;

interface

uses
  SysUtils,
  i_Listener,
  i_MergePolygonsResult,
  i_PolygonLayerConfig,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  u_ChangeableBase;

type
  TGeometryLonLatPolygonChangeableByMergePolygonsResult = class(
    TChangeableWithSimpleLockBase,
    IGeometryLonLatPolygonChangeable
  )
  private
    FConfig: IPolygonLayerConfig;
    FMergePolygonsResult: IMergePolygonsResult;

    FSourceListener: IListener;

    FResult: IGeometryLonLatPolygon;
    procedure OnSourceChange;
  private
    function GetStatic: IGeometryLonLatPolygon;
  public
    constructor Create(
      const AConfig: IPolygonLayerConfig;
      const AMergePolygonsResult: IMergePolygonsResult
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TGeometryLonLatPolygonChangeableByMergePolygonsResult }

constructor TGeometryLonLatPolygonChangeableByMergePolygonsResult.Create(
  const AConfig: IPolygonLayerConfig;
  const AMergePolygonsResult: IMergePolygonsResult
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(AMergePolygonsResult));
  inherited Create;

  FConfig := AConfig;
  FMergePolygonsResult := AMergePolygonsResult;
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);

  FConfig.ChangeNotifier.Add(FSourceListener);
  FMergePolygonsResult.ChangeNotifier.Add(FSourceListener);
end;

destructor TGeometryLonLatPolygonChangeableByMergePolygonsResult.Destroy;
begin
  if Assigned(FConfig) and Assigned(FSourceListener) then begin
    FConfig.ChangeNotifier.Remove(FSourceListener);
  end;

  if Assigned(FMergePolygonsResult) and Assigned(FSourceListener) then begin
    FMergePolygonsResult.ChangeNotifier.Remove(FSourceListener);
  end;

  inherited;
end;

function TGeometryLonLatPolygonChangeableByMergePolygonsResult.GetStatic: IGeometryLonLatPolygon;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TGeometryLonLatPolygonChangeableByMergePolygonsResult.OnSourceChange;
var
  VResult: IGeometryLonLatPolygon;
  VChanged: Boolean;
begin
  CS.BeginWrite;
  try
    VResult := FMergePolygonsResult.Polygon;
    if Assigned(VResult) then begin
      VChanged := not VResult.IsSameGeometry(FResult);
    end else begin
      VChanged := Assigned(FResult);
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

end.
