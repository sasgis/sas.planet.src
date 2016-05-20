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

unit u_GeometryLonLatLineChangeableByPathEdit;

interface

uses
  SysUtils,
  i_Listener,
  i_LineOnMapEdit,
  i_GeometryLonLat,
  i_GeometryLonLatFactory,
  i_GeometryLonLatChangeable,
  u_BaseInterfacedObject,
  u_GeometryLonLatChangeableInternal;

type
  TGeometryLonLatChangeableByPathEdit = class(TBaseInterfacedObject)
  private
    FFactory: IGeometryLonLatFactory;
    FSource: IPathOnMapEdit;
    FSourceListener: IListener;

    FLineChangeable: IGeometryLonLatLineChangeableInternal;
    FFirstPointsChangeable: IGeometryLonLatMultiPointChangeableInternal;
    FActivePointsChangeable: IGeometryLonLatMultiPointChangeableInternal;
    FOtherPointsChangeable: IGeometryLonLatMultiPointChangeableInternal;
    procedure OnSourceChange;
  public
    constructor Create(
      const AFactory: IGeometryLonLatFactory;
      const ASource: IPathOnMapEdit
    );
    destructor Destroy; override;

    function GetLineChangeable: IGeometryLonLatLineChangeable;
    property LineChangeable: IGeometryLonLatLineChangeable read GetLineChangeable;

    function GetFirstPointsChangeable: IGeometryLonLatMultiPointChangeable;
    property FirstPointsChangeable: IGeometryLonLatMultiPointChangeable read GetFirstPointsChangeable;

    function GetActivePointsChangeable: IGeometryLonLatMultiPointChangeable;
    property ActivePointsChangeable: IGeometryLonLatMultiPointChangeable read GetActivePointsChangeable;

    function GetOtherPointsChangeable: IGeometryLonLatMultiPointChangeable;
    property OtherPointsChangeable: IGeometryLonLatMultiPointChangeable read GetOtherPointsChangeable;
  end;

implementation

uses
  Math,
  t_GeoTypes,
  i_DoublePointsAggregator,
  u_ListenerByEvent,
  u_DoublePointsAggregator,
  u_GeoFunc;

{ TGeometryLonLatMultiPointChangeableByPathEdit }

constructor TGeometryLonLatChangeableByPathEdit.Create(
  const AFactory: IGeometryLonLatFactory;
  const ASource: IPathOnMapEdit
);
begin
  Assert(Assigned(AFactory));
  Assert(Assigned(ASource));
  inherited Create;

  FFactory := AFactory;
  FSource := ASource;

  FLineChangeable := TGeometryLonLatLineChangeableInternal.Create;
  FFirstPointsChangeable := TGeometryLonLatMultiPointChangeableInternal.Create;
  FActivePointsChangeable := TGeometryLonLatMultiPointChangeableInternal.Create;
  FOtherPointsChangeable := TGeometryLonLatMultiPointChangeableInternal.Create;
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);

  FSource.ChangeNotifier.Add(FSourceListener);
end;

destructor TGeometryLonLatChangeableByPathEdit.Destroy;
begin
  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Remove(FSourceListener);
    FSourceListener := nil;
  end;

  inherited;
end;

function TGeometryLonLatChangeableByPathEdit.GetActivePointsChangeable: IGeometryLonLatMultiPointChangeable;
begin
  Result := FActivePointsChangeable;
end;

function TGeometryLonLatChangeableByPathEdit.GetFirstPointsChangeable: IGeometryLonLatMultiPointChangeable;
begin
  Result := FFirstPointsChangeable;
end;

function TGeometryLonLatChangeableByPathEdit.GetLineChangeable: IGeometryLonLatLineChangeable;
begin
  Result := FLineChangeable;
end;

function TGeometryLonLatChangeableByPathEdit.GetOtherPointsChangeable: IGeometryLonLatMultiPointChangeable;
begin
  Result := FOtherPointsChangeable;
end;

procedure TGeometryLonLatChangeableByPathEdit.OnSourceChange;
var
  VPath: ILonLatPathWithSelected;
  VLine: IGeometryLonLatLine;
  VFirstPoints: IGeometryLonLatMultiPoint;
  VActivePoints: IGeometryLonLatMultiPoint;
  VOtherPoints: IGeometryLonLatMultiPoint;
  VFirst: IDoublePointsAggregator;
  VOther: IDoublePointsAggregator;
  VPoint: TDoublePoint;
  VSelectedPointIndex: Integer;
  i: Integer;
  VPrevIsEmpty: Boolean;
  VPoints: PDoublePointArray;
begin
  VPath := FSource.Path;
  if Assigned(VPath) then begin
    VLine := VPath.Geometry;
    VSelectedPointIndex := VPath.GetSelectedPointIndex;
    VPoints := VPath.Points;
    VPoint := VPoints[VSelectedPointIndex];
    if PointIsEmpty(VPoint) then begin
      VActivePoints := nil;
    end else begin
      VActivePoints := FFactory.CreateLonLatMultiPoint(Addr(VPoint), 1);
    end;
    VFirst := TDoublePointsAggregator.Create(1);
    VOther := TDoublePointsAggregator.Create(VPath.Count);
    VPrevIsEmpty := True;
    for i := 0 to VPath.Count - 1 do begin
      VPoint := VPoints[i];
      if PointIsEmpty(VPoint) then begin
        VPrevIsEmpty := True;
      end else begin
        if i <> VSelectedPointIndex then begin
          if VPrevIsEmpty then begin
            VFirst.Add(VPoint);
          end else begin
            VOther.Add(VPoint);
          end;
        end;
        VPrevIsEmpty := False;
      end;
    end;
    if VFirst.Count > 0 then begin
      VFirstPoints := FFactory.CreateLonLatMultiPoint(VFirst.Points, VFirst.Count);
    end else begin
      VFirstPoints := nil;
    end;
    if VOther.Count > 0 then begin
      VOtherPoints := FFactory.CreateLonLatMultiPoint(VOther.Points, VOther.Count);
    end else begin
      VOtherPoints := nil;
    end;
  end else begin
    VLine := nil;
    VFirstPoints := nil;
    VActivePoints := nil;
    VOtherPoints := nil;
  end;
  FLineChangeable.SetLine(VLine);
  FFirstPointsChangeable.SetPoints(VFirstPoints);
  FActivePointsChangeable.SetPoints(VActivePoints);
  FOtherPointsChangeable.SetPoints(VOtherPoints);
end;

end.
