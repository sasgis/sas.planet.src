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

unit u_GeometryLonLatPolygonChangeableByPolygonEdit;

interface

uses
  SysUtils,
  i_Listener,
  i_LineOnMapEdit,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  u_ChangeableBase;

type
  TGeometryLonLatPolygonChangeableByPolygonEdit = class(TChangeableBase, IGeometryLonLatPolygonChangeable)
  private
    FSource: IPolygonOnMapEdit;
    FSourceListener: IListener;

    FResult: IGeometryLonLatPolygon;
    FResultCS: IReadWriteSync;
    procedure OnSourceChange;
  private
    function GetStatic: IGeometryLonLatPolygon;
  public
    constructor Create(
      const ASource: IPolygonOnMapEdit
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_Synchronizer;

{ TGeometryLonLatPolygonChangeableByPolygonEdit }

constructor TGeometryLonLatPolygonChangeableByPolygonEdit.Create(
  const ASource: IPolygonOnMapEdit
);
begin
  Assert(Assigned(ASource));
  inherited Create(GSync.SyncVariable.Make(ClassName + 'Notifier'));

  FSource := ASource;
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FResultCS := GSync.SyncVariable.Make(ClassName);

  FSource.ChangeNotifier.Add(FSourceListener);
end;

destructor TGeometryLonLatPolygonChangeableByPolygonEdit.Destroy;
begin
  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Remove(FSourceListener);
    FSourceListener := nil;
  end;

  inherited;
end;

function TGeometryLonLatPolygonChangeableByPolygonEdit.GetStatic: IGeometryLonLatPolygon;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
  end;
end;

procedure TGeometryLonLatPolygonChangeableByPolygonEdit.OnSourceChange;
var
  VPath: ILonLatPolygonWithSelected;
  VResult: IGeometryLonLatPolygon;
  VChanged: Boolean;
begin
  FResultCS.BeginWrite;
  try
    VResult := nil;
    VPath := FSource.Polygon;
    if Assigned(VPath) then begin
      VResult := VPath.Geometry;
    end;
    if Assigned(VResult) then begin
      VChanged := not VResult.IsSameGeometry(FResult);
    end else begin
      VChanged := Assigned(FResult);
    end;
    if VChanged then begin
      FResult := VResult;
    end;
  finally
    FResultCS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.

