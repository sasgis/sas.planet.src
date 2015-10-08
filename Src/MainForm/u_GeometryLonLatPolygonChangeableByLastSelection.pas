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

unit u_GeometryLonLatPolygonChangeableByLastSelection;

interface

uses
  SysUtils,
  i_Listener,
  i_LastSelectionInfo,
  i_LastSelectionLayerConfig,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  u_ChangeableBase;

type
  TGeometryLonLatPolygonChangeableByLastSelection = class(TChangeableWithSimpleLockBase, IGeometryLonLatPolygonChangeable)
  private
    FConfig: ILastSelectionLayerConfig;
    FLastSelectionInfo: ILastSelectionInfo;

    FSourceListener: IListener;

    FResult: IGeometryLonLatPolygon;
    procedure OnSourceChange;
  private
    function GetStatic: IGeometryLonLatPolygon;
  public
    constructor Create(
      const AConfig: ILastSelectionLayerConfig;
      const ALastSelectionInfo: ILastSelectionInfo
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

{ TGeometryLonLatPolygonChangeableByPolygonEdit }

constructor TGeometryLonLatPolygonChangeableByLastSelection.Create(
  const AConfig: ILastSelectionLayerConfig;
  const ALastSelectionInfo: ILastSelectionInfo
);
begin
  Assert(Assigned(AConfig));
  Assert(Assigned(ALastSelectionInfo));
  inherited Create;

  FConfig := AConfig;
  FLastSelectionInfo := ALastSelectionInfo;
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);

  FConfig.ChangeNotifier.Add(FSourceListener);
  FLastSelectionInfo.ChangeNotifier.Add(FSourceListener);
end;

destructor TGeometryLonLatPolygonChangeableByLastSelection.Destroy;
begin
  if Assigned(FConfig) and Assigned(FSourceListener) then begin
    FConfig.ChangeNotifier.Remove(FSourceListener);
  end;

  if Assigned(FLastSelectionInfo) and Assigned(FSourceListener) then begin
    FLastSelectionInfo.ChangeNotifier.Remove(FSourceListener);
  end;

  inherited;
end;

function TGeometryLonLatPolygonChangeableByLastSelection.GetStatic: IGeometryLonLatPolygon;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TGeometryLonLatPolygonChangeableByLastSelection.OnSourceChange;
var
  VResult: IGeometryLonLatPolygon;
  VChanged: Boolean;
begin
  CS.BeginWrite;
  try
    VResult := nil;
    if FConfig.Visible then begin
      VResult := FLastSelectionInfo.Polygon;
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
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.
