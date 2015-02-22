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
  i_GeometryLonLatChangeable,
  u_ChangeableBase;

type
  TGeometryLonLatLineChangeableByPathEdit = class(TChangeableBase, IGeometryLonLatLineChangeable)
  private
    FSource: IPathOnMapEdit;
    FSourceListener: IListener;

    FResult: IGeometryLonLatLine;
    FResultCS: IReadWriteSync;
    procedure OnSourceChange;
  private
    function GetStatic: IGeometryLonLatLine;
  public
    constructor Create(
      const ASource: IPathOnMapEdit
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent,
  u_Synchronizer;

{ TGeometryLonLatLineChangeableByPathEdit }

constructor TGeometryLonLatLineChangeableByPathEdit.Create(
  const ASource: IPathOnMapEdit
);
begin
  Assert(Assigned(ASource));
  inherited Create(GSync.SyncVariable.Make(ClassName + 'Notifier'));

  FSource := ASource;
  FSourceListener := TNotifyNoMmgEventListener.Create(Self.OnSourceChange);
  FResultCS := GSync.SyncVariable.Make(ClassName);

  FSource.ChangeNotifier.Add(FSourceListener);
end;

destructor TGeometryLonLatLineChangeableByPathEdit.Destroy;
begin
  if Assigned(FSource) and Assigned(FSourceListener) then begin
    FSource.ChangeNotifier.Remove(FSourceListener);
    FSourceListener := nil;
  end;

  inherited;
end;

function TGeometryLonLatLineChangeableByPathEdit.GetStatic: IGeometryLonLatLine;
begin
  FResultCS.BeginRead;
  try
    Result := FResult;
  finally
    FResultCS.EndRead;
  end;
end;

procedure TGeometryLonLatLineChangeableByPathEdit.OnSourceChange;
var
  VPath: ILonLatPathWithSelected;
  VResult: IGeometryLonLatLine;
  VChanged: Boolean;
begin
  FResultCS.BeginWrite;
  try
    VResult := nil;
    VPath := FSource.Path;
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
