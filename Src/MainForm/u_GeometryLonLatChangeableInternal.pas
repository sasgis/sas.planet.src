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

unit u_GeometryLonLatChangeableInternal;

interface

uses
  SysUtils,
  i_GeometryLonLat,
  i_GeometryLonLatChangeable,
  u_ChangeableBase;

type
  IGeometryLonLatMultiPointChangeableInternal = interface(IGeometryLonLatMultiPointChangeable)
    procedure SetPoints(const APoints: IGeometryLonLatMultiPoint);
  end;

  IGeometryLonLatLineChangeableInternal = interface(IGeometryLonLatLineChangeable)
    procedure SetLine(const ALine: IGeometryLonLatLine);
  end;

  IGeometryLonLatPolygonChangeableInternal = interface(IGeometryLonLatPolygonChangeable)
    procedure SetPolygon(const APolygon: IGeometryLonLatPolygon);
  end;

  TGeometryLonLatMultiPointChangeableInternal = class(TChangeableWithSimpleLockBase, IGeometryLonLatMultiPointChangeable, IGeometryLonLatMultiPointChangeableInternal)
  private
    FResult: IGeometryLonLatMultiPoint;
  private
    procedure SetPoints(const APoints: IGeometryLonLatMultiPoint);
    function GetStatic: IGeometryLonLatMultiPoint;
  end;

  TGeometryLonLatLineChangeableInternal = class(TChangeableWithSimpleLockBase, IGeometryLonLatLineChangeable, IGeometryLonLatLineChangeableInternal)
  private
    FResult: IGeometryLonLatLine;
  private
    procedure SetLine(const ALine: IGeometryLonLatLine);
    function GetStatic: IGeometryLonLatLine;
  end;

  TGeometryLonLatPolygonChangeableInternal = class(TChangeableWithSimpleLockBase, IGeometryLonLatPolygonChangeable, IGeometryLonLatPolygonChangeableInternal)
  private
    FResult: IGeometryLonLatPolygon;
  private
    procedure SetPolygon(const APolygon: IGeometryLonLatPolygon);
    function GetStatic: IGeometryLonLatPolygon;
  end;

implementation

{ TGeometryLonLatMultiPointChangeableInternal }

function TGeometryLonLatMultiPointChangeableInternal.GetStatic: IGeometryLonLatMultiPoint;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;


procedure TGeometryLonLatMultiPointChangeableInternal.SetPoints(
  const APoints: IGeometryLonLatMultiPoint
);
var
  VChanged: Boolean;
begin
  CS.BeginWrite;
  try
    if Assigned(APoints) then begin
      VChanged := not APoints.IsSame(FResult);
    end else begin
      VChanged := Assigned(FResult);
    end;
    if VChanged then begin
      FResult := APoints;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

{ TGeometryLonLatLineChangeableInternal }

function TGeometryLonLatLineChangeableInternal.GetStatic: IGeometryLonLatLine;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TGeometryLonLatLineChangeableInternal.SetLine(
  const ALine: IGeometryLonLatLine
);
var
  VChanged: Boolean;
begin
  CS.BeginWrite;
  try
    if Assigned(ALine) then begin
      VChanged := not ALine.IsSameGeometry(FResult);
    end else begin
      VChanged := Assigned(FResult);
    end;
    if VChanged then begin
      FResult := ALine;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

{ TGeometryLonLatPolygonChangeableInternal }

function TGeometryLonLatPolygonChangeableInternal.GetStatic: IGeometryLonLatPolygon;
begin
  CS.BeginRead;
  try
    Result := FResult;
  finally
    CS.EndRead;
  end;
end;

procedure TGeometryLonLatPolygonChangeableInternal.SetPolygon(
  const APolygon: IGeometryLonLatPolygon
);
var
  VChanged: Boolean;
begin
  CS.BeginWrite;
  try
    if Assigned(APolygon) then begin
      VChanged := not APolygon.IsSameGeometry(FResult);
    end else begin
      VChanged := Assigned(FResult);
    end;
    if VChanged then begin
      FResult := APolygon;
    end;
  finally
    CS.EndWrite;
  end;
  if VChanged then begin
    DoChangeNotify;
  end;
end;

end.
