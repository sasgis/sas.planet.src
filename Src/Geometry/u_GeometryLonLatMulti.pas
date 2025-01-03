{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_GeometryLonLatMulti;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_LonLatRect,
  i_InterfaceListStatic,
  i_GeometryLonLat,
  u_BaseInterfacedObject;

type
  TGeometryLonLatMultiBase = class(TBaseInterfacedObject)
  private
    FList: IInterfaceListStatic;
    FHash: THashValue;
    FBounds: ILonLatRect;
  private
    function GetCount: Integer;
    function GetHash: THashValue;
    function GetBounds: ILonLatRect;
  public
    constructor Create(
      const ABounds: ILonLatRect;
      const AHash: THashValue;
      const AList: IInterfaceListStatic
    );
  end;

  TGeometryLonLatMultiLine = class(TGeometryLonLatMultiBase, IGeometryLonLat, IGeometryLonLatLine, IGeometryLonLatMultiLine)
  private
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APath: IGeometryLonLatMultiLine): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
  end;

  TGeometryLonLatMultiPolygon = class(TGeometryLonLatMultiBase, IGeometryLonLat, IGeometryLonLatPolygon, IGeometryLonLatMultiPolygon)
  private
    function IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
    function IsSame(const APolygon: IGeometryLonLatMultiPolygon): Boolean;
    function GetGoToPoint: TDoublePoint;
    function GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
  end;

implementation

uses
  SysUtils;

{ TLonLatLineSet }

constructor TGeometryLonLatMultiBase.Create(
  const ABounds: ILonLatRect;
  const AHash: THashValue;
  const AList: IInterfaceListStatic
);
begin
  Assert(AList <> nil);
  Assert(ABounds <> nil);
  Assert(AList.Count > 1);
  inherited Create;
  FBounds := ABounds;
  FHash := AHash;
  FList := AList;
end;

function TGeometryLonLatMultiBase.GetBounds: ILonLatRect;
begin
  Result := FBounds;
end;

function TGeometryLonLatMultiBase.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TGeometryLonLatMultiBase.GetHash: THashValue;
begin
  Result := FHash;
end;

{ TLonLatPath }

function TGeometryLonLatMultiLine.GetGoToPoint: TDoublePoint;
begin
  Result := GetItem(0).GetGoToPoint;
end;

function TGeometryLonLatMultiLine.GetItem(AIndex: Integer): IGeometryLonLatSingleLine;
begin
  if not Supports(FList[AIndex], IGeometryLonLatSingleLine, Result) then begin
    Result := nil;
  end;
end;

function TGeometryLonLatMultiLine.IsSame(const APath: IGeometryLonLatMultiLine): Boolean;
var
  i: Integer;
  VLine: IGeometryLonLatSingleLine;
begin
  if APath = IGeometryLonLatMultiLine(Self) then begin
    Result := True;
    Exit;
  end;

  if FList.Count <> APath.Count then begin
    Result := False;
    Exit;
  end;

  if (FHash <> 0) and (APath.Hash <> 0) then begin
    Result := FHash = APath.Hash;
  end else begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine = nil then begin
        Result := False;
        Exit;
      end;
      if not VLine.IsSame(APath.Item[i]) then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function TGeometryLonLatMultiLine.IsSameGeometry(const AGeometry: IGeometryLonLat): Boolean;
var
  VLine: IGeometryLonLatMultiLine;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatMultiLine, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

{ TLonLatPolygon }

function TGeometryLonLatMultiPolygon.GetGoToPoint: TDoublePoint;
begin
  Result := FBounds.CalcRectCenter;
end;

function TGeometryLonLatMultiPolygon.GetItem(AIndex: Integer): IGeometryLonLatSinglePolygon;
begin
  if not Supports(FList[AIndex], IGeometryLonLatSinglePolygon, Result) then begin
    Result := nil;
  end;
end;

function TGeometryLonLatMultiPolygon.IsSame(const APolygon: IGeometryLonLatMultiPolygon): Boolean;
var
  i: Integer;
  VLine: IGeometryLonLatSinglePolygon;
begin
  if APolygon = IGeometryLonLatMultiPolygon(Self) then begin
    Result := True;
    Exit;
  end;

  if FList.Count <> APolygon.Count then begin
    Result := False;
    Exit;
  end;

  if (FHash <> 0) and (APolygon.Hash <> 0) then begin
    Result := FHash = APolygon.Hash;
  end else begin
    for i := 0 to FList.Count - 1 do begin
      VLine := GetItem(i);
      if VLine = nil then begin
        Result := False;
        Exit;
      end;
      if not VLine.IsSame(APolygon.Item[i]) then begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;
end;

function TGeometryLonLatMultiPolygon.IsSameGeometry(
  const AGeometry: IGeometryLonLat
): Boolean;
var
  VLine: IGeometryLonLatMultiPolygon;
begin
  if AGeometry = nil then begin
    Result := False;
    Exit;
  end;
  if AGeometry = IGeometryLonLat(Self) then begin
    Result := True;
    Exit;
  end;
  if (FHash <> 0) and (AGeometry.Hash <> 0) and (FHash <> AGeometry.Hash) then begin
    Result := False;
    Exit;
  end;

  Result := False;
  if Supports(AGeometry, IGeometryLonLatMultiPolygon, VLine) then begin
    Result := IsSame(VLine);
  end;
end;

end.
