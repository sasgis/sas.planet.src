{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_VectorDataItemPoint;

interface

uses
  t_Hash,
  i_Appearance,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase, IVectorDataItemPoint)
  private
    FPoint: IGeometryLonLatPoint;
  protected
    function GetGeometry: IGeometryLonLat; override;
    function GetPoint: IGeometryLonLatPoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AAppearance: IAppearance;
      const AMainInfo: IVectorDataItemMainInfo;
      const APoint: IGeometryLonLatPoint
    );
  end;

implementation

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  const AHash: THashValue;
  const AAppearance: IAppearance;
  const AMainInfo: IVectorDataItemMainInfo;
  const APoint: IGeometryLonLatPoint
);
begin
  Assert(Assigned(APoint));
  inherited Create(
    AHash,
    AAppearance,
    AMainInfo
  );
  FPoint := APoint;
end;

function TVectorDataItemPoint.GetGeometry: IGeometryLonLat;
begin
  Result := FPoint;
end;

function TVectorDataItemPoint.GetPoint: IGeometryLonLatPoint;
begin
  Result := FPoint;
end;

end.
