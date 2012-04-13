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

unit u_TileIteratorAbstract;

interface

uses
  Types,
  i_VectorItemProjected,
  i_TileIterator;

type
  TTileIteratorAbstract = class(TInterfacedObject, ITileIterator)
  protected
    function GetTilesTotal: Int64; virtual; abstract;
    function GetTilesRect: TRect; virtual; abstract;
  public
    function Next(out ATile: TPoint): Boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
    property TilesTotal: Int64 read GetTilesTotal;
    property TilesRect: TRect read GetTilesRect;
  end;

  TTileIteratorByPolygonAbstract = class(TTileIteratorAbstract)
  private
    FProjected: IProjectedPolygon;
  protected
    FCurrent: TPoint;
    property Projected: IProjectedPolygon read FProjected;
  public
    constructor Create(
      AProjected: IProjectedPolygon
    );
    destructor Destroy; override;
  end;

implementation

{ TTileIteratorByPolygonAbstract }

constructor TTileIteratorByPolygonAbstract.Create(
  AProjected: IProjectedPolygon
);
begin
  FProjected := AProjected;
end;

destructor TTileIteratorByPolygonAbstract.Destroy;
begin
  FProjected := nil;
  inherited;
end;

end.


