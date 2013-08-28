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

unit u_VectorDataItemOfMapPoint;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_StringProvider,
  i_LonLatRect,
  i_VectorDataItemSimple,
  i_HtmlToHintTextConverter,
  u_VectorDataItemOfMapBase;

type
  TVectorDataItemOfMapPoint = class(TVectorDataItemOfMapBase, IVectorDataItemPoint)
  private
    FLLRect: ILonLatRect;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function GetPoint: TDoublePoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string;
      const APoint: TDoublePoint
    );
  end;

implementation

uses
  u_GeoFun,
  u_LonLatRectByPoint;

{ TVectorDataItemPoint }

constructor TVectorDataItemOfMapPoint.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string;
  const APoint: TDoublePoint
);
begin
  Assert(not PointIsEmpty(APoint));
  inherited Create(
    AHash,
    AHintConverter,
    AUrlPrefix,
    AIndex,
    AName,
    ADesc
  );
  FLLRect := TLonLatRectByPoint.Create(APoint);
end;

function TVectorDataItemOfMapPoint.GetGoToLonLat: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TVectorDataItemOfMapPoint.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

function TVectorDataItemOfMapPoint.GetPoint: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

end.
