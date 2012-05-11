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
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase, IVectorDataItemPoint)
  private
    FPoint: TDoublePoint;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetPoint: TDoublePoint;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ADesc: string;
      const APoint: TDoublePoint
    );
  end;

implementation

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  const AName, ADesc: string;
  const APoint: TDoublePoint
);
begin
  inherited Create(
    AHintConverter,
    AName,
    ADesc
  );
  FPoint := APoint;
end;

function TVectorDataItemPoint.GetLLRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TVectorDataItemPoint.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

end.
