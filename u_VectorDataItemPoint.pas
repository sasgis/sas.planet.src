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
  t_GeoTypes,
  i_LonLatRect,
  i_VectorDataItemSimple,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase, IVectorDataItemPoint)
  private
    FLLRect: ILonLatRect;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetPoint: TDoublePoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const ADesc: string;
      const APoint: TDoublePoint
    );
  end;

implementation

uses
  u_LonLatRectByPoint;

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName, ADesc: string;
  const APoint: TDoublePoint
);
begin
  inherited Create(
    AHash,
    AHintConverter,
    AName,
    ADesc
  );
  FLLRect := TLonLatRectByPoint.Create(APoint);
end;

function TVectorDataItemPoint.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

function TVectorDataItemPoint.GetPoint: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

end.
