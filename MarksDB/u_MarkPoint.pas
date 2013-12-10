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

unit u_MarkPoint;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_VectorDataItemSimple,
  i_Category,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkPoint = class(TMarkFullBase, IVectorDataItemPoint)
  private
    FLLRect: ILonLatRect;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function IsEqual(const AMark: IVectorDataItemSimple): Boolean; override;
  private
    function GetPoint: TDoublePoint;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      const AAppearance: IAppearance;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: TDoublePoint
    );
  end;

implementation

uses
  SysUtils,
  u_GeoFun,
  u_LonLatRectByPoint;

{ TMarkPoint }

constructor TMarkPoint.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  const AAppearance: IAppearance;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint
);
begin
  Assert(not PointIsEmpty(APoint));
  inherited Create(AHash, AAppearance, AHintConverter, AName, ACategory, ADesc);
  FLLRect := TLonLatRectByPoint.Create(APoint);
end;

function TMarkPoint.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkPoint: IVectorDataItemPoint;
begin
  if AMark = IVectorDataItemSimple(Self) then begin
    Result := True;
    Exit;
  end;
  if not inherited IsEqual(AMark) then begin
    Result := False;
    Exit;
  end;
  if not FLLRect.IsEqual(AMark.LLRect) then begin
    Result := False;
    Exit;
  end;
  if not Supports(AMark, IVectorDataItemPoint, VMarkPoint) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPoint.GetGoToLonLat: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TMarkPoint.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

function TMarkPoint.GetPoint: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TMarkPoint.GetMarkType: TGUID;
begin
  Result := IVectorDataItemPoint;
end;

end.
