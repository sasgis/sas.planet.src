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

unit u_MarkPointSml;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_Mark,
  i_VectorDataItemSimple,
  i_Category,
  i_HtmlToHintTextConverter,
  u_MarkFullBaseSml;

type
  TMarkPointSml = class(TMarkFullBaseSml, IMarkPoint, IVectorDataItemPoint)
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
      AId: Integer;
      ADbId: Integer;
      AVisible: Boolean;
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

{ TMarkPointSml }

constructor TMarkPointSml.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  ADbId: Integer;
  AVisible: Boolean;
  const AAppearance: IAppearance;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint
);
begin
  Assert(not PointIsEmpty(APoint));
  inherited Create(AHash, AAppearance, AHintConverter, AName, AId, ADbId, ACategory, ADesc, AVisible);
  FLLRect := TLonLatRectByPoint.Create(APoint);
end;

function TMarkPointSml.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkPoint: IMarkPoint;
begin
  if AMark = IMark(Self) then begin
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
  if not Supports(AMark, IMarkPoint, VMarkPoint) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPointSml.GetGoToLonLat: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TMarkPointSml.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

function TMarkPointSml.GetPoint: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TMarkPointSml.GetMarkType: TGUID;
begin
  Result := IMarkPoint;
end;

end.
