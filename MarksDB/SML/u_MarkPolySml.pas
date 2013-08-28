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

unit u_MarkPolySml;

interface

uses
  GR32,
  t_Hash,
  t_GeoTypes,
  i_LonLatRect,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_Mark,
  i_Category,
  i_HtmlToHintTextConverter,
  u_MarkFullBaseSml;

type
  TMarkPolySml = class(TMarkFullBaseSml, IMarkPoly, IVectorDataItemPoly,
    IVectorDataItemPolyWithFillParams, IVectorDataItemWithLineParams)
  private
    FLine: ILonLatPolygon;
    FLineColor: TColor32;
    FFillColor: TColor32;
    FLineWidth: Integer;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function IsEqual(const AMark: IMark): Boolean; override;
  private
    function GetLine: ILonLatPolygon;
    function GetLineColor: TColor32;
    function GetFillColor: TColor32;
    function GetLineWidth: Integer;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      AId: Integer;
      ADbId: Integer;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const ALine: ILonLatPolygon;
      ABorderColor: TColor32;
      AFillColor: TColor32;
      ALineWidth: Integer
    );
  end;

implementation

uses
  SysUtils;

{ TMarkPolySml }

constructor TMarkPolySml.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  ADbId: Integer;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
);
begin
  Assert(Assigned(ALine));
  inherited Create(AHash, AHintConverter, AName, AId, ADbId, ACategory, ADesc, AVisible);
  FLine := ALine;
  FLineColor := ABorderColor;
  FFillColor := AFillColor;
  FLineWidth := ALineWidth;
end;

function TMarkPolySml.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TMarkPolySml.GetFillColor: TColor32;
begin
  Result := FFillColor;
end;

function TMarkPolySml.GetGoToLonLat: TDoublePoint;
var
  VRect: TDoubleRect;
begin
  VRect := FLine.Bounds.Rect;
  Result.X := (VRect.Left + VRect.Right) / 2;
  Result.Y := (VRect.Top + VRect.Bottom) / 2;
end;

function TMarkPolySml.GetLLRect: ILonLatRect;
begin
  Result := FLine.Bounds;
end;

function TMarkPolySml.GetMarkType: TGUID;
begin
  Result := IMarkPoly;
end;

function TMarkPolySml.IsEqual(const AMark: IMark): Boolean;
var
  VMarkPoly: IMarkPoly;
begin
  if AMark = IMark(Self) then begin
    Result := True;
    Exit;
  end;
  if not Supports(AMark, IMarkPoly, VMarkPoly) then begin
    Result := False;
    Exit;
  end;
  if not FLine.Bounds.IsEqual(VMarkPoly.LLRect) then begin
    Result := False;
    Exit;
  end;
  if not inherited IsEqual(AMark) then begin
    Result := False;
    Exit;
  end;
  if FLineColor <> VMarkPoly.LineColor then begin
    Result := False;
    Exit;
  end;
  if FFillColor <> VMarkPoly.FillColor then begin
    Result := False;
    Exit;
  end;
  if FLineWidth <> VMarkPoly.LineWidth then begin
    Result := False;
    Exit;
  end;
  if not FLine.IsSame(VMarkPoly.Line) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPolySml.GetLine: ILonLatPolygon;
begin
  Result := FLine;
end;

function TMarkPolySml.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

end.
