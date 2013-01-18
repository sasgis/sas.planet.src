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

unit u_MarkPoly;

interface

uses
  GR32,
  t_GeoTypes,
  i_LonLatRect,
  i_VectorItemLonLat,
  i_VectorDataItemSimple,
  i_MarksSimple,
  i_MarkCategory,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkPoly = class(TMarkFullBase, IVectorDataItemPoly, IMarkPoly)
  private
    FLine: ILonLatPolygon;
    FBorderColor: TColor32;
    FFillColor: TColor32;
    FLineWidth: Integer;
  protected
    function GetMarkType: TMarkType; override;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function IsEqual(const AMark: IMark): Boolean; override;
    function CloneWithNewId(const ANewId: Integer): IInterface; override;
  private
    function GetLine: ILonLatPolygon;
    function GetBorderColor: TColor32;
    function GetFillColor: TColor32;
    function GetLineWidth: Integer;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      AId: Integer;
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

{ TMarkFull }

function TMarkPoly.CloneWithNewId(const ANewId: Integer): IInterface;
begin
  if (ANewId=Self.GetId) then
    Result := Self
  else
    Result := TMarkPoly.Create(
      Self.FHintConverter,
      Self.GetName,
      ANewId,
      Self.GetVisible,
      Self.GetCategory,
      Self.FDesc,
      Self.FLine,
      Self.FBorderColor,
      Self.FFillColor,
      Self.FLineWidth
    );
end;

constructor TMarkPoly.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: ILonLatPolygon;
  ABorderColor, AFillColor: TColor32;
  ALineWidth: Integer
);
begin
  inherited Create(AHintConverter, AName, AId, ACategory, ADesc, AVisible);
  FLine := ALine;
  FBorderColor := ABorderColor;
  FFillColor := AFillColor;
  FLineWidth := ALineWidth;
end;

function TMarkPoly.GetBorderColor: TColor32;
begin
  Result := FBorderColor;
end;

function TMarkPoly.GetFillColor: TColor32;
begin
  Result := FFillColor;
end;

function TMarkPoly.GetGoToLonLat: TDoublePoint;
var
  VRect: TDoubleRect;
begin
  VRect := FLine.Bounds.Rect;
  Result.X := (VRect.Left + VRect.Right) / 2;
  Result.Y := (VRect.Top + VRect.Bottom) / 2;
end;

function TMarkPoly.GetLLRect: ILonLatRect;
begin
  Result := FLine.Bounds;
end;

function TMarkPoly.GetMarkType: TMarkType;
begin
  Result := mt_Polygon;
end;

function TMarkPoly.IsEqual(const AMark: IMark): Boolean;
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
  if FBorderColor <> VMarkPoly.BorderColor then begin
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

function TMarkPoly.GetLine: ILonLatPolygon;
begin
  Result := FLine;
end;

function TMarkPoly.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

end.
