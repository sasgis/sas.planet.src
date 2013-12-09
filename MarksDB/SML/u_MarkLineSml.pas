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

unit u_MarkLineSml;

interface

uses
  t_Hash,
  t_GeoTypes,
  i_Appearance,
  i_LonLatRect,
  i_GeometryLonLat,
  i_VectorDataItemSimple,
  i_Mark,
  i_Category,
  i_HtmlToHintTextConverter,
  u_MarkFullBaseSml;

type
  TMarkLineSml = class(TMarkFullBaseSml, IVectorDataItemLine, IMarkLine)
  private
    FLine: IGeometryLonLatMultiLine;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function IsEqual(const AMark: IVectorDataItemSimple): Boolean; override;
  private
    function GetLine: IGeometryLonLatMultiLine;
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
      const ALine: IGeometryLonLatMultiLine
    );
  end;

implementation

uses
  SysUtils;

{ TMarkLineSml }

constructor TMarkLineSml.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  ADbId: Integer;
  AVisible: Boolean;
  const AAppearance: IAppearance;
  const ACategory: ICategory;
  const ADesc: string;
  const ALine: IGeometryLonLatMultiLine
);
begin
  Assert(Assigned(ALine));
  inherited Create(AHash, AAppearance, AHintConverter, AName, AId, ADbId, ACategory, ADesc, AVisible);
  FLine := ALine;
end;

function TMarkLineSml.GetGoToLonLat: TDoublePoint;
begin
  FLine.GetEnum.Next(Result);
end;

function TMarkLineSml.GetLLRect: ILonLatRect;
begin
  Result := FLine.Bounds;
end;

function TMarkLineSml.GetMarkType: TGUID;
begin
  Result := IMarkLine;
end;

function TMarkLineSml.IsEqual(const AMark: IVectorDataItemSimple): Boolean;
var
  VMarkPath: IMarkLine;
begin
  if AMark = IMark(Self) then begin
    Result := True;
    Exit;
  end;
  if not inherited IsEqual(AMark) then begin
    Result := False;
    Exit;
  end;
  if not Supports(AMark, IMarkLine, VMarkPath) then begin
    Result := False;
    Exit;
  end;
  if not FLine.Bounds.IsEqual(VMarkPath.LLRect) then begin
    Result := False;
    Exit;
  end;
  if not FLine.IsSame(VMarkPath.Line) then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkLineSml.GetLine: IGeometryLonLatMultiLine;
begin
  Result := FLine;
end;

end.
