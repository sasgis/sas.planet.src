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
  GR32,
  t_GeoTypes,
  i_LonLatRect,
  i_Mark,
  i_VectorDataItemSimple,
  i_Category,
  i_MarkDbSmlInternal,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  u_MarkFullBaseSml;

type
  TMarkPointSml = class(TMarkFullBaseSml, IMarkPoint, IVectorDataItemPoint,
    IVectorDataItemPointWithIconParams, IVectorDataItemPointWithCaptionParams)
  private
    FPicName: string;
    FPic: IMarkPicture;
    FLLRect: ILonLatRect;
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FFontSize: Integer;
    FMarkerSize: Integer;
  protected
    function GetMarkType: TGUID; override;
  protected
    function GetLLRect: ILonLatRect; override;
    function GetGoToLonLat: TDoublePoint; override;
    function IsEqual(const AMark: IMark): Boolean; override;
  private
    function GetPoint: TDoublePoint;
    function GetTextColor: TColor32;
    function GetTextBgColor: TColor32;
    function GetFontSize: Integer;
    function GetMarkerSize: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      const AName: string;
      AId: Integer;
      ADbId: Integer;
      AVisible: Boolean;
      const APicName: string;
      const APic: IMarkPicture;
      const ACategory: ICategory;
      const ADesc: string;
      const APoint: TDoublePoint;
      ATextColor: TColor32;
      ATextBgColor: TColor32;
      AFontSize: Integer;
      AMarkerSize: Integer
    );
  end;

implementation

uses
  SysUtils,
  u_LonLatRectByPoint;

{ TMarkPointSml }

constructor TMarkPointSml.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  const AName: string;
  AId: Integer;
  ADbId: Integer;
  AVisible: Boolean;
  const APicName: string;
  const APic: IMarkPicture;
  const ACategory: ICategory;
  const ADesc: string;
  const APoint: TDoublePoint;
  ATextColor, ATextBgColor: TColor32;
  AFontSize, AMarkerSize: Integer
);
begin
  inherited Create(AHintConverter, AName, AId, ADbId, ACategory, ADesc, AVisible);
  FPicName := APicName;
  FPic := APic;
  FLLRect := TLonLatRectByPoint.Create(APoint);
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FFontSize := AFontSize;
  FMarkerSize := AMarkerSize;
end;

function TMarkPointSml.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TMarkPointSml.IsEqual(const AMark: IMark): Boolean;
var
  VMarkPoint: IMarkPoint;
begin
  if AMark = IMark(Self) then begin
    Result := True;
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
  if not inherited IsEqual(AMark) then begin
    Result := False;
    Exit;
  end;
  if FPic <> VMarkPoint.Pic then begin
    Result := False;
    Exit;
  end;
  if FTextColor <> VMarkPoint.TextColor then begin
    Result := False;
    Exit;
  end;
  if FTextBgColor <> VMarkPoint.TextBgColor then begin
    Result := False;
    Exit;
  end;
  if FFontSize <> VMarkPoint.FontSize then begin
    Result := False;
    Exit;
  end;
  if FMarkerSize <> VMarkPoint.MarkerSize then begin
    Result := False;
    Exit;
  end;
  Result := True;
end;

function TMarkPointSml.GetTextBgColor: TColor32;
begin
  Result := FTextBgColor;
end;

function TMarkPointSml.GetGoToLonLat: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TMarkPointSml.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

function TMarkPointSml.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkPointSml.GetPicName: string;
begin
  Result := FPicName;
end;

function TMarkPointSml.GetPoint: TDoublePoint;
begin
  Result := FLLRect.TopLeft;
end;

function TMarkPointSml.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMarkPointSml.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

function TMarkPointSml.GetMarkType: TGUID;
begin
  Result := IMarkPoint;
end;

end.
