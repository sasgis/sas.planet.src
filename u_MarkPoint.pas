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
  GR32,
  t_GeoTypes,
  i_MarksSimple,
  i_MarkCategory,
  i_MarksDbSmlInternal,
  i_MarkPicture,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkPoint = class(TMarkFullBase, IMarkPoint, IMarkPointSMLInternal)
  private
    FPicName: string;
    FPic: IMarkPicture;
    FPoint: TDoublePoint;
    FTextColor: TColor32;
    FTextBgColor: TColor32;
    FFontSize: Integer;
    FMarkerSize: Integer;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetPoint: TDoublePoint;
    function GetTextColor: TColor32;
    function GetTextBgColor: TColor32;
    function GetFontSize: Integer;
    function GetMarkerSize: Integer;
    function GetPicName: string;
    function GetPic: IMarkPicture;
    function GetGoToLonLat: TDoublePoint; override;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      ADbCode: Integer;
      const AName: string;
      AId: Integer;
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

{ TMarkPoint }

constructor TMarkPoint.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  ADbCode: Integer;
  const AName: string;
  AId: Integer;
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
  inherited Create(AHintConverter, ADbCode, AName, AId, ACategory, ADesc, AVisible);
  FPicName := APicName;
  FPic := APic;
  FPoint := APoint;
  FTextColor := ATextColor;
  FTextBgColor := ATextBgColor;
  FFontSize := AFontSize;
  FMarkerSize := AMarkerSize;
end;

function TMarkPoint.GetTextColor: TColor32;
begin
  Result := FTextColor;
end;

function TMarkPoint.GetTextBgColor: TColor32;
begin
  Result := FTextBgColor;
end;

function TMarkPoint.GetGoToLonLat: TDoublePoint;
begin
  Result := FPoint;
end;

function TMarkPoint.GetLLRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TMarkPoint.GetPic: IMarkPicture;
begin
  Result := FPic;
end;

function TMarkPoint.GetPicName: string;
begin
  Result := FPicName;
end;

function TMarkPoint.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

function TMarkPoint.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMarkPoint.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

end.
