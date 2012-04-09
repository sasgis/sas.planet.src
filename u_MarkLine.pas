{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_MarkLine;

interface

uses
  GR32,
  t_GeoTypes,
  i_VectorItemLonLat,
  i_MarksSimple,
  i_MarkCategory,
  i_HtmlToHintTextConverter,
  u_MarkFullBase;

type
  TMarkLine = class(TMarkFullBase, IMarkLine)
  private
    FLLRect: TDoubleRect;
    FLine: ILonLatPath;
    FLineColor: TColor32;
    FLineWidth: Integer;
  protected
    function GetLLRect: TDoubleRect; override;
    function GetLine: ILonLatPath;
    function GetLineColor: TColor32;
    function GetLineWidth: Integer;
    function GetGoToLonLat: TDoublePoint; override;
  public
    constructor Create(
      const AHintConverter: IHtmlToHintTextConverter;
      ADbCode: Integer;
      const AName: string;
      AId: Integer;
      AVisible: Boolean;
      const ACategory: ICategory;
      const ADesc: string;
      const ALLRect: TDoubleRect;
      const ALine: ILonLatPath;
      ALineColor: TColor32;
      ALineWidth: Integer
    );
  end;

implementation

{ TMarkFull }

constructor TMarkLine.Create(
  const AHintConverter: IHtmlToHintTextConverter;
  ADbCode: Integer;
  const AName: string;
  AId: Integer;
  AVisible: Boolean;
  const ACategory: ICategory;
  const ADesc: string;
  const ALLRect: TDoubleRect;
  const ALine: ILonLatPath;
  ALineColor: TColor32;
  ALineWidth: Integer
);
begin
  inherited Create(AHintConverter, ADbCode, AName, AId, ACategory, ADesc, AVisible);
  FLLRect := ALLRect;
  FLine := ALine;
  FLineColor := ALineColor;
  FLineWidth := ALineWidth;
end;

function TMarkLine.GetLineColor: TColor32;
begin
  Result := FLineColor;
end;

function TMarkLine.GetGoToLonLat: TDoublePoint;
begin
  FLine.GetEnum.Next(Result);
end;

function TMarkLine.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

function TMarkLine.GetLine: ILonLatPath;
begin
  Result := FLine;
end;

function TMarkLine.GetLineWidth: Integer;
begin
  Result := FLineWidth;
end;

end.

