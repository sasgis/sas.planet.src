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

unit u_VectorDataItemPolygon;

interface

uses
  t_GeoTypes,
  i_VectorDataItemSimple,
  i_VectorItemLonLat,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPolygon = class(TVectorDataItemBase)
  private
    FLLRect: TDoubleRect;
  protected
    function GetLLRect: TDoubleRect;  override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      ALLRect: TDoubleRect
    );
  end;

  TVectorDataItemPath = class(TVectorDataItemPolygon, IVectorDataItemLine)
  private
    FLine: ILonLatPath;
  protected
    function GetLine: ILonLatPath;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      ALine: ILonLatPath;
      ALLRect: TDoubleRect
    );
  end;

  TVectorDataItemPoly = class(TVectorDataItemPolygon, IVectorDataItemPoly)
  private
    FLine: ILonLatPolygon;
  protected
    function GetLine: ILonLatPolygon;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      ALine: ILonLatPolygon;
      ALLRect: TDoubleRect
    );
  end;


implementation

{ TVectorDataItemPolygon }

constructor TVectorDataItemPolygon.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string;
  ALLRect: TDoubleRect
);
begin
  inherited Create(AHintConverter, AName, ADesc);
  FLLRect := ALLRect;
end;

function TVectorDataItemPolygon.GetLLRect: TDoubleRect;
begin
  Result := FLLRect;
end;

{ TVectorDataItemPath }

constructor TVectorDataItemPath.Create(AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string; ALine: ILonLatPath; ALLRect: TDoubleRect);
begin
  inherited Create(AHintConverter, AName, ADesc, ALLRect);
  FLine := ALine;
end;

function TVectorDataItemPath.GetLine: ILonLatPath;
begin
  Result := FLine;
end;

{ TVectorDataItemPoly }

constructor TVectorDataItemPoly.Create(AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string; ALine: ILonLatPolygon; ALLRect: TDoubleRect);
begin
  inherited Create(AHintConverter, AName, ADesc, ALLRect);
  FLine := ALine;
end;

function TVectorDataItemPoly.GetLine: ILonLatPolygon;
begin
  Result := FLine;
end;

end.
