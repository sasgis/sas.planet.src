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

unit u_VectorDataItemOfMapPolygon;

interface

uses
  t_Hash,
  i_StringProvider,
  i_LonLatRect,
  i_VectorDataItemSimple,
  i_VectorItemLonLat,
  i_HtmlToHintTextConverter,
  u_VectorDataItemOfMapBase;

type
  TVectorDataItemOfMapPolygon = class(TVectorDataItemOfMapBase)
  private
    FLLRect: ILonLatRect;
  protected
    function GetLLRect: ILonLatRect; override;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string;
      const ALLRect: ILonLatRect
    );
  end;

  TVectorDataItemOfMapPath = class(TVectorDataItemOfMapPolygon, IVectorDataItemLine)
  private
    FLine: ILonLatPath;
  protected
    function GetLine: ILonLatPath;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string;
      const ALine: ILonLatPath
    );
  end;

  TVectorDataItemOfMapPoly = class(TVectorDataItemOfMapPolygon, IVectorDataItemPoly)
  private
    FLine: ILonLatPolygon;
  protected
    function GetLine: ILonLatPolygon;
  public
    constructor Create(
      const AHash: THashValue;
      const AHintConverter: IHtmlToHintTextConverter;
      const AUrlPrefix: IStringProvider;
      const AIndex: Integer;
      const AName: string;
      const ADesc: string;
      const ALine: ILonLatPolygon
    );
  end;


implementation

{ TVectorDataItemPolygon }

constructor TVectorDataItemOfMapPolygon.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string;
  const ALLRect: ILonLatRect
);
begin
  inherited Create(
    AHash,
    AHintConverter,
    AUrlPrefix,
    AIndex,
    AName,
    ADesc
  );
  FLLRect := ALLRect;
end;

function TVectorDataItemOfMapPolygon.GetLLRect: ILonLatRect;
begin
  Result := FLLRect;
end;

{ TVectorDataItemPath }

constructor TVectorDataItemOfMapPath.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string;
  const ALine: ILonLatPath
);
begin
  inherited Create(
    AHash,
    AHintConverter,
    AUrlPrefix,
    AIndex,
    AName,
    ADesc,
    ALine.Bounds
  );
  FLine := ALine;
end;

function TVectorDataItemOfMapPath.GetLine: ILonLatPath;
begin
  Result := FLine;
end;

{ TVectorDataItemPoly }

constructor TVectorDataItemOfMapPoly.Create(
  const AHash: THashValue;
  const AHintConverter: IHtmlToHintTextConverter;
  const AUrlPrefix: IStringProvider;
  const AIndex: Integer;
  const AName, ADesc: string;
  const ALine: ILonLatPolygon
);
begin
  inherited Create(
    AHash,
    AHintConverter,
    AUrlPrefix,
    AIndex,
    AName,
    ADesc,
    ALine.Bounds
  );
  FLine := ALine;
end;

function TVectorDataItemOfMapPoly.GetLine: ILonLatPolygon;
begin
  Result := FLine;
end;

end.
