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

unit u_VectorDataItemPoint;

interface

uses
  t_GeoTypes,
  i_HtmlToHintTextConverter,
  u_VectorDataItemBase;

type
  TVectorDataItemPoint = class(TVectorDataItemBase)
  private
    FPoint: TDoublePoint;
  protected
    function IsPoint: Boolean; override;
    function IsLine: Boolean; override;
    function IsPoly: Boolean; override;
    function GetLLRect: TDoubleRect;  override;
    function GetPoints: TArrayOfDoublePoint;  override;
  public
    constructor Create(
      AHintConverter: IHtmlToHintTextConverter;
      AName: string;
      ADesc: string;
      APoint: TDoublePoint
    );
  end;

implementation

{ TVectorDataItemPoint }

constructor TVectorDataItemPoint.Create(
  AHintConverter: IHtmlToHintTextConverter;
  AName, ADesc: string;
  APoint: TDoublePoint
);
begin
  inherited Create(AHintConverter, AName, ADesc);
  FPoint := APoint;
end;

function TVectorDataItemPoint.GetLLRect: TDoubleRect;
begin
  Result.TopLeft := FPoint;
  Result.BottomRight := FPoint;
end;

function TVectorDataItemPoint.GetPoints: TArrayOfDoublePoint;
begin
  SetLength(Result, 1);
  Result[0] := FPoint;
end;

function TVectorDataItemPoint.IsLine: Boolean;
begin
  Result := False;
end;

function TVectorDataItemPoint.IsPoint: Boolean;
begin
  Result := True;
end;

function TVectorDataItemPoint.IsPoly: Boolean;
begin
  Result := False;
end;

end.
