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

unit u_TileErrorInfo;

interface

uses
  Types,
  u_MapType,
  i_TileError;

type
  TTileErrorInfo = class(TInterfacedObject, ITileErrorInfo)
  private
    FMapType: TMapType;
    FZoom: Byte;
    FTile: TPoint;
    FErrorText: string;
  protected
    function GetMapType: TMapType;
    function GetZoom: Byte;
    function GetTile: TPoint;
    function GetErrorText: string;
  public
    constructor Create(
      AMapType: TMapType;
      AZoom: Byte;
      const ATile: TPoint;
      const AErrorText: string
    );
  end;

implementation

{ TTileErrorInfo }

constructor TTileErrorInfo.Create(
  AMapType: TMapType;
  AZoom: Byte;
  const ATile: TPoint;
  const AErrorText: string
);
begin
  inherited Create;
  FMapType := AMapType;
  FZoom := AZoom;
  FTile := ATile;
  FErrorText := AErrorText;
end;

function TTileErrorInfo.GetErrorText: string;
begin
  Result := FErrorText;
end;

function TTileErrorInfo.GetMapType: TMapType;
begin
  Result := FMapType;
end;

function TTileErrorInfo.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileErrorInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
