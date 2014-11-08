{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_TileRequest;

interface

uses
  Types,
  i_TileRequest,
  i_MapVersionInfo,
  u_BaseInterfacedObject;

type
  TTileRequest = class(TBaseInterfacedObject, ITileRequest)
  private
    FTile: TPoint;
    FZoom: Byte;
    FVersionInfo: IMapVersionInfo;
  private
    function GetTile: TPoint;
    function GetZoom: Byte;
    function GetVersionInfo: IMapVersionInfo;
  public
    constructor Create(
      const ATile: TPoint;
      const AZoom: Byte;
      const AVersionInfo: IMapVersionInfo
    );
  end;

  TTileRequestWithSizeCheck = class(TTileRequest, ITileRequestWithSizeCheck);

implementation

{ TTileRequest }

constructor TTileRequest.Create(
  const ATile: TPoint;
  const AZoom: Byte;
  const AVersionInfo: IMapVersionInfo
);
begin
  inherited Create;
  FTile := ATile;
  FZoom := AZoom;
  FVersionInfo := AVersionInfo;
end;

function TTileRequest.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileRequest.GetVersionInfo: IMapVersionInfo;
begin
  Result := FVersionInfo;
end;

function TTileRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
