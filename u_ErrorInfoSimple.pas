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

unit u_ErrorInfoSimple;

interface

uses
  Types,
  i_ErrorInfo,
  i_TileRequestResult,
  u_BaseInterfacedObject;

type
  TErrorInfoSimple = class(TBaseInterfacedObject, IErrorInfoSimple)
  private
    FText: string;
  private
    function GetErrorText: string;
  public
    constructor Create(
      const AText: string
    );
  end;

  TErrorInfoByTileRequestResult = class(TBaseInterfacedObject, IErrorInfoSimple, IErrorInfoMapType, IErrorInfoTile)
  private
    FResult: ITileRequestResultError;
    FMapTypeGUID: TGUID;
  private
    function GetErrorText: string;
    function GetZoom: Byte;
    function GetTile: TPoint;
    function GetMapTypeGUID: TGUID;
  public
    constructor Create(
      const AResult: ITileRequestResultError;
      const AMapTypeGUID: TGUID
    );
  end;

implementation

{ TErrorInfoSimple }

constructor TErrorInfoSimple.Create(const AText: string);
begin
  inherited Create;
  FText := AText;
end;

function TErrorInfoSimple.GetErrorText: string;
begin
  Result := FText;
end;

{ TErrorInfoByTileRequestResult }

constructor TErrorInfoByTileRequestResult.Create(
  const AResult: ITileRequestResultError;
  const AMapTypeGUID: TGUID
);
begin
  inherited Create;
  FResult := AResult;
  FMapTypeGUID := AMapTypeGUID;
end;

function TErrorInfoByTileRequestResult.GetErrorText: string;
begin
  Result := FResult.ErrorText;
end;

function TErrorInfoByTileRequestResult.GetMapTypeGUID: TGUID;
begin
  Result := FMapTypeGUID;
end;

function TErrorInfoByTileRequestResult.GetTile: TPoint;
begin
  Result := FResult.Request.Tile;
end;

function TErrorInfoByTileRequestResult.GetZoom: Byte;
begin
  Result := FResult.Request.Zoom;
end;

end.
