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

unit u_TileDownloadRequest;

interface

uses
  Types,
  i_ZmpInfo,
  i_DownloadRequest,
  i_TileDownloadRequest;

type
  TTileDownloadRequest = class(TInterfacedObject, IDownloadRequest, ITileDownloadRequest)
  private
    FUrl: string;
    FRequestHeader: string;
    FZmp: IZmpInfo;
    FTile: TPoint;
    FZoom: Byte;
  protected
    function GetUrl: string;
    function GetRequestHeader: string;
  protected
    function GetZmp: IZmpInfo;
    function GetTile: TPoint;
    function GetZoom: Byte;
  public
    constructor Create(
      AUrl: string;
      ARequestHeader: string;
      AZmp: IZmpInfo;
      ATile: TPoint;
      AZoom: Byte
    );
  end;

implementation

{ TTileDownloadRequest }

constructor TTileDownloadRequest.Create(AUrl, ARequestHeader: string;
  AZmp: IZmpInfo; ATile: TPoint; AZoom: Byte);
begin
  FUrl := AUrl;
  FRequestHeader := ARequestHeader;
  FZmp := AZmp;
  FTile := ATile;
  FZoom := AZoom;
end;

function TTileDownloadRequest.GetRequestHeader: string;
begin
  Result := FRequestHeader;
end;

function TTileDownloadRequest.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileDownloadRequest.GetUrl: string;
begin
  Result := FUrl;
end;

function TTileDownloadRequest.GetZmp: IZmpInfo;
begin
  Result := FZmp;
end;

function TTileDownloadRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
