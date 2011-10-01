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

unit u_TileRequestBuilder;

interface

uses
  Windows,
  SyncObjs,
  SysUtils,
  i_TileRequestBuilder,
  i_MapVersionInfo,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileRequestBuilderConfig;

type
  TTileRequestBuilder = class(TInterfacedObject, ITileRequestBuilder)
  private
    FCS: TCriticalSection;
  protected
    FConfig: ITileRequestBuilderConfig;
    procedure Lock;
    procedure Unlock;
  protected
    function BuildRequest(
      ATileXY: TPoint;
      AZoom: Byte;
      AVersionInfo: IMapVersionInfo;
      ALastResponseInfo: ILastResponseInfo
    ): ITileDownloadRequest; virtual; abstract;
  public
    constructor Create(AConfig: ITileRequestBuilderConfig);
    destructor Destroy; override;
  end;

implementation

{ TTileRequestBuilder }

constructor TTileRequestBuilder.Create(AConfig: ITileRequestBuilderConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FCS := TCriticalSection.Create;
end;

destructor TTileRequestBuilder.Destroy;
begin
  FreeAndNil(FCS);
  inherited Destroy;
end;

procedure TTileRequestBuilder.Lock;
begin
  FCS.Acquire;
end;

procedure TTileRequestBuilder.Unlock;
begin
  FCS.Release;
end;

end.
