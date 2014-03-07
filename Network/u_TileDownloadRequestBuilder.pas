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

unit u_TileDownloadRequestBuilder;

interface

uses
  SysUtils,
  i_NotifierOperation,
  i_TileRequest,
  i_TileDownloadRequestBuilder,
  i_LastResponseInfo,
  i_TileDownloadRequest,
  i_TileDownloadRequestBuilderConfig,
  u_BaseInterfacedObject;

type
  TTileDownloadRequestBuilder = class(TBaseInterfacedObject, ITileDownloadRequestBuilder)
  private
    FCS: IReadWriteSync;
    FConfig: ITileDownloadRequestBuilderConfig;
  protected
    property Config: ITileDownloadRequestBuilderConfig read FConfig;
    procedure Lock;
    procedure Unlock;
  protected
    function BuildRequest(
      const ASource: ITileRequest;
      const ALastResponseInfo: ILastResponseInfo;
      const ACancelNotifier: INotifierOperation;
      AOperationID: Integer
    ): ITileDownloadRequest; virtual; abstract;
  public
    constructor Create(const AConfig: ITileDownloadRequestBuilderConfig);
  end;

implementation

uses
  u_Synchronizer;

{ TTileDownloadRequestBuilder }

constructor TTileDownloadRequestBuilder.Create(const AConfig: ITileDownloadRequestBuilderConfig);
begin
  inherited Create;
  FConfig := AConfig;
  FCS := MakeSyncRW_Big(Self, TRUE);
end;

procedure TTileDownloadRequestBuilder.Lock;
begin
  FCS.BeginWrite;
end;

procedure TTileDownloadRequestBuilder.Unlock;
begin
  FCS.EndWrite;
end;

end.
