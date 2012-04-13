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

unit u_TileDownloadRequest;

interface

uses
  i_DownloadRequest,
  i_InetConfig,
  i_TileRequest,
  i_DownloadChecker,
  i_TileDownloadRequest;

type
  TTileDownloadRequest = class(TInterfacedObject, IDownloadRequest, ITileDownloadRequest, IRequestWithChecker)
  private
    FUrl: string;
    FRequestHeader: string;
    FInetConfig: IInetConfigStatic;
    FCheker: IDownloadChecker;
    FSource: ITileRequest;
  protected
    function GetUrl: string;
    function GetRequestHeader: string;
    function GetInetConfig: IInetConfigStatic;
  protected
    function GetSource: ITileRequest;
  protected
    function GetChecker: IDownloadChecker;
  public
    constructor Create(
      const AUrl: string;
      const ARequestHeader: string;
      const AInetConfig: IInetConfigStatic;
      const ACheker: IDownloadChecker;
      const ASource: ITileRequest
    );
  end;

implementation

{ TTileDownloadRequest }

constructor TTileDownloadRequest.Create(
  const AUrl, ARequestHeader: string;
  const AInetConfig: IInetConfigStatic;
  const ACheker: IDownloadChecker;
  const ASource: ITileRequest
);
begin
  FUrl := AUrl;
  FRequestHeader := ARequestHeader;
  FInetConfig := AInetConfig;
  FSource := ASource;
  FCheker := ACheker;
end;

function TTileDownloadRequest.GetChecker: IDownloadChecker;
begin
  Result := FCheker;
end;

function TTileDownloadRequest.GetInetConfig: IInetConfigStatic;
begin
  Result := FInetConfig;
end;

function TTileDownloadRequest.GetRequestHeader: string;
begin
  Result := FRequestHeader;
end;

function TTileDownloadRequest.GetSource: ITileRequest;
begin
  Result := FSource;
end;

function TTileDownloadRequest.GetUrl: string;
begin
  Result := FUrl;
end;

end.
