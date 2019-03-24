{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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

unit u_DownloaderByWinInetFactory;

interface

uses
  i_Downloader,
  i_DownloaderFactory,
  i_DownloadResultFactory,
  i_WinInetConfig,
  u_BaseInterfacedObject;

type
  TDownloaderByWinInetFactory = class(TBaseInterfacedObject, IDownloaderFactory)
  private
    FResultFactory: IDownloadResultFactory;
  private
    { IDownloaderFactory }
    function BuildDownloader(
      const AAllowUseCookie: Boolean;
      const AAllowRedirect: Boolean;
      const ATryDetectContentType: Boolean;
      const AOnDownloadProgress: TOnDownloadProgress
    ): IDownloader;

    function BuildDownloaderAsync(
      const AAllowUseCookie: Boolean;
      const AAllowRedirect: Boolean;
      const ATryDetectContentType: Boolean;
      const AOnDownloadProgress: TOnDownloadProgress
    ): IDownloaderAsync;
  public
    constructor Create(const AWinInetConfig: IWinInetConfig);
  end;

implementation

uses
  WinInetFix,
  u_DownloaderHttp,
  u_DownloadResultFactory;

{ TDownloaderByWinInetFactory }

constructor TDownloaderByWinInetFactory.Create(const AWinInetConfig: IWinInetConfig);
var
  VConnsPerServer: TConnsPerServerRec;
begin
  inherited Create;

  FResultFactory := TDownloadResultFactory.Create;

  // Fix HTTP connections limit
  VConnsPerServer := AWinInetConfig.MaxConnsPerServer;
  if VConnsPerServer.IsAvailable and (VConnsPerServer.Value <> VConnsPerServer.Def) then begin
    SetMaxConnsPerServerLimit(VConnsPerServer.Value);
  end;

  // Fix Proxy connections limit
  VConnsPerServer := AWinInetConfig.MaxConnsPerProxy;
  if VConnsPerServer.IsAvailable and (VConnsPerServer.Value <> VConnsPerServer.Def) then begin
    SetMaxConnsPerProxyLimit(VConnsPerServer.Value);
  end;
end;

function TDownloaderByWinInetFactory.BuildDownloader(
  const AAllowUseCookie: Boolean;
  const AAllowRedirect: Boolean;
  const ATryDetectContentType: Boolean;
  const AOnDownloadProgress: TOnDownloadProgress
): IDownloader;
begin
  Result :=
    TDownloaderHttp.Create(
      FResultFactory,
      AAllowUseCookie,
      AAllowRedirect,
      ATryDetectContentType,
      AOnDownloadProgress
    );
end;

function TDownloaderByWinInetFactory.BuildDownloaderAsync(
  const AAllowUseCookie: Boolean;
  const AAllowRedirect: Boolean;
  const ATryDetectContentType: Boolean;
  const AOnDownloadProgress: TOnDownloadProgress
): IDownloaderAsync;
begin
  Result :=
    TDownloaderHttp.Create(
      FResultFactory,
      AAllowUseCookie,
      AAllowRedirect,
      ATryDetectContentType,
      AOnDownloadProgress
    );
end;

end.
