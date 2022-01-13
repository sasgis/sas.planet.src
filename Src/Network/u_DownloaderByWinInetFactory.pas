{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_DownloaderByWinInetFactory;

interface

uses
  i_Downloader,
  i_DownloaderFactory,
  i_DownloadResultFactory,
  i_WinInetConfig,
  i_ContentTypeManager,
  u_BaseInterfacedObject;

type
  TDownloaderByWinInetFactory = class(TBaseInterfacedObject, IDownloaderFactory)
  private
    FResultFactory: IDownloadResultFactory;
    FContentTypeManager: IContentTypeManager;
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
    constructor Create(
      const AWinInetConfig: IWinInetConfig;
      const AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  WinInetFix,
  u_DownloaderHttpByWinInet,
  u_DownloadResultFactory;

{ TDownloaderByWinInetFactory }

constructor TDownloaderByWinInetFactory.Create(
  const AWinInetConfig: IWinInetConfig;
  const AContentTypeManager: IContentTypeManager
);
var
  VConnsPerServer: TConnsPerServerRec;
begin
  inherited Create;

  FResultFactory := TDownloadResultFactory.Create;
  FContentTypeManager := AContentTypeManager;

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
    TDownloaderHttpByWinInet.Create(
      FResultFactory,
      FContentTypeManager,
      AAllowUseCookie,
      AAllowRedirect,
      True, // ToDo
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
    TDownloaderHttpByWinInet.Create(
      FResultFactory,
      FContentTypeManager,
      AAllowUseCookie,
      AAllowRedirect,
      True, // ToDo
      ATryDetectContentType,
      AOnDownloadProgress
    );
end;

end.
