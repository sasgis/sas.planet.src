{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-Present, SAS.Planet development team.                   *}
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

unit u_DownloaderFactory;

interface

uses
  i_InetConfig,
  i_DownloaderFactory,
  i_ContentTypeManager,
  u_BaseInterfacedObject;

type
  TDownloaderFactory = class(TBaseInterfacedObject, IDownloaderFactory)
  private
    FFactory: IDownloaderFactory;
    FInetConfig: IInetConfig;
    FContentTypeManager: IContentTypeManager;
    function BuildFactory: IDownloaderFactory;
  private
    { IDownloaderFactory }
    property Factory: IDownloaderFactory read FFactory implements IDownloaderFactory;
  public
    constructor Create(
      const AInetConfig: IInetConfig;
      const AContentTypeManager: IContentTypeManager
    );
  end;

implementation

uses
  SysUtils,
  u_DownloaderByCurlFactory,
  u_DownloaderByWinInetFactory;

{ TDownloaderFactory }

constructor TDownloaderFactory.Create(
  const AInetConfig: IInetConfig;
  const AContentTypeManager: IContentTypeManager
);
begin
  inherited Create;

  FInetConfig := AInetConfig;
  FContentTypeManager := AContentTypeManager;

  FFactory := BuildFactory;
end;

function TDownloaderFactory.BuildFactory: IDownloaderFactory;
begin
  case FInetConfig.NetworkEngineType of
    neWinInet: begin
      Result := TDownloaderByWinInetFactory.Create(
        FInetConfig.WinInetConfig,
        FContentTypeManager
      );
    end;
    neCurl: begin
      Result := TDownloaderByCurlFactory.Create(
        FContentTypeManager
      );
    end;
  else
    raise Exception.CreateFmt(
      'Unknown NetworkEngineType: %d', [Integer(FInetConfig.NetworkEngineType)]
    );
  end;
end;

end.
