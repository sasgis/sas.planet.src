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

unit u_TileDownloaderBaseFactory;

interface

uses
  i_TileDownloaderConfig,
  i_DownloadResultFactory,
  i_TileDownlodSession,
  i_SimpleFactory;

type
  TTileDownloaderFactoryBase = class(TInterfacedObject, ITileDownlodSessionFactory)
  private
    function CreateSession: ITileDownlodSession; virtual;
  end;

  TTileDownloaderFactory = class(TTileDownloaderFactoryBase, ISimpleFactory)
  private
    FConfig: ITileDownloaderConfig;
    FResultFactory: IDownloadResultFactory;
    function CreateInstance: IInterface;
    function CreateSession: ITileDownlodSession; override;
  public
    constructor Create(
      AResultFactory: IDownloadResultFactory;
      AConfig: ITileDownloaderConfig
    );
  end;

implementation

uses
  u_TileDownloaderBase;

{ TTileDownloaderBaseFactory }

constructor TTileDownloaderFactory.Create(
  AResultFactory: IDownloadResultFactory;
  AConfig: ITileDownloaderConfig
);
begin
  inherited Create;
  FConfig := AConfig;
  FResultFactory := AResultFactory;
end;

function TTileDownloaderFactory.CreateInstance: IInterface;
begin
  Result := CreateSession;
end;

function TTileDownloaderFactory.CreateSession: ITileDownlodSession;
begin
  Result := TTileDownloaderBase.Create(FResultFactory, FConfig);
end;

{ TTileDownloaderFactoryBase }

function TTileDownloaderFactoryBase.CreateSession: ITileDownlodSession;
begin
  Result := nil;
end;

end.
