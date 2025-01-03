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

unit u_TileDownloaderWithQueue;

interface

uses
  i_NotifierOperation,
  i_ThreadConfig,
  i_TileRequestTask,
  i_InterfaceQueue,
  i_TileDownloaderList,
  i_TileDownloader,
  i_TileRequestProcessorPool,
  i_NotifierTime,
  u_BaseInterfacedObject;

type
  TTileDownloaderWithQueue = class(TBaseInterfacedObject, ITileDownloaderAsync)
  private
    FQueue: IInterfaceQueue;
    FSyncTileRequestProcessorPull: ITileRequestProcessorPool;
  private
    procedure Download(
      const ATileRequestTask: ITileRequestTask
    );
  public
    constructor Create(
      const ATileDownloaderList: ITileDownloaderList;
      const AGCNotifier: INotifierTime;
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      AQueueCapacity: Integer
    );
  end;

implementation

uses
  u_InterfaceQueue,
  u_TileRequestProcessorPool;

{ TTileDownloaderWithQueue }

constructor TTileDownloaderWithQueue.Create(
  const ATileDownloaderList: ITileDownloaderList;
  const AGCNotifier: INotifierTime;
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  AQueueCapacity: Integer
);
begin
  inherited Create;
  FQueue :=
    TInterfaceQueue.Create(
      AAppClosingNotifier,
      AQueueCapacity
    );
  FSyncTileRequestProcessorPull :=
    TTileRequestProcessorPool.Create(
      AGCNotifier,
      AThreadConfig,
      AAppClosingNotifier,
      FQueue,
      ATileDownloaderList
    );
end;

procedure TTileDownloaderWithQueue.Download(
  const ATileRequestTask: ITileRequestTask
);
begin
  FQueue.Push(ATileRequestTask);
  FSyncTileRequestProcessorPull.InitThreadsIfNeed;
end;

end.
