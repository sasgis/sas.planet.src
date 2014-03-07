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

unit u_TileRequestQueueProcessorThread;

interface

uses
  i_NotifierOperation,
  i_Listener,
  i_ThreadConfig,
  i_TileRequestTask,
  i_InterfaceQueue,
  i_TileDownloader,
  u_InterfacedThread;

type
  TTileRequestQueueProcessorThread = class(TInterfacedThread)
  private
    FAppClosingNotifier: INotifierOneOperation;
    FTileRequestQueue: IInterfaceQueue;
    FTileDownloaderSync: ITileDownloader;

    FAppClosingListener: IListener;
    procedure OnAppClosing;
  protected
    procedure Execute; override;
  public
    constructor Create(
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRequestQueue: IInterfaceQueue;
      const ATileDownloaderSync: ITileDownloader
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  i_TileRequestResult,
  u_ListenerByEvent;

{ TTileRequestQueueProcessorThread }

constructor TTileRequestQueueProcessorThread.Create(
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRequestQueue: IInterfaceQueue;
  const ATileDownloaderSync: ITileDownloader
);
begin
  inherited Create(AThreadConfig, Self.ClassName);
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRequestQueue := ATileRequestQueue;
  FTileDownloaderSync := ATileDownloaderSync;

  FAppClosingListener := TNotifyNoMmgEventListener.Create(Self.OnAppClosing);
  FAppClosingNotifier.Add(FAppClosingListener);
  if FAppClosingNotifier.IsExecuted then begin
    OnAppClosing;
  end;
end;

destructor TTileRequestQueueProcessorThread.Destroy;
begin
  if Assigned(FAppClosingNotifier) and Assigned(FAppClosingListener) then begin
    FAppClosingNotifier.Remove(FAppClosingListener);
    FAppClosingListener := nil;
    FAppClosingNotifier := nil;
  end;

  FTileDownloaderSync := nil;
  FTileRequestQueue := nil;
  inherited;
end;

procedure TTileRequestQueueProcessorThread.Execute;
var
  VTileRequestTask: ITileRequestTaskInternal;
  VResult: ITileRequestResult;
begin
  inherited;
  while not Terminated do begin
    if Supports(FTileRequestQueue.Pull, ITileRequestTaskInternal, VTileRequestTask) then begin
      VResult := nil;
      try
        VResult := FTileDownloaderSync.Download(
          VTileRequestTask.SoftCancelNotifier,
          VTileRequestTask.CancelNotifier,
          VTileRequestTask.OperationID,
          VTileRequestTask.TileRequest
        );
      finally
        VTileRequestTask.SetFinished(VResult);
      end;
    end;
  end;
end;

procedure TTileRequestQueueProcessorThread.OnAppClosing;
begin
  Terminate;
end;

end.
