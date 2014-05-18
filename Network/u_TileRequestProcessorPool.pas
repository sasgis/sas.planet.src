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

unit u_TileRequestProcessorPool;

interface

uses
  SysUtils,
  i_Notifier,
  i_NotifierOperation,
  i_Listener,
  i_Thread,
  i_ThreadConfig,
  i_ListenerTime,
  i_NotifierTime,
  i_InterfaceQueue,
  i_TileDownloaderList,
  i_TileRequestProcessorPool,
  u_BaseInterfacedObject;

type
  TTileRequestProcessorPool = class(TBaseInterfacedObject, ITileRequestProcessorPool)
  private
    type
    TArrayOfThread = array of IThread;
  private
    FThreadConfig: IThreadConfig;
    FDownloaderList: ITileDownloaderList;
    FGCNotifier: INotifierTime;
    FAppClosingNotifier: INotifierOneOperation;
    FTileRequestQueue: IInterfaceQueue;

    FTTLListener: IListenerTimeWithUsedFlag;
    FDownloadersListListener: IListener;

    FThreadArray: TArrayOfThread;
    FThreadArrayCS: IReadWriteSync;

    procedure OnTTLTrim;
    procedure OnDownloadersListChange;
    procedure TerminateDownloaders;
  private
    procedure InitThreadsIfNeed;
  public
    constructor Create(
      const AGCNotifier: INotifierTime;
      const AThreadConfig: IThreadConfig;
      const AAppClosingNotifier: INotifierOneOperation;
      const ATileRequestQueue: IInterfaceQueue;
      const ADownloaderList: ITileDownloaderList
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_Synchronizer,
  i_TileDownloader,
  u_ListenerByEvent,
  u_ListenerTime,
  u_TileRequestQueueProcessorThread;

{ TTileRequestProcessorPool }

constructor TTileRequestProcessorPool.Create(
  const AGCNotifier: INotifierTime;
  const AThreadConfig: IThreadConfig;
  const AAppClosingNotifier: INotifierOneOperation;
  const ATileRequestQueue: IInterfaceQueue;
  const ADownloaderList: ITileDownloaderList
);
begin
  inherited Create;
  FGCNotifier := AGCNotifier;
  FThreadConfig := AThreadConfig;
  FAppClosingNotifier := AAppClosingNotifier;
  FTileRequestQueue := ATileRequestQueue;

  FThreadArrayCS := GSync.SyncStd.Make(Self.ClassName);

  FDownloaderList := ADownloaderList;

  FDownloadersListListener := TNotifyNoMmgEventListener.Create(Self.OnDownloadersListChange);
  FDownloaderList.ChangeNotifier.Add(FDownloadersListListener);

  FTTLListener := TListenerTTLCheck.Create(Self.OnTTLTrim, 60000);
  FGCNotifier.Add(FTTLListener);
end;

destructor TTileRequestProcessorPool.Destroy;
begin
  TerminateDownloaders;

  if Assigned(FDownloaderList) and Assigned(FDownloadersListListener) then begin
    FDownloaderList.ChangeNotifier.Remove(FDownloadersListListener);
  end;
  if Assigned(FGCNotifier) and Assigned(FTTLListener) then begin
    FGCNotifier.Remove(FTTLListener);
  end;
  FTTLListener := nil;
  FGCNotifier := nil;
  FDownloadersListListener := nil;
  FDownloaderList := nil;

  FThreadArrayCS := nil;
  inherited;
end;

procedure TTileRequestProcessorPool.InitThreadsIfNeed;
var
  VThreadArray: TArrayOfThread;
  VDownloaderList: ITileDownloaderListStatic;
  i: Integer;
  VTileDownloaderSync: ITileDownloader;
begin
  FTTLListener.UpdateUseTime;

  FThreadArrayCS.BeginRead;
  try
    VThreadArray := FThreadArray;
  finally
    FThreadArrayCS.EndRead;
  end;

  if VThreadArray = nil then begin
    FThreadArrayCS.BeginWrite;
    try
      VThreadArray := FThreadArray;

      if VThreadArray = nil then begin
        VDownloaderList := FDownloaderList.GetStatic;
        if VDownloaderList <> nil then begin
          SetLength(VThreadArray, VDownloaderList.Count);
          for i := 0 to VDownloaderList.Count - 1 do begin
            VTileDownloaderSync := VDownloaderList.Item[i];
            if VTileDownloaderSync <> nil then begin
              VThreadArray[i] :=
                TTileRequestQueueProcessorThread.Create(
                  FThreadConfig,
                  FAppClosingNotifier,
                  FTileRequestQueue,
                  VTileDownloaderSync
                );
              VThreadArray[i].Start;
            end else begin
              VThreadArray[i] := nil;
            end;
          end;

          FThreadArray := VThreadArray;
        end;
      end;
    finally
      FThreadArrayCS.EndWrite;
    end;
  end;
end;

procedure TTileRequestProcessorPool.OnDownloadersListChange;
begin
  TerminateDownloaders;
end;

procedure TTileRequestProcessorPool.OnTTLTrim;
begin
  if not FTileRequestQueue.IsEmpty then begin
    FTTLListener.UpdateUseTime;
  end else begin
    TerminateDownloaders;
  end;
end;

procedure TTileRequestProcessorPool.TerminateDownloaders;
var
  I: Integer;
  VItem: IThread;
  VThreadArray: TArrayOfThread;
begin
  FThreadArrayCS.BeginWrite;
  try
    VThreadArray := FThreadArray;
    FThreadArray := nil;
  finally
    FThreadArrayCS.EndWrite;
  end;

  if VThreadArray <> nil then begin
    for I := 0 to Length(VThreadArray) - 1 do begin
      VItem := VThreadArray[I];
      VThreadArray[I] := nil;
      if VItem <> nil then begin
        VItem.Terminate;
        VItem := nil;
      end;
    end;
    VThreadArray := nil;
  end;
end;

end.
