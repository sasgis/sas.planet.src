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

unit u_TileDownloaderBaseCore;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  i_CoordConverterFactory,
  i_DownloadResultFactory,
  i_LanguageManager,
  i_InvisibleBrowser,
  i_LastResponseInfo,
  i_DownloadChecker,
  i_TileDownloadRequestBuilder,
  i_TileDownloadRequestBuilderConfig,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_ZmpInfo,
  u_TileDownloaderBaseThread;

type
  TDownloaderRec = record
    ThreadID: Cardinal;
    DownloaderThread: TTileDownloaderBaseThread;
    TileDownloadRequestBuilder: ITileDownloadRequestBuilder;
  end;

  TTileDownloaderBaseCore = class(TInterfacedObject, ITileDownloader)
  private
    FEnabled: Boolean;
    FZmp: IZmpInfo;
    FResultFactory: IDownloadResultFactory;
    FMaxConnectToServerCount: Cardinal;
    FTileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCoordConverterFactory: ICoordConverterFactory;
    FLangManager: ILanguageManager;
    FSemaphore: THandle;
    FDownloadesList: array of TDownloaderRec;
    FCS: TCriticalSection;
    procedure Lock;
    procedure UnLock;
    function CreateNewTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
    function TryGetDownloadThread: TTileDownloaderBaseThread;
    procedure OnThreadTTL(Sender: TObject; AThreadID: Cardinal);
    function GetIsEnabled: Boolean;
  public
    constructor Create(
      ATileDownloaderConfig: ITileDownloaderConfig;
      ATileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
      AZmp: IZmpInfo;
      ACoordConverterFactory: ICoordConverterFactory;
      ALangManager: ILanguageManager;
      AInvisibleBrowser: IInvisibleBrowser
    );
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent);
  end;

implementation

uses
  Dialogs,
  u_GlobalState,
  u_DownloadResultFactory,
  u_TileDownloadRequestBuilderPascalScript,
  u_ResStrings;

{ TTileDownloaderBaseCore }

constructor TTileDownloaderBaseCore.Create(
  ATileDownloaderConfig: ITileDownloaderConfig;
  ATileDownloadRequestBuilderConfig: ITileDownloadRequestBuilderConfig;
  AZmp: IZmpInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  ALangManager: ILanguageManager;
  AInvisibleBrowser: IInvisibleBrowser
);
var
  I: Integer;
begin
  inherited Create;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FTileDownloadRequestBuilderConfig := ATileDownloadRequestBuilderConfig;
  FZmp := AZmp;
  FCoordConverterFactory := ACoordConverterFactory;
  FLangManager := ALangManager;
  FCS := TCriticalSection.Create;
  FMaxConnectToServerCount := FTileDownloaderConfig.MaxConnectToServerCount;
  FResultFactory := TDownloadResultFactory.Create(
    GState.DownloadResultTextProvider   // TODO: Избавиться от GState
  );
  FSemaphore := CreateSemaphore(nil, FMaxConnectToServerCount, FMaxConnectToServerCount, nil);

  SetLength(FDownloadesList, FMaxConnectToServerCount);
  for I := 0 to Length(FDownloadesList) - 1 do begin
    FDownloadesList[I].ThreadID := 0;
    FDownloadesList[I].DownloaderThread := nil;
    FDownloadesList[I].TileDownloadRequestBuilder := nil;
  end;

  FEnabled := True;
end;

destructor TTileDownloaderBaseCore.Destroy;
var
  I: Integer;
begin
  try
    for I := 0 to Length(FDownloadesList) - 1 do
    try
      if Assigned(FDownloadesList[I].DownloaderThread) then begin
        FDownloadesList[I].DownloaderThread.Terminate;
        FDownloadesList[I].ThreadID := 0;
        FreeAndNil(FDownloadesList[I].DownloaderThread);
      end;
      if Assigned(FDownloadesList[I].TileDownloadRequestBuilder) then begin
        FDownloadesList[I].TileDownloadRequestBuilder := nil;
      end;
    except
      // ignore all
    end;
    SetLength(FDownloadesList, 0);
    FZmp := nil;
    FTileDownloadRequestBuilderConfig := nil;
    FTileDownloaderConfig := nil;
    FCoordConverterFactory := nil;
    FLangManager := nil;
  finally
    FSemaphore := 0;
    FreeAndNil(FCS);
    inherited Destroy;
  end;
end;

function TTileDownloaderBaseCore.CreateNewTileDownloadRequestBuilder: ITileDownloadRequestBuilder;
begin
  try
    Result := TTileDownloadRequestBuilderPascalScript.Create(
      FZmp,
      FTileDownloadRequestBuilderConfig,
      FTileDownloaderConfig,
      FZmp.DataProvider,
      FCoordConverterFactory,
      FLangManager
    );
    FEnabled := True;
  except
    on E: Exception do begin
      Result := nil;
      ShowMessageFmt(SAS_ERR_UrlScriptError, [FZmp.GUI.Name.GetDefault, E.Message, FZmp.FileName]);
    end;
  else
    Result := nil;
    ShowMessageFmt(SAS_ERR_UrlScriptUnexpectedError, [FZmp.GUI.Name.GetDefault, FZmp.FileName]);
  end;
  if Result = nil then begin
    FEnabled := False;
  end;
end;

function TTileDownloaderBaseCore.TryGetDownloadThread: TTileDownloaderBaseThread;

  function CreateNewDownloaderThread(I: Integer): TTileDownloaderBaseThread;
  begin
    if FDownloadesList[I].TileDownloadRequestBuilder = nil then begin
      FDownloadesList[I].TileDownloadRequestBuilder := CreateNewTileDownloadRequestBuilder;
    end;
    FDownloadesList[I].DownloaderThread :=
      TTileDownloaderBaseThread.Create(
        FResultFactory,
        Self.OnThreadTTL,
        FSemaphore,
        FDownloadesList[I].TileDownloadRequestBuilder,
        FTileDownloaderConfig
      );
    FDownloadesList[I].ThreadID := FDownloadesList[I].DownloaderThread.ThreadID;
    Result := FDownloadesList[I].DownloaderThread;
  end;

var
  I: Integer;
begin
  Result := nil;
  if WaitForSingleObject(FSemaphore, FTileDownloaderConfig.InetConfigStatic.TimeOut) = WAIT_OBJECT_0 then begin
    Lock;
    try
      for I := 0 to Length(FDownloadesList) - 1 do
      try
        if Assigned(FDownloadesList[I].DownloaderThread) then begin
          if not FDownloadesList[I].DownloaderThread.Busy then begin
            Result := FDownloadesList[I].DownloaderThread;
            Break;
          end;
        end else begin
          Result := CreateNewDownloaderThread(I);
          Break;
        end;
      except
        Result := nil;
      end;
    finally
      UnLock;
    end;
  end;
end;

procedure TTileDownloaderBaseCore.Download(AEvent: ITileDownloaderEvent);
var
  VDwnThr: TTileDownloaderBaseThread;
begin
  VDwnThr := TryGetDownloadThread;
  if Assigned(VDwnThr) then begin
    Lock;
    try
      VDwnThr.AddEvent(AEvent);
    finally
      UnLock;
    end;
  end else begin
    raise Exception.Create('No free connections!');
  end;
end;

procedure TTileDownloaderBaseCore.OnThreadTTL(Sender: TObject; AThreadID: Cardinal);
var
  I: Integer;
begin
  Lock;
  try
    for I := 0 to Length(FDownloadesList) - 1 do begin
      if FDownloadesList[I].ThreadID = AThreadID then begin
        if Assigned(FDownloadesList[I].DownloaderThread) then begin
          FDownloadesList[I].DownloaderThread.Terminate;
          FDownloadesList[I].ThreadID := 0;
          FDownloadesList[I].DownloaderThread := nil;
        end;
        Break;
      end;
    end;
  finally
    UnLock;
  end;
end;

function TTileDownloaderBaseCore.GetIsEnabled: Boolean;
begin
  Lock;
  try
    Result := FEnabled;
  finally
    Unlock;
  end;
end;

procedure TTileDownloaderBaseCore.Lock;
begin
  FCS.Acquire;
end;

procedure TTileDownloaderBaseCore.UnLock;
begin
  FCS.Release;
end;

end.
