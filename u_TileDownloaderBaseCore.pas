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
  i_AntiBan,
  i_ConfigDataProvider,
  i_CoordConverterFactory,
  i_LanguageManager,
  i_TileRequestBuilder,
  i_TileRequestBuilderConfig,
  i_TileDownloader,
  i_TileDownloaderConfig,
  i_ZmpInfo,
  u_TileDownloaderBaseThread;

type
  TDownloaderRec = record
    ThreadID: Cardinal;
    DownloaderThread: TTileDownloaderBaseThread;
    TileRequestBuilder: ITileRequestBuilder;
  end;

  TTileDownloaderBaseCore = class(TInterfacedObject, ITileDownloader)
  private
    FEnabled: Boolean;
    FZmp: IZmpInfo;
    FAntiBan: IAntiBan;
    FMaxConnectToServerCount: Cardinal;
    FTileRequestBuilderConfig: ITileRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCoordConverterFactory: ICoordConverterFactory;
    FLangManager: ILanguageManager;
    FSemaphore: THandle;
    FDownloadesList: array of TDownloaderRec;
    FCS: TCriticalSection;
    procedure Lock;
    procedure UnLock;
    function CreateNewTileRequestBuilder: ITileRequestBuilder;
    function TryGetDownloadThread: TTileDownloaderBaseThread;
  public
    constructor Create(
      AConfig: IConfigDataProvider;
      ATileDownloaderConfig: ITileDownloaderConfig;
      ATileRequestBuilderConfig: ITileRequestBuilderConfig;
      AZmp: IZmpInfo;
      ACoordConverterFactory: ICoordConverterFactory;
      ALangManager: ILanguageManager
    );
    destructor Destroy; override;
    function GetIsEnabled: Boolean;
    procedure Download(AEvent: ITileDownloaderEvent);
    procedure OnThreadTTL(Sender: TObject; AThreadID: Cardinal);
    property Enabled: Boolean read GetIsEnabled;
  end;

implementation

uses
  Dialogs,
  IniFiles,
  u_AntiBanStuped,
  u_GlobalState,
  u_ConfigDataProviderByKaZip,
  u_ConfigDataProviderByFolder,
  u_ConfigDataProviderByIniFile,
  u_ConfigDataProviderZmpComplex,
  u_TileRequestBuilder,
  u_TileRequestBuilderPascalScript,
  u_ResStrings;

{ TTileDownloaderBaseCore }

constructor TTileDownloaderBaseCore.Create(
  AConfig: IConfigDataProvider;
  ATileDownloaderConfig: ITileDownloaderConfig;
  ATileRequestBuilderConfig: ITileRequestBuilderConfig;
  AZmp: IZmpInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  ALangManager: ILanguageManager
);
var
  I: Integer;
begin
  inherited Create;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FTileRequestBuilderConfig := ATileRequestBuilderConfig;
  FZmp := AZmp;
  FAntiBan := TAntiBanStuped.Create(FZmp.DataProvider);
  FCoordConverterFactory := ACoordConverterFactory;
  FLangManager := ALangManager;
  FCS := TCriticalSection.Create;
  FTileRequestBuilderConfig.ReadConfig(AConfig);
  FTileDownloaderConfig.ReadConfig(AConfig);
  FMaxConnectToServerCount := FTileDownloaderConfig.MaxConnectToServerCount;

  // В целях упрощения отладки, жёстко задаём число потоков для карты,
  // иначе, это число берётся из zmp карты, из параметра MaxConnectToServerCount (см. выше)
  FMaxConnectToServerCount := 12;
  // --

  FSemaphore := CreateSemaphore(nil, FMaxConnectToServerCount, FMaxConnectToServerCount, nil);

  SetLength(FDownloadesList, FMaxConnectToServerCount);
  for I := 0 to Length(FDownloadesList) - 1 do begin
    FDownloadesList[I].ThreadID := 0;
    FDownloadesList[I].DownloaderThread := nil;
    FDownloadesList[I].TileRequestBuilder := nil;
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
      if Assigned(FDownloadesList[I].TileRequestBuilder) then begin
        FDownloadesList[I].TileRequestBuilder := nil;
      end;
    except
      // ignore all
    end;
    SetLength(FDownloadesList, 0);
    FAntiBan := nil;
    FZmp := nil;
    FTileRequestBuilderConfig := nil;
    FTileDownloaderConfig := nil;
    FCoordConverterFactory := nil;
    FLangManager := nil;
  finally
    FSemaphore := 0;
    FreeAndNil(FCS);
    inherited Destroy;
  end;
end;

function TTileDownloaderBaseCore.CreateNewTileRequestBuilder: ITileRequestBuilder;
begin
  try
    Result := TTileRequestBuilderPascalScript.Create(
      FZmp,
      FTileRequestBuilderConfig,
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
    FDownloadesList[I].DownloaderThread := TTileDownloaderBaseThread.Create(FAntiBan);
    FDownloadesList[I].ThreadID := FDownloadesList[I].DownloaderThread.ThreadID;
    if FDownloadesList[I].TileRequestBuilder = nil then begin
      FDownloadesList[I].TileRequestBuilder := CreateNewTileRequestBuilder;
    end;
    FDownloadesList[I].DownloaderThread.TileRequestBuilder := FDownloadesList[I].TileRequestBuilder;
    FDownloadesList[I].DownloaderThread.TileDownloaderConfig := FTileDownloaderConfig;
    FDownloadesList[I].DownloaderThread.OnTTL := Self.OnThreadTTL;
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
      VDwnThr.Semaphore := FSemaphore;
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
