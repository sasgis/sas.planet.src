unit u_TileDownloaderBaseCore;

interface

uses
  Windows,
  Classes,
  SysUtils,
  i_ConfigDataProvider,
  i_InetConfig,
  i_TileRequestBuilder,
  i_TileDownloader,
  i_ZmpInfo,
  u_TileDownloader,
  u_TileDownloaderBaseThread;

type
  TTileDownloaderBaseCore = class(TTileDownloader)
  private
    FSemaphore: THandle;
    FDownloadesList: TList;
    FRawResponseHeader: string;
    function CreateNewTileRequestBuilder(AConfig: IConfigDataProvider): ITileRequestBuilder;
    function TryGetDownloadThread: TTileDownloaderBaseThread;
  public
    constructor Create(
      AConfig: IConfigDataProvider;
      AInetConfig: IInetConfig;
      AZmp: IZmpInfo
    );
    destructor Destroy; override;
    function GetTileUrl(ATileXY: TPoint; AZoom: Byte): string; override;
    procedure Download(AEvent: ITileDownloaderEvent); override;
    procedure OnTileDownload(AEvent: ITileDownloaderEvent);
  end;

implementation

uses
  Dialogs,
  IniFiles,
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
  AInetConfig: IInetConfig;
  AZmp: IZmpInfo
);
begin
  inherited Create(AConfig, AInetConfig, AZmp);
  FTileRequestBuilder := CreateNewTileRequestBuilder(AConfig);
  FSemaphore := CreateSemaphore(nil, FMaxConnectToServerCount, FMaxConnectToServerCount, nil);
  FDownloadesList := TList.Create;
end;

destructor TTileDownloaderBaseCore.Destroy;
var
  I: Integer;
  VDwnThr: TTileDownloaderBaseThread;
begin
  try
    for I := 0 to FDownloadesList.Count - 1 do
    try
      VDwnThr := FDownloadesList.Items[I];
      if Assigned(VDwnThr) then begin
        VDwnThr.Terminate;
      end;
    except
      // ignore all
    end;
    FDownloadesList.Clear;
    FreeAndNil(FDownloadesList);
  finally
    FSemaphore := 0;
    inherited Destroy;
  end;
end;

function TTileDownloaderBaseCore.CreateNewTileRequestBuilder(AConfig: IConfigDataProvider): ITileRequestBuilder;

  function TryGetMapConfig: IConfigDataProvider;
  var
    VZmpMapConfig: IConfigDataProvider;
    VLocalMapsConfig: IConfigDataProvider;
    VLocalMapConfig: IConfigDataProvider;
    VIni: TMemIniFile;
  begin
    try
      Result := nil;
      if FileExists(FZmp.FileName) then begin
        VZmpMapConfig := TConfigDataProviderByKaZip.Create(FZmp.FileName);
      end else begin
        VZmpMapConfig := TConfigDataProviderByFolder.Create(FZmp.FileName);
      end;
      VIni := TMemIniFile.Create(GState.MapsPath + 'Maps.ini');
      VLocalMapsConfig := TConfigDataProviderByIniFile.Create(VIni);
      VLocalMapConfig := VLocalMapsConfig.GetSubItem(GUIDToString(FZmp.GUID));
      Result := TConfigDataProviderZmpComplex.Create(VZmpMapConfig, VLocalMapConfig);
    except
      Result := nil;
    end;
  end;  // TryGetMapConfig

var
  VMapConfig: IConfigDataProvider;
begin
  try
    VMapConfig := AConfig;
    if VMapConfig = nil then begin
      VMapConfig := TryGetMapConfig;
    end;
    Result := TTileRequestBuilderPascalScript.Create(FTileRequestBuilderConfig, VMapConfig);
    FEnabled := True;
  except
    on E: Exception do begin
      Result := nil;
      ShowMessageFmt(SAS_ERR_UrlScriptError, [FZmp.Name, E.Message, FZmp.FileName]);
    end;
  else
    Result := nil;
    ShowMessageFmt(SAS_ERR_UrlScriptUnexpectedError, [FZmp.Name, FZmp.FileName]);
  end;
  if Result = nil then begin
    FEnabled := False;
  end;
end;

function TTileDownloaderBaseCore.GetTileUrl(ATileXY: TPoint; AZoom: Byte): string;
begin
  if Assigned(FTileRequestBuilder) then begin
    Result := FTileRequestBuilder.BuildRequestUrl(ATileXY, AZoom, FZmp.VersionConfig.Version);
  end else begin
    Result := '';
  end;
end;

function TTileDownloaderBaseCore.TryGetDownloadThread: TTileDownloaderBaseThread;
var
  I: Integer;
begin
  Result := nil;
  if WaitForSingleObject(FSemaphore, FInetConfig.TimeOut) = WAIT_OBJECT_0 then begin
    Lock;
    try
      for I := 0 to FDownloadesList.Count - 1 do
      try
        Result := FDownloadesList.Items[I];
        if Assigned(Result) then begin
          if Result.Busy then begin
            Result := nil
          end else begin
            Break;
          end;
        end;
      except
        Result := nil;
      end;
      if not Assigned(Result) and (FDownloadesList.Count < integer(FMaxConnectToServerCount)) then begin
        Result := TTileDownloaderBaseThread.Create;
        Result.TileRequestBuilder := CreateNewTileRequestBuilder(nil);
        Result.TileDownloaderConfig := FTileDownloaderConfig;
        FDownloadesList.Add(Result);
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
      AEvent.AddToCallBackList(OnTileDownload);
      VDwnThr.TileDownloaderConfig := FTileDownloaderConfig;
      VDwnThr.RawResponseHeader := FRawResponseHeader;
      VDwnThr.Semaphore := FSemaphore;
      VDwnThr.AddEvent(AEvent);
    finally
      UnLock;
    end;
  end else begin
    raise Exception.Create('No free connections!');
  end;
end;

procedure TTileDownloaderBaseCore.OnTileDownload(AEvent: ITileDownloaderEvent);
begin
  Lock;
  try
    if Assigned(AEvent) then
    begin
      FRawResponseHeader := AEvent.RawResponseHeader;
    end;
  finally
    Unlock;
  end;
end;

end.
