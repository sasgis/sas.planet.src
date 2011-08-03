unit u_TileDownloaderBaseCore;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
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
  TTileDownloaderBaseCore = class(TInterfacedObject, ITileDownloader)
  private
    FEnabled: Boolean;
    FZmp: IZmpInfo;
    FMaxConnectToServerCount: Cardinal;
    FTileRequestBuilderConfig: ITileRequestBuilderConfig;
    FTileDownloaderConfig: ITileDownloaderConfig;
    FCoordConverterFactory: ICoordConverterFactory;
    FLangManager: ILanguageManager;
    FSemaphore: THandle;
    FDownloadesList: TList;
    FRawResponseHeader: string;
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
    procedure OnTileDownload(AEvent: ITileDownloaderEvent);
    property Enabled: Boolean read GetIsEnabled;
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
  ATileDownloaderConfig: ITileDownloaderConfig;
  ATileRequestBuilderConfig: ITileRequestBuilderConfig;
  AZmp: IZmpInfo;
  ACoordConverterFactory: ICoordConverterFactory;
  ALangManager: ILanguageManager
);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FTileDownloaderConfig := ATileDownloaderConfig;
  FTileRequestBuilderConfig := ATileRequestBuilderConfig;
  FZmp := AZmp;
  FCoordConverterFactory := ACoordConverterFactory;
  FLangManager := ALangManager;
  FCS := TCriticalSection.Create;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  FTileRequestBuilderConfig.ReadConfig(VParams);
  FTileDownloaderConfig.ReadConfig(VParams);
  FMaxConnectToServerCount := FTileDownloaderConfig.MaxConnectToServerCount;
  FSemaphore := CreateSemaphore(nil, FMaxConnectToServerCount, FMaxConnectToServerCount, nil);
  FDownloadesList := TList.Create;
  FEnabled := True;
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

function TTileDownloaderBaseCore.CreateNewTileRequestBuilder: ITileRequestBuilder;
begin
  try
    Result := TTileRequestBuilderPascalScript.Create(
      FTileRequestBuilderConfig,
      FZmp.DataProvider,
      FCoordConverterFactory,
      FLangManager
    );
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

function TTileDownloaderBaseCore.TryGetDownloadThread: TTileDownloaderBaseThread;
var
  I: Integer;
begin
  Result := nil;
  if WaitForSingleObject(FSemaphore, FTileDownloaderConfig.InetConfigStatic.TimeOut) = WAIT_OBJECT_0 then begin
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
        Result.TileRequestBuilder := CreateNewTileRequestBuilder;
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
    if Assigned(AEvent) then begin
      FRawResponseHeader := AEvent.RawResponseHeader;
    end;
  finally
    Unlock;
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
