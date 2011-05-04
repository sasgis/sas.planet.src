unit u_TileDownloaderBaseCore;

interface

uses
  SysUtils,
  i_ConfigDataProvider,
  i_RequestBuilderScript,
  i_TileDownloader,
  u_RequestBuilderScript,
  u_RequestBuilderPascalScript,
  u_TileDownloader,
  u_TileDownloaderBaseThread;

type
  TTileDownloaderBaseCore = class(TTileDownloader)
  private
    FDownloadThread: TTileDownloaderBaseThread;
    FRawResponseHeader: string;
    procedure LoadRequestBuilderScript(AConfig: IConfigDataProvider);
  public
    constructor Create(AConfig: IConfigDataProvider; AZmpFileName: string);
    destructor Destroy; override;
    procedure Download(AEvent: ITileDownloaderEvent); override;
    procedure OnTileDownload(AEvent: ITileDownloaderEvent);
  end;

implementation

uses
  Dialogs,
  u_ResStrings;

{ TTileDownloaderBaseCore }

constructor TTileDownloaderBaseCore.Create(AConfig: IConfigDataProvider; AZmpFileName: string);
begin
  inherited Create(AConfig, AZmpFileName);
  LoadRequestBuilderScript(AConfig);
  FDownloadThread := TTileDownloaderBaseThread.Create;
end;

destructor TTileDownloaderBaseCore.Destroy;
begin
  FDownloadThread.Terminate;
  inherited Destroy;
end;

procedure TTileDownloaderBaseCore.LoadRequestBuilderScript(AConfig: IConfigDataProvider);
begin
  try
    FRequestBuilderScript := TRequestBuilderPascalScript.Create(AConfig);
    FEnabled := True;
  except
    on E: Exception do
    begin
      FRequestBuilderScript := nil;
      ShowMessageFmt(SAS_ERR_UrlScriptError, [FMapName, E.Message, FZmpFileName]);
    end;
  else
    FRequestBuilderScript := nil;
    ShowMessageFmt(SAS_ERR_UrlScriptUnexpectedError, [FMapName, FZmpFileName]);
  end;
  if FRequestBuilderScript = nil then
  begin
    FEnabled := False;
    FRequestBuilderScript := TRequestBuilderScript.Create(AConfig);
  end;
end;

procedure TTileDownloaderBaseCore.Download(AEvent: ITileDownloaderEvent);
begin
  Lock;
  try
    repeat
      if not FDownloadThread.Busy then
      begin
        AEvent.AddToCallBackList(OnTileDownload);
        FDownloadThread.TimeOut := FTimeOut;
        FDownloadThread.RawResponseHeader := FRawResponseHeader;
        FDownloadThread.AddEvent(AEvent);
        Break;
      end;
      Sleep(30);
    until False;
  finally
    Unlock;
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
