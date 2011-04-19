unit u_TileDownloaderFrontEnd;

interface

uses
  Windows,
  Classes,
  SysUtils,
  Dialogs,
  i_AntiBan,
  i_IPoolOfObjectsSimple,
  i_ConfigDataProvider,
  i_TileDownlodSession;

type
  TTileDownloaderType = (dtBase, dtPlugin, dtUnknown);

  TTileDownloaderFrontEnd = class
  private
    FConfig: IConfigDataProvider;
    FZMPFileName: string;
    FMaxConnectToServerCount: Cardinal;
    FUseDwn: Boolean;
    FDownloaderType: TTileDownloaderType;
    FDownloaderStr: string;
    FBaseDownloaderEnabled: Boolean;
    FPluginDownloaderEnabled: Boolean;

    FPoolOfDownloaders: IPoolOfObjectsSimple;
    FTileDownlodSessionFactory: ITileDownlodSessionFactory;
    FAntiBan: IAntiBan;

    procedure CreateBaseDownloader;
    procedure BaseDownload(AParentThreadEvent: Pointer; AXY: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent);

    procedure CreatePluginDownloader;
    procedure PluginDownload(AParentThreadEvent: Pointer; AXY: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent);
  public
    constructor Create(AConfig: IConfigDataProvider; AZMPFileName: string);
    destructor Destroy; override;
    procedure Download(AParentThreadEvent: Pointer; AXY: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent);
  end;

implementation

uses
  i_ObjectWithTTL,
  i_PoolElement,
  u_AntiBanStuped,
  u_PoolOfObjectsSimple,
  u_GlobalState,
  u_TileDownloaderBaseFactory,
  u_UrlGenerator,
  u_ResStrings;

type
  TBaseDownloaderThreaded = class (TThread)
    FTile: TPoint;
    FZoom: Byte;
    FAntiBan: IAntiBan;
    FPoolOfDownloaders: IPoolOfObjectsSimple;
  protected
    procedure Execute; override;
  public
    property AntiBan: IAntiBan write FAntiBan;
    property PoolOfDownloaders: IPoolOfObjectsSimple write FPoolOfDownloaders;
  end;

{ TTileDownloaderFrontEnd }

constructor TTileDownloaderFrontEnd.Create(AConfig: IConfigDataProvider; AZMPFileName: string);
var
  VParams: IConfigDataProvider;
begin
  inherited Create;
  FConfig := AConfig;
  FZMPFileName := AZMPFileName;

  VParams := FConfig.GetSubItem('params.txt').GetSubItem('PARAMS');

  FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', 1);
  if FMaxConnectToServerCount > 64 then
    FMaxConnectToServerCount := 64;
  if FMaxConnectToServerCount <= 0 then
    FMaxConnectToServerCount := 1;

  FUseDwn := VParams.ReadBool('UseDwn',true);

  FDownloaderType := dtBase;
  FDownloaderStr := VParams.ReadString('Downloader', 'sasplanet');

  if LowerCase(FDownloaderStr) <> 'sasplanet' then
  begin
    if FileExists('.\plugins\' + FDownloaderStr + '.dll') then
    begin
      //TODO: try load plugin to mem
      FDownloaderType := dtPlugin;
    end
    else
      FDownloaderType := dtUnknown;
  end;

  case FDownloaderType of
    dtBase: CreateBaseDownloader;
    dtPlugin: CreatePluginDownloader;
    dtUnknown: raise Exception.Create('Unknown downloader!');
  end;

end;

destructor TTileDownloaderFrontEnd.Destroy;
begin
  FPoolOfDownloaders := nil;
  inherited Destroy;
end;

procedure TTileDownloaderFrontEnd.Download(AParentThreadEvent: Pointer; AXY: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent);
begin
  //
end;

{ BASE DOWNLOADER }

procedure TTileDownloaderFrontEnd.CreateBaseDownloader;
var
  VDownloader: TTileDownloaderFactory;
begin
  if FUseDwn then begin
    try
      VDownloader := TTileDownloaderFactory.Create(FConfig);
      FTileDownlodSessionFactory := VDownloader;
      FPoolOfDownloaders := TPoolOfObjectsSimple.Create(FMaxConnectToServerCount, VDownloader, 60000, 60000);
      GState.GCThread.List.AddObject(FPoolOfDownloaders as IObjectWithTTL);
      FAntiBan := TAntiBanStuped.Create(FConfig);
    except
      if ExceptObject <> nil then begin
        ShowMessageFmt(SAS_ERR_MapDownloadByError,[FZMPFileName, (ExceptObject as Exception).Message]);
      end;
      FTileDownlodSessionFactory := nil;
      FUseDwn := false;
    end;
  end;
  if FTileDownlodSessionFactory = nil then
    FTileDownlodSessionFactory := TTileDownloaderFactoryBase.Create(FConfig);
end;

procedure TTileDownloaderFrontEnd.BaseDownload(AParentThreadEvent: Pointer; AXY: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent);
var
  VDownloader: TBaseDownloaderThreaded;
begin
  VDownloader := TBaseDownloaderThreaded.Create(True);
  VDownloader.FreeOnTerminate := True;

  // TODO: передать все параметры в поток TBaseDownloaderThreaded

  VDownloader.Resume;
end;

{ PLUGIN DOWNLOADER }

procedure TTileDownloaderFrontEnd.CreatePluginDownloader;
begin
  // TODO
end;

procedure TTileDownloaderFrontEnd.PluginDownload(AParentThreadEvent: Pointer; AXY: TPoint; AZoom: Byte; ACheckTileSize: Boolean; AExistsFileSize: Cardinal; AOnDownloadEvent: TOnDownloadEvent);
begin
  // TODO
end;

{ TBaseDownloaderThreaded }

procedure TBaseDownloaderThreaded.Execute;
//var
//  StatusCode: Cardinal;
//  VPoolElement: IPoolElement;
//  VDownloader: ITileDownlodSession;
begin
  try

    //TODO: передать указатель на FAntiBan в VDownloader и оттуда выполнять FAntiBan.PreDownload и FAntiBan.PostCheckDownload

    {
    VPoolElement := FPoolOfDownloaders.TryGetPoolElement(60000);

    if VPoolElement = nil then begin
      raise Exception.Create('No free connections');
    end;

    VDownloader := VPoolElement.GetObject as ITileDownlodSession;

    AUrl := VDownloader.GetLink(FTile, FZoom);

    if FAntiBan <> nil then begin
      FAntiBan.PreDownload(VDownloader, FTile, FZoom, AUrl);
    end;

    Result := VDownloader.DownloadTile(ATile, AZoom, ACheckTileSize, AOldTileSize, fileBuf, StatusCode, AContentType);

    if FAntiBan <> nil then begin
      Result := FAntiBan.PostCheckDownload(VDownloader, ATile, AZoom, AUrl, Result, StatusCode, AContentType, fileBuf.Memory, fileBuf.Size);
    end;
    }
  finally
    Terminate;
  end;
end;


end.
