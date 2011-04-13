unit u_TileDownloaderPhp;

interface

uses
  Windows,
  Classes,
  SysUtils,
  SyncObjs,
  i_ConfigDataProvider,
  i_CoordConverter,
  i_TileDownlodSession,
  t_GeoTypes,
  u_TileDownloaderPhpBase;

type
  PPhpPoolElement = ^TPhpPoolElement;

  TPhpPoolElement = record
    Php: TTileDownloaderPhpBase;
    isBusy: Boolean;
  end;

  TTileDownloaderPhp = class
    private
      FCoordConverter: ICoordConverterSimple;
      FEnableCoordConverter: Boolean;
      FURLBase: string;
      FZMPFileName: string;
      FMaxConnectToServerCount: Cardinal;
      FScriptPath: string;
      FScriptStr: string;
      FScriptEnabled: Boolean;
      FPoolElement: array of PPhpPoolElement;
      FScriptThreadSafe: TCriticalSection;

      procedure Initialize(AConfig: IConfigDataProvider);
      procedure PreparePoolElements(AConfig: IConfigDataProvider);
      function  TryGetPoolElement(ATimeOut: Cardinal): PPhpPoolElement;
      procedure ReleasePoolElement(APhpPoolElement: PPhpPoolElement);
      function  GetCustomParams(AXY: TPoint; AZoom: Byte): TCustomParams;
      procedure SetUrlBase(const AValue: string);
      procedure Lock;
      procedure Unlock;
    public
      constructor Create(AConfig: IConfigDataProvider; const ZmpFileName: string);
      destructor Destroy; override;
      function DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl, AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
      property Enabled: Boolean read FScriptEnabled;
      property UrlBase: string write SetURLBase;
  end;

implementation

uses
  u_GlobalState,
  u_GlobalPhpEngine;

const
  PhpScriptName = 'downloader.php';

{ TTileDownloaderPhp }

constructor TTileDownloaderPhp.Create(AConfig: IConfigDataProvider; const ZmpFileName: string);
begin
  inherited Create;
  FScriptThreadSafe := TCriticalSection.Create;
  FScriptEnabled := False;
  FURLBase := '';
  FScriptPath := '';
  FZMPFileName := ZmpFileName;
  FCoordConverter := nil;
  Initialize(AConfig);
  if FScriptEnabled then
    PreparePoolElements(AConfig);
end;

destructor TTileDownloaderPhp.Destroy;
var
  i: Integer;
begin
  try
    try
      for I := 0 to Length(FPoolElement) - 1 do
        FreeAndNil(FPoolElement[i].Php);
    finally
      for I := 0 to Length(FPoolElement) - 1 do
        System.Dispose(FPoolElement[i]);
    end;
  finally
    FreeAndNil(FScriptThreadSafe);
    inherited Destroy;
  end;
end;

procedure TTileDownloaderPhp.Initialize(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  VCoordConverter: ICoordConverter;
  VStream: TMemoryStream;
begin
  try
    VParams := AConfig.GetSubItem('params.txt').GetSubItem('PHP');
    if VParams <> nil then
    begin
      if VParams.ReadBool('Use', False) then
      begin
        if VParams.ReadBool('Preload', False) then
        begin
            VStream := TMemoryStream.Create;
          try
            AConfig.ReadBinaryStream(PhpScriptName, VStream);
            if VStream.Size > 0 then
            begin
              SetLength(FScriptStr, VStream.Size);
              VStream.Position := 0;
              VStream.ReadBuffer(FScriptStr[1], VStream.Size);
            end;
          finally
            FreeAndNil(VStream);
          end;
        end
        else
          if FileExists(FZMPFileName + '\' + PhpScriptName) then
            FScriptPath := FZMPFileName + '\' + PhpScriptName;

        if (FScriptStr <> '') or ( FScriptPath <> '') then
        begin
          FEnableCoordConverter := VParams.ReadBool('CustomParams', False);

          if Assigned(GPhpEngine) then
            FScriptEnabled := GPhpEngine.Initialize(VParams.ReadBool('Debug', False));


          if FScriptEnabled and FEnableCoordConverter then
          begin
            VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
            if VParams <> nil then
            begin
              VCoordConverter := GState.CoordConverterFactory.GetCoordConverterByConfig(VParams);
              FCoordConverter := VCoordConverter as ICoordConverterSimple;
            end;
          end;
        end;
      end;
    end;
  except
    FScriptEnabled := False;
  end;
end;

procedure TTileDownloaderPhp.SetUrlBase(const AValue: string);
begin
    Lock;
  try
    FURLBase := AValue;
  finally
    Unlock;
  end;
end;

procedure TTileDownloaderPhp.PreparePoolElements(AConfig: IConfigDataProvider);
var
  VParams: IConfigDataProvider;
  i: Integer;
begin
  FMaxConnectToServerCount := 0;
  VParams := AConfig.GetSubItem('params.txt').GetSubItem('PARAMS');
  if VParams <> nil then
  begin
    FMaxConnectToServerCount := VParams.ReadInteger('MaxConnectToServerCount', 1);
    if FMaxConnectToServerCount > 64 then
      FMaxConnectToServerCount := 64
    else
      if FMaxConnectToServerCount <= 0 then
        FMaxConnectToServerCount := 1;
  end;

  if FMaxConnectToServerCount > 0 then
  begin
    SetLength(FPoolElement, FMaxConnectToServerCount);
    for i := 0 to Length(FPoolElement) - 1 do
    begin
      FPoolElement[i] := System.New(PPhpPoolElement);
      FPoolElement[i].Php := TTileDownloaderPhpBase.Create(FScriptPath, FScriptStr, FEnableCoordConverter);
      FPoolElement[i].isBusy := False;
    end;
  end
  else
    FScriptEnabled := False;
end;

function TTileDownloaderPhp.TryGetPoolElement(ATimeOut: Cardinal): PPhpPoolElement;
var
  i: Integer;
  VSelectStart: Cardinal;
begin
  VSelectStart := GetTickCount;
  repeat
    Result := nil;
    Lock;
    try
      for i := 0 to Length(FPoolElement) - 1 do
      if not FPoolElement[i].isBusy then
      begin
        FPoolElement[i].isBusy := True;
        Result := FPoolElement[i];
        Break;
      end;
    finally
      Unlock;
    end;

    if Result <> nil then
      Break
    else
      Sleep(1000);

    if GetTickCount - VSelectStart > ATimeOut then
      Break;
  until False;
end;

procedure TTileDownloaderPhp.ReleasePoolElement(APhpPoolElement: PPhpPoolElement);
begin
  if APhpPoolElement <> nil then
  begin
      Lock;
    try
      APhpPoolElement.isBusy := False;
    finally
      Unlock;
    end;
  end;
end;

function TTileDownloaderPhp.DownloadTile(ACheckTileSize: Boolean; AOldTileSize: Cardinal; ATile: TPoint; AZoom: Byte; out AUrl: string; out AContentType: string; fileBuf: TMemoryStream): TDownloadTileResult;
var
  VDownloader: PPhpPoolElement;
  VErrStr: string;
begin
  VErrStr := '';
  VDownloader := TryGetPoolElement(60000);
  if VDownloader <> nil then
  try
    VDownloader.Php.Tile := ATile;
    VDownloader.Php.Zoom := AZoom + 1;

    if ACheckTileSize then
      VDownloader.Php.OldTileSize := AOldTileSize
    else
      VDownloader.Php.OldTileSize := -1;

    VDownloader.Php.FileBuf := fileBuf;

      Lock;
    try
      VDownloader.Php.Url := FURLBase;
      VDownloader.Php.SetProxyConfig(GState.InetConfig);
      VDownloader.Php.Language := GState.LanguageManager.GetCurrentLanguageCode;
      if FEnableCoordConverter and Assigned(FCoordConverter) then
        VDownloader.Php.SetCustomParams(Self.GetCustomParams(ATile, AZoom));
    finally
      Unlock;
    end;

    VDownloader.Php.SetMainParams;

    Result := TDownloadTileResult( VDownloader.Php.Exec(VErrStr) );

    if Result <> dtrUnknownError then
    begin
      AUrl := VDownloader.Php.Url;
      AContentType := VDownloader.Php.MIMEType;
    end;
  finally
    ReleasePoolElement(VDownloader);
  end
  else
    raise Exception.Create('PHP: No free connections');

  if VErrStr <> '' then
      raise Exception.Create(VErrStr);
end;

function TTileDownloaderPhp.GetCustomParams(AXY: TPoint; AZoom: Byte): TCustomParams;
var
  XY: TPoint;
  LL: TDoublePoint;
begin
  ZeroMemory(@result, SizeOf(TCustomParams));
  if Assigned(FCoordConverter) then
  begin
    LL := FCoordConverter.Pos2LonLat(AXY, AZoom);
    Result.LLon := LL.X;
    Result.TLat := LL.Y;

    LL := FCoordConverter.LonLat2Metr(LL);
    Result.LMetr := LL.X;
    Result.TMetr := LL.Y;

    XY := AXY;
    Inc(XY.X);
    Inc(XY.Y);

    LL := FCoordConverter.Pos2LonLat(XY, AZoom);
    Result.RLon := LL.X;
    Result.BLat := LL.Y;

    LL := FCoordConverter.LonLat2Metr(LL);
    Result.RMetr := LL.X;
    Result.BMetr := LL.Y;
  end;
end;

procedure TTileDownloaderPhp.Lock;
begin
  FScriptThreadSafe.Acquire;
end;

procedure TTileDownloaderPhp.Unlock;
begin
  FScriptThreadSafe.Release;
end;

end.
