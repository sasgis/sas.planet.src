unit u_DownloadUIConfig;

interface

uses
  t_CommonTypes,
  i_IConfigDataProvider,
  i_IConfigDataWriteProvider,
  i_IDownloadUIConfig,
  u_ConfigDataElementBase;

type
  TDownloadUIConfig = class(TConfigDataElementBase, IDownloadUIConfig)
  private
    FUseDownload: TTileSource;
    FTilesOut: Integer;
    FTileMaxAgeInInternet: TDateTime;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetUseDownload: TTileSource;
    procedure SetUseDownload(AValue: TTileSource);

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(AValue: TDateTime);

    function GetTilesOut: Integer;
    procedure SetTilesOut(AValue: Integer);
  public
    constructor Create;
  end;
implementation

{ TDownloadUIConfig }

constructor TDownloadUIConfig.Create;
begin
  inherited;
  FUseDownload := tsCache;
  FTilesOut := 0;
end;

procedure TDownloadUIConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    case AConfigData.ReadInteger('TileSource', 1) of
      0: FUseDownload := tsInternet;
      2: FUseDownload := tsCacheInternet;
    else
      FUseDownload := tsCache;
    end;
    FTileMaxAgeInInternet := AConfigData.ReadTime('TileMaxAgeInInternet', FTileMaxAgeInInternet);
    FTilesOut := AConfigData.ReadInteger('TilesOut', FTilesOut);
  end;
end;

procedure TDownloadUIConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  case FUseDownload of
    tsInternet: AConfigData.WriteInteger('TileSource', 0);
    tsCache: AConfigData.WriteInteger('TileSource', 1);
    tsCacheInternet: AConfigData.WriteInteger('TileSource', 2);
  end;
  AConfigData.WriteTime('TileMaxAgeInInternet', FTileMaxAgeInInternet);
  AConfigData.WriteInteger('TilesOut', FTilesOut);
end;

function TDownloadUIConfig.GetTileMaxAgeInInternet: TDateTime;
begin
  LockRead;
  try
    Result := FTileMaxAgeInInternet;
  finally
    UnlockRead;
  end;
end;

function TDownloadUIConfig.GetTilesOut: Integer;
begin
  LockRead;
  try
    Result := FTilesOut;
  finally
    UnlockRead;
  end;
end;

function TDownloadUIConfig.GetUseDownload: TTileSource;
begin
  LockRead;
  try
    Result := FUseDownload;
  finally
    UnlockRead;
  end;
end;

procedure TDownloadUIConfig.SetTileMaxAgeInInternet(AValue: TDateTime);
begin
  LockWrite;
  try
    if FTileMaxAgeInInternet <> AValue then begin
      FTileMaxAgeInInternet := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TDownloadUIConfig.SetTilesOut(AValue: Integer);
begin
  LockWrite;
  try
    if FTilesOut <> AValue then begin
      FTilesOut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TDownloadUIConfig.SetUseDownload(AValue: TTileSource);
begin
  LockWrite;
  try
    if FUseDownload <> AValue then begin
      FUseDownload := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
