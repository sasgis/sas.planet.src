{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_DownloadUIConfig;

interface

uses
  t_CommonTypes,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_DownloadUIConfig,
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
    procedure SetUseDownload(const AValue: TTileSource);

    function GetTileMaxAgeInInternet: TDateTime;
    procedure SetTileMaxAgeInInternet(const AValue: TDateTime);

    function GetTilesOut: Integer;
    procedure SetTilesOut(const AValue: Integer);
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
  FTileMaxAgeInInternet := 1/24/60;
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
    SetChanged;
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

procedure TDownloadUIConfig.SetTileMaxAgeInInternet(const AValue: TDateTime);
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

procedure TDownloadUIConfig.SetTilesOut(const AValue: Integer);
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

procedure TDownloadUIConfig.SetUseDownload(const AValue: TTileSource);
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
