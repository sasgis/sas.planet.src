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

unit u_GPSConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GPSModuleByCOMPortConfig,
  i_GPSConfig,
  u_ConfigDataElementComplexBase;

type
  TGPSConfig = class(TConfigDataElementComplexBase, IGPSConfig)
  private
    FGPSEnabled: Boolean;
    FNoDataTimeOut: Integer;
    FWriteLog: Boolean;
    FLogPath: WideString;
    FModuleConfig: IGPSModuleByCOMPortConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetGPSEnabled: Boolean;
    procedure SetGPSEnabled(const AValue: Boolean);

    function GetNoDataTimeOut: Integer;
    procedure SetNoDataTimeOut(const AValue: Integer);

    function GetWriteLog: Boolean;
    procedure SetWriteLog(const AValue: Boolean);

    function GetLogPath: WideString;
    function GetModuleConfig: IGPSModuleByCOMPortConfig;
  public
    constructor Create(ALogPath: string);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_GPSModuleByCOMPortConfig;

{ TGPSConfig }

constructor TGPSConfig.Create(ALogPath: string);
begin
  inherited Create;
  FLogPath := ALogPath;
  FGPSEnabled := False;
  FWriteLog := True;
  FNoDataTimeOut := 5000;  
  FModuleConfig := TGPSModuleByCOMPortConfig.Create(FLogPath);
  Add(FModuleConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('Module'));
end;

procedure TGPSConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FGPSEnabled := AConfigData.ReadBool('Enabled', FGPSEnabled);
    FNoDataTimeOut := AConfigData.ReadInteger('NoDataTimeOut', FNoDataTimeOut);
    FWriteLog := AConfigData.ReadBool('LogWrite', FWriteLog);
    SetChanged;
  end;
end;

procedure TGPSConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Enabled', FGPSEnabled);
  AConfigData.WriteBool('LogWrite', FWriteLog);
  AConfigData.WriteInteger('NoDataTimeOut', FNoDataTimeOut);
end;

function TGPSConfig.GetGPSEnabled: Boolean;
begin
  LockRead;
  try
    Result := FGPSEnabled;
  finally
    UnlockRead;
  end;
end;

function TGPSConfig.GetLogPath: WideString;
begin
  Result := FLogPath;
end;

function TGPSConfig.GetModuleConfig: IGPSModuleByCOMPortConfig;
begin
  Result := FModuleConfig;
end;

function TGPSConfig.GetNoDataTimeOut: Integer;
begin
  LockRead;
  try
    Result := FNoDataTimeOut;
  finally
    UnlockRead;
  end;
end;

function TGPSConfig.GetWriteLog: Boolean;
begin
  LockRead;
  try
    Result := FWriteLog;
  finally
    UnlockRead;
  end;
end;

procedure TGPSConfig.SetGPSEnabled(const AValue: Boolean);
begin
  LockWrite;
  try
    if FGPSEnabled <> AValue then begin
      FGPSEnabled := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSConfig.SetNoDataTimeOut(const AValue: Integer);
begin
  LockWrite;
  try
    if FNoDataTimeOut <> AValue then begin
      FNoDataTimeOut := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGPSConfig.SetWriteLog(const AValue: Boolean);
begin
  LockWrite;
  try
    if FWriteLog <> AValue then begin
      FWriteLog := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
