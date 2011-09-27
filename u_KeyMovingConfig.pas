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

unit u_KeyMovingConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_KeyMovingConfig,
  u_ConfigDataElementBase;

type
  TKeyMovingConfig = class(TConfigDataElementBase, IKeyMovingConfig)
  private
    FFirstKeyPressDelta: Double;
    FMinPixelPerSecond: Double;
    FMaxPixelPerSecond: Double;
    FSpeedChangeTime: Double;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetFirstKeyPressDelta: Double;
    procedure SetFirstKeyPressDelta(AValue: Double);

    function GetMinPixelPerSecond: Double;
    procedure SetMinPixelPerSecond(AValue: Double);

    function GetMaxPixelPerSecond: Double;
    procedure SetMaxPixelPerSecond(AValue: Double);

    function GetSpeedChangeTime: Double;
    procedure SetSpeedChangeTime(AValue: Double);
  public
    constructor Create;
  end;

implementation

{ TKeyMovingConfig }

constructor TKeyMovingConfig.Create;
begin
  inherited;
  FFirstKeyPressDelta := 30;
  FMinPixelPerSecond := 20;
  FMaxPixelPerSecond := 1024;
  FSpeedChangeTime := 3;
end;

procedure TKeyMovingConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FFirstKeyPressDelta := AConfigData.ReadFloat('FirstKeyPressDelta', FFirstKeyPressDelta);
    FMinPixelPerSecond := AConfigData.ReadFloat('MinPixelPerSecond', FMinPixelPerSecond);
    FMaxPixelPerSecond := AConfigData.ReadFloat('MaxPixelPerSecond', FMaxPixelPerSecond);
    FSpeedChangeTime := AConfigData.ReadFloat('SpeedChangeTime', FSpeedChangeTime);
    SetChanged;
  end;
end;

procedure TKeyMovingConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('FirstKeyPressDelta', FFirstKeyPressDelta);
  AConfigData.WriteFloat('MinPixelPerSecond', FMinPixelPerSecond);
  AConfigData.WriteFloat('MaxPixelPerSecond', FMaxPixelPerSecond);
  AConfigData.WriteFloat('SpeedChangeTime', FSpeedChangeTime);
end;

function TKeyMovingConfig.GetFirstKeyPressDelta: Double;
begin
  LockRead;
  try
    Result := FFirstKeyPressDelta;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetMaxPixelPerSecond: Double;
begin
  LockRead;
  try
    Result := FMaxPixelPerSecond;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetMinPixelPerSecond: Double;
begin
  LockRead;
  try
    Result := FMinPixelPerSecond;
  finally
    UnlockRead;
  end;
end;

function TKeyMovingConfig.GetSpeedChangeTime: Double;
begin
  LockRead;
  try
    Result := FSpeedChangeTime;
  finally
    UnlockRead;
  end;
end;

procedure TKeyMovingConfig.SetFirstKeyPressDelta(AValue: Double);
begin
  LockWrite;
  try
    if FFirstKeyPressDelta <> AValue then begin
      FFirstKeyPressDelta := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMaxPixelPerSecond(AValue: Double);
begin
  LockWrite;
  try
    if FMaxPixelPerSecond <> AValue then begin
      FMaxPixelPerSecond := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetMinPixelPerSecond(AValue: Double);
begin
  LockWrite;
  try
    if FMinPixelPerSecond <> AValue then begin
      FMinPixelPerSecond := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKeyMovingConfig.SetSpeedChangeTime(AValue: Double);
begin
  LockWrite;
  try
    if FSpeedChangeTime <> AValue then begin
      FSpeedChangeTime := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
