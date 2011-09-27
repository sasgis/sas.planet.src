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

unit u_SensorBatteryStatus;

interface

uses
  Windows,
  ExtCtrls,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorBatteryStatus = class(TSensorBase, ISensorText)
  private
    FStatusText: string;
    FTimer: TTimer;
    procedure OnTimer(Sender: TObject);
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  protected
    function GetText: string;
  public
    constructor Create(
      ALanguageManager: ILanguageManager
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  c_SensorsGUIDSimple,
  u_ResStrings;

{ TSensorBatteryStatus }

constructor TSensorBatteryStatus.Create(ALanguageManager: ILanguageManager);
begin
  inherited Create(CSensorBatteryGUID, False, ISensorText, ALanguageManager);
  FStatusText := '-';
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 1000;
  FTimer.OnTimer := Self.OnTimer;
  FTimer.Enabled := True;
end;

destructor TSensorBatteryStatus.Destroy;
begin
  FreeAndNil(FTimer);
  inherited;
end;

function TSensorBatteryStatus.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorBatteryStatusCaption;
end;

function TSensorBatteryStatus.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorBatteryStatusDescription;
end;

function TSensorBatteryStatus.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorBatteryStatusMenuItemName;
end;

function TSensorBatteryStatus.GetText: string;
begin
  LockRead;
  try
    Result := FStatusText
  finally
    UnlockRead;
  end;
end;

procedure TSensorBatteryStatus.OnTimer(Sender: TObject);
var
  sps: _SYSTEM_POWER_STATUS;
  VResult: string;
  VDataUpdate: Boolean;
begin
  GetSystemPowerStatus(sps);
  if sps.ACLineStatus=0 then begin
    case sps.BatteryFlag of
    128: begin
      VResult := SAS_STR_BattaryStateOnLine;
    end;
    8: begin
      VResult := SAS_STR_BattaryStateCharge;
    end;
    else
      if sps.BatteryLifePercent=255 then begin
        VResult := SAS_STR_BattaryStateUnknown
      end else begin
        VResult := inttostr(sps.BatteryLifePercent)+'%';
      end;
    end
  end else begin
    VResult := SAS_STR_BattaryStateOnLine;
  end;
  VDataUpdate := False;
  LockWrite;
  try
    if FStatusText <> VResult then begin
      FStatusText := VResult;
      VDataUpdate := True;
    end;
  finally
    UnlockWrite;
  end;
  if VDataUpdate then begin
    NotifyDataUpdate;
  end;
end;

end.
