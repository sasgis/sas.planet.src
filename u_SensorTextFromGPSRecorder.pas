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

unit u_SensorTextFromGPSRecorder;

interface

uses
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromGPSRecorder = class(TSensorBase, ISensorText)
  private
    FGPSRecorder: IGPSRecorder;
    FValueConverterConfig: IValueToStringConverterConfig;
    FValueConverter: IValueToStringConverter;

    FLastValue: Double;
    procedure OnConverterConfigChange(Sender: TObject);
    procedure OnRecorderChanged(Sender: TObject);
  protected
    property GPSRecorder: IGPSRecorder read FGPSRecorder;
    property ValueConverter: IValueToStringConverter read FValueConverter;

    function ValueToText(AValue: Double): string; virtual; abstract;
    function GetValue: Double; virtual; abstract;
  protected
    function GetText: string;
  public
    constructor Create(
      AGUID: TGUID;
      ACanReset: Boolean;
      ALanguageManager: ILanguageManager;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  SysUtils,
  u_NotifyEventListener;

{ TSensorTextFromGPSRecorder }

constructor TSensorTextFromGPSRecorder.Create(
  AGUID: TGUID;
  ACanReset: Boolean;
  ALanguageManager: ILanguageManager;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
begin
  inherited Create(AGUID, ACanReset, ISensorText, ALanguageManager);
  FGPSRecorder := AGPSRecorder;
  FValueConverterConfig := AValueConverterConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnRecorderChanged),
    FGPSRecorder.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConverterConfigChange),
    FValueConverterConfig.GetChangeNotifier
  );

  OnConverterConfigChange(nil);
  OnRecorderChanged(nil);
end;

function TSensorTextFromGPSRecorder.GetText: string;
var
  VValue: Double;
begin
  LockRead;
  try
    VValue := FLastValue;
    Result := ValueToText(VValue);
  finally
    UnlockRead;
  end;
end;

procedure TSensorTextFromGPSRecorder.OnConverterConfigChange(Sender: TObject);
begin
  LockWrite;
  try
    FValueConverter := FValueConverterConfig.GetStatic;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TSensorTextFromGPSRecorder.OnRecorderChanged(Sender: TObject);
var
  VValue: Double;
  VNeedNotify: Boolean;
begin
  VNeedNotify := False;
  VValue := GetValue;
  LockWrite;
  try
    if Abs(FLastValue - VValue) > 0.001 then begin
      FLastValue := VValue;
      VNeedNotify := True;
    end;
  finally
    UnlockWrite;
  end;
  if VNeedNotify then begin
    NotifyDataUpdate;
  end;
end;

end.
