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

unit u_SensorListStuped;

interface

uses
  i_NavigationToPoint,
  i_ViewPortState,
  i_GPSRecorder,
  i_ValueToStringConverter,
  i_LanguageManager,
  u_SensorListBase;

type
  TSensorListStuped = class(TSensorListBase)
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AViewPortState: IViewPortState;
      ANavigationToPoint: INavigationToPoint;
      AGPSRecorder: IGPSRecorder;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  i_SensorList,
  u_SensorTextFromNavToPoint,
  u_SensorBatteryStatus,
  u_SensorsFromGPSRecorder;


{ TSensorListStuped }

constructor TSensorListStuped.Create(
  ALanguageManager: ILanguageManager;
  AViewPortState: IViewPortState;
  ANavigationToPoint: INavigationToPoint;
  AGPSRecorder: IGPSRecorder;
  AValueConverterConfig: IValueToStringConverterConfig
);
var
  VEntity: ISensorListEntity;
begin
  inherited Create;
  VEntity := TSensorFromGPSRecorderLastSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderAvgSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderMaxSpeed.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderDist.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderOdometer1.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderOdometer2.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorTextFromNavToPoint.Create(ALanguageManager, AViewPortState, ANavigationToPoint, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorBatteryStatus.Create(ALanguageManager);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderAltitude.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);

  VEntity := TSensorFromGPSRecorderHeading.Create(ALanguageManager, AGPSRecorder, AValueConverterConfig);
  Self.Add(VEntity);
end;

end.
