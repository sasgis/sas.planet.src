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

unit u_SensorTextFromNavToPoint;

interface

uses
  i_NavigationToPoint,
  i_ViewPortState,
  i_ValueToStringConverter,
  i_LocalCoordConverter,
  i_LanguageManager,
  i_Sensor,
  u_SensorBase;

type
  TSensorTextFromNavToPoint = class(TSensorBase, ISensorText)
  private
    FViewPortState: IViewPortState;
    FNavigationToPoint: INavigationToPoint;
    FVisualConverter: ILocalCoordConverter;
    FValueConverterConfig: IValueToStringConverterConfig;
    FValueConverter: IValueToStringConverter;

    procedure OnConverterConfigChange(Sender: TObject);
    procedure OnPosChanged(Sender: TObject);
    procedure OnNavToPointChanged(Sender: TObject);
  protected
    function GetCaptionTranslated: string; override;
    function GetDescriptionTranslated: string; override;
    function GetMenuItemNameTranslated: string; override;
  protected
    function GetText: string;
  public
    constructor Create(
      ALanguageManager: ILanguageManager;
      AViewPortState: IViewPortState;
      ANavigationToPoint: INavigationToPoint;
      AValueConverterConfig: IValueToStringConverterConfig
    );
  end;

implementation

uses
  SysUtils,
  c_SensorsGUIDSimple,
  t_GeoTypes,
  u_NotifyEventListener,
  u_ResStrings;

{ TSensorTextFromNavToPoint }

constructor TSensorTextFromNavToPoint.Create(
  ALanguageManager: ILanguageManager; AViewPortState: IViewPortState;
  ANavigationToPoint: INavigationToPoint;
  AValueConverterConfig: IValueToStringConverterConfig);
begin
  inherited Create(CSensorDistToMarkGUID, False, ISensorText, ALanguageManager);
  FViewPortState := AViewPortState;
  FNavigationToPoint := ANavigationToPoint;
  FValueConverterConfig := AValueConverterConfig;

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnPosChanged),
    FViewPortState.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnNavToPointChanged),
    FNavigationToPoint.GetChangeNotifier
  );

  LinksList.Add(
    TNotifyEventListener.Create(Self.OnConverterConfigChange),
    FValueConverterConfig.GetChangeNotifier
  );

  OnConverterConfigChange(nil);
end;

function TSensorTextFromNavToPoint.GetCaptionTranslated: string;
begin
  Result := SAS_STR_SensorNavToPointCaption;
end;

function TSensorTextFromNavToPoint.GetDescriptionTranslated: string;
begin
  Result := SAS_STR_SensorNavToPointDescription;
end;

function TSensorTextFromNavToPoint.GetMenuItemNameTranslated: string;
begin
  Result := SAS_STR_SensorNavToPointMenuItemName;
end;

function TSensorTextFromNavToPoint.GetText: string;
var
  VValue: Double;
  VNavActive: Boolean;
  VNavLonLat: TDoublePoint;
  VCenterLonLat: TDoublePoint;
begin
  LockRead;
  try
    Result := '-';
    if FVisualConverter <> nil then begin
      FNavigationToPoint.LockRead;
      try
        VNavActive := FNavigationToPoint.IsActive;
        VNavLonLat := FNavigationToPoint.LonLat;      
      finally
        FNavigationToPoint.UnlockRead;
      end;
      if VNavActive then begin
        VCenterLonLat := FVisualConverter.GetCenterLonLat;
        FVisualConverter.GetGeoConverter.CheckLonLatPos(VNavLonLat);
        FVisualConverter.GetGeoConverter.CheckLonLatPos(VCenterLonLat);
        VValue := FVisualConverter.GetGeoConverter.Datum.CalcDist(VNavLonLat, VCenterLonLat);
        Result := FValueConverter.DistConvert(VValue);
      end;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TSensorTextFromNavToPoint.OnConverterConfigChange(Sender: TObject);
begin
  LockWrite;
  try
    FValueConverter := FValueConverterConfig.GetStatic;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TSensorTextFromNavToPoint.OnNavToPointChanged(Sender: TObject);
begin
  NotifyDataUpdate;
end;

procedure TSensorTextFromNavToPoint.OnPosChanged(Sender: TObject);
begin
  LockWrite;
  try
    FVisualConverter := FViewPortState.GetVisualCoordConverter;
    NotifyDataUpdate;
  finally
    UnlockWrite;
  end;
end;

end.
