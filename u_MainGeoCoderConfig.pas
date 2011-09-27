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

unit u_MainGeoCoderConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GeoCoderList,
  i_StringHistory,
  i_MainGeoCoderConfig,
  u_ConfigDataElementComplexBase;

type
  TMainGeoCoderConfig = class(TConfigDataElementComplexBase, IMainGeoCoderConfig)
  private
    FList: IGeoCoderList;
    FActiveGeoCoderGUID: TGUID;
    FSearchHistory: IStringHistory;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetSearchHistory: IStringHistory;
    function GetList: IGeoCoderList;
    function GetActiveGeoCoderGUID: TGUID;
    procedure SetActiveGeoCoderGUID(AValue: TGUID);
    function GetActiveGeoCoder: IGeoCoderListEntity;
  public
    constructor Create(AList: IGeoCoderList);
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_StringHistory;

{ TMainGeoCoderConfig }

constructor TMainGeoCoderConfig.Create(AList: IGeoCoderList);
var
  i: Cardinal;
begin
  inherited Create;
  FList := AList;
  if FList.GetGUIDEnum.Next(1, FActiveGeoCoderGUID, i) <> S_OK then begin
    raise Exception.Create('В списке геокодеров пусто');
  end;
  FSearchHistory := TStringHistory.Create;
  Add(FSearchHistory, TConfigSaveLoadStrategyBasicProviderSubItem.Create('History'));
end;

procedure TMainGeoCoderConfig.DoReadConfig(AConfigData: IConfigDataProvider);
var
  VGUID: TGUID;
  VGUIDStr: string;
begin
  inherited;
  if AConfigData <> nil then begin
    VGUID := CGUID_Zero;
    VGUIDStr := AConfigData.ReadString('GeoCoderGUID', '');
    if VGUIDStr <> '' then begin
      try
        VGUID := StringToGUID(VGUIDStr);
      except
        VGUID := CGUID_Zero;
      end;
    end;
    SetActiveGeoCoderGUID(VGUID);
  end;
end;

procedure TMainGeoCoderConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteString('GeoCoderGUID', GUIDToString(FActiveGeoCoderGUID));
end;

function TMainGeoCoderConfig.GetActiveGeoCoder: IGeoCoderListEntity;
begin
  LockRead;
  try
    Result := FList.Get(FActiveGeoCoderGUID);
  finally
    UnlockRead;
  end;
end;

function TMainGeoCoderConfig.GetActiveGeoCoderGUID: TGUID;
begin
  LockRead;
  try
    Result := FActiveGeoCoderGUID;
  finally
    UnlockRead;
  end;
end;

function TMainGeoCoderConfig.GetList: IGeoCoderList;
begin
  Result := FList;
end;

function TMainGeoCoderConfig.GetSearchHistory: IStringHistory;
begin
  Result := FSearchHistory;
end;

procedure TMainGeoCoderConfig.SetActiveGeoCoderGUID(AValue: TGUID);
begin
  if not IsEqualGUID(AValue, CGUID_Zero) then begin
    LockWrite;
    try
      if FList.Get(AValue) <> nil then begin
        if not IsEqualGUID(FActiveGeoCoderGUID, AValue) then begin
          FActiveGeoCoderGUID := AValue;
          SetChanged;
        end;
      end;
    finally
      UnlockWrite;
    end;
  end;
end;

end.
