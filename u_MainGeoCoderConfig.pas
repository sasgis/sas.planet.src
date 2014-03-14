{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
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
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetSearchHistory: IStringHistory;
    function GetList: IGeoCoderList;
    function GetActiveGeoCoderGUID: TGUID;
    procedure SetActiveGeoCoderGUID(const AValue: TGUID);
    function GetActiveGeoCoder: IGeoCoderListEntity;
  public
    constructor Create(const AList: IGeoCoderList);
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  u_ConfigSaveLoadStrategyBasicProviderSubItem,
  u_StringHistory;

{ TMainGeoCoderConfig }

constructor TMainGeoCoderConfig.Create(const AList: IGeoCoderList);
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

procedure TMainGeoCoderConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
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
  const AConfigData: IConfigDataWriteProvider
);
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

procedure TMainGeoCoderConfig.SetActiveGeoCoderGUID(const AValue: TGUID);
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
