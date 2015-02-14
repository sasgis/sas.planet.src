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

unit u_ActiveLayersConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ActiveMapsConfig,
  i_GUIDListStatic,
  u_ConfigDataElementBase;

type
  TActiveLayersConfig = class(TConfigDataElementBase, IActiveLayersConfig)
  private
    FLayerGuids: IGUIDSetStatic;
  protected
    function GetLayerGuids: IGUIDSetStatic;
    procedure SetLayerGuids(const AValue: IGUIDSetStatic);

    procedure InvertLayerSelectionByGUID(const AMapGUID: TGUID);
    procedure SelectLayerByGUID(const AMapGUID: TGUID);
    procedure UnSelectLayerByGUID(const AMapGUID: TGUID);
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  StrUtils,
  c_ZeroGUID,
  i_StringListStatic,
  u_GUIDListStatic;

const
  CKeyNameLayer = 'Layer';

{ TActiveLayersConfig }

constructor TActiveLayersConfig.Create;
begin
  inherited Create;
end;

procedure TActiveLayersConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VList: IStringListStatic;
  i: Integer;
  VKeyName: string;
  VGUIDString: string;
  VGUID: TGUID;
begin
  inherited;
  if AConfigData <> nil then begin
    VList := AConfigData.ReadValuesList;
    for i := 0 to VList.Count - 1 do begin
      VKeyName := VList.Items[i];
      if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
        VGUIDString := AConfigData.ReadString(VKeyName, '');
        if VGUIDString <> '' then begin
          try
            VGUID := StringToGUID(VGUIDString);
          except
            VGUID := CGUID_Zero;
          end;
        end else begin
          VGUID := CGUID_Zero;
        end;
        if not IsEqualGUID(VGUID, CGUID_Zero) then begin
          SelectLayerByGUID(VGUID);
        end;
      end;
    end;
  end;
end;

procedure TActiveLayersConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
var
  VList: IStringListStatic;
  i: Cardinal;
  VKeyName: string;
  VGUIDString: string;
  VIndex: Integer;
begin
  inherited;
  VList := AConfigData.ReadValuesList;
  for i := 0 to VList.Count - 1 do begin
    VKeyName := VList.Items[i];
    if SameText(LeftStr(VKeyName, length(CKeyNameLayer)), CKeyNameLayer) then begin
      AConfigData.DeleteValue(VKeyName);
    end;
  end;

  if Assigned(FLayerGuids) then begin
    VIndex := 0;
    for i := 0 to FLayerGuids.Count - 1 do begin
      VGUIDString := GUIDToString(FLayerGuids.Items[i]);
      AConfigData.WriteString(CKeyNameLayer + IntToStr(VIndex), VGUIDString);
      Inc(VIndex);
    end;
  end;
end;

function TActiveLayersConfig.GetLayerGuids: IGUIDSetStatic;
begin
  LockRead;
  try
    Result := FLayerGuids;
  finally
    UnlockRead;
  end;
end;

procedure TActiveLayersConfig.InvertLayerSelectionByGUID(const AMapGUID: TGUID);
begin
  LockWrite;
  try
    if not Assigned(FLayerGuids) or not FLayerGuids.IsExists(AMapGUID) then begin
      FLayerGuids := TGUIDSetStatic.CreateByAdd(FLayerGuids, AMapGUID);
    end else begin
      FLayerGuids := TGUIDSetStatic.CreateByRemove(FLayerGuids, AMapGUID);
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TActiveLayersConfig.SelectLayerByGUID(const AMapGUID: TGUID);
begin
  LockWrite;
  try
    if not Assigned(FLayerGuids) or not FLayerGuids.IsExists(AMapGUID) then begin
      FLayerGuids := TGUIDSetStatic.CreateByAdd(FLayerGuids, AMapGUID);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TActiveLayersConfig.SetLayerGuids(const AValue: IGUIDSetStatic);
begin
  LockWrite;
  try
    FLayerGuids := AValue;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TActiveLayersConfig.UnSelectLayerByGUID(const AMapGUID: TGUID);
begin
  LockWrite;
  try
    if Assigned(FLayerGuids) and FLayerGuids.IsExists(AMapGUID) then begin
      FLayerGuids := TGUIDSetStatic.CreateByRemove(FLayerGuids, AMapGUID);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
