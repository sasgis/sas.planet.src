{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
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
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MainActiveMap;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapTypes,
  i_MapTypeSet,
  i_ActiveMapsConfig,
  u_ConfigDataElementComplexBase,
  u_NotifyWithGUIDEvent;

type
  TMainActiveMap = class(TConfigDataElementComplexBase, IMainActiveMap)
  private
    FIsAllowNil: Boolean;
    FMapsSet: IMapTypeSet;
    FMainMapChangeNotyfier: INotifierWithGUID;
    FActiveMap: IMapTypeChangeable;
  protected
    property MainMapChangeNotyfier: INotifierWithGUID read FMainMapChangeNotyfier;
  protected
    procedure SelectMainByGUID(const AMapGUID: TGUID);
    function GetActiveMap: IMapTypeChangeable;

    function GetMapsSet: IMapTypeSet;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(
      const AIsAllowNil: Boolean;
      const AMapsSet: IMapTypeSet;
      const AMainMapChangeNotyfier: INotifierWithGUID = nil
    );
  end;

implementation

uses
  SysUtils,
  ActiveX,
  c_ZeroGUID,
  u_ActiveMapConfig;

const
  CKeyNameMap = 'Map';

{ TMainActiveMap }

constructor TMainActiveMap.Create(
  const AIsAllowNil: Boolean;
  const AMapsSet: IMapTypeSet;
  const AMainMapChangeNotyfier: INotifierWithGUID
);
begin
  inherited Create;
  FIsAllowNil := AIsAllowNil;
  FMapsSet := AMapsSet;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  if FMainMapChangeNotyfier = nil then begin
    FMainMapChangeNotyfier := TNotifierWithGUID.Create;
  end;

  FActiveMap := TMapTypeChangeableByNotifier.Create(FIsAllowNil, FMainMapChangeNotyfier, FMapsSet);
  Add(FActiveMap);
end;

procedure TMainActiveMap.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
  inherited;
  VGUID := CGUID_Zero;
  if AConfigData <> nil then begin
    VGUIDString := AConfigData.ReadString(CKeyNameMap, '');
    if VGUIDString <> '' then begin
      try
        VGUID := StringToGUID(VGUIDString);
      except
        VGUID := CGUID_Zero;
      end;
    end;
  end;
  if IsEqualGUID(VGUID, CGUID_Zero) then begin
    if FIsAllowNil then begin
      SelectMainByGUID(VGUID);
    end;
  end else begin
    if FMapsSet.GetMapTypeByGUID(VGUID) <> nil then begin
      SelectMainByGUID(VGUID);
    end;
  end;
end;

procedure TMainActiveMap.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  VMapType: IMapType;
  VGUID: TGUID;
  VGUIDString: string;
begin
  inherited;
  VGUID := CGUID_Zero;
  VMapType := FActiveMap.GetStatic;
  if Assigned(VMapType) then begin
    VGUID := VMapType.GUID;
  end;
  VGUIDString := GUIDToString(VGUID);
  AConfigData.WriteString(CKeyNameMap, VGUIDString);
end;

function TMainActiveMap.GetActiveMap: IMapTypeChangeable;
begin
  Result := FActiveMap;
end;

function TMainActiveMap.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

procedure TMainActiveMap.SelectMainByGUID(const AMapGUID: TGUID);
begin
  if (FMapsSet.GetMapTypeByGUID(AMapGUID) <> nil) or (FIsAllowNil and IsEqualGUID(AMapGUID, CGUID_Zero)) then begin
    LockWrite;
    try
      FMainMapChangeNotyfier.NotifyByGUID(AMapGUID);
    finally
      UnlockWrite;
    end;
  end;
end;

end.
