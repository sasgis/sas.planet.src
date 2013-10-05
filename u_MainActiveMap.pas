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
  i_GUIDSet,
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
    FMapsSet: IMapTypeSet;
    FMainMapChangeNotyfier: INotifierWithGUID;
    FActiveMap: IMapTypeChangeable;
    FMapSingleSet: IActiveMapSingleSet;
  protected
    property MainMapChangeNotyfier: INotifierWithGUID read FMainMapChangeNotyfier;
  protected
    procedure SelectMainByGUID(const AMapGUID: TGUID);
    function GetActiveMap: IMapTypeChangeable;

    function GetMapSingleSet: IActiveMapSingleSet;
    function GetMapsSet: IMapTypeSet;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(
      const AMapsSet: IMapTypeSet;
      const AMainMapChangeNotyfier: INotifierWithGUID = nil;
      const AMapSingleSet: IActiveMapSingleSet = nil
    );
  end;

implementation

uses
  SysUtils,
  ActiveX,
  u_GUIDInterfaceSet,
  u_ActiveMapSingleAbstract,
  u_ActiveMapSingleSet,
  u_ActiveMapConfig;

const
  CKeyNameMap = 'Map';

{ TMainActiveMap }

constructor TMainActiveMap.Create(
  const AMapsSet: IMapTypeSet;
  const AMainMapChangeNotyfier: INotifierWithGUID;
  const AMapSingleSet: IActiveMapSingleSet
);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VSingleMap: IActiveMapSingle;
  VSingleSet: IGUIDInterfaceSet;
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FMainMapChangeNotyfier := AMainMapChangeNotyfier;
  FMapSingleSet := AMapSingleSet;
  if FMainMapChangeNotyfier = nil then begin
    FMainMapChangeNotyfier := TNotifierWithGUID.Create;
  end;
  if FMapSingleSet = nil then begin
    VSingleSet := TGUIDInterfaceSet.Create(False);

    VEnun := FMapsSet.GetIterator;
    while VEnun.Next(1, VGUID, i) = S_OK do begin
      VMapType := AMapsSet.GetMapTypeByGUID(VGUID);
      VSingleMap := TActiveMapSingleMainMap.Create(VMapType, False, FMainMapChangeNotyfier);
      VSingleSet.Add(VGUID, VSingleMap);
    end;

    FMapSingleSet := TActiveMapSingleSet.Create(VSingleSet);
  end;

  FActiveMap := TMapTypeChangeableByNotifier.Create(FMainMapChangeNotyfier, FMapsSet);
  Add(FActiveMap);
end;

procedure TMainActiveMap.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  VGUIDString: string;
  VGUID: TGUID;
  VValidGUID: Boolean;
begin
  inherited;
  VValidGUID := False;
  if AConfigData <> nil then begin
    VGUIDString := AConfigData.ReadString(CKeyNameMap, '');
    if VGUIDString <> '' then begin
      try
        VGUID := StringToGUID(VGUIDString);
        VValidGUID := True;
      except
      end;
    end;
  end;
  if VValidGUID then begin
    if FMapsSet.GetMapTypeByGUID(VGUID) <> nil then begin
      SelectMainByGUID(VGUID);
    end;
  end;
end;

procedure TMainActiveMap.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  VGUIDString: string;
  VGUID: TGUID;
begin
  inherited;
  VGUID := FActiveMap.GetStatic.GUID;
  VGUIDString := GUIDToString(VGUID);
  AConfigData.WriteString(CKeyNameMap, VGUIDString);
end;

function TMainActiveMap.GetActiveMap: IMapTypeChangeable;
begin
  Result := FActiveMap;
end;

function TMainActiveMap.GetMapSingleSet: IActiveMapSingleSet;
begin
  Result := FMapSingleSet;
end;

function TMainActiveMap.GetMapsSet: IMapTypeSet;
begin
  Result := FMapsSet;
end;

procedure TMainActiveMap.SelectMainByGUID(const AMapGUID: TGUID);
begin
  if FMapsSet.GetMapTypeByGUID(AMapGUID) <> nil then begin
    LockWrite;
    try
      FMainMapChangeNotyfier.NotifyByGUID(AMapGUID);
    finally
      UnlockWrite;
    end;
  end;
end;

end.
