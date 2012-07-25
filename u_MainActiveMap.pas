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
  i_ActiveMapsConfig,
  u_ConfigDataElementComplexBase,
  u_NotifyWithGUIDEvent;

type
  TActiveMapSingleSet = class(TInterfacedObject, IActiveMapSingleSet)
  private
    FSet: IGUIDInterfaceSet;
  private
    function GetMapSingle(const AMapGUID: TGUID): IActiveMapSingle;
  public
    constructor Create(ASet: IGUIDInterfaceSet);
  end;

type
  TMainActiveMap = class(TConfigDataElementComplexBase, IMainActiveMap)
  private
    FMapsSet: IMapTypeSet;
    FMainMapChangeNotyfier: INotifierWithGUID;
    FActiveMap: IMapTypeChangeable;
    FMapSingleSet: IActiveMapSingleSet;
    FSingleSet: IGUIDInterfaceSet;
  protected
    property MainMapChangeNotyfier: INotifierWithGUID read FMainMapChangeNotyfier;
    property SingleSet: IGUIDInterfaceSet read FSingleSet;
  protected
    procedure SelectMainByGUID(const AMapGUID: TGUID);
    function GetActiveMap: IMapTypeChangeable;

    function GetMapSingleSet: IActiveMapSingleSet;
    function GetMapsSet: IMapTypeSet;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create(const AMapsSet: IMapTypeSet);
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  ActiveX,
  u_GUIDInterfaceSet,
  u_ActiveMapSingleAbstract,
  u_ActiveMapsSet,
  u_ActiveMapConfig;

const
  CKeyNameMap = 'Map';

{ TMainActiveMap }

constructor TMainActiveMap.Create(const AMapsSet: IMapTypeSet);
var
  VEnun: IEnumGUID;
  VGUID: TGUID;
  i: Cardinal;
  VMapType: IMapType;
  VSingleMap: IActiveMapSingle;
  VSelected: IMapType;
begin
  inherited Create;
  FMapsSet := AMapsSet;
  FMainMapChangeNotyfier := TNotifierWithGUID.Create;
  FSingleSet := TGUIDInterfaceSet.Create(False);
  FMapSingleSet := TActiveMapSingleSet.Create(FSingleSet);

  FActiveMap := TMapTypeChangeableByNotifier.Create(FMainMapChangeNotyfier, FMapsSet);
  Add(FActiveMap);
  VSelected := FActiveMap.GetStatic;

  VEnun := FMapsSet.GetIterator;
  while VEnun.Next(1, VGUID, i) = S_OK do begin
    VMapType := FMapsSet.GetMapTypeByGUID(VGUID);
    VSingleMap := TActiveMapSingleMainMap.Create(VMapType, VMapType = VSelected, FMainMapChangeNotyfier);
    FSingleSet.Add(VGUID, VSingleMap);
  end;
end;

destructor TMainActiveMap.Destroy;
begin
  FMainMapChangeNotyfier := nil;
  FMapsSet := nil;
  FActiveMap := nil;
  inherited;
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

{ TActiveMapSingleSet }

constructor TActiveMapSingleSet.Create(ASet: IGUIDInterfaceSet);
begin
  inherited Create;
  FSet := ASet;
end;

function TActiveMapSingleSet.GetMapSingle(
  const AMapGUID: TGUID): IActiveMapSingle;
begin
  Result := IActiveMapSingle(FSet.GetByGUID(AMapGUID));
end;

end.
