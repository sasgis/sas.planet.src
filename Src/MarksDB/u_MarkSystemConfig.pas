{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2015, SAS.Planet development team.                      *}
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

unit u_MarkSystemConfig;

interface

uses
  Windows,
  i_IDList,
  i_MarkSystemConfig,
  i_MarkSystemImplConfig,
  i_InterfaceListStatic,
  i_InterfaceListSimple,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TMarkSystemConfig = class (TConfigDataElementBase, IMarkSystemConfigListChangeable)
  private
    FID: Integer;
    FList: IIDInterfaceList;
    FActiveConfigID: Integer;
  private
    function _NewID: Integer;
  private
    { IMarkSystemConfigListChangeable }
    function GetCount: Integer;
    function GetByID(const AID: Integer): IMarkSystemConfigStatic;
    function GetActiveConfigID: Integer;
    procedure SetActiveConfigID(const AID: Integer);
    function GetActiveConfig: IMarkSystemConfigStatic;
    procedure DeleteByID(const AID: Integer);
    function Add(
      const ADatabaseGUID: TGUID;
      const ADisplayName: string;
      const AImplConfig: IMarkSystemImplConfigStatic;
      const ASetAsActive: Boolean
    ): Integer; overload;
    function Add(
      const AConfig: IMarkSystemConfigStatic;
      const ASetAsActive: Boolean
    ): Integer; overload;
    function GetIDListStatic: IInterfaceListStatic;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  c_ZeroGUID,
  c_MarkSystem,
  i_EnumID,
  u_IDInterfaceList,
  u_MarkSystemImplConfigBase,
  u_InterfaceListSimple,
  u_BaseInterfacedObject;

type
  TMarkSystemConfigStatic = class(TBaseInterfacedObject, IMarkSystemConfigStatic)
  private
    FID: Integer;
    FDatabaseGUID: TGUID;
    FDisplayName: string;
    FImplConfig: IMarkSystemImplConfigStatic;
  private
    function GetID: Integer;
    function GetDatabaseGUID: TGUID;
    function GetDisplayName: string;
    function GetImplConfig: IMarkSystemImplConfigStatic;
  public
    constructor Create(
      const AID: Integer;
      const ADatabaseGUID: TGUID;
      const ADisplayName: string;
      const AImplConfig: IMarkSystemImplConfigStatic
    );
  end;

{ TMarkSystemConfigStatic }

constructor TMarkSystemConfigStatic.Create(
  const AID: Integer;
  const ADatabaseGUID: TGUID;
  const ADisplayName: string;
  const AImplConfig: IMarkSystemImplConfigStatic
);
begin
  Assert(AID > 0);
  inherited Create;
  FID := AID;
  FDatabaseGUID := ADatabaseGUID;
  FDisplayName := ADisplayName;
  FImplConfig := AImplConfig;
end;

function TMarkSystemConfigStatic.GetDatabaseGUID: TGUID;
begin
  Result := FDatabaseGUID;
end;

function TMarkSystemConfigStatic.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TMarkSystemConfigStatic.GetImplConfig: IMarkSystemImplConfigStatic;
begin
  Result := FImplConfig;
end;

function TMarkSystemConfigStatic.GetID: Integer;
begin
  Result := FID;
end;

function _GetDefaultConfig(const AID: Integer; const ADB: TGUID): IMarkSystemConfigStatic;
var
  VImpl: IMarkSystemImplConfigStatic;
begin
  if IsEqualGUID(ADB, cSMLMarksDbGUID) then begin
    VImpl := TMarkSystemImplConfigBase.Create('marks.sml', False);
  end else if IsEqualGUID(ADB, cORMSQLiteMarksDbGUID) then begin
    VImpl := TMarkSystemImplConfigBase.Create('Marks.db3', False);
  end else begin
    raise Exception.Create('MarkSystemConfig: Unknown Database GUID: ' + GUIDToString(ADB));
  end;
  Result := TMarkSystemConfigStatic.Create(AID, ADB, 'My Marks', VImpl);
end;

{ TMarkSystemConfig }

constructor TMarkSystemConfig.Create;
begin
  inherited Create;
  FID := 0;
  FList := TIDInterfaceList.Create;
  FActiveConfigID := 0;
end;

function TMarkSystemConfig._NewID: Integer;
begin
  Result := InterlockedIncrement(FID);
end;

procedure TMarkSystemConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  I: Integer;
  VCount: Integer;
  VConfId: string;
  VConfig: IConfigDataProvider;
  VImplGUID: TGUID;
  VDatabaseGUID: TGUID;
  VDisplayName: string;
  VFileName: string;
  VIsReadOnly: Boolean;
  VTmp: string;
  VZeroGUID: string;
  VItem: IMarkSystemConfigStatic;
  VImpl: IMarkSystemImplConfigStatic;
begin
  inherited;

  if AConfigData <> nil then begin
    VConfig := AConfigData.GetSubItem('MarkSystemConfig');
    if VConfig <> nil then begin
      VCount := VConfig.ReadInteger('Count', 0);
      FActiveConfigID := VConfig.ReadInteger('ActiveIndex', -1);

      VZeroGUID := GUIDToString(CGUID_Zero);

      for I := 0 to VCount - 1 do begin
        VConfId := 'Item' + IntToStr(I+1) + '_';

        VTmp := VConfig.ReadString(VConfId + 'Database', VZeroGUID);
        VDatabaseGUID := StringToGUID(VTmp);

        if IsEqualGUID(VDatabaseGUID, CGUID_Zero) then begin
          Continue;
          //raise Exception.Create('MarkSystemConfig: Incorrect Database GUID!');
        end;

        VDisplayName := VConfig.ReadString(VConfId + 'DisplayName', '');

        VTmp := VConfig.ReadString(VConfId + 'Impl', VZeroGUID);
        VImplGUID := StringToGUID(VTmp);

        if IsEqualGUID(VImplGUID, CGUID_Zero) then begin
          Continue;
          //raise Exception.Create('MarkSystemConfig: Incorrect Impl GUID!');
        end;

        VFileName := VConfig.ReadString(VConfId + 'FileName', '');
        VIsReadOnly := VConfig.ReadBool(VConfId + 'IsReadOnly', False);

        if IsEqualGUID(IMarkSystemImplConfigStatic, VImplGUID) then begin
          VImpl := TMarkSystemImplConfigBase.Create(VFileName, VIsReadOnly);
        end else begin
          Continue;
          //raise Exception.Create('MarkSystemConfig: Unknown Impl GUID: ' + GUIDToString(VImplGUID));
        end;

        VItem := TMarkSystemConfigStatic.Create(_NewID, VDatabaseGUID, VDisplayName, VImpl);
        FList.Add(VItem.ID, VItem);
      end;
    end;
  end;
  if FList.Count = 0 then begin
    VItem := _GetDefaultConfig(_NewID, cSMLMarksDbGUID);
    FList.Add(VItem.ID, VItem);
    FActiveConfigID := VItem.ID; // sml as default
    VItem := _GetDefaultConfig(_NewID, cORMSQLiteMarksDbGUID);
    FList.Add(VItem.ID, VItem);
  end;
  if FActiveConfigID = 0 then begin
    FActiveConfigID := 1;
  end;
end;

procedure TMarkSystemConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  I: Cardinal;
  VId: Integer;
  VCount: Integer;
  VEnum: IEnumID;
  VConfId: string;
  VConfig: IConfigDataWriteProvider;
  VItem: IMarkSystemConfigStatic;
  VImpl: IMarkSystemImplConfigStatic;
begin
  inherited;

  VConfig := AConfigData.GetOrCreateSubItem('MarkSystemConfig');

  VConfig.DeleteValues; // clear section

  VConfig.WriteInteger('Count', FList.Count);
  VConfig.WriteInteger('ActiveIndex', FActiveConfigID);

  VCount := 0;
  VEnum := FList.GetIDEnum;
  while VEnum.Next(1, VId, I) = S_OK do begin
    VItem := IMarkSystemConfigStatic(FList.GetByID(VId));
    VImpl := VItem.ImplConfig;

    Inc(VCount);
    VConfId := 'Item' + IntToStr(VCount) + '_';

    VConfig.WriteString(VConfId + 'Database', GUIDToString(VItem.DatabaseGUID));
    VConfig.WriteString(VConfId + 'DisplayName', VItem.DisplayName);

    // ToDo: Check extended Impl GUID
    VConfig.WriteString(VConfId + 'Impl', GUIDToString(IMarkSystemImplConfigStatic));

    VConfig.WriteString(VConfId + 'FileName', VImpl.FileName);
    VConfig.WriteBool(VConfId + 'IsReadOnly', VImpl.IsReadOnly);
  end;
end;

function TMarkSystemConfig.GetCount: Integer;
begin
  LockRead;
  try
    Result := FList.Count;
  finally
    UnlockRead;
  end;
end;

function TMarkSystemConfig.GetByID(const AID: Integer): IMarkSystemConfigStatic;
begin
  Assert(AID > 0);
  LockRead;
  try
    if FList.IsExists(AID) then begin
      Result := IMarkSystemConfigStatic(FList.GetByID(AID));
    end else begin
      Result := nil;
    end;
  finally
    UnlockRead;
  end;
end;

function TMarkSystemConfig.GetActiveConfigID: Integer;
begin
  LockRead;
  try
    Result := FActiveConfigID;
  finally
    UnlockRead;
  end;
end;

procedure TMarkSystemConfig.SetActiveConfigID(const AID: Integer);
begin
  Assert(AID > 0);
  LockWrite;
  try
    if (FActiveConfigID <> AID) then begin
      FActiveConfigID := AID;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkSystemConfig.GetActiveConfig: IMarkSystemConfigStatic;
begin
  LockRead;
  try
    if FActiveConfigID > 0 then begin
      Result := IMarkSystemConfigStatic(FList.GetByID(FActiveConfigID));
    end else begin
      Result := nil;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TMarkSystemConfig.DeleteByID(const AID: Integer);
var
  I: Cardinal;
  VId: Integer;
  VEnum: IEnumID;
begin
  LockWrite;
  try
    if FList.IsExists(AID) then begin
      FList.Remove(AID);
      if FActiveConfigID = AID then begin
        VEnum := FList.GetIDEnum;
        if VEnum.Next(1, VId, I) = S_OK then begin
          FActiveConfigID := VId;
        end else begin
          FActiveConfigID := 0;
        end;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

function TMarkSystemConfig.Add(
  const ADatabaseGUID: TGUID;
  const ADisplayName: string;
  const AImplConfig: IMarkSystemImplConfigStatic;
  const ASetAsActive: Boolean
): Integer;
var
  VConfig: IMarkSystemConfigStatic;
begin
  VConfig := TMarkSystemConfigStatic.Create(_NewID, ADatabaseGUID, ADisplayName, AImplConfig);
  Result := Self.Add(VConfig, ASetAsActive);
end;

function TMarkSystemConfig.Add(
  const AConfig: IMarkSystemConfigStatic;
  const ASetAsActive: Boolean
): Integer;
begin
  LockWrite;
  try
    FList.Add(AConfig.ID, AConfig);
    Result := AConfig.ID;
    if ASetAsActive then begin
      FActiveConfigID := Result;
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TMarkSystemConfig.GetIDListStatic: IInterfaceListStatic;
var
  I: Cardinal;
  VId: Integer;
  VEnum: IEnumID;
  VItem: IMarkSystemConfigStatic;
  VResultList: IInterfaceListSimple;
begin
  LockRead;
  try
    VResultList := TInterfaceListSimple.Create;
    VEnum := FList.GetIDEnum;
    while VEnum.Next(1, VId, I) = S_OK do begin
      VItem := IMarkSystemConfigStatic(FList.GetByID(VId));
      VResultList.Add(VItem);
    end;
    Result := VResultList.MakeStaticAndClear;
  finally
    UnlockRead;
  end;
end;

end.
