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
  i_MarkSystemConfig,
  i_MarkSystemImplConfig,
  i_InterfaceListSimple,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase;

type
  TMarkSystemConfig = class (TConfigDataElementBase, IMarkSystemConfigListChangeable)
  private
    FList: IInterfaceListSimple;
    FActiveConfigIndex: Integer;
    FDefaultConfig: IMarkSystemConfigStatic;
  private
    { IMarkSystemConfigListChangeable }
    function GetCount: Integer;
    function Get(const AIndex: Integer): IMarkSystemConfigStatic;
    function GetActiveConfigIndex: Integer;
    procedure SetActiveConfigIndex(const AValue: Integer);
    function GetActiveConfig: IMarkSystemConfigStatic;
    procedure Delete(const AIndex: Integer);
    function Add(
      const ADatabaseUID: TGUID;
      const ADisplayName: string;
      const AImplConfig: IMarkSystemImplConfigStatic;
      const ASetAsActive: Boolean
    ): Integer; overload;
    function Add(
      const AConfig: IMarkSystemConfigStatic;
      const ASetAsActive: Boolean
    ): Integer; overload;
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
  u_MarkSystemImplConfigBase,
  u_InterfaceListSimple,
  u_BaseInterfacedObject;

type
  TMarkSystemConfigStatic = class(TBaseInterfacedObject, IMarkSystemConfigStatic)
  private
    FDatabaseUID: TGUID;
    FDisplayName: string;
    FImplConfig: IMarkSystemImplConfigStatic;
  private
    function GetDatabaseUID: TGUID;
    function GetDisplayName: string;
    function GetImplConfig: IMarkSystemImplConfigStatic;
  public
    constructor Create(
      const ADatabaseUID: TGUID;
      const ADisplayName: string;
      const AImplConfig: IMarkSystemImplConfigStatic
    );
  end;

{ TMarkSystemConfigStatic }

constructor TMarkSystemConfigStatic.Create(
  const ADatabaseUID: TGUID;
  const ADisplayName: string;
  const AImplConfig: IMarkSystemImplConfigStatic
);
begin
  inherited Create;
  FDatabaseUID := ADatabaseUID;
  FDisplayName := ADisplayName;
  FImplConfig := AImplConfig;
end;

function TMarkSystemConfigStatic.GetDatabaseUID: TGUID;
begin
  Result := FDatabaseUID;
end;

function TMarkSystemConfigStatic.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TMarkSystemConfigStatic.GetImplConfig: IMarkSystemImplConfigStatic;
begin
  Result := FImplConfig;
end;

function _GetDefConfigSML: IMarkSystemConfigStatic;
var
  VImpl: IMarkSystemImplConfigStatic;
begin
  VImpl := TMarkSystemImplConfigBase.Create('marks.sml', False);
  Result :=
    TMarkSystemConfigStatic.Create(
      cSMLMarksDbGUID,
      'marks',
      VImpl
    );
end;

function _GetDefConfigSQL: IMarkSystemConfigStatic;
var
  VImpl: IMarkSystemImplConfigStatic;
begin
  VImpl := TMarkSystemImplConfigBase.Create('Marks.db3', False);
  Result :=
    TMarkSystemConfigStatic.Create(
      cORMSQLiteMarksDbGUID,
      'Marks',
      VImpl
    );
end;

{ TMarkSystemConfig }

constructor TMarkSystemConfig.Create;
begin
  inherited Create;
  FList := TInterfaceListSimple.Create;
  FActiveConfigIndex := -1;
  FDefaultConfig := _GetDefConfigSML;
end;

procedure TMarkSystemConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
var
  I: Integer;
  VCount: Integer;
  VConfId: string;
  VConfig: IConfigDataProvider;
  VImplGUID: TGUID;
  VDatabaseUID: TGUID;
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
      FActiveConfigIndex := VConfig.ReadInteger('ActiveIndex', -1);

      VZeroGUID := GUIDToString(CGUID_Zero);

      for I := 0 to VCount - 1 do begin
        VConfId := 'Item' + IntToStr(I) + '_';

        VTmp := VConfig.ReadString(VConfId + 'DatabaseUID', VZeroGUID);
        VDatabaseUID := StringToGUID(VTmp);

        if IsEqualGUID(VDatabaseUID, CGUID_Zero) then begin
          raise Exception.Create('MarkSystemConfig: Incorrect DatabaseUID!');
        end;

        VDisplayName := VConfig.ReadString(VConfId + 'DisplayName', '');

        VTmp := VConfig.ReadString(VConfId + 'GUID', VZeroGUID);
        VImplGUID := StringToGUID(VTmp);

        if IsEqualGUID(VImplGUID, CGUID_Zero) then begin
          raise Exception.Create('MarkSystemConfig: Incorrect Impl GUID!');
        end;

        VFileName := VConfig.ReadString(VConfId + 'FileName', '');
        VIsReadOnly := VConfig.ReadBool(VConfId + 'IsReadOnly', False);

        if IsEqualGUID(IMarkSystemImplConfigStatic, VImplGUID) then begin
          VImpl := TMarkSystemImplConfigBase.Create(VFileName, VIsReadOnly);
        end else begin
          raise Exception.Create('MarkSystemConfig: Unknown Impl GUID: ' + GUIDToString(VImplGUID));
        end;

        VItem := TMarkSystemConfigStatic.Create(VDatabaseUID, VDisplayName, VImpl);
        FList.Add(VItem);
      end;
    end;
  end;
  if FList.Count = 0 then begin
    FActiveConfigIndex := FList.Add(FDefaultConfig);
  end;
end;

procedure TMarkSystemConfig.DoWriteConfig(const AConfigData: IConfigDataWriteProvider);
var
  I: Integer;
  VConfId: string;
  VConfig: IConfigDataWriteProvider;
  VItem: IMarkSystemConfigStatic;
  VImpl: IMarkSystemImplConfigStatic;
begin
  inherited;

  VConfig := AConfigData.GetOrCreateSubItem('MarkSystemConfig');

  VConfig.DeleteValues; // clear section

  VConfig.WriteInteger('Count', FList.Count);
  VConfig.WriteInteger('ActiveIndex', FActiveConfigIndex);

  for I := 0 to FList.Count - 1 do begin
    VItem := IMarkSystemConfigStatic(FList.Items[I]);
    VImpl := VItem.ImplConfig;

    VConfId := 'Item' + IntToStr(I) + '_';

    VConfig.WriteString(VConfId + 'DatabaseUID', GUIDToString(VItem.DatabaseUID));
    VConfig.WriteString(VConfId + 'DisplayName', VItem.DisplayName);

    // ToDo: Check extended Impl GUID
    VConfig.WriteString(VConfId + 'GUID', GUIDToString(IMarkSystemImplConfigStatic));

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

function TMarkSystemConfig.Get(const AIndex: Integer): IMarkSystemConfigStatic;
begin
  LockRead;
  try
    Assert( (AIndex >= 0) and (FList.Count > AIndex) );
    Result := IMarkSystemConfigStatic(FList.Items[AIndex]);
  finally
    UnlockRead;
  end;
end;

function TMarkSystemConfig.GetActiveConfigIndex: Integer;
begin
  LockRead;
  try
    Result := FActiveConfigIndex;
  finally
    UnlockRead;
  end;
end;

procedure TMarkSystemConfig.SetActiveConfigIndex(const AValue: Integer);
begin
  LockWrite;
  try
    Assert( (AValue >= 0) and (FList.Count > AValue) );
    if FActiveConfigIndex <> AValue then begin
      FActiveConfigIndex := AValue;
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
    if FActiveConfigIndex >= 0 then begin
      Result := IMarkSystemConfigStatic(FList.Items[FActiveConfigIndex]);
    end else begin
      Result := nil;
    end;
  finally
    UnlockRead;
  end;
end;

procedure TMarkSystemConfig.Delete(const AIndex: Integer);
begin
  LockWrite;
  try
    Assert( (AIndex >= 0) and (FList.Count > AIndex) );
    FList.Delete(AIndex);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

function TMarkSystemConfig.Add(
  const ADatabaseUID: TGUID;
  const ADisplayName: string;
  const AImplConfig: IMarkSystemImplConfigStatic;
  const ASetAsActive: Boolean
): Integer;
var
  VConfig: IMarkSystemConfigStatic;
begin
  VConfig := TMarkSystemConfigStatic.Create(ADatabaseUID, ADisplayName, AImplConfig);
  Result := Self.Add(VConfig, ASetAsActive);
end;

function TMarkSystemConfig.Add(
  const AConfig: IMarkSystemConfigStatic;
  const ASetAsActive: Boolean
): Integer;
begin
  LockWrite;
  try
    Result := FList.Add(AConfig);
    if ASetAsActive then begin
      FActiveConfigIndex := Result;
    end;
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

end.
