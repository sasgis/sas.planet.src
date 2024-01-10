{******************************************************************************}
{* This file is part of SAS.Planet project.                                   *}
{*                                                                            *}
{* Copyright (C) 2007-2022, SAS.Planet development team.                      *}
{*                                                                            *}
{* SAS.Planet is free software: you can redistribute it and/or modify         *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* SAS.Planet is distributed in the hope that it will be useful,              *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the               *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with SAS.Planet. If not, see <http://www.gnu.org/licenses/>.         *}
{*                                                                            *}
{* https://github.com/sasgis/sas.planet.src                                   *}
{******************************************************************************}

unit u_ComponentPropertyStorage;

interface

{$IFDEF DEBUG}
  {.$DEFINE USE_PROPERTY_STORAGE_DEBUG_LOG}
  {$IFDEF USE_PROPERTY_STORAGE_DEBUG_LOG}
    {.$DEFINE USE_PROPERTY_STORAGE_LOG_NAMES}
    {$DEFINE USE_PROPERTY_STORAGE_PERF_COUNTER}
  {$ENDIF}
{$ENDIF}

uses
  Types,
  Classes,
  StrUtils,
  Rtti,
  Generics.Collections,
  t_ComponentProperty,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ComponentPropertyStorage,
  u_BaseInterfacedObject;

type
  TComponentPropertyStorage = class(TBaseInterfacedObject, IComponentPropertyStorage)
  private const
    CIniFileName = 'FormsState.ini';
  private type
    TPropertiesByClassName = TDictionary<string, TStringDynArray>;
    TWorkerType = (wtSave, wtRestore);
  private
    FRttiContext: TRttiContext;
    FPropertiesByClassName: TPropertiesByClassName;
    FConfigDataWriteProvider: IConfigDataWriteProvider;
    FConfigDataWriteProviderTemp: IConfigDataWriteProvider;
    FMemoryStreamForTempProvider: TMemoryStream;
    {$IFDEF USE_PROPERTY_STORAGE_DEBUG_LOG}
    FLogStream: TMemoryStream;
    procedure WriteLog(const AText: string);
    {$ENDIF}
    procedure BuildConfigDataProviders(const AIniFileName: string);
    function GetClassPropertiesToSave(const AClassName: string): TStringDynArray; inline;

    procedure DoSaveProperties(
      const AComponent: TComponent;
      const ANamePath: string;
      const AIsTemporary: Boolean
    );
    procedure DoRestoreProperties(
      const AComponent: TComponent;
      const ANamePath: string;
      const AIsTemporary: Boolean
    );
    procedure DoProcessComponent(
      const AWorkerType: TWorkerType;
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      var ACache: TComponentPropertyStorageCache
    );
  private
    class function BuildNamePath(const AParent, AChild: string): string; static; inline;
    class function GetComponentName(const AComponent: TComponent): string; static; inline;

    class function GetParentNamePath(
      const AComponent: TComponent;
      const AChildName: string
    ): string; static;

    class function IsComponentInList(
      const AComponent: TComponent;
      const AComponentNamePath: string;
      const AList: TComponentDynArray;
      const ANamePathArr: TStringDynArray
    ): Boolean; static; inline;

    class function PrepareNamePath(
      const AList: TComponentDynArray
    ): TStringDynArray; static;
  private
    { IComponentPropertyStorage }
    procedure Save(
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      var ACache: TComponentPropertyStorageCache
    );
    procedure Restore(
      const AComponent: TComponent;
      const AIgnore: TComponentDynArray;
      const ATemporary: TComponentDynArray;
      var ACache: TComponentPropertyStorageCache
    );
  public
    constructor Create(const ABasePath: string);
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF USE_PROPERTY_STORAGE_PERF_COUNTER}
  Diagnostics,
  {$ENDIF}
  SysUtils,
  IniFiles,
  u_ConfigDataWriteProviderByIniFile;

procedure InitPropDict(const ADict: TComponentPropertyStorage.TPropertiesByClassName);

  procedure Add(const AName: string; const APropArr: TStringDynArray);
  begin
    ADict.Add(LowerCase(AName), APropArr);
  end;

begin
  Add('TSaveDialog', ['InitialDir', 'FilterIndex']);

  Add('TTBItem', ['Checked']);
  Add('TCheckBox', ['Checked']);
  Add('TCheckListBox', ['CheckedBitMask']); // TCheckListBoxExt

  Add('TComboBox', ['ItemIndex']);
  Add('TPageControl', ['ActivePageIndex']);

  Add('TEdit', ['Text']);
  Add('TSpinEdit', ['Value']);

  Add('TfrmRegionProcess', ['ClientHeight', 'ClientWidth']);
end;

{ TComponentPropertyStorage }

constructor TComponentPropertyStorage.Create(const ABasePath: string);
begin
  inherited Create;

  FRttiContext := TRttiContext.Create; // initialize the record

  FPropertiesByClassName := TPropertiesByClassName.Create;
  InitPropDict(FPropertiesByClassName);

  FMemoryStreamForTempProvider := TMemoryStream.Create;
  BuildConfigDataProviders(IncludeTrailingPathDelimiter(ABasePath) + CIniFileName);

  {$IFDEF USE_PROPERTY_STORAGE_DEBUG_LOG}
  FLogStream := TMemoryStream.Create;
  {$ENDIF}
end;

destructor TComponentPropertyStorage.Destroy;
begin
  {$IFDEF USE_PROPERTY_STORAGE_DEBUG_LOG}
  FLogStream.SaveToFile(ExtractFilePath(ParamStr(0)) + ChangeFileExt(CIniFileName, '.log'));
  FreeAndNil(FLogStream);
  {$ENDIF}

  FreeAndNil(FPropertiesByClassName);

  FConfigDataWriteProviderTemp := nil;
  FreeAndNil(FMemoryStreamForTempProvider);

  inherited Destroy;
end;

{$IFDEF USE_PROPERTY_STORAGE_DEBUG_LOG}
procedure TComponentPropertyStorage.WriteLog(const AText: string);
var
  VText: UTF8String;
begin
  if AText <> '' then begin
    VText := UTF8Encode(FormatDateTime('hh:mm:ss.zzz', Now) + ' ' + AText + #13#10);
    FLogStream.WriteBuffer(VText[1], Length(VText));
  end;
end;
{$ENDIF}

procedure TComponentPropertyStorage.BuildConfigDataProviders(const AIniFileName: string);

  function CreateProvider(const AFileName: string): IConfigDataWriteProvider;
  var
    VIniFile: TMemIniFile;
  begin
    if AFileName = '' then begin
      VIniFile := TMemIniFile.Create(FMemoryStreamForTempProvider);
    end else begin
      VIniFile := TMemIniFile.Create(AFileName);
    end;
    try
      VIniFile.Encoding := TEncoding.UTF8;
      Result := TConfigDataWriteProviderByIniFile.CreateWithOwn(VIniFile);
      VIniFile := nil;
    finally
      VIniFile.Free;
    end;
  end;

begin
  FConfigDataWriteProvider := CreateProvider(AIniFileName);
  FConfigDataWriteProviderTemp := CreateProvider('');
end;

function TComponentPropertyStorage.GetClassPropertiesToSave(const AClassName: string): TStringDynArray;
begin
  if not FPropertiesByClassName.TryGetValue(LowerCase(AClassName), Result) then begin
    Result := nil;
  end;
end;

class function TComponentPropertyStorage.BuildNamePath(const AParent, AChild: string): string;
begin
  if (AParent <> '') and (AChild <> '') then begin
    Result := AParent + '.' + AChild;
  end else
  if AParent <> '' then begin
    Result := AParent;
  end else begin
    Result := AChild;
  end;
end;

procedure TComponentPropertyStorage.DoSaveProperties(
  const AComponent: TComponent;
  const ANamePath: string;
  const AIsTemporary: Boolean
);

  procedure PrepareWriteProvider(var AProvider: IConfigDataWriteProvider);
  begin
    if AProvider = nil then begin
      if AIsTemporary then begin
        AProvider := FConfigDataWriteProviderTemp.GetOrCreateSubItem(ANamePath);
      end else begin
        AProvider := FConfigDataWriteProvider.GetOrCreateSubItem(ANamePath);
      end;
    end;
  end;

var
  I: Integer;
  VType: TRttiType;
  VProp: TRttiProperty;
  VPropArr: TStringDynArray;
  VProvider: IConfigDataWriteProvider;
begin
  if AComponent.Name = '' then begin
    Exit;
  end;

  VProvider := nil;

  VPropArr := GetClassPropertiesToSave(AComponent.ClassName);
  if Length(VPropArr) = 0 then begin
    Exit;
  end;

  {$IFDEF USE_PROPERTY_STORAGE_LOG_NAMES}
  WriteLog('Save properties for: ' + ANamePath);
  {$ENDIF}

  VType := FRttiContext.GetType(AComponent.ClassType);

  for I := 0 to Length(VPropArr) - 1 do begin
    VProp := VType.GetProperty(VPropArr[I]);
    if VProp <> nil then begin
      case VProp.PropertyType.TypeKind of
        tkInteger, tkEnumeration: begin
          PrepareWriteProvider(VProvider);
          if VProp.PropertyType.Name = 'Boolean' then begin
            VProvider.WriteBool(VProp.Name, VProp.GetValue(AComponent).AsBoolean);
          end else begin
            VProvider.WriteInteger(VProp.Name, VProp.GetValue(AComponent).AsInteger);
          end;
        end;
        tkFloat: begin
          PrepareWriteProvider(VProvider);
          VProvider.WriteFloat(VProp.Name, VProp.GetValue(AComponent).AsExtended);
        end;
        tkString, tkLString, tkWString, tkUString: begin
          PrepareWriteProvider(VProvider);
          VProvider.WriteString(VProp.Name, VProp.GetValue(AComponent).AsString);
        end;
      end;
    end;
  end;
end;

procedure TComponentPropertyStorage.DoRestoreProperties(
  const AComponent: TComponent;
  const ANamePath: string;
  const AIsTemporary: Boolean
);

  function PrepareReadProvider(var AProvider: IConfigDataProvider): Boolean;
  begin
    if AProvider = nil then begin
      if AIsTemporary then begin
        AProvider := FConfigDataWriteProviderTemp.GetSubItem(ANamePath);
      end else begin
        AProvider := FConfigDataWriteProvider.GetSubItem(ANamePath);
      end;
    end;
    Result := AProvider <> nil;
  end;

var
  I: Integer;
  VType: TRttiType;
  VProp: TRttiProperty;
  VPropArr: TStringDynArray;
  VProvider: IConfigDataProvider;
  VBoolVal: Boolean;
  VIntVal: Integer;
  VFloatVal: Double;
  VStrVal: string;
begin
  if AComponent.Name = '' then begin
    Exit;
  end;

  VProvider := nil;

  VPropArr := GetClassPropertiesToSave(AComponent.ClassName);
  if Length(VPropArr) = 0 then begin
    Exit;
  end;

  {$IFDEF USE_PROPERTY_STORAGE_LOG_NAMES}
  WriteLog('Restore properties for: ' + ANamePath);
  {$ENDIF}

  VType := FRttiContext.GetType(AComponent.ClassType);

  for I := 0 to Length(VPropArr) - 1 do begin
    VProp := VType.GetProperty(VPropArr[I]);
    if VProp <> nil then begin
      case VProp.PropertyType.TypeKind of
        tkInteger, tkEnumeration: begin
          if not PrepareReadProvider(VProvider) then begin
            Continue;
          end;
          if VProp.PropertyType.Name = 'Boolean' then begin
            VBoolVal := VProvider.ReadBool(VProp.Name, VProp.GetValue(AComponent).AsBoolean);
            VProp.SetValue(AComponent, VBoolVal);
          end else begin
            VIntVal := VProvider.ReadInteger(VProp.Name, VProp.GetValue(AComponent).AsInteger);
            VProp.SetValue(AComponent, VIntVal);
          end;
        end;
        tkFloat: begin
          if not PrepareReadProvider(VProvider) then begin
            Continue;
          end;
          VFloatVal := VProvider.ReadFloat(VProp.Name, VProp.GetValue(AComponent).AsExtended);
          VProp.SetValue(AComponent, VFloatVal);
        end;
        tkString, tkLString, tkWString, tkUString: begin
          if not PrepareReadProvider(VProvider) then begin
            Continue;
          end;
          VStrVal := VProvider.ReadString(VProp.Name, VProp.GetValue(AComponent).AsString);
          VProp.SetValue(AComponent, VStrVal);
        end;
      end;
    end;
  end;
end;

class function TComponentPropertyStorage.GetComponentName(
  const AComponent: TComponent
): string;
begin
  Assert(AComponent <> nil);
  Result := AComponent.Name;
  if Result = '' then begin
    //Assert(False, 'The component does not have a name!');
    Result := AComponent.ClassName + '_' + IntToStr(AComponent.Tag);
  end;
end;

class function TComponentPropertyStorage.GetParentNamePath(
  const AComponent: TComponent;
  const AChildName: string
): string;
var
  VName: string;
  VParent: TComponent;
begin
  VParent := AComponent.GetParentComponent;
  if VParent <> nil then begin
    VName := BuildNamePath(GetComponentName(VParent), AChildName);
    Result := GetParentNamePath(VParent, VName);
  end else begin
    Result := AChildName;
  end;
end;

class function TComponentPropertyStorage.IsComponentInList(
  const AComponent: TComponent;
  const AComponentNamePath: string;
  const AList: TComponentDynArray;
  const ANamePathArr: TStringDynArray
): Boolean;
var
  I: Integer;
begin
  Assert(Length(AList) = Length(ANamePathArr));
  Result := False;
  for I := 0 to Length(AList) - 1 do begin
    Result := (AComponent = AList[I]) or StartsText(ANamePathArr[I], AComponentNamePath);
    if Result then begin
      Exit;
    end;
  end;
end;

class function TComponentPropertyStorage.PrepareNamePath(
  const AList: TComponentDynArray
): TStringDynArray;
var
  I: Integer;
begin
  SetLength(Result, Length(AList));
  for I := 0 to Length(AList) - 1 do begin
    Result[I] := BuildNamePath(GetParentNamePath(AList[I], ''), AList[I].Name);
  end;
end;

procedure TComponentPropertyStorage.DoProcessComponent(
  const AWorkerType: TWorkerType;
  const AComponent: TComponent;
  const AIgnore: TComponentDynArray;
  const ATemporary: TComponentDynArray;
  var ACache: TComponentPropertyStorageCache
);

  procedure ProcessChilds(const AChild: TComponent; const AParentNamePath: string);
  var
    I: Integer;
    VIsTemp: Boolean;
    VIsIgnore: Boolean;
    VComponent: TComponent;
    VNamePath: string;
    VParentNamePath: string;
  begin
    for I := 0 to AChild.ComponentCount - 1 do begin
      VComponent := AChild.Components[I];

      VParentNamePath := GetParentNamePath(VComponent, '');
      if (AParentNamePath <> '') and (VParentNamePath = '') then begin
        VParentNamePath := AParentNamePath;
      end;

      VNamePath := BuildNamePath(VParentNamePath, GetComponentName(VComponent));
      VIsIgnore := IsComponentInList(VComponent, VNamePath, AIgnore, ACache.FIgnoreNamePath);

      if not VIsIgnore then begin
        VIsTemp := IsComponentInList(VComponent, VNamePath, ATemporary, ACache.FTemporaryNamePath);
        if AWorkerType = wtRestore then begin
          DoRestoreProperties(VComponent, VNamePath, VIsTemp);
        end else begin
          DoSaveProperties(VComponent, VNamePath, VIsTemp);
        end;
        ProcessChilds(VComponent, VNamePath); // recursion
      end;
    end;
  end;

var
  VIsTemp: Boolean;
  {$IFDEF USE_PROPERTY_STORAGE_PERF_COUNTER}
  VStopwatch: TStopwatch;
  {$ENDIF}
begin
  {$IFDEF USE_PROPERTY_STORAGE_PERF_COUNTER}
  VStopwatch := TStopwatch.StartNew;
  {$ENDIF}

  if ACache.FNamePath = '' then begin
    ACache.FNamePath :=
      BuildNamePath(
        GetParentNamePath(AComponent, ''),
        GetComponentName(AComponent)
      );
  end;

  if Length(AIgnore) <> Length(ACache.FIgnoreNamePath) then begin
    ACache.FIgnoreNamePath := PrepareNamePath(AIgnore);
  end;

  if Length(ATemporary) <> Length(ACache.FTemporaryNamePath) then begin
    ACache.FTemporaryNamePath := PrepareNamePath(ATemporary);
  end;

  VIsTemp := IsComponentInList(AComponent, ACache.FNamePath, ATemporary, ACache.FTemporaryNamePath);

  if AWorkerType = wtRestore then begin
    DoRestoreProperties(AComponent, ACache.FNamePath, VIsTemp);
  end else begin
    DoSaveProperties(AComponent, ACache.FNamePath, VIsTemp);
  end;

  ProcessChilds(AComponent, ACache.FNamePath);

  {$IFDEF USE_PROPERTY_STORAGE_PERF_COUNTER}
  VStopwatch.Stop;
  WriteLog('Component: ' + ACache.FNamePath);
  if AWorkerType = wtRestore then begin
    WriteLog('> Restored at: ' + VStopwatch.ElapsedMilliseconds.ToString + ' ms');
  end else begin
    WriteLog('< Saved at: ' + VStopwatch.ElapsedMilliseconds.ToString + ' ms');
  end;
  {$ENDIF}
end;

procedure TComponentPropertyStorage.Save(
  const AComponent: TComponent;
  const AIgnore: TComponentDynArray;
  const ATemporary: TComponentDynArray;
  var ACache: TComponentPropertyStorageCache
);
begin
  if AComponent = nil then begin
    Assert(False);
    Exit;
  end;

  DoProcessComponent(wtSave, AComponent, AIgnore, ATemporary, ACache);
end;

procedure TComponentPropertyStorage.Restore(
  const AComponent: TComponent;
  const AIgnore: TComponentDynArray;
  const ATemporary: TComponentDynArray;
  var ACache: TComponentPropertyStorageCache
);
begin
  if AComponent = nil then begin
    Assert(False);
    Exit;
  end;

  DoProcessComponent(wtRestore, AComponent, AIgnore, ATemporary, ACache);
end;

end.
