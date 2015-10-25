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

unit u_SimpleTileStorageConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_SimpleTileStorageConfig,
  u_ConfigDataElementBase;

type
  TSimpleTileStorageConfig = class(TConfigDataElementWithStaticBase, ISimpleTileStorageConfig)
  private
    FDefConfig: ISimpleTileStorageConfigStatic;

    FCacheTypeCode: Integer;
    FNameInCache: string;
    FIsReadOnly: Boolean;
    FAllowDelete: Boolean;
    FAllowAdd: Boolean;
    FAllowReplace: Boolean;
    FUseMemCache: Boolean;
    FMemCacheCapacity: Integer;
    FMemCacheTTL: Cardinal;
    FMemCacheClearStrategy: Integer;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  private
    function GetCacheTypeCode: Integer;
    procedure SetCacheTypeCode(AValue: Integer);

    function GetNameInCache: string;
    procedure SetNameInCache(const AValue: string);

    function GetTileFileExt: AnsiString;

    function GetIsReadOnly: Boolean;
    procedure SetIsReadOnly(AValue: Boolean);

    function GetAllowDelete: Boolean;
    procedure SetAllowDelete(AValue: Boolean);

    function GetAllowAdd: Boolean;
    procedure SetAllowAdd(AValue: Boolean);

    function GetAllowReplace: Boolean;
    procedure SetAllowReplace(AValue: Boolean);

    function GetUseMemCache: Boolean;
    procedure SetUseMemCache(const AValue: Boolean);

    function GetMemCacheCapacity: Integer;
    procedure SetMemCacheCapacity(const AValue: Integer);

    function GetMemCacheTTL: Cardinal;
    procedure SetMemCacheTTL(const AValue: Cardinal);

    function GetMemCacheClearStrategy: Integer;
    procedure SetMemCacheClearStrategy(const AValue: Integer);

    function GetStatic: ISimpleTileStorageConfigStatic;
  public
    constructor Create(const ADefConfig: ISimpleTileStorageConfigStatic);
  end;

implementation

uses
  c_CacheTypeCodes,
  i_TileStorageAbilities,
  u_TileStorageAbilities,
  u_SimpleTileStorageConfigStatic;

{ TSimpleTileStorageConfig }

constructor TSimpleTileStorageConfig.Create(
  const ADefConfig: ISimpleTileStorageConfigStatic
);
begin
  inherited Create;
  FDefConfig := ADefConfig;

  FCacheTypeCode := FDefConfig.CacheTypeCode;
  FNameInCache := FDefConfig.NameInCache;

  FIsReadOnly := FDefConfig.Abilities.IsReadOnly;
  FAllowDelete := FDefConfig.Abilities.AllowDelete;
  FAllowAdd := FDefConfig.Abilities.AllowAdd;
  FAllowReplace := FDefConfig.Abilities.AllowReplace;
  FUseMemCache := FDefConfig.UseMemCache;
  FMemCacheCapacity := FDefConfig.MemCacheCapacity;
  FMemCacheTTL := FDefConfig.MemCacheTTL;
  FMemCacheClearStrategy := FDefConfig.MemCacheClearStrategy;
end;

function TSimpleTileStorageConfig.CreateStatic: IInterface;
var
  VStatic: ISimpleTileStorageConfigStatic;
  VStorageAbilities: ITileStorageAbilities;
begin
  VStorageAbilities :=
    TTileStorageAbilities.Create(
      FIsReadOnly,
      True,
      True,
      FAllowAdd,
      FAllowDelete,
      FAllowReplace
    );
  VStatic :=
    TSimpleTileStorageConfigStatic.Create(
      FCacheTypeCode,
      FNameInCache,
      FDefConfig.TileFileExt,
      VStorageAbilities,
      FUseMemCache,
      FMemCacheCapacity,
      FMemCacheTTL,
      FMemCacheClearStrategy
    );
  Result := VStatic;
end;

procedure TSimpleTileStorageConfig.DoReadConfig(
  const AConfigData: IConfigDataProvider
);
begin
  inherited;
  if AConfigData <> nil then begin
    SetCacheTypeCode(AConfigData.ReadInteger('CacheType', FCacheTypeCode));
    SetNameInCache(AConfigData.ReadString('NameInCache', FNameInCache));
    SetIsReadOnly(AConfigData.ReadBool('IsReadOnly', FIsReadOnly));
    SetChanged;
  end;
end;

procedure TSimpleTileStorageConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if FCacheTypeCode <> FDefConfig.CacheTypeCode then begin
    AConfigData.WriteInteger('CacheType', FCacheTypeCode);
  end else begin
    AConfigData.DeleteValue('CacheType');
  end;
  if FNameInCache <> FDefConfig.NameInCache then begin
    AConfigData.WriteString('NameInCache', FNameInCache);
  end else begin
    AConfigData.DeleteValue('NameInCache');
  end;
  if FIsReadOnly <> FDefConfig.Abilities.IsReadOnly then begin
    AConfigData.WriteBool('IsReadOnly', FIsReadOnly);
  end else begin
    AConfigData.DeleteValue('IsReadOnly');
  end;
end;

function TSimpleTileStorageConfig.GetAllowAdd: Boolean;
begin
  LockRead;
  try
    Result := FAllowAdd;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetAllowDelete: Boolean;
begin
  LockRead;
  try
    Result := FAllowDelete;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetAllowReplace: Boolean;
begin
  LockRead;
  try
    Result := FAllowReplace;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetCacheTypeCode: Integer;
begin
  LockRead;
  try
    Result := FCacheTypeCode;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetIsReadOnly: Boolean;
begin
  LockRead;
  try
    Result := FIsReadOnly;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetNameInCache: string;
begin
  LockRead;
  try
    Result := FNameInCache;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetStatic: ISimpleTileStorageConfigStatic;
begin
  Result := ISimpleTileStorageConfigStatic(GetStaticInternal);
end;

function TSimpleTileStorageConfig.GetTileFileExt: AnsiString;
begin
  Result := FDefConfig.TileFileExt;
end;

function TSimpleTileStorageConfig.GetUseMemCache: Boolean;
begin
  LockRead;
  try
    Result := FUseMemCache;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetMemCacheCapacity: Integer;
begin
  LockRead;
  try
    Result := FMemCacheCapacity;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetMemCacheTTL: Cardinal;
begin
  LockRead;
  try
    Result := FMemCacheTTL;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetMemCacheClearStrategy: Integer;
begin
  LockRead;
  try
    Result := FMemCacheClearStrategy;
  finally
    UnlockRead;
  end;
end;

procedure TSimpleTileStorageConfig.SetAllowAdd(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.Abilities.AllowAdd and (not FIsReadOnly) and AValue;
    if FAllowAdd <> VValue then begin
      FAllowAdd := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetAllowDelete(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.Abilities.AllowDelete and (not FIsReadOnly) and AValue;
    if FAllowDelete <> VValue then begin
      FAllowDelete := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetAllowReplace(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.Abilities.AllowReplace and (not FIsReadOnly) and AValue;
    if FAllowReplace <> VValue then begin
      FAllowReplace := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetCacheTypeCode(AValue: Integer);
begin
  if FDefConfig.CacheTypeCode <> 5 then begin
    if AValue <> c_File_Cache_Id_GE then begin
      LockWrite;
      try
        if FCacheTypeCode <> AValue then begin
          FCacheTypeCode := AValue;
          SetChanged;
        end;
      finally
        UnlockWrite;
      end;
    end;
  end;
end;

procedure TSimpleTileStorageConfig.SetIsReadOnly(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.Abilities.IsReadOnly or (FNameInCache = '') or AValue;
    if FIsReadOnly <> VValue then begin
      FIsReadOnly := VValue;
      SetAllowDelete(FAllowDelete);
      SetAllowAdd(FAllowAdd);
      SetAllowReplace(FAllowReplace);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetNameInCache(const AValue: string);
begin
  LockWrite;
  try
    if FNameInCache <> AValue then begin
      FNameInCache := AValue;
      SetIsReadOnly(FIsReadOnly);
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetUseMemCache(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUseMemCache <> AValue then begin
      FUseMemCache := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetMemCacheCapacity(const AValue: Integer);
begin
  LockWrite;
  try
    if FMemCacheCapacity <> AValue then begin
      FMemCacheCapacity := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetMemCacheTTL(const AValue: Cardinal);
begin
  LockWrite;
  try
    if FMemCacheTTL <> AValue then begin
      FMemCacheTTL := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TSimpleTileStorageConfig.SetMemCacheClearStrategy(const AValue: Integer);
begin
  LockWrite;
  try
    if FMemCacheClearStrategy <> AValue then begin
      FMemCacheClearStrategy := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
