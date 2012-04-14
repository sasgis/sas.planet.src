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

unit u_SimpleTileStorageConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_CoordConverter,
  i_SimpleTileStorageConfig,
  u_ConfigDataElementBase;

type
  TSimpleTileStorageConfig = class(TConfigDataElementWithStaticBase, ISimpleTileStorageConfig)
  private
    FDefConfig: ISimpleTileStorageConfigStatic;

    FCacheTypeCode: Integer;
    FNameInCache: string;
    FIsReadOnly: boolean;
    FAllowDelete: boolean;
    FAllowAdd: boolean;
    FAllowReplace: boolean;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetCoordConverter: ICoordConverter;

    function GetCacheTypeCode: Integer;
    procedure SetCacheTypeCode(AValue: Integer);

    function GetNameInCache: string;
    procedure SetNameInCache(const AValue: string);

    function GetTileFileExt: string;
    function GetIsStoreFileCache: Boolean;

    function GetIsReadOnly: boolean;
    procedure SetIsReadOnly(AValue: Boolean);

    function GetAllowDelete: boolean;
    procedure SetAllowDelete(AValue: Boolean);

    function GetAllowAdd: boolean;
    procedure SetAllowAdd(AValue: Boolean);

    function GetAllowReplace: boolean;
    procedure SetAllowReplace(AValue: Boolean);

    function GetStatic: ISimpleTileStorageConfigStatic;
  public
    constructor Create(ADefConfig: ISimpleTileStorageConfigStatic);
  end;

implementation

uses
  u_SimpleTileStorageConfigStatic;

{ TSimpleTileStorageConfig }

constructor TSimpleTileStorageConfig.Create(
  ADefConfig: ISimpleTileStorageConfigStatic);
begin
  inherited Create;
  FDefConfig := ADefConfig;

  FCacheTypeCode := FDefConfig.CacheTypeCode;
  FNameInCache := FDefConfig.NameInCache;

  FIsReadOnly := FDefConfig.IsReadOnly;
  FAllowDelete := FDefConfig.AllowDelete;
  FAllowAdd := FDefConfig.AllowAdd;
  FAllowReplace := FDefConfig.AllowReplace;
end;

function TSimpleTileStorageConfig.CreateStatic: IInterface;
var
  VStatic: ISimpleTileStorageConfigStatic;
begin
  VStatic :=
    TSimpleTileStorageConfigStatic.Create(
      FDefConfig.CoordConverter,
      FCacheTypeCode,
      FNameInCache,
      FDefConfig.TileFileExt,
      FDefConfig.IsStoreFileCache,
      FIsReadOnly,
      FAllowDelete,
      FAllowAdd,
      FAllowReplace
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
  if FIsReadOnly <> FDefConfig.IsReadOnly then begin
    AConfigData.WriteBool('IsReadOnly', FIsReadOnly);
  end else begin
    AConfigData.DeleteValue('IsReadOnly');
  end;
end;

function TSimpleTileStorageConfig.GetAllowAdd: boolean;
begin
  LockRead;
  try
    Result := FAllowAdd;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetAllowDelete: boolean;
begin
  LockRead;
  try
    Result := FAllowDelete;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetAllowReplace: boolean;
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

function TSimpleTileStorageConfig.GetCoordConverter: ICoordConverter;
begin
  Result := FDefConfig.CoordConverter;
end;

function TSimpleTileStorageConfig.GetIsReadOnly: boolean;
begin
  LockRead;
  try
    Result := FIsReadOnly;
  finally
    UnlockRead;
  end;
end;

function TSimpleTileStorageConfig.GetIsStoreFileCache: Boolean;
begin
  Result := FDefConfig.IsStoreFileCache;
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

function TSimpleTileStorageConfig.GetTileFileExt: string;
begin
  Result := FDefConfig.TileFileExt;
end;

procedure TSimpleTileStorageConfig.SetAllowAdd(AValue: Boolean);
var
  VValue: Boolean;
begin
  LockWrite;
  try
    VValue := FDefConfig.AllowAdd and (not FIsReadOnly) and AValue;
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
    VValue := FDefConfig.AllowDelete and (not FIsReadOnly) and AValue;
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
    VValue := FDefConfig.AllowReplace and (not FIsReadOnly) and AValue;
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
  if FDefConfig.CacheTypeCode <> 5  then begin
    if AValue <> 5 then begin
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
    VValue := FDefConfig.IsReadOnly or (FNameInCache = '') or AValue;
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

end.
