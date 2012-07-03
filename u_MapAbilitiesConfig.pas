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

unit u_MapAbilitiesConfig;

interface

uses
  i_Notifier, i_Listener,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapAbilitiesConfig,
  i_SimpleTileStorageConfig,
  u_ConfigDataElementBase;

type
  TMapAbilitiesConfig = class(TConfigDataElementWithStaticBase, IMapAbilitiesConfig)
  private
    FDefConfig: IMapAbilitiesConfigStatic;
    FStorageConfig: ISimpleTileStorageConfig;
    FStorageConfigListener: IListener;

    FIsShowOnSmMap: Boolean;
    FIsUseStick: Boolean;
    FIsUseGenPrevious: Boolean;
    FUseDownload: Boolean;

    procedure OnStorageConfigChange;
  protected
    function CreateStatic: IInterface; override;
  protected
    procedure DoReadConfig(const AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(const AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsLayer: Boolean;

    function GetIsShowOnSmMap: Boolean;
    procedure SetIsShowOnSmMap(AValue: Boolean);

    function GetIsUseStick: Boolean;
    procedure SetIsUseStick(AValue: Boolean);

    function GetIsUseGenPrevious: Boolean;
    procedure SetIsUseGenPrevious(AValue: Boolean);

    function GetUseDownload: Boolean;
    procedure SetUseDownload(AValue: Boolean);

    function GetStatic: IMapAbilitiesConfigStatic;
  public
    constructor Create(
      const ADefConfig: IMapAbilitiesConfigStatic;
      const AStorageConfig: ISimpleTileStorageConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener,
  u_MapAbilitiesConfigStatic;

{ TMapAbilitiesConfig }

constructor TMapAbilitiesConfig.Create(
  const ADefConfig: IMapAbilitiesConfigStatic;
  const AStorageConfig: ISimpleTileStorageConfig
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FStorageConfig := AStorageConfig;

  FIsShowOnSmMap := FDefConfig.IsShowOnSmMap;
  FIsUseStick := FDefConfig.IsUseStick;
  FIsUseGenPrevious := FDefConfig.IsUseGenPrevious;
  FUseDownload := FDefConfig.UseDownload;

  FStorageConfigListener := TNotifyNoMmgEventListener.Create(Self.OnStorageConfigChange);
  FStorageConfig.GetChangeNotifier.Add(FStorageConfigListener);
end;

destructor TMapAbilitiesConfig.Destroy;
begin
  FStorageConfig.GetChangeNotifier.Remove(FStorageConfigListener);
  FStorageConfigListener := nil;
  FStorageConfig := nil;

  inherited;
end;

function TMapAbilitiesConfig.CreateStatic: IInterface;
var
  VStatic: IMapAbilitiesConfigStatic;
begin
  VStatic :=
    TMapAbilitiesConfigStatic.Create(
      FDefConfig.IsLayer,
      FIsShowOnSmMap,
      FIsUseStick,
      FIsUseGenPrevious,
      FUseDownload
    );
  Result := VStatic;
end;

procedure TMapAbilitiesConfig.DoReadConfig(const AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    SetIsShowOnSmMap(AConfigData.ReadBool('CanShowOnSmMap', FIsShowOnSmMap));
    SetIsUseStick(AConfigData.ReadBool('Usestick', FIsUseStick));
    SetIsUseGenPrevious(AConfigData.ReadBool('UseGenPrevious', FIsUseGenPrevious));
    SetUseDownload(AConfigData.ReadBool('UseDwn', FUseDownload));
    SetChanged;
  end;
end;

procedure TMapAbilitiesConfig.DoWriteConfig(
  const AConfigData: IConfigDataWriteProvider
);
begin
  inherited;
  if FIsShowOnSmMap <> FDefConfig.IsShowOnSmMap then begin
    AConfigData.WriteBool('CanShowOnSmMap', FIsShowOnSmMap);
  end else begin
    AConfigData.DeleteValue('CanShowOnSmMap');
  end;
  if FIsUseStick <> FDefConfig.IsUseStick then begin
    AConfigData.WriteBool('Usestick', FIsUseStick);
  end else begin
    AConfigData.DeleteValue('Usestick');
  end;
  if FIsUseGenPrevious <> FDefConfig.IsUseGenPrevious then begin
    AConfigData.WriteBool('UseGenPrevious', FIsUseGenPrevious);
  end else begin
    AConfigData.DeleteValue('UseGenPrevious');
  end;
  if FUseDownload <> FDefConfig.UseDownload then begin
    AConfigData.WriteBool('UseDwn', FUseDownload);
  end else begin
    AConfigData.DeleteValue('UseDwn');
  end;
end;

function TMapAbilitiesConfig.GetIsLayer: Boolean;
begin
  LockRead;
  try
    Result := FDefConfig.IsLayer;
  finally
    UnlockRead;
  end;
end;

function TMapAbilitiesConfig.GetIsShowOnSmMap: Boolean;
begin
  LockRead;
  try
    Result := FIsShowOnSmMap;
  finally
    UnlockRead;
  end;
end;

function TMapAbilitiesConfig.GetIsUseGenPrevious: Boolean;
begin
  LockRead;
  try
    Result := FIsUseGenPrevious;
  finally
    UnlockRead;
  end;
end;

function TMapAbilitiesConfig.GetIsUseStick: Boolean;
begin
  LockRead;
  try
    Result := FIsUseStick;
  finally
    UnlockRead;
  end;
end;

function TMapAbilitiesConfig.GetUseDownload: Boolean;
begin
  LockRead;
  try
    Result := FUseDownload;
  finally
    UnlockRead;
  end;
end;

procedure TMapAbilitiesConfig.OnStorageConfigChange;
begin
  LockWrite;
  try
    SetIsUseGenPrevious(FIsUseGenPrevious);
    SetUseDownload(FUseDownload);
  finally
    UnlockWrite;
  end;
end;

function TMapAbilitiesConfig.GetStatic: IMapAbilitiesConfigStatic;
begin
  Result := IMapAbilitiesConfigStatic(GetStaticInternal);
end;

procedure TMapAbilitiesConfig.SetIsShowOnSmMap(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsShowOnSmMap <> AValue then begin
      FIsShowOnSmMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapAbilitiesConfig.SetIsUseGenPrevious(AValue: Boolean);
var
  VValue: Boolean;
  VStorageConfig: ISimpleTileStorageConfigStatic;
begin
  VStorageConfig := FStorageConfig.GetStatic;
  LockWrite;
  try
    VValue := FDefConfig.IsUseGenPrevious and VStorageConfig.AllowAdd and AValue;
    if FIsUseGenPrevious <> VValue then begin
      FIsUseGenPrevious := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapAbilitiesConfig.SetIsUseStick(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsUseStick <> AValue then begin
      FIsUseStick := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TMapAbilitiesConfig.SetUseDownload(AValue: Boolean);
var
  VValue: Boolean;
  VStorageConfig: ISimpleTileStorageConfigStatic;
begin
  VStorageConfig := FStorageConfig.GetStatic;
  LockWrite;
  try
    VValue := FDefConfig.UseDownload and VStorageConfig.AllowAdd and AValue;
    if FUseDownload <> VValue then begin
      FUseDownload := VValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.


