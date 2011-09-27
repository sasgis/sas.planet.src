{******************************************************************************}
{* SAS.Планета (SAS.Planet)                                                   *}
{* Copyright (C) 2007-2011, авторы программы SAS.Планета (SAS.Planet).        *}
{* Это программа является свободным программным обеспечением. Вы можете       *}
{* распространять и/или модифицировать её согласно условиям Стандартной       *}
{* Общественной Лицензии GNU, опубликованной Фондом Свободного Программного   *}
{* Обеспечения, версии 3. Эта программа распространяется в надежде, что она   *}
{* будет полезной, но БЕЗ ВСЯКИХ ГАРАНТИЙ, в том числе подразумеваемых        *}
{* гарантий ТОВАРНОГО СОСТОЯНИЯ ПРИ ПРОДАЖЕ и ГОДНОСТИ ДЛЯ ОПРЕДЕЛЁННОГО      *}
{* ПРИМЕНЕНИЯ. Смотрите Стандартную Общественную Лицензию GNU версии 3, для   *}
{* получения дополнительной информации. Вы должны были получить копию         *}
{* Стандартной Общественной Лицензии GNU вместе с программой. В случае её     *}
{* отсутствия, посмотрите http://www.gnu.org/licenses/.                       *}
{*                                                                            *}
{* http://sasgis.ru/sasplanet                                                 *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_MapAbilitiesConfig;

interface

uses
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapAbilitiesConfig,
  i_SimpleTileStorageConfig,
  u_ConfigDataElementBase;

type
  TMapAbilitiesConfig = class(TConfigDataElementBase, IMapAbilitiesConfig)
  private
    FDefConfig: IMapAbilitiesConfigStatic;
    FStorageConfig: ISimpleTileStorageConfig;
    FStorageConfigListener: IJclListener;

    FIsShowOnSmMap: Boolean;
    FIsUseStick: Boolean;
    FIsUseGenPrevious: Boolean;
    FUseDownload: Boolean;

    FStatic: IMapAbilitiesConfigStatic;
    function CreateStatic: IMapAbilitiesConfigStatic;
    procedure OnStorageConfigChange(Sender: TObject);
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
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
      ADefConfig: IMapAbilitiesConfigStatic;
      AStorageConfig: ISimpleTileStorageConfig
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_NotifyEventListener,
  u_MapAbilitiesConfigStatic;

{ TMapAbilitiesConfig }

constructor TMapAbilitiesConfig.Create(
  ADefConfig: IMapAbilitiesConfigStatic;
  AStorageConfig: ISimpleTileStorageConfig
);
begin
  inherited Create;
  FDefConfig := ADefConfig;
  FStorageConfig := AStorageConfig;

  FIsShowOnSmMap := FDefConfig.IsShowOnSmMap;
  FIsUseStick := FDefConfig.IsUseStick;
  FIsUseGenPrevious := FDefConfig.IsUseGenPrevious;
  FUseDownload := FDefConfig.UseDownload;

  FStorageConfigListener := TNotifyEventListener.Create(Self.OnStorageConfigChange);
  FStorageConfig.GetChangeNotifier.Add(FStorageConfigListener);
  FStatic := CreateStatic;
end;

destructor TMapAbilitiesConfig.Destroy;
begin
  FStorageConfig.GetChangeNotifier.Remove(FStorageConfigListener);
  FStorageConfigListener := nil;
  FStorageConfig := nil;

  inherited;
end;

function TMapAbilitiesConfig.CreateStatic: IMapAbilitiesConfigStatic;
begin
  Result :=
    TMapAbilitiesConfigStatic.Create(
      FDefConfig.IsLayer,
      FIsShowOnSmMap,
      FIsUseStick,
      FIsUseGenPrevious,
      FUseDownload
    );
end;

procedure TMapAbilitiesConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TMapAbilitiesConfig.DoReadConfig(AConfigData: IConfigDataProvider);
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
  AConfigData: IConfigDataWriteProvider);
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

procedure TMapAbilitiesConfig.OnStorageConfigChange(Sender: TObject);
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
  Result := FStatic;
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
