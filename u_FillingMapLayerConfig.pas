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

unit u_FillingMapLayerConfig;

interface

uses
  GR32,
  i_LocalCoordConverter,
  i_ActiveMapsConfig,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_FillingMapLayerConfig,
  u_ConfigDataElementComplexBase;

type
  TFillingMapLayerConfig = class(TConfigDataElementComplexBase, IFillingMapLayerConfig)
  private
    FVisible: Boolean;
    FUseRelativeZoom: Boolean;
    FZoom: Byte;
    FNoTileColor: TColor32;
    FShowTNE: Boolean;
    FTNEColor: TColor32;
    FSourceMap: IFillingMapMapsConfig;
    FStatic: IFillingMapLayerConfigStatic;
    function CreateStatic: IFillingMapLayerConfigStatic;
  protected
    procedure DoBeforeChangeNotify; override;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetVisible: Boolean;
    procedure SetVisible(const AValue: Boolean);

    function GetUseRelativeZoom: Boolean;
    procedure SetUseRelativeZoom(const AValue: Boolean);

    function GetZoom: Byte;
    procedure SetZoom(const AValue: Byte);

    function GetNoTileColor: TColor32;
    procedure SetNoTileColor(const AValue: TColor32);

    function GetShowTNE: Boolean;
    procedure SetShowTNE(const AValue: Boolean);

    function GetTNEColor: TColor32;
    procedure SetTNEColor(const AValue: TColor32);

    function GetSourceMap: IFillingMapMapsConfig;
    function GetStatic: IFillingMapLayerConfigStatic;
    function GetActualZoom(ALocalConverter: ILocalCoordConverter): Byte;
  public
    constructor Create(AMapsConfig: IMainMapsConfig);
  end;

implementation

uses
  u_ConfigSaveLoadStrategyBasicUseProvider,
  u_ConfigProviderHelpers,
  u_FillingMapMapsConfig,
  u_FillingMapLayerConfigStatic;

{ TFillingMapLayerConfig }

constructor TFillingMapLayerConfig.Create(AMapsConfig: IMainMapsConfig);
begin
  inherited Create;
  FVisible := False;
  FUseRelativeZoom := False;
  FZoom := 0;
  FShowTNE := True;
  FNoTileColor := SetAlpha(clBlack32, 110);
  FTNEColor := SetAlpha(clRed32, 110);

  FSourceMap := TFillingMapMapsConfig.Create(AMapsConfig);
  Add(FSourceMap, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FStatic := CreateStatic;
end;

function TFillingMapLayerConfig.CreateStatic: IFillingMapLayerConfigStatic;
begin
  Result :=
    TFillingMapLayerConfigStatic.Create(
      FVisible,
      FSourceMap.GetActualMap,
      FUseRelativeZoom,
      FZoom,
      FNoTileColor,
      FShowTNE,
      FTNEColor
    );
end;

procedure TFillingMapLayerConfig.DoBeforeChangeNotify;
begin
  inherited;
  LockWrite;
  try
    FStatic := CreateStatic;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FVisible := AConfigData.ReadBool('Visible', FVisible);
    FUseRelativeZoom := AConfigData.ReadBool('RelativeZoom', FUseRelativeZoom);
    FZoom := AConfigData.ReadInteger('Zoom', FZoom);
    FShowTNE := AConfigData.ReadBool('ShowTNE', FShowTNE);
    FNoTileColor := ReadColor32(AConfigData, 'NoTileColor', FNoTileColor);
    FTNEColor := ReadColor32(AConfigData, 'TNEColor', FTNEColor);

    SetChanged;
  end;
end;

procedure TFillingMapLayerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('Visible', FVisible);
  AConfigData.WriteBool('RelativeZoom', FUseRelativeZoom);
  AConfigData.WriteInteger('Zoom', FZoom);
  AConfigData.WriteBool('ShowTNE', FShowTNE);
  WriteColor32(AConfigData, 'NoTileColor', FNoTileColor);
  WriteColor32(AConfigData, 'TNEColor', FTNEColor);
end;

function TFillingMapLayerConfig.GetActualZoom(
  ALocalConverter: ILocalCoordConverter): Byte;
var
  VZoom: Integer;
  VRelative: Boolean;
begin
  LockRead;
  try
    VZoom := FZoom;
    VRelative := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
  if VRelative then begin
    VZoom := VZoom + ALocalConverter.GetZoom;
  end;
  if VZoom < 0 then begin
    Result := 0;
  end else begin
    Result := VZoom;
    ALocalConverter.GetGeoConverter.CheckZoom(Result);
  end;
end;

function TFillingMapLayerConfig.GetNoTileColor: TColor32;
begin
  LockRead;
  try
    Result := FNoTileColor;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetShowTNE: Boolean;
begin
  LockRead;
  try
    Result := FShowTNE;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetSourceMap: IFillingMapMapsConfig;
begin
  Result := FSourceMap;
end;

function TFillingMapLayerConfig.GetZoom: Byte;
begin
  LockRead;
  try
    Result := FZoom;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetStatic: IFillingMapLayerConfigStatic;
begin
  LockRead;
  try
    Result := FStatic;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetTNEColor: TColor32;
begin
  LockRead;
  try
    Result := FTNEColor;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetUseRelativeZoom: Boolean;
begin
  LockRead;
  try
    Result := FUseRelativeZoom;
  finally
    UnlockRead;
  end;
end;

function TFillingMapLayerConfig.GetVisible: Boolean;
begin
  LockRead;
  try
    Result := FVisible;
  finally
    UnlockRead;
  end;
end;

procedure TFillingMapLayerConfig.SetNoTileColor(const AValue: TColor32);
begin
  LockWrite;
  try
    if FNoTileColor <> AValue then begin
      FNoTileColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetShowTNE(const AValue: Boolean);
begin
  LockWrite;
  try
    if FShowTNE <> AValue then begin
      FShowTNE := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetZoom(const AValue: Byte);
begin
  LockWrite;
  try
    if FZoom <> AValue then begin
      FZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetTNEColor(const AValue: TColor32);
begin
  LockWrite;
  try
    if FTNEColor <> AValue then begin
      FTNEColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetUseRelativeZoom(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUseRelativeZoom <> AValue then begin
      FUseRelativeZoom := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TFillingMapLayerConfig.SetVisible(const AValue: Boolean);
begin
  LockWrite;
  try
    if FVisible <> AValue then begin
      FVisible := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
