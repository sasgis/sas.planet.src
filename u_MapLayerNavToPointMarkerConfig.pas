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

unit u_MapLayerNavToPointMarkerConfig;

interface

uses
  Types,
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_MapLayerNavToPointMarkerConfig,
  i_BitmapMarkerProviderSimpleConfig,
  u_ConfigDataElementComplexBase;

type
  TMapLayerNavToPointMarkerConfig = class(TConfigDataElementComplexBase, IMapLayerNavToPointMarkerConfig)
  private
    FCrossDistInPixels: Double;
    FArrowMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    FReachedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetCrossDistInPixels: Double;
    procedure SetCrossDistInPixels(AValue: Double);

    function GetArrowMarkerConfig: IBitmapMarkerProviderSimpleConfig;
    function GetReachedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
  public
    constructor Create;
  end;

implementation

uses
  SysUtils,
  u_BitmapMarkerProviderSimpleConfig,
  u_BitmapMarkerProviderSimpleConfigStatic,
  u_ConfigSaveLoadStrategyBasicProviderSubItem;

{ TMapLayerGPSMarkerConfig }

constructor TMapLayerNavToPointMarkerConfig.Create;
begin
  inherited;
  FCrossDistInPixels := 100;

  FArrowMarkerConfig :=
    TBitmapMarkerProviderSimpleConfig.Create(
      TBitmapMarkerProviderSimpleConfigStatic.Create(
        25,
        SetAlpha(clRed32, 150),
        SetAlpha(clBlack32, 200)
      )
    );
  Add(FArrowMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ArrowMarker'));

  FReachedMarkerConfig :=
    TBitmapMarkerProviderSimpleConfig.Create(
      TBitmapMarkerProviderSimpleConfigStatic.Create(
        20,
        SetAlpha(clRed32, 200),
        SetAlpha(clBlack32, 200)
      )
    );
  Add(FReachedMarkerConfig, TConfigSaveLoadStrategyBasicProviderSubItem.Create('ReachedPointMarker'));
end;

procedure TMapLayerNavToPointMarkerConfig.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FCrossDistInPixels := AConfigData.ReadFloat('CrossDistInPixels', FCrossDistInPixels);
    SetChanged;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteFloat('CrossDistInPixels', FCrossDistInPixels);
end;

function TMapLayerNavToPointMarkerConfig.GetArrowMarkerConfig: IBitmapMarkerProviderSimpleConfig;
begin
  Result := FArrowMarkerConfig;
end;

function TMapLayerNavToPointMarkerConfig.GetReachedMarkerConfig: IBitmapMarkerProviderSimpleConfig;
begin
  Result := FReachedMarkerConfig;
end;

function TMapLayerNavToPointMarkerConfig.GetCrossDistInPixels: Double;
begin
  LockRead;
  try
    Result := FCrossDistInPixels;
  finally
    UnlockRead;
  end;
end;

procedure TMapLayerNavToPointMarkerConfig.SetCrossDistInPixels(AValue: Double);
begin
  LockWrite;
  try
    if FCrossDistInPixels <> AValue then begin
      FCrossDistInPixels := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
