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

unit u_KmlLayerConfig;

interface

uses
  GR32,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_KmlLayerConfig,
  u_ConfigDataElementBase;

type
  TKmlLayerConfig = class(TConfigDataElementBase, IKmlLayerConfig)
  private
    FMainColor: TColor32;
    FPointColor: TColor32;
    FShadowColor: TColor32;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetMainColor: TColor32;
    procedure SetMainColor(AValue: TColor32);

    function GetPointColor: TColor32;
    procedure SetPointColor(AValue: TColor32);

    function GetShadowColor: TColor32;
    procedure SetShadowColor(AValue: TColor32);
  public
    constructor Create;
  end;
implementation

uses
  u_ConfigProviderHelpers;

{ TKmlLayerConfig }

constructor TKmlLayerConfig.Create;
begin
  inherited;
  FMainColor := clWhite32;
  FShadowColor := clBlack32;
  FPointColor := SetAlpha(clWhite32, 170);
end;

procedure TKmlLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FMainColor := ReadColor32(AConfigData, 'MainColor', FMainColor);
    FPointColor := ReadColor32(AConfigData, 'PointColor', FPointColor);
    FShadowColor := ReadColor32(AConfigData, 'ShadowColor', FShadowColor);
    SetChanged;
  end;
end;

procedure TKmlLayerConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  WriteColor32(AConfigData, 'MainColor', FMainColor);
  WriteColor32(AConfigData, 'PointColor', FPointColor);
  WriteColor32(AConfigData, 'ShadowColor', FShadowColor);
end;

function TKmlLayerConfig.GetMainColor: TColor32;
begin
  LockRead;
  try
    Result := FMainColor;
  finally
    UnlockRead;
  end;
end;

function TKmlLayerConfig.GetPointColor: TColor32;
begin
  LockRead;
  try
    Result := FPointColor;
  finally
    UnlockRead;
  end;
end;

function TKmlLayerConfig.GetShadowColor: TColor32;
begin
  LockRead;
  try
    Result := FShadowColor;
  finally
    UnlockRead;
  end;
end;

procedure TKmlLayerConfig.SetMainColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FMainColor <> AValue then begin
      FMainColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKmlLayerConfig.SetPointColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FPointColor <> AValue then begin
      FPointColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TKmlLayerConfig.SetShadowColor(AValue: TColor32);
begin
  LockWrite;
  try
    if FShadowColor <> AValue then begin
      FShadowColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
