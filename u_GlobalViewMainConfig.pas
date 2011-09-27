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

unit u_GlobalViewMainConfig;

interface

uses
  Graphics,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GlobalViewMainConfig,
  u_ConfigDataElementBase;

type
  TGlobalViewMainConfig = class(TConfigDataElementBase, IGlobalViewMainConfig)
  private
    FBackGroundColor: TColor;
    FUsePrevZoomAtMap: Boolean;
    FUsePrevZoomAtLayer: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetBackGroundColor: TColor;
    procedure SetBackGroundColor(const AValue: TColor);

    function GetUsePrevZoomAtMap: Boolean;
    procedure SetUsePrevZoomAtMap(const AValue: Boolean);

    function GetUsePrevZoomAtLayer: Boolean;
    procedure SetUsePrevZoomAtLayer(const AValue: Boolean);
  public
    constructor Create;
  end;

implementation

{ TGlobalViewMainConfig }

constructor TGlobalViewMainConfig.Create;
begin
  inherited;
  FBackGroundColor := clSilver;
  FUsePrevZoomAtMap := True;
  FUsePrevZoomAtLayer := True;
end;

procedure TGlobalViewMainConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FUsePrevZoomAtMap := AConfigData.ReadBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
    FUsePrevZoomAtLayer := AConfigData.ReadBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
    FBackGroundColor := TColor(AConfigData.ReadInteger('BackgroundColor', FBackGroundColor));
    SetChanged;
  end;
end;

procedure TGlobalViewMainConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('UsePrevZoomAtMap', FUsePrevZoomAtMap);
  AConfigData.WriteBool('UsePrevZoomAtLayer', FUsePrevZoomAtLayer);
  AConfigData.WriteInteger('BackgroundColor', Integer(FBackGroundColor));
end;

function TGlobalViewMainConfig.GetBackGroundColor: TColor;
begin
  LockRead;
  try
    Result := FBackGroundColor;
  finally
    UnlockRead;
  end;
end;

function TGlobalViewMainConfig.GetUsePrevZoomAtLayer: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtLayer;
  finally
    UnlockRead;
  end;
end;

function TGlobalViewMainConfig.GetUsePrevZoomAtMap: Boolean;
begin
  LockRead;
  try
    Result := FUsePrevZoomAtMap;
  finally
    UnlockRead;
  end;
end;

procedure TGlobalViewMainConfig.SetBackGroundColor(const AValue: TColor);
begin
  LockWrite;
  try
    if FBackGroundColor <> AValue then begin
      FBackGroundColor := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalViewMainConfig.SetUsePrevZoomAtLayer(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtLayer <> AValue then begin
      FUsePrevZoomAtLayer := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

procedure TGlobalViewMainConfig.SetUsePrevZoomAtMap(const AValue: Boolean);
begin
  LockWrite;
  try
    if FUsePrevZoomAtMap <> AValue then begin
      FUsePrevZoomAtMap := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
