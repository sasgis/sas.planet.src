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

unit u_GotoLayerConfig;

interface
uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_GotoLayerConfig,
  u_ConfigDataElementBase;

type
  TGotoLayerConfig = class(TConfigDataElementBase, IGotoLayerConfig)
  private
    FShowTickCount: Cardinal;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetShowTickCount: Cardinal;
    procedure SetShowTickCount(AValue: Cardinal);
  public
    constructor Create;
  end;

implementation

uses
  Types,
  SysUtils;

{ TGotoLayerConfig }

constructor TGotoLayerConfig.Create;
begin
  inherited;
  FShowTickCount := 20000;
end;

procedure TGotoLayerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FShowTickCount := AConfigData.ReadInteger('ShowTickCount', FShowTickCount);
    SetChanged;
  end;
end;

procedure TGotoLayerConfig.DoWriteConfig(AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('ShowTickCount', FShowTickCount);
end;

function TGotoLayerConfig.GetShowTickCount: Cardinal;
begin
  LockRead;
  try
    Result := FShowTickCount;
  finally
    UnlockRead;
  end;
end;

procedure TGotoLayerConfig.SetShowTickCount(AValue: Cardinal);
begin
  LockWrite;
  try
    if FShowTickCount <> AValue then begin
      FShowTickCount := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
