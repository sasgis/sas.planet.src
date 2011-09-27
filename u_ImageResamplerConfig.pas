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

unit u_ImageResamplerConfig;

interface

uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ImageResamplerFactory,
  i_ImageResamplerConfig,
  u_ConfigDataElementBase;

type
  TImageResamplerConfig = class(TConfigDataElementBase, IImageResamplerConfig)
  private
    FList: IImageResamplerFactoryList;
    FActiveIndex: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetList: IImageResamplerFactoryList;
    function GetActiveIndex: Integer;
    procedure SetActiveIndex(AValue: Integer);
    function GetActiveFactory: IImageResamplerFactory;
  public
    constructor Create(AList: IImageResamplerFactoryList);
  end;

implementation

{ TMainFormMainConfig }

constructor TImageResamplerConfig.Create(AList: IImageResamplerFactoryList);
begin
  inherited Create;
  FList := AList;
end;

procedure TImageResamplerConfig.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FActiveIndex := AConfigData.ReadInteger('ResamplingType', FActiveIndex);
    if FActiveIndex < 0 then begin
      FActiveIndex := 0;
    end else if FActiveIndex >= FList.Count then begin
      FActiveIndex := FList.Count - 1;
    end;
    SetChanged;
  end;
end;

procedure TImageResamplerConfig.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('ResamplingType', FActiveIndex);
end;

function TImageResamplerConfig.GetActiveFactory: IImageResamplerFactory;
begin
  LockRead;
  try
    Result := FList.Items[FActiveIndex];
  finally
    UnlockRead;
  end;
end;

function TImageResamplerConfig.GetActiveIndex: Integer;
begin
  LockRead;
  try
    Result := FActiveIndex;
  finally
    UnlockRead;
  end;
end;

function TImageResamplerConfig.GetList: IImageResamplerFactoryList;
begin
  Result := FList;
end;

procedure TImageResamplerConfig.SetActiveIndex(AValue: Integer);
begin
  LockWrite;
  try
    if FActiveIndex <> AValue then begin
      FActiveIndex := AValue;
      if FActiveIndex < 0 then begin
        FActiveIndex := 0;
      end else if FActiveIndex >= FList.Count then begin
        FActiveIndex := FList.Count - 1;
      end;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
