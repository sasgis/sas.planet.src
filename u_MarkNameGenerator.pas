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

unit u_MarkNameGenerator;

interface


uses
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_StringConfigDataElement,
  i_MarkNameGenerator,
  u_ConfigDataElementComplexBase;

type
  TMarkNameGenerator = class(TConfigDataElementComplexBase, IMarkNameGenerator)
  private
    FFormatString: IStringConfigDataElement;
    FCounter: Integer;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetFormatString: IStringConfigDataElement;

    function GetCounter: Integer;
    procedure SetCounter(AValue: Integer);

    function GetNewName: string;
  public
    constructor Create(AFormatString: IStringConfigDataElement);
  end;

implementation

uses
  SysUtils,
  u_ConfigSaveLoadStrategyBasicUseProvider;

{ TMarkNameGenerator }

constructor TMarkNameGenerator.Create(AFormatString: IStringConfigDataElement);
begin
  inherited Create;
  FFormatString := AFormatString;
  Add(FFormatString, TConfigSaveLoadStrategyBasicUseProvider.Create);
  FCounter := 0;
end;

procedure TMarkNameGenerator.DoReadConfig(AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FCounter := AConfigData.ReadInteger('Counter', FCounter);
    SetChanged;
  end;
end;

procedure TMarkNameGenerator.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteInteger('Counter', FCounter);
end;

function TMarkNameGenerator.GetCounter: Integer;
begin
  LockRead;
  try
    Result := FCounter;
  finally
    UnlockRead;
  end;
end;

function TMarkNameGenerator.GetFormatString: IStringConfigDataElement;
begin
  Result := FFormatString;
end;

function TMarkNameGenerator.GetNewName: string;
begin
  LockWrite;
  try
    Result := Format(FFormatString.Value, [FCounter]);
    Inc(FCounter);
    SetChanged;
  finally
    UnlockWrite;
  end;
end;

procedure TMarkNameGenerator.SetCounter(AValue: Integer);
begin
  LockWrite;
  try
    if FCounter <> AValue then begin
      FCounter := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
