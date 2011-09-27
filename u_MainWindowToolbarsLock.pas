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

unit u_MainWindowToolbarsLock;

interface

uses
  Types,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  u_ConfigDataElementBase,
  i_MainFormConfig;

type
  TMainWindowToolbarsLock = class(TConfigDataElementBase, IMainWindowToolbarsLock)
  private
    FIsLock: Boolean;
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  protected
    function GetIsLock: Boolean;
    procedure SetLock(AValue: Boolean);
  public
    constructor Create;
  end;


implementation

{ TMainWindowToolbarsLock }

constructor TMainWindowToolbarsLock.Create;
begin
  inherited;
  FIsLock := False;
end;

procedure TMainWindowToolbarsLock.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
  inherited;
  if AConfigData <> nil then begin
    FIsLock := AConfigData.ReadBool('lock_toolbars', FIsLock);
    SetChanged;
  end;
end;

procedure TMainWindowToolbarsLock.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  inherited;
  AConfigData.WriteBool('lock_toolbars', FIsLock);
end;

function TMainWindowToolbarsLock.GetIsLock: Boolean;
begin
  LockRead;
  try
    Result := FIsLock;
  finally
    UnlockRead;
  end;
end;

procedure TMainWindowToolbarsLock.SetLock(AValue: Boolean);
begin
  LockWrite;
  try
    if FIsLock <> AValue then begin
      FIsLock := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
