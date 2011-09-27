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

unit u_OperationNotifier;

interface

uses
  Windows,
  i_JclNotify,
  i_OperationNotifier;

type
  IOperationNotifierInternal = interface
    procedure NextOperation;
  end;

  TOperationNotifier = class(TInterfacedObject, IOperationNotifier, IOperationNotifierInternal)
  private
    FNotifier: IJclNotifier;
    FCurrentOperationID: Integer;
  protected
    procedure NextOperation;
  protected
    function GetCurrentOperation: Integer; stdcall;
    function IsOperationCanceled(AID: Integer): Boolean; stdcall;

    procedure AddListener(AListener: IJclListener); stdcall;
    procedure RemoveListener(AListener: IJclListener); stdcall;
  public
    constructor Create();
  end;

implementation

uses
  u_JclNotify;

{ TOperationCancelNotifier }

constructor TOperationNotifier.Create;
begin
  FNotifier := TJclBaseNotifier.Create;
  FCurrentOperationID := 0;
end;

procedure TOperationNotifier.AddListener(AListener: IJclListener);
begin
  FNotifier.Add(AListener);
end;

function TOperationNotifier.IsOperationCanceled(AID: Integer): Boolean;
begin
  Result := FCurrentOperationID <> AID;
end;

function TOperationNotifier.GetCurrentOperation: Integer;
begin
  Result := FCurrentOperationID;
end;

procedure TOperationNotifier.NextOperation;
begin
  InterlockedIncrement(FCurrentOperationID);
end;

procedure TOperationNotifier.RemoveListener(AListener: IJclListener);
begin
  FNotifier.Remove(AListener);
end;

end.
