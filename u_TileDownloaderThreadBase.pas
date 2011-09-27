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

unit u_TileDownloaderThreadBase;

interface

uses
  Windows,
  Classes,
  SyncObjs,
  Types,
  i_OperationNotifier,
  u_OperationNotifier,
  u_MapType;

type
  TTileDownloaderThreadBase = class(TThread)
  private
    FCancelNotifierInternal: IOperationNotifierInternal;
  protected
    FMapType: TMapType;
    FZoom: byte;
    FLoadUrl: string;
    FCancelEvent: TEvent;
    FCancelNotifier: IOperationNotifier;

    procedure SleepCancelable(ATime: Cardinal);
  public
    constructor Create(CreateSuspended: Boolean);
    destructor Destroy; override;
    procedure Terminate; reintroduce;
    property MapType: TMapType read FMapType;
  end;

implementation

uses
  SysUtils;

constructor TTileDownloaderThreadBase.Create(CreateSuspended: Boolean);
var
  VOperationNotifier: TOperationNotifier;
begin
  inherited Create(CreateSuspended);
  FCancelEvent := TEvent.Create;
  VOperationNotifier := TOperationNotifier.Create;
  FCancelNotifierInternal := VOperationNotifier;
  FCancelNotifier := VOperationNotifier;
end;

destructor TTileDownloaderThreadBase.Destroy;
begin
  Terminate;
  inherited;
  FreeAndNil(FCancelEvent);
end;

procedure TTileDownloaderThreadBase.SleepCancelable(ATime: Cardinal);
begin
  if  ATime > 0 then begin
    FCancelEvent.WaitFor(ATime);
  end;
end;

procedure TTileDownloaderThreadBase.Terminate;
begin
  inherited;
  FCancelEvent.SetEvent;
  FCancelNotifierInternal.NextOperation;
end;

end.
