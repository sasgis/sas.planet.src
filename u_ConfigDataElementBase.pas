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

unit u_ConfigDataElementBase;

interface

uses
  Windows,
  SysUtils,
  i_JclNotify,
  i_ConfigDataProvider,
  i_ConfigDataWriteProvider,
  i_ConfigDataElement;

type
  TConfigDataElementBase = class(TInterfacedObject, IConfigDataElement)
  private
    FBeforeChangeNotifier: IJclNotifier;
    FChangeNotifier: IJclNotifier;
    FAfterChangeNotifier: IJclNotifier;
    FLock: TMultiReadExclusiveWriteSynchronizer;
    FStopNotifyCounter: Longint;
    FNeedNotify: Longint;
  protected
    procedure SetChanged;
    function CheckIsChangedAndReset: Boolean;
    procedure DoBeforeChangeNotify; virtual;
    procedure DoChangeNotify; virtual;
    procedure DoAfterChangeNotify; virtual;
    procedure DoReadConfig(AConfigData: IConfigDataProvider); virtual; abstract;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); virtual; abstract;
  protected
    procedure LockRead; virtual;
    procedure LockWrite; virtual;
    procedure UnlockRead; virtual;
    procedure UnlockWrite; virtual;
    procedure ReadConfig(AConfigData: IConfigDataProvider); virtual;
    procedure WriteConfig(AConfigData: IConfigDataWriteProvider); virtual;
    procedure StopNotify; virtual;
    procedure StartNotify; virtual;
    function GetBeforeChangeNotifier: IJclNotifier;
    function GetChangeNotifier: IJclNotifier;
    function GetAfterChangeNotifier: IJclNotifier;
  public
    constructor Create();
    destructor Destroy; override;
  end;

type
  TConfigDataElementBaseEmptySaveLoad = class(TConfigDataElementBase)
  protected
    procedure DoReadConfig(AConfigData: IConfigDataProvider); override;
    procedure DoWriteConfig(AConfigData: IConfigDataWriteProvider); override;
  end;

implementation

uses
  u_JclNotify;

{ TConfigDataElementBase }

constructor TConfigDataElementBase.Create;
begin
  FLock := TMultiReadExclusiveWriteSynchronizer.Create;
  FBeforeChangeNotifier := TJclBaseNotifier.Create;
  FChangeNotifier := TJclBaseNotifier.Create;
  FAfterChangeNotifier := TJclBaseNotifier.Create;
  FStopNotifyCounter := 0;
end;

destructor TConfigDataElementBase.Destroy;
begin
  FreeAndNil(FLock);
  FBeforeChangeNotifier := nil;
  FChangeNotifier := nil;
  FAfterChangeNotifier := nil;
  inherited;
end;

function TConfigDataElementBase.CheckIsChangedAndReset: Boolean;
begin
  Result := InterlockedExchange(FNeedNotify, 0) <> 0;
end;

procedure TConfigDataElementBase.DoAfterChangeNotify;
begin
  FAfterChangeNotifier.Notify(nil);
end;

procedure TConfigDataElementBase.DoBeforeChangeNotify;
begin
  FBeforeChangeNotifier.Notify(nil);
end;

procedure TConfigDataElementBase.DoChangeNotify;
begin
  DoBeforeChangeNotify;
  try
    FChangeNotifier.Notify(nil);
  finally
    DoAfterChangeNotify;
  end;
end;

function TConfigDataElementBase.GetAfterChangeNotifier: IJclNotifier;
begin
  Result := FAfterChangeNotifier;
end;

function TConfigDataElementBase.GetBeforeChangeNotifier: IJclNotifier;
begin
  Result := FBeforeChangeNotifier;
end;

function TConfigDataElementBase.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

procedure TConfigDataElementBase.LockRead;
begin
  FLock.BeginRead;
end;

procedure TConfigDataElementBase.LockWrite;
begin
  StopNotify;
  FLock.BeginWrite;
end;

procedure TConfigDataElementBase.ReadConfig(AConfigData: IConfigDataProvider);
begin
  LockWrite;
  try
    DoReadConfig(AConfigData);
  finally
    UnlockWrite;
  end;
end;

procedure TConfigDataElementBase.SetChanged;
begin
  InterlockedIncrement(FNeedNotify);
end;

procedure TConfigDataElementBase.StartNotify;
var
  VCouner: Longint;
begin
  VCouner := InterlockedDecrement(FStopNotifyCounter);
  if VCouner = 0 then begin
    if CheckIsChangedAndReset then begin
      DoChangeNotify;
    end;
  end;
end;

procedure TConfigDataElementBase.StopNotify;
begin
  InterlockedIncrement(FStopNotifyCounter);
end;

procedure TConfigDataElementBase.UnlockRead;
begin
  FLock.EndRead;
end;

procedure TConfigDataElementBase.UnlockWrite;
begin
  FLock.EndWrite;
  StartNotify;
end;

procedure TConfigDataElementBase.WriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
  LockRead;
  try
    DoWriteConfig(AConfigData);
  finally
    UnlockRead;
  end;
end;

{ TConfigDataElementBaseEmptySaveLoad }

procedure TConfigDataElementBaseEmptySaveLoad.DoReadConfig(
  AConfigData: IConfigDataProvider);
begin
end;

procedure TConfigDataElementBaseEmptySaveLoad.DoWriteConfig(
  AConfigData: IConfigDataWriteProvider);
begin
end;

end.
