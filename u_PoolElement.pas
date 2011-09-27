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

unit u_PoolElement;

interface

uses
  Windows,
  i_SimpleFactory,
  i_PoolElement;

type
  TPoolElement = class(TObject, IPoolElement)
  protected
    FRefCount: Integer;
    FObject: Iunknown;
    FLastUseTime: Cardinal;
    FSemaphore: THandle;
    FFactory: ISimpleFactory;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  public
    constructor Create(AFactory: ISimpleFactory; ASemaphore: THandle);
    destructor Destroy; override;
    function GetLastUseTime: Cardinal;
    function GetObject: IUnknown;
    function TryLock: IPoolElement;
    procedure FreeObjectByTTL(AMinTime: Cardinal);
  end;

implementation

uses
  SysUtils;

constructor TPoolElement.Create(AFactory: ISimpleFactory; ASemaphore: THandle);
begin
  FSemaphore := ASemaphore;
  FFactory := AFactory;
  FLastUseTime := 0;
  FRefCount := 0;
  FObject := nil;
end;

destructor TPoolElement.Destroy;
begin
  try
    if FRefCount <> 0 then begin
      raise Exception.Create('Item locked');
    end;
  except
  end;
  FObject := nil;
  FFactory := nil;
  inherited;
end;

// Для Delphi 7 в юните Windows заменить объявление функции InterlockedCompareExchange
function InterlockedCompareExchange(var Destination: Longint; Exchange: Longint; Comperand: Longint): Longint stdcall; external kernel32 name 'InterlockedCompareExchange';

procedure TPoolElement.FreeObjectByTTL(AMinTime: Cardinal);
begin
  if Integer(InterlockedCompareExchange(FRefCount, 1, 0)) = 0 then begin
    if (FLastUseTime > 0) and ((FLastUseTime <= AMinTime) or ((AMinTime < 1 shl 29) and (FLastUseTime > 1 shl 30))) then begin
      Fobject := nil;
      FLastUseTime := 0;
    end;
    InterlockedDecrement(FRefCount);
  end;
end;

function TPoolElement.GetLastUseTime: Cardinal;
begin
  Result := FLastUseTime;
end;

function TPoolElement.GetObject: IUnknown;
begin
  if Fobject = nil then begin
    Fobject := FFactory.CreateInstance;
  end;
  Result := Fobject;
end;

function TPoolElement.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then begin
    Result := 0;
  end else begin
    Result := E_NOINTERFACE;
  end;
end;

function TPoolElement.TryLock: IPoolElement;
begin
  if Integer(InterlockedCompareExchange(FRefCount, 1, 0)) = 0 then begin
    Result := Self;
    _Release;
  end else begin
    Result := nil;
  end;
end;

function TPoolElement._AddRef: Integer;
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TPoolElement._Release: Integer;
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then begin
    FLastUseTime := GetTickCount;
    if FSemaphore <> 0 then begin
      ReleaseSemaphore(FSemaphore, 1, nil);
    end;
  end;
end;

end.
