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

unit u_LogForTaskThread;

interface

uses
  SyncObjs,
  u_WideStrings,
  i_LogSimple,
  i_LogForTaskThread;

type
  TLogForTaskThread = class(TInterfacedObject, ILogForTaskThread, ILogSimple)
  private
    FMinLogLevel: Integer;
    FNextId: Cardinal;
    FMaxRowsCount: Cardinal;
    FLock: TCriticalSection;
    FList: TWideStringList;
    FLinesSeparator: WideString;
  public
    constructor Create(AMaxLinesCount: Cardinal; AMinLogLevel: Integer);
    destructor Destroy; override;

    function GetLastMessages(AMaxRowsCount: Cardinal; var ALastId: Cardinal; out AcntLines: Cardinal): WideString; virtual; safecall;
    procedure WriteText(AMessage: WideString; ALogLevel: integer); safecall;
  end;

implementation

uses
  SysUtils;

{ TLogForTaskThread }

constructor TLogForTaskThread.Create(AMaxLinesCount: Cardinal; AMinLogLevel: Integer);
var
  i: Integer;
begin
  FMinLogLevel := AMinLogLevel;
  FMaxRowsCount := AMaxLinesCount;
  FLock := TCriticalSection.Create;
  FList := TWideStringList.Create;
  FList.Capacity := FMaxRowsCount;
  for i := 0 to FMaxRowsCount - 1 do begin
    FList.Add('');
  end;
  FLinesSeparator := #13#10;
end;

destructor TLogForTaskThread.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FList);
  inherited;
end;

function TLogForTaskThread.GetLastMessages(AMaxRowsCount: Cardinal;
  var ALastId: Cardinal; out AcntLines: Cardinal): WideString;
var
  VNewRowsCount: Cardinal;
  i: Cardinal;
  VFirstLine: Boolean;
begin
  Result := '';
  if ALastId < FNextId then begin
    FLock.Acquire;
    try
      VNewRowsCount := FNextId - ALastId;
      if VNewRowsCount > AMaxRowsCount then begin
        VNewRowsCount := AMaxRowsCount;
      end;
      if VNewRowsCount > FMaxRowsCount then begin
        VNewRowsCount := FMaxRowsCount;
      end;
      VFirstLine := true;
      for i := FNextId - VNewRowsCount to FNextId - 1 do begin
        if VFirstLine then begin
          Result := FList.Strings[i mod FMaxRowsCount];
          VFirstLine := False;
        end else begin
          Result := Result + FLinesSeparator + FList.Strings[i mod FMaxRowsCount];
        end;
      end;
      AcntLines := VNewRowsCount;
      ALastId := FNextId;
    finally
      FLock.Release;
    end;
  end else begin
    AcntLines := 0;
    ALastId := FNextId;
  end;
end;

procedure TLogForTaskThread.WriteText(AMessage: WideString;
  ALogLevel: integer);
var
  VIndex: Cardinal;
begin
  if ALogLevel >= FMinLogLevel then begin
    FLock.Acquire;
    try
      VIndex := FNextId mod FMaxRowsCount;
      FList.Strings[VIndex] := AMessage;
      Inc(FNextId);
    finally
      FLock.Release;
    end;
  end;
end;

end.
