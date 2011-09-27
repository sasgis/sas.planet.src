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

unit u_LastResponseInfo;

interface

uses
  i_LastResponseInfo,
  u_ConfigDataElementBase;

type
  TLastResponseInfo = class(TConfigDataElementBaseEmptySaveLoad, ILastResponseInfo)
  private
    FResponseHead: string;
  protected
    function GetResponseHead: string;
    procedure SetResponseHead(const AValue: string);
  end;

implementation

{ TLastResponseInfo }

function TLastResponseInfo.GetResponseHead: string;
begin
  LockRead;
  try
    Result := FResponseHead;
  finally
    UnlockRead;
  end;
end;

procedure TLastResponseInfo.SetResponseHead(const AValue: string);
begin
  LockWrite;
  try
    if FResponseHead <> AValue then begin
      FResponseHead := AValue;
      SetChanged;
    end;
  finally
    UnlockWrite;
  end;
end;

end.
