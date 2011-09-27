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

unit u_TileFileNameGMV;

interface

uses
  Types,
  i_TileFileNameGenerator;

type
  TTileFileNameGMV = class(TInterfacedObject, ITileFileNameGenerator)
  public
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
  end;

implementation

uses
  SysUtils;

{ TTileFileNameGMV }

function TTileFileNameGMV.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
var
  i: byte;
  VMask: Integer;
  c: Char;
begin
  if (Azoom >= 9) then begin
    Result := IntToStr(Azoom + 1);
  end else begin
    Result := '0' + IntToStr(Azoom + 1);
  end;
  Result := Result + PathDelim + 't';
  if Azoom > 0 then begin
    VMask := 1 shl (Azoom - 1);
    for i := 1 to Azoom do begin
      if (AXY.X and VMask) = 0 then begin
        if (AXY.Y and VMask) = 0 then begin
          c := 'q';
        end else begin
          c := 't';
        end;
      end else begin
        if (AXY.Y and VMask) = 0 then begin
          c := 'r';
        end else begin
          c := 's';
        end;
      end;
      Result := Result + c;
      VMask := VMask shr 1;
    end;
  end;
end;

end.
