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

unit u_TileFileNameES;

interface

uses
  Types,
  i_TileFileNameGenerator;

type
  TTileFileNameES = class(TInterfacedObject, ITileFileNameGenerator)
  private
    class function FullInt(i: Integer; AZoom: byte): string;
  public
    function GetTileFileName(AXY: TPoint; Azoom: byte): string;
  end;

implementation

uses
  StrUtils,
  SysUtils;

{ TTileFileNameES }

class function TTileFileNameES.FullInt(i: Integer; AZoom: byte): string;
begin
  Result := IntToStr(i);
  if AZoom < 4 then begin
  end else if AZoom < 7 then begin
    Result := RightStr('0' + Result, 2);
  end else if AZoom < 10 then begin
    Result := RightStr('00' + Result, 3);
  end else if AZoom < 14 then begin
    Result := RightStr('000' + Result, 4);
  end else if AZoom < 17 then begin
    Result := RightStr('0000' + Result, 5);
  end else if AZoom < 20 then begin
    Result := RightStr('00000' + Result, 6);
  end else begin
    Result := RightStr('000000' + Result, 7);
  end;
end;

function TTileFileNameES.GetTileFileName(AXY: TPoint;
  Azoom: byte): string;
var
  VZoomStr: string;
  VFileName: string;
begin
  inherited;
  if (Azoom >= 9) then begin
    VZoomStr := IntToStr(Azoom + 1);
  end else begin
    VZoomStr := '0' + IntToStr(Azoom + 1);
  end;
  VFileName := VZoomStr + '-' + FullInt(AXY.X, AZoom) + '-' + FullInt(AXY.Y, AZoom);
  if Azoom < 6 then begin
    Result := VZoomStr + PathDelim;
  end else if Azoom < 10 then begin
    Result := VZoomStr + PathDelim +
      Chr(60 + Azoom) + FullInt(AXY.X shr 5, Azoom - 5) + FullInt(AXY.Y shr 5, Azoom - 5) + PathDelim;
  end else begin
    Result := '10' + '-' + FullInt(AXY.X shr (AZoom - 9), 9) + '-' + FullInt(AXY.Y shr (AZoom - 9), 9) + PathDelim + VZoomStr + PathDelim + Chr(60 + Azoom) + FullInt(AXY.X shr 5, Azoom - 5) + FullInt(AXY.Y shr 5, Azoom - 5) + PathDelim;
  end;
  Result := Result + VFileName;
end;

end.
