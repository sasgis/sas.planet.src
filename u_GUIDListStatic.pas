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

unit u_GUIDListStatic;

interface

uses
  i_GUIDListStatic;

type
  TGUIDListStatic =  class(TInterfacedObject, IGUIDListStatic)
  private
    FList: array of TGUID;
  protected
    function GetItem(AIndex: Integer): TGUID;
    function GetCount: Integer;
  public
    constructor Create(
      AList: array of TGUID;
      ACount: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGUIDListStatic }

constructor TGUIDListStatic.Create(AList: array of TGUID; ACount: Integer);
var
  i: Integer;
begin
  SetLength(FList, ACount);
  for i := 0 to ACount - 1 do begin
    FList[i] := AList[i];
  end;
end;

destructor TGUIDListStatic.Destroy;
begin
  FList := nil;
  inherited;
end;

function TGUIDListStatic.GetCount: Integer;
begin
  Result := Length(FList);
end;

function TGUIDListStatic.GetItem(AIndex: Integer): TGUID;
begin
  Result := FList[AIndex];
end;

end.
