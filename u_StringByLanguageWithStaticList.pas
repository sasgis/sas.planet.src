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

unit u_StringByLanguageWithStaticList;

interface

uses
  Classes,
  i_StringByLanguage;

type
  TStringByLanguageWithStaticList = class(TInterfacedObject, IStringByLanguage)
  private
    FValueList: TStringList;
  protected
    function GetString(ALangIndex: Integer): string;
    function GetDefault: string;
  public
    constructor Create(
      AValueList: TStrings
    );
    destructor Destroy; override;
  end;
implementation

uses
  SysUtils;

{ TStringByLangByStaticList }

constructor TStringByLanguageWithStaticList.Create(AValueList: TStrings);
begin
  FValueList := TStringList.Create;
  Assert(AValueList.Count > 0);
  FValueList.Assign(AValueList);
end;

destructor TStringByLanguageWithStaticList.Destroy;
begin
  FreeAndNil(FValueList);
  inherited;
end;

function TStringByLanguageWithStaticList.GetDefault: string;
begin
  Result := FValueList.Strings[0];
end;

function TStringByLanguageWithStaticList.GetString(ALangIndex: Integer): string;
begin
  if (ALangIndex > 0) and (ALangIndex < FValueList.Count) then begin
    Result := FValueList.Strings[ALangIndex];
  end else begin
    Result := GetDefault;
  end;
end;

end.
