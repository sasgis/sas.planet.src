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

unit u_ContentTypeListByKey;

interface

uses
  Classes,
  i_ContentTypeInfo;

type
  TContentTypeListByKey = class
  private
    FList: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AKey: string; AType: IContentTypeInfoBasic);
    function Get(AKey: string):  IContentTypeInfoBasic;
    function GetEnumerator: TStringsEnumerator;
  end;


implementation

uses
  SysUtils;

{ TContentTypeListByKey }

procedure TContentTypeListByKey.Add(AKey: string; AType: IContentTypeInfoBasic);
begin
  AType._AddRef;
  FList.AddObject(AKey, Pointer(AType));
end;

constructor TContentTypeListByKey.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentTypeListByKey.Destroy;
var
  i: Integer;
begin
  if FList <> nil then begin
    for i := 0 to FList.Count - 1 do begin
      IInterface(Pointer(FList.Objects[i]))._Release;
    end;
    FreeAndNil(FList);
  end;
  inherited;
end;

function TContentTypeListByKey.Get(AKey: string): IContentTypeInfoBasic;
var
  VIndex: Integer;
begin
  if FList.Find(AKey, VIndex) then begin
    Result := IContentTypeInfoBasic(Pointer(FList.Objects[VIndex]));
  end else begin
    Result := nil;
  end;
end;

function TContentTypeListByKey.GetEnumerator: TStringsEnumerator;
begin
  Result := FList.GetEnumerator;
end;

end.
