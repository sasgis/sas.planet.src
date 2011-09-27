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

unit u_ContentConvertersListByKey;

interface

uses
  Classes,
  i_ContentConverter;

type
  TContentConvertersListByKey = class
  private
    FList: TStringList;
  public
    constructor Create();
    destructor Destroy; override;
    procedure Add(AKey: string; AConverter: IContentConverter);
    function Get(AKey: string): IContentConverter;
  end;

implementation

uses
  SysUtils;

{ TContentConvertersListByKey }

procedure TContentConvertersListByKey.Add(AKey: string;
  AConverter: IContentConverter);
begin
  AConverter._AddRef;
  FList.AddObject(AKey, Pointer(AConverter));
end;

constructor TContentConvertersListByKey.Create;
begin
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
end;

destructor TContentConvertersListByKey.Destroy;
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

function TContentConvertersListByKey.Get(AKey: string): IContentConverter;
var
  VIndex: Integer;
begin
  if FList.Find(AKey, VIndex) then begin
    Result := IContentConverter(Pointer(FList.Objects[VIndex]));
  end else begin
    Result := nil;
  end;
end;

end.
