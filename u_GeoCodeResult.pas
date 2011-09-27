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

unit u_GeoCodeResult;

interface

uses
  ActiveX,
  Classes,
  i_GeoCoder;

type
  TGeoCodeResult = class(TInterfacedObject, IGeoCodeResult)
  private
    FSearchText: WideString;
    FMessage: WideString;
    FResultCode: Integer;
    FList: IInterfaceList;
    function GetSearchText: WideString; safecall;
    function GetResultCode: Integer; safecall;
    function GetMessage: WideString; safecall;
    function GetPlacemarks: IEnumUnknown; safecall;
    function GetPlacemarksCount: integer; safecall;
  public
    constructor Create(ASearchText: WideString; AResultCode: integer; AMessage: WideString; AList: IInterfaceList);
    destructor Destroy; override;
  end;

implementation

uses
  u_EnumUnknown;

{ TGeoCodeResult }

constructor TGeoCodeResult.Create(ASearchText: WideString;
  AResultCode: integer; AMessage: WideString; AList: IInterfaceList);
begin
  FSearchText := ASearchText;
  FList := AList;
  FMessage := AMessage;
  FResultCode := AResultCode;
end;

destructor TGeoCodeResult.Destroy;
begin
  FList := nil;
  FSearchText := '';
  FMessage := '';
  inherited;
end;

function TGeoCodeResult.GetMessage: WideString;
begin
  Result := FMessage;
end;

function TGeoCodeResult.GetPlacemarks: IEnumUnknown;
begin
  Result := TEnumUnknown.Create(FList);
end;

function TGeoCodeResult.GetPlacemarksCount: integer;
begin
  Result := FList.Count;
end;

function TGeoCodeResult.GetResultCode: Integer;
begin
  Result := FResultCode;
end;

function TGeoCodeResult.GetSearchText: WideString;
begin
  Result := FSearchText;
end;

end.
 