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

unit u_GeoCodePlacemark;

interface

uses
  t_GeoTypes,
  i_GeoCoder;

type
  TGeoCodePlacemark = class(TInterfacedObject, IGeoCodePlacemark)
  private
    FPoint: TDoublePoint;
    FAddress: WideString;
    FDesc: WideString;
    FFullDesc: WideString;
    FAccuracy: Integer;
    function GetPoint: TDoublePoint; safecall;
    function GetAddress: WideString; safecall;
    function GetDesc: WideString; safecall;
    function GetFullDesc: WideString; safecall;
    function GetAccuracy: Integer; safecall;
  public
    constructor Create(
      APoint: TDoublePoint;
      AAddress: WideString;
      ADesc: WideString;
      AFullDesc: WideString;
      AAccuracy: Integer
    );
    destructor Destroy; override;
  end;

implementation

{ TGeoCodePlacemark }

constructor TGeoCodePlacemark.Create(APoint: TDoublePoint;
  AAddress: WideString; ADesc: WideString; AFullDesc: WideString; AAccuracy: Integer);
begin
  FAddress := AAddress;
  FDesc := ADesc;
  FFullDesc := AFullDesc;
  FPoint := APoint;
  FAccuracy := AAccuracy;
end;

destructor TGeoCodePlacemark.Destroy;
begin
  FAddress := '';
  inherited;
end;

function TGeoCodePlacemark.GetAccuracy: Integer;
begin
  Result := FAccuracy;
end;

function TGeoCodePlacemark.GetAddress: WideString;
begin
  Result := FAddress;
end;

function TGeoCodePlacemark.GetDesc: WideString;
begin
  Result := FDesc;
end;

function TGeoCodePlacemark.GetFullDesc: WideString;
begin
  Result := FFullDesc;
end;

function TGeoCodePlacemark.GetPoint: TDoublePoint;
begin
  Result := FPoint;
end;

end.
 