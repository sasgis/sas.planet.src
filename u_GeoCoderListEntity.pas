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

unit u_GeoCoderListEntity;

interface

uses
  i_GeoCoder,
  i_GeoCoderList;

type
  TGeoCoderListEntity = class(TInterfacedObject, IGeoCoderListEntity)
  private
    FGUID: TGUID;
    FCaption: WideString;
    FGeoCoder: IGeoCoder;
  protected
    function GetGUID: TGUID;
    function GetCaption: WideString;
    function GetGeoCoder: IGeoCoder;
  public
    constructor Create(
      AGUID: TGUID;
      ACaption: WideString;
      AGeoCoder: IGeoCoder
    );
  end;

implementation

{ TGeoCoderListEntity }

constructor TGeoCoderListEntity.Create(AGUID: TGUID; ACaption: WideString;
  AGeoCoder: IGeoCoder);
begin
  FGUID := AGUID;
  FCaption := ACaption;
  FGeoCoder := AGeoCoder;
end;

function TGeoCoderListEntity.GetCaption: WideString;
begin
  Result := FCaption;
end;

function TGeoCoderListEntity.GetGeoCoder: IGeoCoder;
begin
  Result := FGeoCoder;
end;

function TGeoCoderListEntity.GetGUID: TGUID;
begin
  Result := FGUID;
end;

end.
