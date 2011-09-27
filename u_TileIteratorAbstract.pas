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

unit u_TileIteratorAbstract;

interface

uses
  Types,
  t_GeoTypes,
  i_CoordConverter,
  i_TileIterator;

type
  TTileIteratorAbstract = class(TInterfacedObject, ITileIterator)
  protected
    function GetTilesTotal: Int64; virtual; abstract;
    function GetTilesRect: TRect; virtual; abstract;
  public
    function Next(out ATile: TPoint): Boolean; virtual; abstract;
    procedure Reset; virtual; abstract;
    property TilesTotal: Int64 read GetTilesTotal;
    property TilesRect: TRect read GetTilesRect;
  end;

  TTileIteratorByPolygonAbstract = class(TTileIteratorAbstract)
  protected
    FPolygLL: TArrayOfDoublePoint;
    FZoom: byte;
    FGeoConvert: ICoordConverter;
    FCurrent: TPoint;
  public
    constructor Create(AZoom: byte; APolygLL: TArrayOfDoublePoint; AGeoConvert: ICoordConverter); virtual;
    destructor Destroy; override;
  end;

implementation



{ TTileIteratorByPolygonAbstract }

constructor TTileIteratorByPolygonAbstract.Create(AZoom: byte;
  APolygLL: TArrayOfDoublePoint; AGeoConvert: ICoordConverter);
begin
  FZoom := AZoom;
  FPolygLL := Copy(APolygLL);
  FGeoConvert := AGeoConvert;
end;

destructor TTileIteratorByPolygonAbstract.Destroy;
begin
  FPolygLL := nil;
  FGeoConvert := nil;
  inherited;
end;

end.


