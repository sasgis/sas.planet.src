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

unit u_TileErrorInfo;

interface

uses
  Types,
  u_MapType,
  i_TileError;

type
  TTileErrorInfo = class(TInterfacedObject, ITileErrorInfo)
  private
    FMapType: TMapType;
    FZoom: Byte;
    FTile: TPoint;
    FErrorText: string;
  protected
    function GetMapType: TMapType;
    function GetZoom: Byte;
    function GetTile: TPoint;
    function GetErrorText: string;
  public
    constructor Create(
      AMapType: TMapType;
      AZoom: Byte;
      ATile: TPoint;
      AErrorText: string
    );
  end;

implementation

{ TTileErrorInfo }

constructor TTileErrorInfo.Create(AMapType: TMapType; AZoom: Byte;
  ATile: TPoint; AErrorText: string);
begin
  FMapType := AMapType;
  FZoom := AZoom;
  FTile := ATile;
  FErrorText := AErrorText;
end;

function TTileErrorInfo.GetErrorText: string;
begin
  Result := FErrorText;
end;

function TTileErrorInfo.GetMapType: TMapType;
begin
  Result := FMapType;
end;

function TTileErrorInfo.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileErrorInfo.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
