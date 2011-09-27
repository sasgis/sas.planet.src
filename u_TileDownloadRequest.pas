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

unit u_TileDownloadRequest;

interface

uses
  Types,
  i_ZmpInfo,
  i_DownloadRequest,
  i_TileDownloadRequest;

type
  TTileDownloadRequest = class(TInterfacedObject, IDownloadRequest, ITileDownloadRequest)
  private
    FUrl: string;
    FRequestHeader: string;
    FZmp: IZmpInfo;
    FTile: TPoint;
    FZoom: Byte;
  protected
    function GetUrl: string;
    function GetRequestHeader: string;
  protected
    function GetZmp: IZmpInfo;
    function GetTile: TPoint;
    function GetZoom: Byte;
  public
    constructor Create(
      AUrl: string;
      ARequestHeader: string;
      AZmp: IZmpInfo;
      ATile: TPoint;
      AZoom: Byte
    );
  end;

implementation

{ TTileDownloadRequest }

constructor TTileDownloadRequest.Create(AUrl, ARequestHeader: string;
  AZmp: IZmpInfo; ATile: TPoint; AZoom: Byte);
begin
  FUrl := AUrl;
  FRequestHeader := ARequestHeader;
  FZmp := AZmp;
  FTile := ATile;
  FZoom := AZoom;
end;

function TTileDownloadRequest.GetRequestHeader: string;
begin
  Result := FRequestHeader;
end;

function TTileDownloadRequest.GetTile: TPoint;
begin
  Result := FTile;
end;

function TTileDownloadRequest.GetUrl: string;
begin
  Result := FUrl;
end;

function TTileDownloadRequest.GetZmp: IZmpInfo;
begin
  Result := FZmp;
end;

function TTileDownloadRequest.GetZoom: Byte;
begin
  Result := FZoom;
end;

end.
