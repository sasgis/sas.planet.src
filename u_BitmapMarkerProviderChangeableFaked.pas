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

unit u_BitmapMarkerProviderChangeableFaked;

interface

uses
  i_JclNotify,
  i_BitmapMarker;

type
  TBitmapMarkerProviderChangeableFaked = class(TInterfacedObject, IBitmapMarkerProviderChangeable)
  private
    FProviderStatic: IBitmapMarkerProvider;
    FChangeNotifier: IJclNotifier;
  protected
    function GetStatic: IBitmapMarkerProvider;
    function GetChangeNotifier: IJclNotifier;
  public
    constructor Create(
      AProviderStatic: IBitmapMarkerProvider
    );
  end;

implementation

uses
  u_JclNotify;

{ TBitmapMarkerProviderChangeableFaked }

constructor TBitmapMarkerProviderChangeableFaked.Create(
  AProviderStatic: IBitmapMarkerProvider);
begin
  FProviderStatic := AProviderStatic;
  FChangeNotifier := TJclBaseNotifierFaked.Create;
end;

function TBitmapMarkerProviderChangeableFaked.GetChangeNotifier: IJclNotifier;
begin
  Result := FChangeNotifier;
end;

function TBitmapMarkerProviderChangeableFaked.GetStatic: IBitmapMarkerProvider;
begin
  Result := FProviderStatic;
end;

end.
