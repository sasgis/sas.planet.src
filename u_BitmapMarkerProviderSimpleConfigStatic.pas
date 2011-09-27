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

unit u_BitmapMarkerProviderSimpleConfigStatic;

interface

uses
  GR32,
  i_BitmapMarkerProviderSimpleConfig;

type
  TBitmapMarkerProviderSimpleConfigStatic = class(TInterfacedObject, IBitmapMarkerProviderSimpleConfigStatic)
  private
    FMarkerSize: Integer;
    FMarkerColor: TColor32;
    FBorderColor: TColor32;
  protected
    function GetMarkerSize: Integer;
    function GetMarkerColor: TColor32;
    function GetBorderColor: TColor32;
  public
    constructor Create(
      AMarkerSize: Integer;
      AMarkerColor: TColor32;
      ABorderColor: TColor32
    );
  end;

implementation

{ TBitmapMarkerProviderSimpleConfigStatic }

constructor TBitmapMarkerProviderSimpleConfigStatic.Create(
  AMarkerSize: Integer; AMarkerColor, ABorderColor: TColor32);
begin
  FMarkerSize :=  AMarkerSize;
  FMarkerColor := AMarkerColor;
  FBorderColor := ABorderColor;
end;

function TBitmapMarkerProviderSimpleConfigStatic.GetBorderColor: TColor32;
begin
  Result := FBorderColor;
end;

function TBitmapMarkerProviderSimpleConfigStatic.GetMarkerColor: TColor32;
begin
  Result := FMarkerColor;
end;

function TBitmapMarkerProviderSimpleConfigStatic.GetMarkerSize: Integer;
begin
  Result := FMarkerSize;
end;

end.
