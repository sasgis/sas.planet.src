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

unit u_MapTypeGUIConfigStatic;

interface

uses
  Graphics,
  Classes,
  i_MapTypeGUIConfig;

type
  TMapTypeGUIConfigStatic = class(TInterfacedObject, IMapTypeGUIConfigStatic)
  private
    FName: string;
    FSortIndex: Integer;
    FHotKey: TShortCut;
    FSeparator: Boolean;
    FParentSubMenu: string;
    FEnabled: Boolean;
    FInfoUrl: string;
    FBmp18: TBitmap;
    FBmp24: TBitmap;
  protected
    function GetName: string;
    function GetSortIndex: Integer;
    function GetHotKey: TShortCut;
    function GetSeparator: Boolean;
    function GetParentSubMenu: string;
    function GetEnabled: Boolean;
    function GetInfoUrl: string;
    function GetBmp18: TBitmap;
    function GetBmp24: TBitmap;
  public
    constructor Create(
      AName: string;
      ASortIndex: Integer;
      AHotKey: TShortCut;
      ASeparator: Boolean;
      AParentSubMenu: string;
      AEnabled: Boolean;
      AInfoUrl: string;
      ABmp18: TBitmap;
      ABmp24: TBitmap
    );
  end;

implementation

{ TMapTypeGUIConfigStatic }

constructor TMapTypeGUIConfigStatic.Create(
  AName: string;
  ASortIndex: Integer;
  AHotKey: TShortCut;
  ASeparator: Boolean;
  AParentSubMenu: string;
  AEnabled: Boolean;
  AInfoUrl: string;
  ABmp18: TBitmap;
  ABmp24: TBitmap
);
begin
  FName := AName;
  FSortIndex := ASortIndex;
  FHotKey := AHotKey;
  FSeparator := ASeparator;
  FParentSubMenu := AParentSubMenu;
  FEnabled :=  AEnabled;
  FInfoUrl := AInfoUrl;
  FBmp18 := ABmp18;
  FBmp24 := ABmp24;
end;

function TMapTypeGUIConfigStatic.GetBmp18: TBitmap;
begin
  Result := FBmp18;
end;

function TMapTypeGUIConfigStatic.GetBmp24: TBitmap;
begin
  Result := FBmp24
end;

function TMapTypeGUIConfigStatic.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TMapTypeGUIConfigStatic.GetHotKey: TShortCut;
begin
  Result := FHotKey;
end;

function TMapTypeGUIConfigStatic.GetInfoUrl: string;
begin
  Result := FInfoUrl;
end;

function TMapTypeGUIConfigStatic.GetName: string;
begin
  Result := FName;
end;

function TMapTypeGUIConfigStatic.GetParentSubMenu: string;
begin
  Result := FParentSubMenu;
end;

function TMapTypeGUIConfigStatic.GetSeparator: Boolean;
begin
  Result := FSeparator;
end;

function TMapTypeGUIConfigStatic.GetSortIndex: Integer;
begin
  Result := FSortIndex;
end;

end.
