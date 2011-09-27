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

unit u_MarksDrawConfigStatic;

interface

uses
  Types,
  i_MarksDrawConfig;

type
  TMarksDrawConfigStatic = class(TInterfacedObject, IMarksDrawConfigStatic)
  private
    FShowPointCaption: Boolean;
    FUseSimpleDrawOrder: Boolean;
    FMagnetDraw: Boolean;
    FOverSizeRect: TRect;
  protected
    function GetShowPointCaption: Boolean;
    function GetUseSimpleDrawOrder: Boolean;
    function GetOverSizeRect: TRect;
    function GerMagnetDraw: Boolean;
  public
    constructor Create(
      AShowPointCaption: Boolean;
      AUseSimpleDrawOrder: Boolean;
      AMagnetDraw: Boolean;
      AOverSizeRect: TRect
    );
  end;

implementation

{ TMarksDrawConfigStatic }

constructor TMarksDrawConfigStatic.Create(AShowPointCaption,
  AUseSimpleDrawOrder: Boolean; AMagnetDraw: Boolean; AOverSizeRect: TRect);
begin
  FShowPointCaption := AShowPointCaption;
  FUseSimpleDrawOrder := AUseSimpleDrawOrder;
  FOverSizeRect := AOverSizeRect;
  FMagnetDraw := AMagnetDraw;
end;

function TMarksDrawConfigStatic.GetOverSizeRect: TRect;
begin
  Result := FOverSizeRect;
end;

function TMarksDrawConfigStatic.GetShowPointCaption: Boolean;
begin
  Result := FShowPointCaption;
end;

function TMarksDrawConfigStatic.GetUseSimpleDrawOrder: Boolean;
begin
  Result := FUseSimpleDrawOrder;
end;

function TMarksDrawConfigStatic.GerMagnetDraw: Boolean;
begin
  Result := FMagnetDraw;
end;

end.
