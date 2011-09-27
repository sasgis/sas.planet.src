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

unit u_TBXSubmenuItemWithIndicator;

interface

uses
  Graphics,
  Classes,
  TB2Item,
  TBX;

type
  TTBXSubmenuItemWithIndicator = class(TTBXSubmenuItem)
  private
    procedure AdjustFont(Item: TTBCustomItem;
      Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

{ TTBXSubmenuItemWithIndicator }

constructor TTBXSubmenuItemWithIndicator.Create(AOwner: TComponent);
begin
  inherited;
  OnAdjustFont := Self.AdjustFont;
end;

procedure TTBXSubmenuItemWithIndicator.AdjustFont(Item: TTBCustomItem;
  Viewer: TTBItemViewer; Font: TFont; StateFlags: Integer);
var
  VChildSelected: Boolean;
  i: Integer;
begin
  VChildSelected := False;
  for i := 0 to  Self.Count - 1 do begin
    if Self.Items[i].Checked then begin
      VChildSelected := True;
      Break;
    end;
  end;
  if VChildSelected then begin
    Self.FontSettings.Bold := tsTrue;
  end else begin
    Self.FontSettings.Bold := tsDefault;
  end;
end;

end.
