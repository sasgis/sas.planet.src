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

unit u_LanguageTBXItem;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_JclNotify,
  i_LanguageManager;

type
  TLanguageTBXItem = class(TTBXCustomItem)
  private
    FParentMenu: TTBCustomItem;
    FLangIndex: Integer;
    FLanguageManager: ILanguageManager;
    FListener: IJclListener;
    procedure OnLangChange(Sender: TObject);
    procedure OnClickItem(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; AParentMenu: TTBCustomItem; ALanguageManager: ILanguageManager; ALangIndex: Integer); reintroduce;
    destructor Destroy; override;
  end;


implementation

uses
  u_NotifyEventListener;

{ TLanguageTBXItem }

constructor TLanguageTBXItem.Create(
  AOwner: TComponent;
  AParentMenu: TTBCustomItem;
  ALanguageManager: ILanguageManager;
  ALangIndex: Integer
);
begin
  inherited Create(AOwner);
  Assert(ALangIndex < ALanguageManager.LanguageList.Count);
  FLanguageManager := ALanguageManager;
  FLangIndex := ALangIndex;
  FParentMenu := AParentMenu;

  Self.OnClick := Self.OnClickItem;
  Self.Caption := FLanguageManager.GetLangNameByIndex(FLangIndex);
  Self.RadioItem := True;

  FListener := TNotifyEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);

  FParentMenu.Add(Self);
  OnLangChange(nil);
end;

destructor TLanguageTBXItem.Destroy;
begin
  FLanguageManager.GetChangeNotifier.Remove(FListener);
  FListener := nil;
  FLanguageManager := nil;
  inherited;
end;

procedure TLanguageTBXItem.OnClickItem(Sender: TObject);
begin
  FLanguageManager.SetCurrentLanguageIndex(FLangIndex);
end;

procedure TLanguageTBXItem.OnLangChange(Sender: TObject);
begin
  if FLangIndex = FLanguageManager.GetCurrentLanguageIndex then begin
    Self.Checked := True;
  end else begin
    Self.Checked := False;
  end;
end;

end.
