{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2012, SAS.Planet development team.                      *}
{* This program is free software: you can redistribute it and/or modify       *}
{* it under the terms of the GNU General Public License as published by       *}
{* the Free Software Foundation, either version 3 of the License, or          *}
{* (at your option) any later version.                                        *}
{*                                                                            *}
{* This program is distributed in the hope that it will be useful,            *}
{* but WITHOUT ANY WARRANTY; without even the implied warranty of             *}
{* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              *}
{* GNU General Public License for more details.                               *}
{*                                                                            *}
{* You should have received a copy of the GNU General Public License          *}
{* along with this program.  If not, see <http://www.gnu.org/licenses/>.      *}
{*                                                                            *}
{* http://sasgis.ru                                                           *}
{* az@sasgis.ru                                                               *}
{******************************************************************************}

unit u_LanguageTBXItem;

interface

uses
  Classes,
  TB2Item,
  TBX,
  i_Notifier,
  i_Listener,
  i_LanguageManager;

type
  TLanguageTBXItem = class(TTBXCustomItem)
  private
    FParentMenu: TTBCustomItem;
    FLangIndex: Integer;
    FLanguageManager: ILanguageManager;
    FListener: IListener;
    procedure OnLangChange;
    procedure OnClickItem(Sender: TObject);
  public
    constructor Create(
      AOwner: TComponent;
      AParentMenu: TTBCustomItem;
      const ALanguageManager: ILanguageManager;
      ALangIndex: Integer
    ); reintroduce;
    destructor Destroy; override;
  end;


implementation

uses
  u_ListenerByEvent;

{ TLanguageTBXItem }

constructor TLanguageTBXItem.Create(
  AOwner: TComponent;
  AParentMenu: TTBCustomItem;
  const ALanguageManager: ILanguageManager;
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

  FListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);

  FParentMenu.Add(Self);
  OnLangChange;
end;

destructor TLanguageTBXItem.Destroy;
begin
  if Assigned(FLanguageManager) and Assigned(FListener) then begin
    FLanguageManager.GetChangeNotifier.Remove(FListener);
    FListener := nil;
    FLanguageManager := nil;
  end;
  inherited;
end;

procedure TLanguageTBXItem.OnClickItem(Sender: TObject);
begin
  FLanguageManager.SetCurrentLanguageIndex(FLangIndex);
end;

procedure TLanguageTBXItem.OnLangChange;
begin
  if FLangIndex = FLanguageManager.GetCurrentLanguageIndex then begin
    Self.Checked := True;
  end else begin
    Self.Checked := False;
  end;
end;

end.
