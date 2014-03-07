{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2014, SAS.Planet development team.                      *}
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
{* http://sasgis.org                                                          *}
{* info@sasgis.org                                                            *}
{******************************************************************************}

unit u_LayerScaleLinePopupMenu;

interface

uses
  Classes,
  GR32_Image,
  TBX,
  TB2Item,
  i_Notifier,
  i_Listener,
  i_LanguageManager,
  i_PopUp,
  i_ScaleLineConfig,
  u_BaseInterfacedObject;

type
  TLayerScaleLinePopupMenu = class(TBaseInterfacedObject, IPopUp)
  private
    FParentMap: TImage32;
    FPopup: TTBXPopupMenu;
    FConfig: IScaleLineConfig;
    FOnOptionsClick: TNotifyEvent;
    FLanguageManager: ILanguageManager;
    FListener: IListener;
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnLangChange;
  private
    procedure PopUp;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AParentMap: TImage32;
      const AConfig: IScaleLineConfig;
      const AOnOptionsClick: TNotifyEvent
    );
    destructor Destroy; override;
  end;

implementation

uses
  u_ListenerByEvent;

resourcestring
  rsShowVerticalScaleLegend = 'Show Vertical Scale Legend';
  rsNumbersFormat = 'Numbers Format';
  rsNice = 'Nice';
  rsRound = 'Round';
  rsScience = 'Science';
  rsHideScaleLegend = 'Hide Scale Legend';
  rsOptions = 'Options...';

type
  TMenuItemTag = (
    tagExtended,
    tagNumbersFormat,
    tagNice,
    tagRound,
    tagScience,
    tagHide,
    tagOptions
  );

  TMenuItemList = array [TMenuItemTag] of string;

function GetMenuItemList: TMenuItemList; inline;
begin
  Result[TMenuItemTag(0)] := rsShowVerticalScaleLegend;
  Result[TMenuItemTag(1)] := rsNumbersFormat;
  Result[TMenuItemTag(2)] := rsNice;
  Result[TMenuItemTag(3)] := rsRound;
  Result[TMenuItemTag(4)] := rsScience;
  Result[TMenuItemTag(5)] := rsHideScaleLegend;
  Result[TMenuItemTag(6)] := rsOptions;
end;

{ TLayerScaleLinePopupMenu }

constructor TLayerScaleLinePopupMenu.Create(
  const ALanguageManager: ILanguageManager;
  const AParentMap: TImage32;
  const AConfig: IScaleLineConfig;
  const AOnOptionsClick: TNotifyEvent
);
begin
  inherited Create;
  FLanguageManager := ALanguageManager;
  FParentMap := AParentMap;
  FConfig := AConfig;
  FOnOptionsClick := AOnOptionsClick;

  FPopup := TTBXPopupMenu.Create(FParentMap);
  FPopup.Name := 'PopupScaleLine';

  BuildPopUpMenu;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnLangChange);
  FLanguageManager.GetChangeNotifier.Add(FListener);
end;

destructor TLayerScaleLinePopupMenu.Destroy;
begin
  if Assigned(FLanguageManager) and Assigned(FListener) then begin
    FLanguageManager.GetChangeNotifier.Remove(FListener);
    FListener := nil;
    FLanguageManager := nil;
  end;
  FConfig := nil;
  inherited;
end;

procedure TLayerScaleLinePopupMenu.BuildPopUpMenu;
var
  I: TMenuItemTag;
  VMenuItem: TTBXItem;
  VMenuSubItem: TTBXSubmenuItem;
  VMenuItemList: TMenuItemList;
begin
  VMenuItemList := GetMenuItemList;

  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.AutoCheck := True;
  VMenuItem.Caption := VMenuItemList[tagExtended];
  VMenuItem.Tag := Integer(tagExtended);
  VMenuItem.OnClick := OnMenuItemClick;
  FPopup.Items.Add(VMenuItem);

  VMenuSubItem := TTBXSubmenuItem.Create(FPopup);
  VMenuSubItem.Caption := VMenuItemList[tagNumbersFormat];
  VMenuSubItem.Tag := Integer(tagNumbersFormat);
  for I := tagNice to tagScience do begin
    VMenuItem := TTBXItem.Create(FPopup);
    VMenuItem.RadioItem := True;
    VMenuItem.AutoCheck := True;
    VMenuItem.GroupIndex := 1;
    VMenuItem.Caption := VMenuItemList[I];
    VMenuItem.Tag := Integer(I);
    VMenuItem.OnClick := OnMenuItemClick;
    VMenuSubItem.Add(VMenuItem);
  end;
  FPopup.Items.Add(VMenuSubItem);

  for I := tagHide to tagOptions do begin
    VMenuItem := TTBXItem.Create(FPopup);
    VMenuItem.Caption := VMenuItemList[I];
    VMenuItem.Tag := Integer(I);
    VMenuItem.OnClick := OnMenuItemClick;
    FPopup.Items.Add(VMenuItem);
  end;

  InitItemsState;
end;

procedure TLayerScaleLinePopupMenu.InitItemsState;
var
  I, J: Integer;
  VMenuItem: TTBCustomItem;
begin
  for I := 0 to FPopup.Items.Count - 1 do begin
    VMenuItem := FPopup.Items[I];
    case TMenuItemTag(VMenuItem.Tag) of
      tagExtended: VMenuItem.Checked := FConfig.Extended;
      tagNumbersFormat: begin
        for J := 0 to FPopup.Items[I].Count - 1 do begin
          VMenuItem := FPopup.Items[I].Items[J];
          case TMenuItemTag(VMenuItem.Tag) of
            tagNice: VMenuItem.Checked := FConfig.NumbersFormat = slnfNice;
            tagRound: VMenuItem.Checked := FConfig.NumbersFormat = slnfScienceRound;
            tagScience: VMenuItem.Checked := FConfig.NumbersFormat = slnfScience;
          end;
        end;
      end;
    end;
  end;
end;

procedure TLayerScaleLinePopupMenu.PopUp;
begin
  InitItemsState;
  FParentMap.PopupMenu := FPopup;
end;

procedure TLayerScaleLinePopupMenu.OnMenuItemClick(Sender: TObject);
var
  VMenuItem: TTBXItem;
begin
  if Sender is TTBXItem then begin
    VMenuItem := Sender as TTBXItem;
    case TMenuItemTag(VMenuItem.Tag) of
      tagExtended: FConfig.Extended := VMenuItem.Checked;
      tagNice: FConfig.NumbersFormat := slnfNice;
      tagRound: FConfig.NumbersFormat := slnfScienceRound;
      tagScience: FConfig.NumbersFormat := slnfScience;
      tagHide: FConfig.Visible := False;
      tagOptions: if Assigned(FOnOptionsClick) then FOnOptionsClick(Self);
    end;
  end;
end;

procedure TLayerScaleLinePopupMenu.OnLangChange;
var
  I, J: Integer;
  VMenuItemList: TMenuItemList;
begin
  VMenuItemList := GetMenuItemList;
  for I := 0 to FPopup.Items.Count - 1 do begin
    FPopup.Items[I].Caption := VMenuItemList[TMenuItemTag(FPopup.Items[I].Tag)];
    if TMenuItemTag(FPopup.Items[I].Tag) = tagNumbersFormat then begin
      for J := 0 to FPopup.Items[I].Count - 1 do begin
        FPopup.Items[I].Items[J].Caption :=
          VMenuItemList[TMenuItemTag(FPopup.Items[I].Items[J].Tag)];
      end;
    end;
  end;
end;

end.
