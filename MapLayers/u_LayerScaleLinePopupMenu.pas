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

unit u_LayerScaleLinePopupMenu;

interface

uses
  Classes,
  GR32_Image,
  TBX,
  TB2Item,
  i_ScaleLineConfig;

type
  TLayerScaleLinePopupMenu = class(TObject)
    private
    FParentMap: TImage32;
    FPopup: TTBXPopupMenu;
    FConfig: IScaleLineConfig;
    FOnOptionsClick: TNotifyEvent;
    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
  public
    constructor Create(
      const AParentMap: TImage32;
      const AConfig: IScaleLineConfig;
      const AOnOptionsClick: TNotifyEvent
    );
    destructor Destroy; override;
    procedure PopUp;
  end;

implementation

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

const
  cMenuItemList: array [TMenuItemTag] of string = (
    'Show Vertical Scale Legend',
    'Numbers Format',
    'Nice',
    'Round',
    'Science',
    'Hide Scale Legend',
    'Options...'
  );

{ TLayerScaleLinePopupMenu }

constructor TLayerScaleLinePopupMenu.Create(
  const AParentMap: TImage32;
  const AConfig: IScaleLineConfig;
  const AOnOptionsClick: TNotifyEvent 
);
begin
  inherited Create;
  FParentMap := AParentMap;
  FConfig := AConfig;
  FOnOptionsClick := AOnOptionsClick;
  FPopup := TTBXPopupMenu.Create(FParentMap);
  FPopup.Name := 'PopupScaleLine';
  BuildPopUpMenu;
end;

destructor TLayerScaleLinePopupMenu.Destroy;
begin
  inherited Destroy;
end;

procedure TLayerScaleLinePopupMenu.BuildPopUpMenu;
var
  I: TMenuItemTag;
  VMenuItem: TTBXItem;
  VMenuSubItem: TTBXSubmenuItem;
begin
  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.AutoCheck := True;
  VMenuItem.Caption := cMenuItemList[tagExtended];
  VMenuItem.Tag := Integer(tagExtended);
  VMenuItem.OnClick := OnMenuItemClick;
  FPopup.Items.Add(VMenuItem);

  VMenuSubItem := TTBXSubmenuItem.Create(FPopup);
  VMenuSubItem.Caption := cMenuItemList[tagNumbersFormat];
  VMenuSubItem.Tag := Integer(tagNumbersFormat);
  for I := tagNice to tagScience do begin
    VMenuItem := TTBXItem.Create(FPopup);
    VMenuItem.RadioItem := True;
    VMenuItem.AutoCheck := True;
    VMenuItem.GroupIndex := 1;
    VMenuItem.Caption := cMenuItemList[I];
    VMenuItem.Tag := Integer(I);
    VMenuItem.OnClick := OnMenuItemClick;
    VMenuSubItem.Add(VMenuItem);
  end;
  FPopup.Items.Add(VMenuSubItem);

  for I := tagHide to tagOptions do begin
    VMenuItem := TTBXItem.Create(FPopup);
    VMenuItem.Caption := cMenuItemList[I];
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

end.
