{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2017, SAS.Planet development team.                      *}
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

unit u_LayerSunCalcPopupMenu;

interface

uses
  GR32_Image,
  TBX,
  TB2Item,
  i_PopUp,
  i_Listener,
  i_LanguageManager,
  i_SunCalcConfig,
  i_SunCalcProvider,
  u_BaseInterfacedObject;

type
  TLayerSunCalcPopupMenu = class(TBaseInterfacedObject, IPopUp)
  private
    FLanguageManager: ILanguageManager;
    FParentMap: TImage32;
    FSunCalcConfig: ISunCalcConfig;
    FSunCalcProvider: ISunCalcProvider;

    FPopup: TTBXPopupMenu;
    FListener: IListener;

    procedure BuildPopUpMenu;
    procedure InitItemsState;
    procedure OnMenuItemClick(Sender: TObject);
    procedure OnChange;
  private
    { IPopUp }
    procedure PopUp;
  public
    constructor Create(
      const ALanguageManager: ILanguageManager;
      const AParentMap: TImage32;
      const ASunCalcConfig: ISunCalcConfig;
      const ASunCalcProvider: ISunCalcProvider
    );
    destructor Destroy; override;
  end;

implementation

uses
  SysUtils,
  u_ListenerByEvent;

resourcestring
  rsDetailedViewCaption = 'Detailed View';
  rsHideSunCalcCaption = 'Hide Sun Calculator';

const
  cDetailedViewTag = 1;
  cHideSunCalcTag = 2;
  cColorSchemaTagOffset = 100;

{ TLayerSunCalcPopupMenu }

constructor TLayerSunCalcPopupMenu.Create(
  const ALanguageManager: ILanguageManager;
  const AParentMap: TImage32;
  const ASunCalcConfig: ISunCalcConfig;
  const ASunCalcProvider: ISunCalcProvider
);
begin
  Assert(Assigned(ALanguageManager));
  Assert(Assigned(AParentMap));
  Assert(Assigned(ASunCalcConfig));
  Assert(Assigned(ASunCalcProvider));

  inherited Create;

  FLanguageManager := ALanguageManager;
  FParentMap := AParentMap;
  FSunCalcConfig := ASunCalcConfig;
  FSunCalcProvider := ASunCalcProvider;

  FListener := TNotifyNoMmgEventListener.Create(Self.OnChange);

  FLanguageManager.ChangeNotifier.Add(FListener);
  FSunCalcConfig.ChangeNotifier.Add(FListener);

  FPopup := TTBXPopupMenu.Create(nil);
  FPopup.Name := 'PopupSunCalc';

  BuildPopUpMenu;
end;

destructor TLayerSunCalcPopupMenu.Destroy;
begin
  if Assigned(FLanguageManager) and Assigned(FListener) then begin
    FLanguageManager.ChangeNotifier.Remove(FListener);
    FLanguageManager := nil;
  end;
  if Assigned(FSunCalcConfig) and Assigned(FListener) then begin
    FSunCalcConfig.ChangeNotifier.Remove(FListener);
    FSunCalcConfig := nil;
  end;
  FreeAndNil(FPopup);
  inherited;
end;

procedure TLayerSunCalcPopupMenu.OnChange;
begin
  FPopup.Items.Clear;
  BuildPopUpMenu;
end;

procedure TLayerSunCalcPopupMenu.OnMenuItemClick(Sender: TObject);
var
  VTag: Integer;
  VMenuItem: TTBXItem;
begin
  if Sender is TTBXItem then begin
    VMenuItem := Sender as TTBXItem;
    VTag := VMenuItem.Tag;
    if VTag < cColorSchemaTagOffset then begin
      case VTag of
        cDetailedViewTag: begin
          FSunCalcConfig.IsDetailedView := not FSunCalcConfig.IsDetailedView;
        end;
        cHideSunCalcTag: begin
          FSunCalcConfig.Visible := False;
        end;
      end;
    end else begin
      VTag := VTag - cColorSchemaTagOffset;
      FSunCalcConfig.ColorSchemaList.ActiveSchemaIndex := VTag;
    end;
  end;
end;

procedure TLayerSunCalcPopupMenu.BuildPopUpMenu;
var
  I: Integer;
  VMenuItem: TTBXItem;
  VColorSchema: ISunCalcColorSchemaStatic;
  VColorSchemaList: ISunCalcColorSchemaList;
begin
  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.Caption := rsDetailedViewCaption;
  VMenuItem.Tag := cDetailedViewTag;
  VMenuItem.OnClick := OnMenuItemClick;
  FPopup.Items.Add(VMenuItem);

  FPopup.Items.Add(
    TTBSeparatorItem.Create(FPopup)
  );

  VColorSchemaList := FSunCalcConfig.ColorSchemaList;
  VColorSchemaList.LockRead;
  try
    for I := 0 to VColorSchemaList.Count - 1 do begin
      VColorSchema := VColorSchemaList.GetColorSchemaByIndex(I).GetStatic;

      VMenuItem := TTBXItem.Create(FPopup);
      VMenuItem.RadioItem := True;
      VMenuItem.GroupIndex := 1;
      VMenuItem.Caption := VColorSchema.SchemaName;
      VMenuItem.Tag := 100 + I;
      VMenuItem.OnClick := OnMenuItemClick;
      FPopup.Items.Add(VMenuItem);
    end;
  finally
    VColorSchemaList.UnlockRead;
  end;

  FPopup.Items.Add(
    TTBSeparatorItem.Create(FPopup)
  );

  VMenuItem := TTBXItem.Create(FPopup);
  VMenuItem.Caption := rsHideSunCalcCaption;
  VMenuItem.Tag := cHideSunCalcTag;
  VMenuItem.OnClick := OnMenuItemClick;
  FPopup.Items.Add(VMenuItem);
end;

procedure TLayerSunCalcPopupMenu.InitItemsState;
var
  I: Integer;
  VTag: Integer;
  VMenuItem: TTBCustomItem;
  VActiveSchemaIndex: Integer;
begin
  VActiveSchemaIndex := FSunCalcConfig.ColorSchemaList.ActiveSchemaIndex;
  for I := 0 to FPopup.Items.Count - 1 do begin
    VMenuItem := FPopup.Items[I];
    VTag := VMenuItem.Tag;
    if VTag < cColorSchemaTagOffset then begin
      case VTag of
        cDetailedViewTag: begin
          VMenuItem.Checked := FSunCalcConfig.IsDetailedView;
        end;
      end;
    end else begin
      if (VTag - cColorSchemaTagOffset) = VActiveSchemaIndex then begin
        VMenuItem.Checked := True;
      end;
    end;
  end;
end;

procedure TLayerSunCalcPopupMenu.PopUp;
begin
  InitItemsState;
  FParentMap.PopupMenu := FPopup;
end;

end.
