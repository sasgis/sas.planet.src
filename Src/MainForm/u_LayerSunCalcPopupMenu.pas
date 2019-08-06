{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2019, SAS.Planet development team.                      *}
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
  rsSunCalcCaption = 'Sun Calculator';
  rsMoonCalcCaption = 'Moon Calculator';
  rsCloseCalcCaption = 'Close the Calculator';
  rsShowDayInfoPanelCaption = 'Show Info Panel';
  rsShowCurrAzAndAltCaption = 'Show Current Altitude and Azimuth';

const
  cDetailedViewTag = 1;
  cCloseCalcTag = 2;
  cShowCurrAzAndAltTag = 3;
  cShowDayInfoPanelTag = 4;
  cSunProviderTypeTag = 5;
  cMoonProviderTypeTag = 6;
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
        cShowDayInfoPanelTag: begin
          FSunCalcConfig.ShowDayInfoPanel := not FSunCalcConfig.ShowDayInfoPanel;
        end;
        cShowCurrAzAndAltTag: begin
          FSunCalcConfig.ShowCaptionNearSun := not FSunCalcConfig.ShowCaptionNearSun;
        end;
        cSunProviderTypeTag: begin
          FSunCalcConfig.DataProviderType := scdpSun;
        end;
        cMoonProviderTypeTag: begin
          FSunCalcConfig.DataProviderType := scdpMoon;
        end;
        cCloseCalcTag: begin
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

  procedure AddMenuItem(const ACaption: string; const ATag: Integer; const AGroup: Integer = 0);
  var
    VMenuItem: TTBXItem;
  begin
    VMenuItem := TTBXItem.Create(FPopup);
    VMenuItem.Caption := ACaption;
    VMenuItem.Tag := ATag;
    VMenuItem.OnClick := OnMenuItemClick;
    if AGroup > 0 then begin
      VMenuItem.RadioItem := True;
      VMenuItem.GroupIndex := AGroup;
    end;
    FPopup.Items.Add(VMenuItem);
  end;

  procedure AddSeparator;
  begin
    FPopup.Items.Add( TTBSeparatorItem.Create(FPopup) );
  end;

var
  I: Integer;
  VGroup: Integer;
  VColorSchema: ISunCalcColorSchemaStatic;
  VColorSchemaList: ISunCalcColorSchemaList;
begin
  VGroup := 0;

  AddMenuItem(rsDetailedViewCaption, cDetailedViewTag);
  AddMenuItem(rsShowDayInfoPanelCaption, cShowDayInfoPanelTag);
  AddMenuItem(rsShowCurrAzAndAltCaption, cShowCurrAzAndAltTag);

  AddSeparator;

  VColorSchemaList := FSunCalcConfig.ColorSchemaList;
  VColorSchemaList.LockRead;
  try
    Inc(VGroup);
    for I := 0 to VColorSchemaList.Count - 1 do begin
      VColorSchema := VColorSchemaList.GetColorSchemaByIndex(I).GetStatic;
      AddMenuItem(VColorSchema.SchemaName, cColorSchemaTagOffset + I, VGroup);
    end;
  finally
    VColorSchemaList.UnlockRead;
  end;

  AddSeparator;

  Inc(VGroup);
  AddMenuItem(rsSunCalcCaption, cSunProviderTypeTag, VGroup);
  AddMenuItem(rsMoonCalcCaption, cMoonProviderTypeTag, VGroup);

  AddSeparator;

  AddMenuItem(rsCloseCalcCaption, cCloseCalcTag);
end;

procedure TLayerSunCalcPopupMenu.InitItemsState;
var
  I: Integer;
  VTag: Integer;
  VMenuItem: TTBCustomItem;
  VProvider: TSunCalcDataProviderType;
  VActiveSchemaIndex: Integer;
begin
  VProvider := FSunCalcConfig.DataProviderType;
  VActiveSchemaIndex := FSunCalcConfig.ColorSchemaList.ActiveSchemaIndex;
  for I := 0 to FPopup.Items.Count - 1 do begin
    VMenuItem := FPopup.Items[I];
    VTag := VMenuItem.Tag;
    if VTag < cColorSchemaTagOffset then begin
      case VTag of
        cDetailedViewTag: begin
          VMenuItem.Checked := FSunCalcConfig.IsDetailedView;
        end;
        cShowDayInfoPanelTag: begin
          VMenuItem.Checked := FSunCalcConfig.ShowDayInfoPanel;
        end;
        cShowCurrAzAndAltTag: begin
          VMenuItem.Checked := FSunCalcConfig.ShowCaptionNearSun;
        end;
        cSunProviderTypeTag: begin
          VMenuItem.Checked := VProvider = scdpSun;
        end;
        cMoonProviderTypeTag: begin
          VMenuItem.Checked := VProvider = scdpMoon;
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
