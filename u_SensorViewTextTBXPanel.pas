{******************************************************************************}
{* SAS.Planet (SAS.Планета)                                                   *}
{* Copyright (C) 2007-2011, SAS.Planet development team.                      *}
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

unit u_SensorViewTextTBXPanel;

interface

uses
  Windows,
  Classes,
  ImgList,
  TB2Item,
  TB2Dock,
  TBX,
  TBXControls,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_SensorList,
  i_Sensor;

type
  TSensorViewTBXPanelBase = class(TInterfacedObject, ISensorView)
  private
    FListEntity: ISensorListEntity;
    FSensor: ISensor;
    FConfig: ISensorViewConfig;
    FOwner: TComponent;
    FDefaultDoc: TTBDock;
    FParentMenu: TTBCustomItem;
    FImages: TCustomImageList;
    FImageIndexReset: TImageIndex;
    FLinksList: IJclListenerNotifierLinksList;

    FBar: TTBXToolWindow;
    FpnlTop: TTBXAlignmentPanel;
    FlblCaption: TTBXLabel;
    FbtnReset: TTBXButton;

    FResetItem: TTBXItem;
    FVisibleItem: TTBXCustomItem;
    FVisibleItemWithReset: TTBXSubmenuItem;

    FValueChangeId: Integer;
    FValueShowId: Integer;

    function GuidToComponentName(APrefix: string; AGUID: TGUID): string;
    procedure CreateMenu;
    procedure UpdateControls;

    procedure OnBarVisibleChanged(Sender: TObject);
    procedure OnVisibleItemClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnTimer;
    procedure OnConfigChange;
    procedure OnSensorChange;
    procedure OnSensorDataUpdate;
  protected
    procedure CreatePanel; virtual;
    procedure UpdateDataView; virtual; abstract;
  protected
    function GetConfig: ISensorViewConfig;
    function GetSensor: ISensor;
  public
    constructor Create(
      AListEntity: ISensorListEntity;
      AConfig: ISensorViewConfig;
      ATimerNoifier: IJclNotifier;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
    destructor Destroy; override;
  end;

  TSensorViewTextTBXPanel = class(TSensorViewTBXPanelBase)
  private
    FSensor: ISensorText;
    FlblValue: TTBXLabel;
    FLastText: string;

  protected
    procedure CreatePanel; override;
    procedure UpdateDataView; override;
  public
    constructor Create(
      AListEntity: ISensorListEntity;
      AConfig: ISensorViewConfig;
      ATimerNoifier: IJclNotifier;
      AOwner: TComponent;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem;
      AImages: TCustomImageList;
      AImageIndexReset: TImageIndex
    );
  end;

implementation

uses
  Graphics,
  Controls,
  SysUtils,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_ResStrings;

{ TSensorViewTBXPanelBase }

constructor TSensorViewTBXPanelBase.Create(AListEntity: ISensorListEntity;
  AConfig: ISensorViewConfig; ATimerNoifier: IJclNotifier; AOwner: TComponent;
  ADefaultDoc: TTBDock; AParentMenu: TTBCustomItem; AImages: TCustomImageList;
  AImageIndexReset: TImageIndex);
begin
  FListEntity := AListEntity;
  FSensor := FListEntity.GetSensor;
  FConfig := AConfig;
  FOwner := AOwner;
  FValueChangeId := 0;
  FValueShowId := 0;
  Assert(FOwner is TWinControl);
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;
  FImages := AImages;
  FImageIndexReset := AImageIndexReset;

  FLinksList := TJclListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorChange),
    FListEntity.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyNoMmgEventListener.Create(Self.OnSensorDataUpdate),
    FSensor.GetDataUpdateNotifier
  );

  CreatePanel;
  CreateMenu;
  UpdateControls;

  FLinksList.ActivateLinks;
  OnConfigChange;
  OnSensorDataUpdate;
end;

destructor TSensorViewTBXPanelBase.Destroy;
begin
  FLinksList.DeactivateLinks;
  FLinksList := nil;
  FreeAndNil(FBar);
  FConfig := nil;
  FSensor := nil;
  inherited;
end;

procedure TSensorViewTBXPanelBase.CreateMenu;
begin
  if FSensor.CanReset then begin
    FVisibleItemWithReset := TTBXSubmenuItem.Create(FBar);
    FVisibleItemWithReset.DropdownCombo := True;
    FVisibleItem := FVisibleItemWithReset;

    FResetItem := TTBXItem.Create(FBar);

    FResetItem.Name := GuidToComponentName('SensorReset_', FListEntity.GetGUID);
    FResetItem.OnClick := Self.OnResetClick;
    FResetItem.Hint := '';
    FResetItem.Images := FImages;
    FResetItem.ImageIndex := FImageIndexReset;
    FVisibleItemWithReset.Add(FResetItem);
  end else begin
    FVisibleItem := TTBXItem.Create(FBar);
    FVisibleItemWithReset := nil;
  end;
  FVisibleItem.Name := GuidToComponentName('Sensor_', FListEntity.GetGUID);
  FVisibleItem.AutoCheck := True;
  FVisibleItem.OnClick := Self.OnVisibleItemClick;
  FParentMenu.Add(FVisibleItem);
end;

procedure TSensorViewTBXPanelBase.CreatePanel;
begin
  FBar := TTBXToolWindow.Create(FOwner);
  FpnlTop := TTBXAlignmentPanel.Create(FBar);
  FlblCaption := TTBXLabel.Create(FBar);

  FBar.Name := GuidToComponentName('Sensor_', FListEntity.GetGUID);
  FBar.Align := alTop;
  FBar.ActivateParent := True;
  FBar.DefaultDock := FDefaultDoc;
  FBar.UseLastDock := False;
  FBar.ClientAreaHeight := 32;
  FBar.ClientAreaWidth := 150;
  FBar.DockRow := FDefaultDoc.GetHighestRow(False) + 1;
  FBar.DockPos := 0;
  FBar.Stretch := True;
  FBar.OnVisibleChanged := Self.OnBarVisibleChanged;
  FBar.Visible := True;
  FBar.Parent := FDefaultDoc;
  FBar.CurrentDock := FDefaultDoc;

  FpnlTop.Parent := FBar;
  FpnlTop.Left := 0;
  FpnlTop.Top := 0;
  FpnlTop.Width := 150;
  FpnlTop.Height := 18;
  FpnlTop.Align := alTop;

  if FSensor.CanReset then begin
    FbtnReset := TTBXButton.Create(FBar);
    FbtnReset.Parent := FpnlTop;
    FbtnReset.Left := 133;
    FbtnReset.Top := 0;
    FbtnReset.Width := 18;
    FbtnReset.Height := 18;
    FbtnReset.Align := alRight;
    FbtnReset.TabStop := False;
    FbtnReset.Images := FImages;
    FbtnReset.ImageIndex := FImageIndexReset;
    FbtnReset.ButtonStyle := bsFlat;

    FbtnReset.OnClick := Self.OnResetClick;
  end;

  FlblCaption.Parent := FpnlTop;
  FlblCaption.Left := 0;
  FlblCaption.Top := 0;
  FlblCaption.Width := 133;
  FlblCaption.Height := 18;
  FlblCaption.Align := alClient;
  FlblCaption.Wrapping := twEndEllipsis;
end;

function TSensorViewTBXPanelBase.GetConfig: ISensorViewConfig;
begin
  Result := FConfig;
end;

function TSensorViewTBXPanelBase.GetSensor: ISensor;
begin
  Result := FSensor;
end;

function TSensorViewTBXPanelBase.GuidToComponentName(
  APrefix: string;
  AGUID: TGUID
): string;
var
  VGUIDStr: string;
begin
  VGUIDStr := GUIDToString(AGUID);
  VGUIDStr := StringReplace(VGUIDStr, '{', '', [rfReplaceAll]);
  VGUIDStr := StringReplace(VGUIDStr, '}', '', [rfReplaceAll]);
  VGUIDStr := StringReplace(VGUIDStr, '-', '_', [rfReplaceAll]);
  Result := APrefix + VGUIDStr;
end;

procedure TSensorViewTBXPanelBase.OnBarVisibleChanged(Sender: TObject);
begin
  FConfig.Visible := FBar.Visible;
end;

procedure TSensorViewTBXPanelBase.OnConfigChange;
var
  VVisible: Boolean;
begin
  VVisible := FConfig.Visible;
  FBar.Visible := VVisible;
  FVisibleItem.Checked := VVisible;
end;

procedure TSensorViewTBXPanelBase.OnResetClick(Sender: TObject);
begin
  if FSensor.CanReset then begin
    if (MessageBox(TWinControl(FOwner).Handle, pchar(SAS_MSG_youasurerefrsensor),pchar(SAS_MSG_coution),36)=IDYES) then begin
      FSensor.Reset;
      OnTimer;
    end;
  end;
end;

procedure TSensorViewTBXPanelBase.OnSensorChange;
begin
  UpdateControls;
end;

procedure TSensorViewTBXPanelBase.OnSensorDataUpdate;
begin
  InterlockedIncrement(FValueChangeId);
end;

procedure TSensorViewTBXPanelBase.OnTimer;
begin
  if FConfig.Visible then begin
    if FValueChangeId <> FValueShowId then begin
      UpdateDataView;
      FValueShowId := FValueChangeId;
    end;
  end;
end;

procedure TSensorViewTBXPanelBase.OnVisibleItemClick(Sender: TObject);
begin
  FConfig.Visible := FVisibleItem.Checked;
end;

procedure TSensorViewTBXPanelBase.UpdateControls;
begin
  FVisibleItem.Caption := FListEntity.GetMenuItemName;
  FBar.Caption := FListEntity.GetCaption;
  FBar.Hint := FListEntity.GetDescription;
  FlblCaption.Caption := FListEntity.GetCaption;
  if FResetItem <> nil then begin
    FResetItem.Caption := SAS_STR_SensorReset;
  end;
  if FbtnReset <> nil then begin
    FbtnReset.Hint := SAS_STR_SensorReset;
  end;
end;

{ TSensorViewTextTBXPanel }

constructor TSensorViewTextTBXPanel.Create(
  AListEntity: ISensorListEntity;
  AConfig: ISensorViewConfig;
  ATimerNoifier: IJclNotifier;
  AOwner: TComponent;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem;
  AImages: TCustomImageList;
  AImageIndexReset: TImageIndex
);
begin
  inherited;
  if not Supports(FListEntity.GetSensor, ISensorText, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
end;

procedure TSensorViewTextTBXPanel.CreatePanel;
begin
  inherited;
  FlblValue := TTBXLabel.Create(FBar);

  FlblValue.Parent := FBar;
  FlblValue.AutoSize := True;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alTop;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

  FBar.ClientAreaHeight := FlblValue.Top + FlblValue.Height + 2;
end;

procedure TSensorViewTextTBXPanel.UpdateDataView;
var
  VText: string;
begin
  VText := FSensor.GetText;
  if FLastText <> VText then begin
    FLastText := VText;
    FlblValue.Caption := FLastText;
  end;
end;

end.
