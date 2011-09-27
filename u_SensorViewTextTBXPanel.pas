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
  TSensorViewTextTBXPanel = class(TInterfacedObject, ISensorView)
  private
    FListEntity: ISensorListEntity;
    FSensor: ISensorText;
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
    FlblValue: TTBXLabel;

    FResetItem: TTBXItem;
    FVisibleItem: TTBXCustomItem;
    FVisibleItemWithReset: TTBXSubmenuItem;

    FTextChangeId: Integer;
    FTextShowId: Integer;
    FLastText: string;

    function GuidToComponentName(APrefix: string; AGUID: TGUID): string;
    procedure CreatePanel;
    procedure CreateMenu;

    procedure UpdateControls;

    procedure OnBarVisibleChanged(Sender: TObject);
    procedure OnVisibleItemClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
    procedure OnSensorChange(Sender: TObject);
    procedure OnSensorDataUpdate(Sender: TObject);
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

implementation

uses
  Graphics,
  Controls,
  SysUtils,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener,
  u_ResStrings;

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
  FListEntity := AListEntity;
  if not Supports(FListEntity.GetSensor, ISensorText, FSensor) then begin
    raise Exception.Create('Неподдерживаемый тип сенсора');
  end;
  FConfig := AConfig;
  FOwner := AOwner;
  FTextChangeId := 0;
  FTextShowId := 0;
  Assert(FOwner is TWinControl);
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;
  FImages := AImages;
  FImageIndexReset := AImageIndexReset;

  FLinksList := TJclListenerNotifierLinksList.Create;

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnConfigChange),
    FConfig.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnTimer),
    ATimerNoifier
  );

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnSensorChange),
    FListEntity.GetChangeNotifier
  );

  FLinksList.Add(
    TNotifyEventListener.Create(Self.OnSensorDataUpdate),
    FSensor.GetDataUpdateNotifier
  );

  CreatePanel;
  CreateMenu;
  UpdateControls;

  FLinksList.ActivateLinks;
  OnConfigChange(nil);
  OnSensorDataUpdate(nil);
end;

destructor TSensorViewTextTBXPanel.Destroy;
begin
  FLinksList.DeactivateLinks;
  FLinksList := nil;
  FreeAndNil(FBar);
  FConfig := nil;
  FSensor := nil;
  inherited;
end;

procedure TSensorViewTextTBXPanel.CreateMenu;
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

procedure TSensorViewTextTBXPanel.CreatePanel;
begin
  FBar := TTBXToolWindow.Create(FOwner);
  FlblValue := TTBXLabel.Create(FBar);
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

//  FlblCaption.Name := '';
  FlblCaption.Parent := FpnlTop;
  FlblCaption.Left := 0;
  FlblCaption.Top := 0;
  FlblCaption.Width := 133;
  FlblCaption.Height := 18;
  FlblCaption.Align := alClient;
  FlblCaption.Wrapping := twEndEllipsis;

//  FlblValue.Name := '';
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

{ TSensorViewTextTBXPanel }

procedure TSensorViewTextTBXPanel.OnBarVisibleChanged(Sender: TObject);
begin
  FConfig.Visible := FBar.Visible;
end;

procedure TSensorViewTextTBXPanel.OnConfigChange(Sender: TObject);
var
  VVisible: Boolean;
begin
  VVisible := FConfig.Visible;
  FBar.Visible := VVisible;
  FVisibleItem.Checked := VVisible;
end;

function TSensorViewTextTBXPanel.GetConfig: ISensorViewConfig;
begin
  Result := FConfig;
end;

function TSensorViewTextTBXPanel.GetSensor: ISensor;
begin
  Result := FSensor;
end;

function TSensorViewTextTBXPanel.GuidToComponentName(APrefix: string;
  AGUID: TGUID): string;
var
  VGUIDStr: string;
begin
  VGUIDStr := GUIDToString(AGUID);
  VGUIDStr := StringReplace(VGUIDStr, '{', '', [rfReplaceAll]);
  VGUIDStr := StringReplace(VGUIDStr, '}', '', [rfReplaceAll]);
  VGUIDStr := StringReplace(VGUIDStr, '-', '_', [rfReplaceAll]);
  Result := APrefix + VGUIDStr;
end;

procedure TSensorViewTextTBXPanel.OnResetClick(Sender: TObject);
begin
  if FSensor.CanReset then begin
    if (MessageBox(TWinControl(FOwner).Handle, pchar(SAS_MSG_youasurerefrsensor),pchar(SAS_MSG_coution),36)=IDYES) then begin
      FSensor.Reset;
      OnTimer(nil);
    end;
  end;
end;

procedure TSensorViewTextTBXPanel.OnSensorChange(Sender: TObject);
begin
  UpdateControls;
end;

procedure TSensorViewTextTBXPanel.OnSensorDataUpdate(Sender: TObject);
begin
  InterlockedIncrement(FTextChangeId);
end;

procedure TSensorViewTextTBXPanel.OnTimer(Sender: TObject);
var
  VText: string;
begin
  if FConfig.Visible then begin
    if FTextChangeId <> FTextShowId then begin
      VText := FSensor.GetText;
      if FLastText <> VText then begin
        FLastText := VText;
        FlblValue.Caption := FLastText;
      end;
      FTextShowId := FTextChangeId;
    end;
  end;
end;

procedure TSensorViewTextTBXPanel.OnVisibleItemClick(Sender: TObject);
begin
  FConfig.Visible := FVisibleItem.Checked;
end;

procedure TSensorViewTextTBXPanel.UpdateControls;
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

end.
