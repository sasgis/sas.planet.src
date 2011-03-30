unit u_SensorViewTextTBXPanel;

interface

uses
  SyncObjs,
  Buttons,
  TB2Item,
  TB2Dock,
  TBX,
  TBXControls,
  i_JclNotify,
  i_JclListenerNotifierLinksList,
  i_Sensor;

type
  TSensorViewTextTBXPanel = class(TInterfacedObject, ISensorView)
  private
    FSensor: ISensorText;
    FConfig: ISensorViewConfig;

    FDefaultDoc: TTBDock;
    FParentMenu: TTBCustomItem;

    FCS: TCriticalSection;
    FLinksList: IJclListenerNotifierLinksList;

    FBar: TTBXToolWindow;
    FpnlTop: TTBXAlignmentPanel;
    FlblCaption: TTBXLabel;
    FbtnReset: TSpeedButton;
    FlblValue: TTBXLabel;

    FResetItem: TTBXItem;
    FVisibleItem: TTBXCustomItem;
    FVisibleItemWithReset: TTBXSubmenuItem;

    FTextChanged: Boolean;
    FLastText: string;

    procedure CreatePanel;
    procedure CreateMenu;

    procedure OnBarVisibleChanged(Sender: TObject);
    procedure OnVisibleItemClick(Sender: TObject);
    procedure OnResetClick(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    procedure OnConfigChange(Sender: TObject);
    procedure OnSensorChange(Sender: TObject);
  protected
    function GetConfig: ISensorViewConfig;
    function GetSensor: ISensor;
  public
    constructor Create(
      ASensor: ISensorText;
      AConfig: ISensorViewConfig;
      ATimerNoifier: IJclNotifier;
      ADefaultDoc: TTBDock;
      AParentMenu: TTBCustomItem
    );
    destructor Destroy; override;
  end;

implementation

uses
  Graphics,
  Controls,
  SysUtils,
  u_JclNotify,
  u_JclListenerNotifierLinksList,
  u_NotifyEventListener;

constructor TSensorViewTextTBXPanel.Create(
  ASensor: ISensorText;
  AConfig: ISensorViewConfig;
  ATimerNoifier: IJclNotifier;
  ADefaultDoc: TTBDock;
  AParentMenu: TTBCustomItem
);
begin
  FSensor := ASensor;
  FConfig := AConfig;
  FDefaultDoc := ADefaultDoc;
  FParentMenu := AParentMenu;

  FCS := TCriticalSection.Create;
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
    FSensor.GetChangeNotifier
  );

  CreatePanel;
  CreateMenu;

  FLinksList.ActivateLinks;
end;

destructor TSensorViewTextTBXPanel.Destroy;
begin
  FLinksList.DeactivateLinks;
  FLinksList := nil;
  FConfig := nil;
  FSensor := nil;
  FreeAndNil(FCS);
  inherited;
end;

procedure TSensorViewTextTBXPanel.CreateMenu;
begin
  if FSensor.CanReset then begin
    FVisibleItemWithReset := TTBXSubmenuItem.Create(FBar);
//    FVisibleItemWithReset.Name := '';
    FVisibleItemWithReset.AutoCheck := True;
    FVisibleItemWithReset.DropdownCombo := True;
    FVisibleItemWithReset.OnClick := Self.OnVisibleItemClick;
    FVisibleItemWithReset.Caption := FSensor.GetMenuItemName;

    FVisibleItem := FVisibleItemWithReset;

    FResetItem := TTBXItem.Create(FBar);

//    FResetItem.Name := '';
    FResetItem.OnClick := Self.OnResetClick;
    FResetItem.Caption := '—бросить';
    FResetItem.Hint := '';
    FVisibleItemWithReset.Add(FResetItem);
  end else begin
    FVisibleItem := TTBXItem.Create(FBar);
//    FVisibleItem.Name := '';
    FVisibleItemWithReset := nil;
  end;
  FVisibleItem.AutoCheck := True;
  FVisibleItem.OnClick := Self.OnVisibleItemClick;
  FVisibleItem.Caption := FSensor.GetMenuItemName;
  FParentMenu.Add(FVisibleItem);
end;

procedure TSensorViewTextTBXPanel.CreatePanel;
begin
  FBar := TTBXToolWindow.Create(nil);
  FlblValue := TTBXLabel.Create(FBar);
  FpnlTop := TTBXAlignmentPanel.Create(FBar);
  FlblCaption := TTBXLabel.Create(FBar);
  if FSensor.CanReset then begin
    FbtnReset := TSpeedButton.Create(FBar);
  end;

//  FBar.Name := '';
//  FBar.Parent := TBXDock1;
  FBar.Left := 0;
  FBar.Top := 144;
  FBar.Hint := FSensor.GetDescription;
  FBar.ClientAreaHeight := 32;
  FBar.ClientAreaWidth := 150;
//  FBar.DockPos := 18;
//  FBar.DockRow := 4;
  FBar.Stretch := True;
  FBar.TabOrder := 7;
  FBar.OnVisibleChanged := Self.OnBarVisibleChanged;
  FBar.Caption := FSensor.GetCaption;
  FBar.CurrentDock := FDefaultDoc;

//  FpnlTop.Name := '';
  FpnlTop.Parent := FBar;
  FpnlTop.Left := 0;
  FpnlTop.Top := 0;
  FpnlTop.Width := 150;
  FpnlTop.Height := 17;
  FpnlTop.Align := alTop;
  FpnlTop.TabOrder := 1;

  if FbtnReset <> nil then begin
//    FbtnReset.Name := '';
    FbtnReset.Parent := FpnlTop;
    FbtnReset.Tag := 3;
    FbtnReset.Left := 133;
    FbtnReset.Top := 0;
    FbtnReset.Width := 17;
    FbtnReset.Height := 17;
    FbtnReset.Hint := '—бросить';
    FbtnReset.Align := alRight;
    FbtnReset.Flat := True;
    FbtnReset.Margin := 0;
    FbtnReset.Spacing := 0;
    FbtnReset.OnClick := Self.OnResetClick;
//    FbtnReset.Glyph.
  end;

//  FlblCaption.Name := '';
  FlblCaption.Parent := FpnlTop;
  FlblCaption.Left := 0;
  FlblCaption.Top := 0;
  FlblCaption.Width := 133;
  FlblCaption.Height := 17;
  FlblCaption.Align := alClient;
  FlblCaption.Wrapping := twEndEllipsis;
  FlblCaption.Caption := FSensor.GetCaption;

//  FlblValue.Name := '';
  FlblValue.Parent := FBar;
  FlblValue.Left := 0;
  FlblValue.Top := 17;
  FlblValue.Width := 150;
  FlblValue.Height := 15;
  FlblValue.Align := alClient;
//  FlblValue.Font.Charset := RUSSIAN_CHARSET;
//  FlblValue.Font.Color := clWindowText;
  FlblValue.Font.Height := -16;
  FlblValue.Font.Name := 'Arial';
  FlblValue.Font.Style := [fsBold];
  FlblValue.ParentFont := False;
  FlblValue.Wrapping := twEndEllipsis;
  FlblValue.Caption := '';

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

procedure TSensorViewTextTBXPanel.OnResetClick(Sender: TObject);
begin
  if FSensor.CanReset then begin
    FSensor.Reset;
  end;
end;

procedure TSensorViewTextTBXPanel.OnSensorChange(Sender: TObject);
begin
  FCS.Acquire;
  try
    FTextChanged := True;
  finally
    FCS.Release;
  end;
end;

procedure TSensorViewTextTBXPanel.OnTimer(Sender: TObject);
var
  VText: string;
begin
  if FConfig.Visible then begin
    FCS.Acquire;
    try
      if FTextChanged then begin
        VText := FSensor.GetText;
        if FLastText <> VText then begin
          FLastText := VText;
          FlblValue.Caption := FLastText;
        end;
        FTextChanged := False;
      end;
    finally
      FCS.Release;
    end;
  end;
end;

procedure TSensorViewTextTBXPanel.OnVisibleItemClick(Sender: TObject);
begin
  FConfig.Visible := FVisibleItem.Checked;
end;

end.
