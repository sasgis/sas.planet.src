unit u_SensorViewTextTBXPanel;

interface

uses
  Buttons,
  TB2Dock,
  TBX,
  TBXControls,
  i_JclNotify,
  i_Sensor;

type
  TSensorViewTextTBXPanel = class(TInterfacedObject, ISensorView, ISensorViewText)
  private
    FSensorInfo: ISensorInfo;
    FConfig: ISensorViewConfig;
    FBar: TTBXToolWindow;
    FpnlTop: TTBXAlignmentPanel;
    FlblCaption: TTBXLabel;
    FbtnReset: TSpeedButton;
    FlblValue: TTBXLabel;
  protected
    function GetConfig: ISensorViewConfig;
    function GetResetNotify: IJclNotifier;
  protected
    procedure SetText(AValue: string);
  public
    constructor Create(ASensorInfo: ISensorInfo; AConfig: ISensorViewConfig; ADefaultDoc: TTBDock);
  end;

implementation


//procedure TfrmMain.CreateComponents;
//var
//  TBXsensorOdometrBar: TTBXToolWindow;
//  TBXSensorOdometr: TTBXLabel;
//  pnlSensorOdometrTop: TTBXAlignmentPanel;
//  btnSensorOdometrReset: TSpeedButton;
//  lblSensorOdometr: TTBXLabel;
//begin
//  //TBXsensorOdometrBar
//  TBXsensorOdometrBar := TTBXToolWindow.Create(Self);
//
//  //TBXSensorOdometr
//  TBXSensorOdometr := TTBXLabel.Create(Self);
//
//  //pnlSensorOdometrTop
//  pnlSensorOdometrTop := TTBXAlignmentPanel.Create(Self);
//
//  //btnSensorOdometrReset
//  btnSensorOdometrReset := TSpeedButton.Create(Self);
//
//  //lblSensorOdometr
//  lblSensorOdometr := TTBXLabel.Create(Self);
//
//  //TBXsensorOdometrBar
//  TBXsensorOdometrBar.Name := 'TBXsensorOdometrBar';
//  TBXsensorOdometrBar.Parent := TBXDock1;
//  TBXsensorOdometrBar.Left := 0;
//  TBXsensorOdometrBar.Top := 144;
//  TBXsensorOdometrBar.Hint := 
//#1054#1090#1086#1073#1088#1072#1078#1072#1077#1090' '#1074#1077#1089#1100' 
//'#1087#1088#1086#1081#1076#1077#1085#1085#1099#1081' 
//'#1087#1091#1090#1100;
//  TBXsensorOdometrBar.ClientAreaHeight := 32;
//  TBXsensorOdometrBar.ClientAreaWidth := 150;
//  TBXsensorOdometrBar.DockPos := 18;
//  TBXsensorOdometrBar.DockRow := 4;
//  TBXsensorOdometrBar.Stretch := True;
//  TBXsensorOdometrBar.TabOrder := 7;
//  TBXsensorOdometrBar.OnVisibleChanged := TBXSensorsBarVisibleChanged;
//  TBXsensorOdometrBar.Caption := #1054#1076#1086#1084#1077#1090#1088;
//
//  //TBXSensorOdometr
//  TBXSensorOdometr.Name := 'TBXSensorOdometr';
//  TBXSensorOdometr.Parent := TBXsensorOdometrBar;
//  TBXSensorOdometr.Left := 0;
//  TBXSensorOdometr.Top := 17;
//  TBXSensorOdometr.Width := 150;
//  TBXSensorOdometr.Height := 15;
//  TBXSensorOdometr.Align := alClient;
//  TBXSensorOdometr.Font.Charset := RUSSIAN_CHARSET;
//  TBXSensorOdometr.Font.Color := clWindowText;
//  TBXSensorOdometr.Font.Height := -16;
//  TBXSensorOdometr.Font.Name := 'Arial';
//  TBXSensorOdometr.Font.Style := [fsBold];
//  TBXSensorOdometr.ParentFont := False;
//  TBXSensorOdometr.Wrapping := twEndEllipsis;
//  TBXSensorOdometr.Caption := '-';
//
//  //pnlSensorOdometrTop
//  pnlSensorOdometrTop.Name := 'pnlSensorOdometrTop';
//  pnlSensorOdometrTop.Parent := TBXsensorOdometrBar;
//  pnlSensorOdometrTop.Left := 0;
//  pnlSensorOdometrTop.Top := 0;
//  pnlSensorOdometrTop.Width := 150;
//  pnlSensorOdometrTop.Height := 17;
//  pnlSensorOdometrTop.Align := alTop;
//  pnlSensorOdometrTop.TabOrder := 1;
//
//  //btnSensorOdometrReset
//  btnSensorOdometrReset.Name := 'btnSensorOdometrReset';
//  btnSensorOdometrReset.Parent := pnlSensorOdometrTop;
//  btnSensorOdometrReset.Tag := 3;
//  btnSensorOdometrReset.Left := 133;
//  btnSensorOdometrReset.Top := 0;
//  btnSensorOdometrReset.Width := 17;
//  btnSensorOdometrReset.Height := 17;
//  btnSensorOdometrReset.Hint := #1057#1073#1088#1086#1089#1080#1090#1100;
//  btnSensorOdometrReset.Align := alRight;
//  btnSensorOdometrReset.Flat := True;
//  btnSensorOdometrReset.Margin := 0;
//  btnSensorOdometrReset.Spacing := 0;
//  btnSensorOdometrReset.OnClick := SBClearSensorClick;
//
//  //lblSensorOdometr
//  lblSensorOdometr.Name := 'lblSensorOdometr';
//  lblSensorOdometr.Parent := pnlSensorOdometrTop;
//  lblSensorOdometr.Left := 0;
//  lblSensorOdometr.Top := 0;
//  lblSensorOdometr.Width := 133;
//  lblSensorOdometr.Height := 17;
//  lblSensorOdometr.Align := alClient;
//  lblSensorOdometr.Wrapping := twEndEllipsis;
//  lblSensorOdometr.Caption := #1054#1076#1086#1084#1077#1090#1088', 
//'#1082#1084':';
//end;

{ TSensorViewTextTBXPanel }

constructor TSensorViewTextTBXPanel.Create(ASensorInfo: ISensorInfo;
  AConfig: ISensorViewConfig; ADefaultDoc: TTBDock);
begin

end;

function TSensorViewTextTBXPanel.GetConfig: ISensorViewConfig;
begin

end;

function TSensorViewTextTBXPanel.GetResetNotify: IJclNotifier;
begin

end;

procedure TSensorViewTextTBXPanel.SetText(AValue: string);
begin

end;

end.
