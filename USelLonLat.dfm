object frmLonLatRectEdit: TfrmLonLatRectEdit
  Left = 192
  Top = 289
  BorderStyle = bsSizeToolWin
  Caption = #1042#1099#1076#1077#1083#1077#1085#1080#1077' '#1087#1086' '#1082#1086#1086#1088#1076#1080#1085#1072#1090#1072#1084
  ClientHeight = 217
  ClientWidth = 291
  Color = clBtnFace
  Constraints.MinHeight = 198
  Constraints.MinWidth = 260
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 186
    Width = 291
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 213
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = #1054#1090#1084#1077#1085#1080#1090#1100
      ModalResult = 2
      TabOrder = 0
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 132
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = #1055#1088#1080#1085#1103#1090#1100
      Default = True
      ModalResult = 1
      TabOrder = 1
    end
  end
  object grdpnlMain: TGridPanel
    Left = 0
    Top = 0
    Width = 291
    Height = 186
    Align = alClient
    BevelOuter = bvNone
    ColumnCollection = <
      item
        Value = 100.000000000000000000
      end>
    ControlCollection = <
      item
        Column = 0
        Control = grpTopLeft
        Row = 0
      end
      item
        Column = 0
        Control = grpBottomRight
        Row = 1
      end>
    RowCollection = <
      item
        Value = 50.000000000000000000
      end
      item
        Value = 50.000000000000000000
      end>
    TabOrder = 1
    object grpTopLeft: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 285
      Height = 87
      Align = alClient
      Caption = #1051#1077#1074#1099#1081' '#1074#1077#1088#1093#1085#1080#1081' '#1091#1075#1086#1083
      TabOrder = 0
    end
    object grpBottomRight: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 96
      Width = 285
      Height = 87
      Align = alClient
      Caption = #1055#1088#1072#1074#1099#1081' '#1085#1080#1078#1085#1080#1081' '#1091#1075#1086#1083
      TabOrder = 1
    end
  end
end
