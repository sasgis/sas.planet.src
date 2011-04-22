object frmGoTo: TfrmGoTo
  Left = 295
  Top = 179
  AlphaBlendValue = 220
  Caption = 'Go to...'
  ClientHeight = 233
  ClientWidth = 331
  Color = clBtnFace
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  PixelsPerInch = 96
  TextHeight = 13
  object grpMarks: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 325
    Height = 45
    Align = alTop
    Caption = ' '
    TabOrder = 0
    ExplicitWidth = 331
    object cbbAllMarks: TComboBox
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 315
      Height = 21
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbAllMarksDropDown
      OnDropDown = cbbAllMarksDropDown
      OnEnter = cbbAllMarksEnter
      ExplicitWidth = 321
    end
  end
  object grpGeoCode: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 54
    Width = 325
    Height = 49
    Align = alTop
    Caption = ' '
    TabOrder = 1
    ExplicitWidth = 331
    object edtGeoCode: TEdit
      AlignWithMargins = True
      Left = 7
      Top = 18
      Width = 311
      Height = 24
      Margins.Left = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Align = alClient
      TabOrder = 0
      OnClick = edtGeoCodeClick
      ExplicitWidth = 317
      ExplicitHeight = 21
    end
  end
  object grpLonLat: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 109
    Width = 325
    Height = 90
    Align = alClient
    Caption = ' '
    TabOrder = 2
    OnEnter = grpLonLatEnter
    ExplicitWidth = 331
    ExplicitHeight = 87
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 202
    Width = 331
    Height = 31
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
    ExplicitTop = 199
    ExplicitWidth = 337
    object lblZoom: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 58
      Height = 20
      Margins.Bottom = 8
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Zoom: x'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object cbbZoom: TComboBox
      AlignWithMargins = True
      Left = 67
      Top = 3
      Width = 41
      Height = 21
      Align = alLeft
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '01'
      Items.Strings = (
        '01'
        '02'
        '03'
        '04'
        '05'
        '06'
        '07'
        '08'
        '09'
        '10'
        '11'
        '12'
        '13'
        '14'
        '15'
        '16'
        '17'
        '18'
        '19'
        '20'
        '21'
        '22'
        '23'
        '24')
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 253
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      ExplicitLeft = 259
    end
    object btnGoTo: TButton
      AlignWithMargins = True
      Left = 172
      Top = 3
      Width = 75
      Height = 25
      Hint = ''
      Align = alRight
      Caption = 'Go to'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnGoToClick
      ExplicitLeft = 178
    end
  end
  object RB3: TRadioButton
    Left = 13
    Top = 1
    Width = 124
    Height = 17
    Caption = 'Stored placemarks'
    TabOrder = 4
  end
  object RB2: TRadioButton
    Left = 13
    Top = 51
    Width = 62
    Height = 17
    Caption = 'Google!'
    Checked = True
    TabOrder = 5
    TabStop = True
  end
  object RB4: TRadioButton
    Left = 72
    Top = 51
    Width = 58
    Height = 17
    Caption = 'Yandex'
    TabOrder = 6
  end
  object RB1: TRadioButton
    Left = 13
    Top = 106
    Width = 85
    Height = 17
    Caption = 'Coordinates'
    TabOrder = 7
  end
end
