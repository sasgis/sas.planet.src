object frGPSDeviceCom: TfrGPSDeviceCom
  Left = 0
  Top = 0
  Width = 281
  Height = 160
  Align = alClient
  TabOrder = 0
  object flwpnlComPort: TFlowPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 275
    Height = 24
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object lblPort: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 24
      Height = 13
      Caption = 'Port:'
    end
    object cbbPort: TComboBox
      Left = 30
      Top = 0
      Width = 80
      Height = 21
      TabOrder = 0
      Text = 'COM1'
    end
    object btnAutodetectComPort: TButton
      AlignWithMargins = True
      Left = 113
      Top = 0
      Width = 27
      Height = 21
      Hint = 'Autodetect device COM port'
      Margins.Top = 0
      Caption = '?'
      TabOrder = 1
      OnClick = btnAutodetectComPortClick
    end
  end
  object flwpnlComPortSpeed: TFlowPanel
    AlignWithMargins = True
    Left = 3
    Top = 33
    Width = 275
    Height = 21
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    object lblSpeed: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 34
      Height = 13
      Alignment = taRightJustify
      Caption = 'Speed:'
    end
    object cbbBoudRate: TComboBox
      Left = 40
      Top = 0
      Width = 80
      Height = 21
      Style = csDropDownList
      ItemIndex = 5
      TabOrder = 0
      Text = '4800'
      Items.Strings = (
        '110'
        '300'
        '600'
        '1200'
        '2400'
        '4800'
        '9600'
        '14400'
        '19200'
        '38400'
        '57600'
        '115200'
        '230400')
    end
  end
  object grpAutodetectComPort: TGroupBox
    Left = 0
    Top = 64
    Width = 276
    Height = 83
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Autodetect port on connect'
    TabOrder = 2
    object chkAutodetectAll: TCheckBox
      Left = 8
      Top = 16
      Width = 122
      Height = 17
      Caption = 'All sources'
      TabOrder = 0
      OnClick = OnAutodetectItemClick
    end
    object chkAutodetectBluetooth: TCheckBox
      Left = 8
      Top = 39
      Width = 97
      Height = 17
      Caption = 'Bluetooth'
      TabOrder = 1
      OnClick = OnAutodetectItemClick
    end
    object chkAutodetectUsb: TCheckBox
      Left = 8
      Top = 62
      Width = 97
      Height = 17
      Caption = 'USB'
      TabOrder = 2
      OnClick = OnAutodetectItemClick
    end
    object chkAutodetectCom: TCheckBox
      Left = 136
      Top = 16
      Width = 104
      Height = 17
      Caption = 'COM'
      TabOrder = 3
      OnClick = OnAutodetectItemClick
    end
    object chkAutodetectComVirtual: TCheckBox
      Left = 136
      Top = 39
      Width = 104
      Height = 17
      Caption = 'COM Virtual'
      TabOrder = 4
      OnClick = OnAutodetectItemClick
    end
    object chkAutodetectOthers: TCheckBox
      Left = 136
      Top = 62
      Width = 104
      Height = 17
      Caption = 'Others'
      TabOrder = 5
      OnClick = OnAutodetectItemClick
    end
  end
end
