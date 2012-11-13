object frGPSConfig: TfrGPSConfig
  Left = 0
  Top = 0
  Width = 655
  Height = 344
  Align = alClient
  TabOrder = 0
  object pnlGPSLeft: TPanel
    Left = 0
    Top = 0
    Width = 405
    Height = 344
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 344
    Constraints.MinWidth = 405
    Padding.Right = 3
    TabOrder = 0
    object flwpnlGpsPort: TFlowPanel
      Left = 0
      Top = 0
      Width = 402
      Height = 75
      Align = alTop
      AutoSize = True
      BevelEdges = [beBottom]
      BevelKind = bkTile
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      object Label4: TLabel
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 49
        Height = 13
        Caption = 'Serial port'
      end
      object ComboBoxCOM: TComboBox
        Left = 58
        Top = 3
        Width = 68
        Height = 21
        ItemHeight = 0
        TabOrder = 0
        Text = 'COM1'
      end
      object btnGPSAutodetectCOM: TButton
        Left = 126
        Top = 3
        Width = 21
        Height = 21
        Hint = 'Autodetect COM port'
        Caption = '?'
        TabOrder = 2
        OnClick = btnGPSAutodetectCOMClick
      end
      object Label65: TLabel
        AlignWithMargins = True
        Left = 150
        Top = 6
        Width = 73
        Height = 13
        Alignment = taRightJustify
        Caption = 'Bits per second'
      end
      object ComboBoxBoudRate: TComboBox
        Left = 226
        Top = 3
        Width = 63
        Height = 21
        ItemHeight = 13
        ItemIndex = 5
        TabOrder = 1
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
          '115200')
      end
      object lbGPSDelimiter1: TLabel
        AlignWithMargins = True
        Left = 292
        Top = 6
        Width = 8
        Height = 13
        Alignment = taRightJustify
        AutoSize = False
      end
      object btnGPSSwitch: TButton
        Left = 303
        Top = 3
        Width = 94
        Height = 21
        Hint = 'Disable or enable GPS'
        Caption = 'GPS On/Off'
        TabOrder = 3
        OnClick = btnGPSSwitchClick
      end
      object CB_GPSAutodetectCOMOnConnect: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 27
        Width = 80
        Height = 17
        Hint = 'Autodetect COM port on connect'
        Caption = 'Autodetect:'
        TabOrder = 4
      end
      object CB_GPSAutodetectCOMSerial: TCheckBox
        AlignWithMargins = True
        Left = 92
        Top = 27
        Width = 48
        Height = 17
        Caption = 'Serial'
        TabOrder = 5
      end
      object CB_GPSAutodetectCOMVirtual: TCheckBox
        AlignWithMargins = True
        Left = 146
        Top = 27
        Width = 50
        Height = 17
        Caption = 'Virtual'
        TabOrder = 6
      end
      object CB_GPSAutodetectCOMBluetooth: TCheckBox
        AlignWithMargins = True
        Left = 202
        Top = 27
        Width = 67
        Height = 17
        Caption = 'Bluetooth'
        TabOrder = 7
      end
      object CB_GPSAutodetectCOMUSBSer: TCheckBox
        AlignWithMargins = True
        Left = 275
        Top = 27
        Width = 56
        Height = 17
        Caption = 'USBSer'
        TabOrder = 8
      end
      object CB_GPSAutodetectCOMOthers: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 50
        Width = 60
        Height = 17
        Caption = 'Others'
        TabOrder = 9
      end
      object CB_USBGarmin: TCheckBox
        AlignWithMargins = True
        Left = 72
        Top = 50
        Width = 82
        Height = 17
        Hint = 'Use Garmin via USB (overrides COM settings if checked)'
        Caption = 'USB Garmin'
        TabOrder = 10
      end
    end
    object flwpnlGpsParams: TFlowPanel
      Left = 0
      Top = 75
      Width = 402
      Height = 194
      Align = alTop
      BevelEdges = []
      BevelKind = bkTile
      BevelOuter = bvNone
      BorderWidth = 3
      FlowStyle = fsTopBottomLeftRight
      TabOrder = 1
      object Label6: TLabel
        Left = 3
        Top = 3
        Width = 98
        Height = 13
        Caption = 'Device timeout (sec)'
      end
      object SE_ConnectionTimeout: TSpinEdit
        Left = 3
        Top = 17
        Width = 57
        Height = 22
        MaxValue = 86400
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
      object Label11: TLabel
        Left = 3
        Top = 40
        Width = 85
        Height = 13
        Caption = 'Refresh rate (ms)'
      end
      object SpinEdit1: TSpinEdit
        Left = 3
        Top = 54
        Width = 57
        Height = 22
        MaxValue = 3600000
        MinValue = 100
        TabOrder = 1
        Value = 100
      end
      object Label20: TLabel
        Left = 3
        Top = 77
        Width = 59
        Height = 13
        Caption = 'Track width:'
      end
      object SESizeTrack: TSpinEdit
        Left = 3
        Top = 91
        Width = 57
        Height = 22
        MaxValue = 50
        MinValue = 1
        TabOrder = 2
        Value = 50
      end
      object Label5: TLabel
        Left = 3
        Top = 114
        Width = 159
        Height = 13
        Caption = 'Maximum number of track points:'
      end
      object SE_NumTrackPoints: TSpinEdit
        Left = 3
        Top = 128
        Width = 73
        Height = 22
        MaxValue = 1000000
        MinValue = 10
        TabOrder = 3
        Value = 10000
      end
    end
    object GB_GpsTrackSave: TGroupBox
      Left = 0
      Top = 269
      Width = 402
      Height = 42
      Align = alTop
      Caption = 'Autosave track to:'
      TabOrder = 2
      object CB_GPSlogPLT: TCheckBox
        AlignWithMargins = True
        Left = 75
        Top = 16
        Width = 51
        Height = 17
        Caption = '.plt'
        TabOrder = 0
      end
      object CB_GPSlogNmea: TCheckBox
        AlignWithMargins = True
        Left = 132
        Top = 16
        Width = 99
        Height = 17
        Caption = '.nmea/.garmin'
        TabOrder = 1
      end
      object CB_GPSlogGPX: TCheckBox
        AlignWithMargins = True
        Left = 11
        Top = 16
        Width = 51
        Height = 17
        Caption = '.gpx'
        TabOrder = 2
      end
    end
    object pnlGpsSensors: TPanel
      Left = 0
      Top = 311
      Width = 402
      Height = 31
      Align = alTop
      AutoSize = True
      BevelEdges = [beBottom]
      BevelKind = bkTile
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 3
      object CBSensorsBarAutoShow: TCheckBox
        AlignWithMargins = True
        Left = 6
        Top = 6
        Width = 390
        Height = 17
        Align = alTop
        Caption = 'Auto show/hide sensors toolbar'
        TabOrder = 0
      end
    end
  end
  object pnlGpsRight: TPanel
    Left = 405
    Top = 0
    Width = 250
    Height = 344
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox3: TGroupBox
      Left = 0
      Top = 0
      Width = 250
      Height = 344
      Align = alClient
      Caption = 'Satellites'
      TabOrder = 0
    end
  end
end
