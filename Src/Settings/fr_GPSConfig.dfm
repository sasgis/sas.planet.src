object frGPSConfig: TfrGPSConfig
  Left = 0
  Top = 0
  Width = 655
  Height = 470
  Align = alClient
  Constraints.MinHeight = 470
  TabOrder = 0
  object pnlGPSLeft: TPanel
    Left = 0
    Top = 0
    Width = 405
    Height = 470
    Align = alClient
    BevelOuter = bvNone
    Constraints.MinHeight = 344
    Constraints.MinWidth = 405
    Padding.Right = 3
    TabOrder = 0
    object pnlGpsPort: TPanel
      Left = 0
      Top = 0
      Width = 402
      Height = 169
      Align = alTop
      BevelEdges = []
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      VerticalAlignment = taAlignTop
      object btnGPSSwitch: TButton
        Left = 3
        Top = 3
        Width = 147
        Height = 32
        Hint = 'Disable or enable GPS'
        Caption = 'GPS On/Off'
        TabOrder = 1
        OnClick = btnGPSSwitchClick
      end
      object rgConnectionType: TRadioGroup
        Left = 0
        Top = 41
        Width = 150
        Height = 113
        Caption = 'GPS type'
        ItemIndex = 0
        Items.Strings = (
          'COM NMEA'
          'USB Garmin'
          'Location API'
          'Replay Track(s)')
        TabOrder = 2
        OnClick = rgConnectionTypeClick
      end
      object pnlComParams: TPanel
        Left = 156
        Top = 7
        Width = 261
        Height = 178
        BevelOuter = bvNone
        BorderWidth = 2
        TabOrder = 0
        object flwpnlComPort: TFlowPanel
          AlignWithMargins = True
          Left = 5
          Top = 5
          Width = 251
          Height = 24
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object Label4: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 25
            Height = 15
            Caption = 'Port:'
          end
          object ComboBoxCOM: TComboBox
            Left = 31
            Top = 0
            Width = 80
            Height = 23
            TabOrder = 0
            Text = 'COM1'
          end
          object btnGPSAutodetectCOM: TButton
            AlignWithMargins = True
            Left = 114
            Top = 0
            Width = 27
            Height = 21
            Hint = 'Autodetect COM port'
            Margins.Top = 0
            Caption = '?'
            TabOrder = 1
            OnClick = btnGPSAutodetectCOMClick
          end
        end
        object flwpnlComPortSpeed: TFlowPanel
          AlignWithMargins = True
          Left = 5
          Top = 35
          Width = 251
          Height = 23
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 1
          object Label65: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 35
            Height = 15
            Alignment = taRightJustify
            Caption = 'Speed:'
          end
          object ComboBoxBoudRate: TComboBox
            Left = 41
            Top = 0
            Width = 80
            Height = 23
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
        object chklstAutodetect: TCheckListBox
          Left = 8
          Top = 86
          Width = 235
          Height = 57
          BorderStyle = bsNone
          Color = clBtnFace
          Columns = 2
          CheckBoxPadding = 4
          IntegralHeight = True
          ItemHeight = 19
          Items.Strings = (
            'All sources'
            'Bluetooth'
            'USB Serial'
            'COM'
            'COM (Virtual)'
            'Others')
          TabOrder = 2
          OnClickCheck = chklstAutodetectClickCheck
        end
        object chkEnableAutodetectComPort: TCheckBox
          Left = 8
          Top = 63
          Width = 235
          Height = 17
          Caption = 'Enable autodetect port on connection'
          TabOrder = 3
          OnClick = chkEnableAutodetectComPortClick
        end
      end
    end
    object flwpnlGpsParams: TFlowPanel
      Left = 0
      Top = 169
      Width = 402
      Height = 166
      Align = alTop
      BevelEdges = []
      BevelOuter = bvNone
      BorderWidth = 3
      FlowStyle = fsTopBottomLeftRight
      TabOrder = 1
      object Label6: TLabel
        Left = 3
        Top = 3
        Width = 114
        Height = 15
        Caption = 'Device timeout, (sec):'
      end
      object SE_ConnectionTimeout: TSpinEdit
        Left = 3
        Top = 18
        Width = 57
        Height = 24
        MaxValue = 86400
        MinValue = 1
        TabOrder = 0
        Value = 1
      end
      object Label11: TLabel
        Left = 3
        Top = 42
        Width = 95
        Height = 15
        Caption = 'Refresh rate, (ms):'
      end
      object SpinEdit1: TSpinEdit
        Left = 3
        Top = 57
        Width = 57
        Height = 24
        MaxValue = 3600000
        MinValue = 100
        TabOrder = 1
        Value = 100
      end
      object Label20: TLabel
        Left = 3
        Top = 81
        Width = 63
        Height = 15
        Caption = 'Track width:'
      end
      object SESizeTrack: TSpinEdit
        Left = 3
        Top = 96
        Width = 57
        Height = 24
        MaxValue = 50
        MinValue = 1
        TabOrder = 2
        Value = 50
      end
      object Label5: TLabel
        Left = 3
        Top = 120
        Width = 182
        Height = 15
        Caption = 'Maximum number of track points:'
      end
      object SE_NumTrackPoints: TSpinEdit
        Left = 3
        Top = 135
        Width = 73
        Height = 24
        MaxValue = 1000000
        MinValue = 10
        TabOrder = 3
        Value = 10000
      end
    end
    object GB_GpsTrackSave: TGroupBox
      Left = 0
      Top = 335
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
        TabOrder = 1
      end
      object CB_GPSlogNmea: TCheckBox
        AlignWithMargins = True
        Left = 132
        Top = 16
        Width = 168
        Height = 17
        Caption = '.nmea/.garmin/.locationapi'
        TabOrder = 2
      end
      object CB_GPSlogGPX: TCheckBox
        AlignWithMargins = True
        Left = 11
        Top = 16
        Width = 51
        Height = 17
        Caption = '.gpx'
        TabOrder = 0
      end
    end
    object pnlGpsSensors: TPanel
      Left = 0
      Top = 377
      Width = 402
      Height = 29
      Align = alTop
      AutoSize = True
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
    Height = 470
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 1
    object GroupBox3: TGroupBox
      Left = 0
      Top = 0
      Width = 250
      Height = 470
      Align = alClient
      Caption = 'Satellites'
      TabOrder = 0
    end
  end
end
