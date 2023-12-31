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
    object pnlGpsConfig: TPanel
      Left = 0
      Top = 0
      Width = 402
      Height = 185
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
          'Replay Track')
        TabOrder = 2
        OnClick = rgConnectionTypeClick
      end
      object pnlComParams: TPanel
        Left = 156
        Top = 7
        Width = 261
        Height = 162
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
        object grpAutodetectComPort: TGroupBox
          Left = 0
          Top = 64
          Width = 243
          Height = 83
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
      object chkUseReplayTrackFile: TCheckBox
        Left = 6
        Top = 160
        Width = 393
        Height = 17
        Align = alCustom
        Caption = 'Get list of tracks to replay from ReplayTrack.txt'
        Checked = True
        Enabled = False
        State = cbChecked
        TabOrder = 3
      end
    end
    object flwpnlGpsParams: TFlowPanel
      Left = 0
      Top = 185
      Width = 402
      Height = 166
      Align = alTop
      BevelEdges = []
      BevelOuter = bvNone
      BorderWidth = 3
      FlowStyle = fsTopBottomLeftRight
      TabOrder = 1
    end
    object pgcGps: TPageControl
      Left = 6
      Top = 183
      Width = 393
      Height = 287
      ActivePage = tsCommon
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 2
      object tsCommon: TTabSheet
        Caption = 'Common'
        object CBSensorsBarAutoShow: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 118
          Width = 379
          Height = 17
          Margins.Top = 6
          Align = alTop
          Caption = 'Auto show/hide sensors toolbar'
          TabOrder = 0
        end
        object GB_GpsTrackSave: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 67
          Width = 379
          Height = 42
          Align = alTop
          Caption = 'Autosave track to:'
          TabOrder = 1
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
        object pnlConnectionTimeout: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 3
          Width = 385
          Height = 26
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 2
          object Label6: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 106
            Height = 23
            Align = alLeft
            Caption = 'Device timeout, sec:'
          end
          object SE_ConnectionTimeout: TSpinEdit
            Left = 112
            Top = 0
            Width = 57
            Height = 26
            Align = alLeft
            MaxValue = 86400
            MinValue = 1
            TabOrder = 0
            Value = 1
          end
        end
        object pnlRefreshRate: TPanel
          AlignWithMargins = True
          Left = 0
          Top = 35
          Width = 385
          Height = 26
          Margins.Left = 0
          Margins.Right = 0
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 3
          object Label11: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 87
            Height = 23
            Align = alLeft
            Caption = 'Refresh rate, ms:'
          end
          object SpinEdit1: TSpinEdit
            Left = 93
            Top = 0
            Width = 57
            Height = 26
            Align = alLeft
            MaxValue = 3600000
            MinValue = 100
            TabOrder = 0
            Value = 100
          end
        end
      end
      object tsTrackAndMarker: TTabSheet
        Caption = 'Marker and Track'
        ImageIndex = 1
        object grpGpsTrack: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 154
          Width = 379
          Height = 81
          Align = alTop
          Caption = 'Track'
          TabOrder = 0
          object pnlTrackPoints: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 51
            Width = 369
            Height = 25
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            object Label5: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 198
              Height = 19
              Margins.Bottom = 3
              Align = alLeft
              Caption = 'Maximum number of points to show:'
            end
            object SE_NumTrackPoints: TSpinEdit
              Left = 204
              Top = 0
              Width = 73
              Height = 25
              Align = alLeft
              MaxValue = 1000000
              MinValue = 10
              TabOrder = 0
              Value = 10000
            end
          end
          object pnlTrackWidth: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 20
            Width = 369
            Height = 25
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 1
            object Label20: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 35
              Height = 19
              Margins.Bottom = 3
              Align = alLeft
              Caption = 'Width:'
            end
            object SESizeTrack: TSpinEdit
              Left = 41
              Top = 0
              Width = 57
              Height = 25
              Align = alLeft
              MaxValue = 50
              MinValue = 1
              TabOrder = 0
              Value = 50
            end
          end
        end
        object grpGpsMarker: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 379
          Height = 145
          Align = alTop
          Caption = 'Marker'
          TabOrder = 1
          object pnlMarkerColor: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 51
            Width = 369
            Height = 22
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 0
            object lblGPSMarkerColor: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 65
              Height = 19
              Align = alLeft
              Caption = 'Arrow color:'
            end
            object ColorBoxGPSstr: TColorBox
              Left = 71
              Top = 0
              Width = 105
              Height = 22
              Align = alLeft
              Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
              TabOrder = 0
            end
          end
          object pnlMarkerPointerSize: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 20
            Width = 369
            Height = 25
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 1
            object lblGPSMarkerSize: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 63
              Height = 22
              Align = alLeft
              Caption = 'Pointer size:'
            end
            object SESizeStr: TSpinEdit
              Left = 69
              Top = 0
              Width = 57
              Height = 25
              Align = alLeft
              MaxValue = 150
              MinValue = 10
              TabOrder = 0
              Value = 100
            end
          end
          object pnlMarker2: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 79
            Width = 369
            Height = 25
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 2
            object lblGPSMarkerRingsCount: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 90
              Height = 22
              Align = alLeft
              Caption = 'Number of rings:'
            end
            object seGPSMarkerRingsCount: TSpinEdit
              Left = 96
              Top = 0
              Width = 105
              Height = 25
              Align = alLeft
              MaxValue = 20
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
          end
          object pnlMarker3: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 110
            Width = 369
            Height = 24
            Align = alTop
            AutoSize = True
            BevelOuter = bvNone
            TabOrder = 3
            object lblGPSMarkerRingRadius: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 79
              Height = 21
              Align = alLeft
              Caption = 'Ring radius, m:'
            end
            object seGPSMarkerRingRadius: TSpinEdit
              Left = 85
              Top = 0
              Width = 105
              Height = 24
              Align = alLeft
              MaxValue = 20000
              MinValue = 0
              TabOrder = 0
              Value = 0
            end
          end
        end
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
