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
      object lblGpsSource: TLabel
        Left = 3
        Top = 44
        Width = 58
        Height = 13
        Caption = 'GPS source:'
      end
      object btnGPSSwitch: TButton
        Left = 3
        Top = 3
        Width = 147
        Height = 32
        Caption = 'GPS On'
        TabOrder = 1
        OnClick = btnGPSSwitchClick
      end
      object cbbGpsType: TComboBox
        Left = 3
        Top = 65
        Width = 144
        Height = 21
        Style = csDropDownList
        ItemIndex = 0
        TabOrder = 2
        Text = 'COM NMEA'
        OnChange = OnGpsTypeChange
        Items.Strings = (
          'COM NMEA'
          'USB Garmin'
          'Location API'
          'Replay Track'
          'TCP NMEA'
          'UDP NMEA')
      end
      object pnlDeviceParams: TPanel
        Left = 156
        Top = 7
        Width = 236
        Height = 172
        Align = alCustom
        Anchors = [akLeft, akTop, akRight, akBottom]
        BevelOuter = bvNone
        TabOrder = 0
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
        object GB_GpsTrackSave: TGroupBox
          AlignWithMargins = True
          Left = 3
          Top = 67
          Width = 379
          Height = 42
          Align = alTop
          Caption = 'Autosave track to:'
          TabOrder = 0
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
          TabOrder = 1
          object Label6: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 98
            Height = 20
            Align = alLeft
            Caption = 'Device timeout, sec:'
          end
          object SE_ConnectionTimeout: TSpinEdit
            Left = 104
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
          TabOrder = 2
          object Label11: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 85
            Height = 20
            Align = alLeft
            Caption = 'Refresh rate, ms:'
          end
          object SpinEdit1: TSpinEdit
            Left = 91
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
        object CBSensorsBarAutoShow: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 118
          Width = 379
          Height = 17
          Margins.Top = 6
          Align = alTop
          Caption = 'Auto show/hide sensors toolbar'
          TabOrder = 3
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
            Top = 49
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
              Width = 173
              Height = 19
              Align = alLeft
              Caption = 'Maximum number of points to show:'
            end
            object SE_NumTrackPoints: TSpinEdit
              Left = 179
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
            Top = 18
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
              Width = 32
              Height = 19
              Align = alLeft
              Caption = 'Width:'
            end
            object SESizeTrack: TSpinEdit
              Left = 38
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
            Top = 49
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
              Width = 59
              Height = 16
              Align = alLeft
              Caption = 'Arrow color:'
            end
            object ColorBoxGPSstr: TColorBox
              Left = 65
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
            Top = 18
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
              Width = 59
              Height = 19
              Align = alLeft
              Caption = 'Pointer size:'
            end
            object SESizeStr: TSpinEdit
              Left = 65
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
            Top = 77
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
              Width = 80
              Height = 19
              Align = alLeft
              Caption = 'Number of rings:'
            end
            object seGPSMarkerRingsCount: TSpinEdit
              Left = 86
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
            Top = 108
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
              Width = 72
              Height = 18
              Align = alLeft
              Caption = 'Ring radius, m:'
            end
            object seGPSMarkerRingRadius: TSpinEdit
              Left = 78
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
