object frGPSDeviceNetwork: TfrGPSDeviceNetwork
  Left = 0
  Top = 0
  Width = 321
  Height = 154
  Align = alClient
  TabOrder = 0
  object lblConnectionState: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 126
    Width = 315
    Height = 13
    Align = alTop
  end
  object lblServerAddress: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 315
    Height = 13
    Align = alTop
    Caption = 'Address:'
  end
  object btnServerProbe: TButton
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 315
    Height = 25
    Hint = 'Try to connect and check the server for nmea/gpsd data'
    Align = alTop
    Caption = 'Probe'
    TabOrder = 0
    OnClick = btnServerProbeClick
  end
  object chkAutoReconnect: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 80
    Width = 315
    Height = 17
    Align = alTop
    Caption = 'Auto reconnect'
    TabOrder = 1
    OnClick = OnAutoReconnectClick
  end
  object chkSilentMode: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 103
    Width = 315
    Height = 17
    Hint = 
      'Suppress connection error messages while auto reconnect is enabl' +
      'ed'
    Align = alTop
    Caption = 'Silent mode'
    TabOrder = 2
  end
  object edtServerAddress: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 315
    Height = 21
    Align = alTop
    TabOrder = 3
  end
end
