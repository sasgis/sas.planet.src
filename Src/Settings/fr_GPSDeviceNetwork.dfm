object frGPSDeviceNetwork: TfrGPSDeviceNetwork
  Left = 0
  Top = 0
  Width = 321
  Height = 111
  Align = alClient
  TabOrder = 0
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
  object edtServerAddress: TEdit
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 315
    Height = 21
    Align = alTop
    TabOrder = 1
  end
end
