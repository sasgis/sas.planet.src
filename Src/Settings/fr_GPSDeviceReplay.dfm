object frGPSDeviceReplay: TfrGPSDeviceReplay
  Left = 0
  Top = 0
  Width = 361
  Height = 173
  Align = alClient
  TabOrder = 0
  object lblTrackFile: TLabel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 355
    Height = 13
    Align = alTop
    Caption = 'Track file:'
  end
  object chkUseReplayTrackFile: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 49
    Width = 355
    Height = 17
    Align = alTop
    Caption = 'Get list of tracks from ReplayTrack.txt'
    Checked = True
    State = cbChecked
    TabOrder = 0
    OnClick = OnUseReplayTrackFileClick
  end
  object chkLoopPlayback: TCheckBox
    AlignWithMargins = True
    Left = 3
    Top = 72
    Width = 355
    Height = 17
    Align = alTop
    Caption = 'Play tracks in a loop'
    TabOrder = 1
  end
  object flwpnlPlaybackSpeed: TFlowPanel
    AlignWithMargins = True
    Left = 3
    Top = 95
    Width = 355
    Height = 42
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    object lblPlaybackSpeed: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 47
      Height = 13
      Caption = 'Speed, x:'
    end
    object sePlaybackSpeed: TSpinEdit
      Left = 53
      Top = 0
      Width = 60
      Height = 22
      Hint = 
        'The playback speed of the tracks, x times the real time (1 = rea' +
        'l time)'
      MaxValue = 10000
      MinValue = 1
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      Value = 1
    end
  end
  object pnlTrackFileRow: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 22
    Width = 355
    Height = 21
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 3
    object edtTrackFile: TEdit
      Left = 0
      Top = 0
      Width = 325
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnBrowseTrackFile: TButton
      Left = 325
      Top = 0
      Width = 30
      Height = 21
      Hint = 'Select a track file to replay'
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnBrowseTrackFileClick
    end
  end
end
