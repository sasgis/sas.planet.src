object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsSizeToolWin
  Caption = 'Images available'
  ClientHeight = 571
  ClientWidth = 280
  Color = clBtnFace
  Constraints.MinHeight = 332
  Constraints.MinWidth = 264
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnCanResize = FormCanResize
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spltDesc: TSplitter
    Left = 0
    Top = 365
    Width = 280
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    OnCanResize = spltDescCanResize
    ExplicitTop = 238
    ExplicitWidth = 270
  end
  object gbImageParams: TGroupBox
    Left = 0
    Top = 368
    Width = 280
    Height = 203
    Align = alBottom
    Caption = 'Description:'
    Constraints.MinHeight = 80
    TabOrder = 0
    object veImageParams: TValueListEditor
      Left = 2
      Top = 15
      Width = 276
      Height = 186
      Align = alClient
      TabOrder = 0
      TitleCaptions.Strings = (
        'Parameter'
        'Value')
      ColWidths = (
        120
        150)
    end
  end
  object gbAvailImages: TGroupBox
    Left = 0
    Top = 161
    Width = 280
    Height = 204
    Align = alClient
    Caption = 'Images available'
    Constraints.MinHeight = 144
    TabOrder = 1
    ExplicitTop = 137
    ExplicitHeight = 228
    object tvFound: TTreeView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 189
      Height = 181
      Align = alClient
      HideSelection = False
      HotTrack = True
      Indent = 19
      ParentShowHint = False
      ShowHint = True
      SortType = stText
      TabOrder = 0
      OnChange = tvFoundChange
      OnClick = tvFoundClick
      OnDeletion = tvFoundDeletion
      OnMouseDown = tvFoundMouseDown
      ExplicitHeight = 205
    end
    object pnlRight: TPanel
      Left = 197
      Top = 15
      Width = 81
      Height = 187
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 211
      object btnUp: TButton
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 75
        Height = 25
        Align = alTop
        Caption = 'Up'
        TabOrder = 1
        OnClick = btnUpClick
      end
      object btnDown: TButton
        AlignWithMargins = True
        Left = 3
        Top = 65
        Width = 75
        Height = 25
        Align = alTop
        Caption = 'Down'
        TabOrder = 2
        OnClick = btnDownClick
      end
      object btnCopy: TButton
        AlignWithMargins = True
        Left = 3
        Top = 96
        Width = 75
        Height = 25
        Hint = 'Copy TID'#39's to clipboard'
        Align = alTop
        Caption = 'Copy'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 3
        OnClick = btnCopyClick
      end
      object btnRefresh: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 75
        Height = 25
        Align = alTop
        Caption = 'Refresh'
        TabOrder = 0
        OnClick = btnRefreshClick
      end
      object btnMakePoly: TButton
        AlignWithMargins = True
        Left = 3
        Top = 127
        Width = 75
        Height = 25
        Hint = 'Make Polygon'
        Align = alTop
        Caption = 'Make Polygon'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 4
        OnClick = btnMakePolyClick
      end
      object chkALLImages: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 158
        Width = 75
        Height = 17
        Align = alTop
        Caption = 'All Images'
        TabOrder = 5
        OnClick = chkALLImagesClick
      end
    end
  end
  object gbImagesSource: TGroupBox
    Left = 0
    Top = 0
    Width = 280
    Height = 161
    Align = alTop
    Caption = 'Image services'
    TabOrder = 2
    ExplicitTop = -6
    DesignSize = (
      280
      161)
    object lbZoom: TLabel
      Left = 198
      Top = 22
      Width = 47
      Height = 13
      Hint = '(zoom %)'
      Caption = '(zoom %)'
      ParentShowHint = False
      ShowHint = False
    end
    object lbNMC: TLabel
      Left = 13
      Top = 71
      Width = 87
      Height = 13
      Caption = 'Nokia map creator'
    end
    object lbNMCZoom: TLabel
      Left = 13
      Top = 90
      Width = 26
      Height = 13
      Caption = 'Zoom'
      ParentShowHint = False
      ShowHint = False
    end
    object cbDGstacks: TComboBox
      AlignWithMargins = True
      Left = 60
      Top = 44
      Width = 212
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 4
    end
    object chkDG: TCheckBox
      AlignWithMargins = True
      Left = 13
      Top = 44
      Width = 41
      Height = 17
      Caption = 'DG'
      TabOrder = 3
    end
    object chkBing: TCheckBox
      AlignWithMargins = True
      Left = 152
      Top = 21
      Width = 44
      Height = 17
      Caption = 'Bing'
      TabOrder = 2
    end
    object chkTerraserver: TCheckBox
      AlignWithMargins = True
      Left = 13
      Top = 21
      Width = 84
      Height = 17
      Caption = 'Terraserver'
      TabOrder = 0
    end
    object chkNMC15: TCheckBox
      Left = 86
      Top = 90
      Width = 36
      Height = 17
      Caption = '15'
      TabOrder = 5
    end
    object chkNMC18: TCheckBox
      Left = 170
      Top = 90
      Width = 36
      Height = 17
      Caption = '18'
      TabOrder = 7
    end
    object chkNMC20: TCheckBox
      Left = 212
      Top = 90
      Width = 36
      Height = 17
      Caption = '20'
      TabOrder = 8
    end
    object chkNMC16: TCheckBox
      Left = 128
      Top = 90
      Width = 36
      Height = 17
      Caption = '16'
      TabOrder = 6
    end
    object chkESRI: TCheckBox
      Left = 98
      Top = 21
      Width = 44
      Height = 17
      Caption = 'ESRI'
      TabOrder = 1
    end
    object chkDG2: TCheckBox
      Left = 13
      Top = 113
      Width = 232
      Height = 17
      Caption = 'DigitalGlobe (Catalogservice)'
      TabOrder = 9
    end
    object chkALLServices: TCheckBox
      Left = 13
      Top = 136
      Width = 109
      Height = 17
      Caption = 'All Services'
      TabOrder = 10
      OnClick = chkALLServicesClick
    end
    object chkLowResolutionToo: TCheckBox
      Left = 128
      Top = 136
      Width = 117
      Height = 17
      Caption = 'Low Resolution too'
      TabOrder = 11
      OnClick = chkLowResolutionTooClick
    end
  end
end
