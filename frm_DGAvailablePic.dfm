object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsSizeToolWin
  Caption = 'Images available'
  ClientHeight = 396
  ClientWidth = 256
  Color = clBtnFace
  Constraints.MinHeight = 420
  Constraints.MinWidth = 264
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object gbImageParams: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 237
    Width = 250
    Height = 156
    Align = alBottom
    Caption = 'Description:'
    TabOrder = 0
    ExplicitTop = 239
    object veImageParams: TValueListEditor
      Left = 2
      Top = 15
      Width = 246
      Height = 139
      Align = alClient
      TabOrder = 0
      TitleCaptions.Strings = (
        'Parameter'
        'Value')
      ExplicitTop = 14
      ExplicitHeight = 129
      ColWidths = (
        120
        120)
    end
  end
  object gbAvailImages: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 87
    Width = 250
    Height = 144
    Align = alClient
    Caption = 'Images available'
    TabOrder = 1
    ExplicitLeft = 8
    ExplicitHeight = 240
    object tvFound: TTreeView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 159
      Height = 121
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
      ExplicitLeft = 13
      ExplicitTop = 20
    end
    object pnlRight: TPanel
      Left = 167
      Top = 15
      Width = 81
      Height = 127
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 216
      ExplicitHeight = 170
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
        ExplicitTop = 43
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
        ExplicitTop = 35
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
        ExplicitTop = 97
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
        ExplicitTop = 0
      end
    end
  end
  object gbImagesSource: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 250
    Height = 78
    Align = alTop
    Caption = 'Image services'
    TabOrder = 2
    ExplicitLeft = -2
    DesignSize = (
      250
      78)
    object lbZoom: TLabel
      Left = 186
      Top = 22
      Width = 47
      Height = 13
      Hint = '(zoom %)'
      Caption = '(zoom %)'
      ParentShowHint = False
      ShowHint = False
    end
    object cbDGstacks: TComboBox
      AlignWithMargins = True
      Left = 71
      Top = 44
      Width = 174
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 2
    end
    object chkNMC: TCheckBox
      AlignWithMargins = True
      Left = 71
      Top = 21
      Width = 109
      Height = 17
      Caption = 'Nokia map creator'
      TabOrder = 0
    end
    object chkDG: TCheckBox
      AlignWithMargins = True
      Left = 13
      Top = 44
      Width = 41
      Height = 17
      Caption = 'DG'
      TabOrder = 1
    end
    object chkBing: TCheckBox
      AlignWithMargins = True
      Left = 13
      Top = 21
      Width = 52
      Height = 17
      Caption = 'Bing'
      TabOrder = 3
    end
  end
end
