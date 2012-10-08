object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsSizeToolWin
  Caption = 'Images available'
  ClientHeight = 416
  ClientWidth = 512
  Color = clBtnFace
  Constraints.MinHeight = 410
  Constraints.MinWidth = 500
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
    Top = 227
    Width = 512
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    OnCanResize = spltDescCanResize
    ExplicitTop = 198
    ExplicitWidth = 492
  end
  object gbImageParams: TGroupBox
    Left = 0
    Top = 232
    Width = 512
    Height = 184
    Align = alBottom
    Caption = 'Description:'
    Constraints.MinHeight = 80
    TabOrder = 0
    ExplicitTop = 202
    ExplicitWidth = 492
    object veImageParams: TValueListEditor
      Left = 2
      Top = 15
      Width = 508
      Height = 167
      Align = alClient
      DefaultColWidth = 160
      TabOrder = 0
      TitleCaptions.Strings = (
        'Parameter'
        'Value')
      ExplicitTop = 14
      ColWidths = (
        160
        342)
    end
  end
  object Up: TPanel
    Left = 0
    Top = 0
    Width = 512
    Height = 227
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 492
    ExplicitHeight = 197
    object gbAvailImages: TGroupBox
      Left = 271
      Top = 0
      Width = 239
      Height = 225
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Images available'
      Constraints.MinHeight = 191
      TabOrder = 0
      ExplicitWidth = 219
      ExplicitHeight = 195
      object tvFound: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 134
        Height = 202
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
        ExplicitWidth = 114
        ExplicitHeight = 172
      end
      object pnlRight: TPanel
        Left = 142
        Top = 15
        Width = 95
        Height = 208
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 122
        ExplicitHeight = 178
        object btnUp: TButton
          AlignWithMargins = True
          Left = 3
          Top = 34
          Width = 89
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
          Width = 89
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
          Width = 89
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
          Width = 89
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
          Width = 89
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
          Top = 188
          Width = 89
          Height = 17
          Align = alBottom
          Caption = 'All Images'
          TabOrder = 5
          OnClick = chkALLImagesClick
          ExplicitTop = 158
        end
      end
    end
    object PnlSearch: TGroupBox
      Left = 2
      Top = 1
      Width = 263
      Height = 225
      Align = alCustom
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Image services'
      Constraints.MinHeight = 191
      TabOrder = 1
      ExplicitHeight = 195
      DesignSize = (
        263
        225)
      object lbNMC: TLabel
        Left = 8
        Top = 60
        Width = 87
        Height = 13
        Caption = 'Nokia map creator'
      end
      object lbNMCZoom: TLabel
        Left = 8
        Top = 79
        Width = 26
        Height = 13
        Caption = 'Zoom'
        ParentShowHint = False
        ShowHint = False
      end
      object lbZoom: TLabel
        Left = 186
        Top = 15
        Width = 47
        Height = 13
        Hint = '(zoom %)'
        Caption = '(zoom %)'
        ParentShowHint = False
        ShowHint = False
      end
      object LabelDatadoors: TLabel
        Left = 8
        Top = 147
        Width = 97
        Height = 13
        Caption = 'www.datadoors.net'
      end
      object cbDGstacks: TComboBox
        AlignWithMargins = True
        Left = 55
        Top = 33
        Width = 198
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 0
      end
      object chkALLServices: TCheckBox
        Left = 8
        Top = 203
        Width = 109
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'All Services'
        TabOrder = 1
        OnClick = chkALLServicesClick
        ExplicitTop = 220
      end
      object chkBing: TCheckBox
        AlignWithMargins = True
        Left = 136
        Top = 14
        Width = 44
        Height = 17
        Caption = 'Bing'
        TabOrder = 2
      end
      object chkDG: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 37
        Width = 41
        Height = 17
        Caption = 'DG'
        TabOrder = 3
      end
      object chkDG2: TCheckBox
        Left = 8
        Top = 101
        Width = 232
        Height = 17
        Caption = 'DigitalGlobe (Catalogservice)'
        TabOrder = 4
      end
      object chkESRI: TCheckBox
        Left = 86
        Top = 14
        Width = 44
        Height = 17
        Caption = 'ESRI'
        TabOrder = 5
      end
      object chkLowResolutionToo: TCheckBox
        Left = 143
        Top = 203
        Width = 117
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Low Resolution too'
        TabOrder = 6
        OnClick = chkLowResolutionTooClick
        ExplicitTop = 173
      end
      object chkNMC15: TCheckBox
        Left = 86
        Top = 78
        Width = 36
        Height = 17
        Caption = '15'
        TabOrder = 7
      end
      object chkNMC16: TCheckBox
        Left = 128
        Top = 78
        Width = 36
        Height = 17
        Caption = '16'
        TabOrder = 8
      end
      object chkNMC18: TCheckBox
        Left = 170
        Top = 78
        Width = 36
        Height = 17
        Caption = '18'
        TabOrder = 9
      end
      object chkNMC20: TCheckBox
        Left = 212
        Top = 78
        Width = 36
        Height = 17
        Caption = '20'
        TabOrder = 10
      end
      object chkTerraserver: TCheckBox
        AlignWithMargins = True
        Left = 8
        Top = 14
        Width = 72
        Height = 17
        Caption = 'Terraserver'
        TabOrder = 11
      end
      object ChkDD2: TCheckBox
        Left = 15
        Top = 177
        Width = 85
        Height = 17
        Caption = 'WorldView-2'
        TabOrder = 12
      end
      object ChkDD1: TCheckBox
        Left = 15
        Top = 162
        Width = 87
        Height = 17
        Caption = 'WorldView-1'
        TabOrder = 13
      end
      object ChkDD3: TCheckBox
        Left = 110
        Top = 162
        Width = 62
        Height = 17
        Caption = 'QuickBird'
        TabOrder = 14
      end
      object ChkDD4: TCheckBox
        Left = 110
        Top = 177
        Width = 63
        Height = 17
        Caption = 'GeoEye'
        TabOrder = 15
      end
      object ChkDD5: TCheckBox
        Left = 185
        Top = 162
        Width = 58
        Height = 17
        Caption = 'IKONOS'
        TabOrder = 16
      end
      object chkGeoFuse: TCheckBox
        Left = 8
        Top = 124
        Width = 232
        Height = 17
        Caption = 'GeoFuse.GeoEye (GeoEye+IKONOS)'
        TabOrder = 17
      end
    end
  end
end
