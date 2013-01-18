object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsSizeToolWin
  Caption = 'Images available'
  ClientHeight = 461
  ClientWidth = 584
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
    Top = 225
    Width = 584
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    OnCanResize = spltDescCanResize
  end
  object gbImageParams: TGroupBox
    Left = 0
    Top = 230
    Width = 584
    Height = 231
    Align = alBottom
    Caption = 'Description:'
    Constraints.MinHeight = 80
    TabOrder = 0
    object veImageParams: TValueListEditor
      Left = 2
      Top = 15
      Width = 580
      Height = 214
      Align = alClient
      DefaultColWidth = 160
      TabOrder = 0
      TitleCaptions.Strings = (
        'Parameter'
        'Value')
      OnDblClick = veImageParamsDblClick
      ColWidths = (
        160
        414)
    end
  end
  object Up: TPanel
    Left = 0
    Top = 0
    Width = 584
    Height = 225
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object gbAvailImages: TGroupBox
      Left = 271
      Top = 0
      Width = 311
      Height = 223
      Align = alCustom
      Anchors = [akLeft, akTop, akRight, akBottom]
      Caption = 'Images available'
      Constraints.MinHeight = 220
      TabOrder = 0
      object tvFound: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 206
        Height = 200
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
      end
      object pnlRight: TPanel
        Left = 214
        Top = 15
        Width = 95
        Height = 206
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
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
          Top = 186
          Width = 89
          Height = 17
          Align = alBottom
          Caption = 'All Images'
          TabOrder = 5
          OnClick = chkALLImagesClick
        end
        object chkSkipExistingPolygons: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 163
          Width = 89
          Height = 17
          Align = alBottom
          Caption = 'Skip existing'
          Checked = True
          State = cbChecked
          TabOrder = 6
          ExplicitTop = 186
        end
      end
    end
    object PnlSearch: TGroupBox
      Left = 2
      Top = 1
      Width = 263
      Height = 223
      Align = alCustom
      Anchors = [akLeft, akTop, akBottom]
      Caption = 'Image services'
      Constraints.MinHeight = 191
      TabOrder = 1
      DesignSize = (
        263
        223)
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
        Top = 201
        Width = 109
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'All Services'
        TabOrder = 1
        OnClick = chkALLServicesClick
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
        Top = 201
        Width = 117
        Height = 18
        Anchors = [akLeft, akBottom]
        Caption = 'Low Resolution too'
        TabOrder = 6
        OnClick = chkLowResolutionTooClick
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
      object chkMNCasColorOnly: TCheckBox
        Left = 128
        Top = 60
        Width = 65
        Height = 17
        Caption = 'Color only'
        TabOrder = 18
      end
    end
  end
end
