object frJpegImportOptions: TfrJpegImportOptions
  Left = 0
  Top = 0
  Width = 508
  Height = 224
  Align = alClient
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  ParentFont = False
  TabOrder = 0
  ExplicitWidth = 615
  ExplicitHeight = 321
  object grpPoint: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 119
    Width = 502
    Height = 76
    Align = alTop
    Caption = 'Caption parameters'
    TabOrder = 0
    ExplicitTop = 117
    ExplicitWidth = 609
    object pnlPointParams: TPanel
      Left = 2
      Top = 15
      Width = 498
      Height = 59
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 605
      ExplicitHeight = 67
      object flwpnlPointText: TFlowPanel
        Left = 0
        Top = 0
        Width = 498
        Height = 28
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 605
        object lblPointTextColor: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 48
          Height = 13
          Caption = 'Text color'
        end
        object clrbxPointTextColor: TColorBox
          AlignWithMargins = True
          Left = 57
          Top = 3
          Width = 120
          Height = 22
          Selected = clYellow
          Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object btnPointTextColor: TSpeedButton
          AlignWithMargins = True
          Left = 183
          Top = 3
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = btnPointTextColorClick
        end
        object lblPointTextTransp: TLabel
          AlignWithMargins = True
          Left = 206
          Top = 3
          Width = 51
          Height = 13
          Caption = 'Opacity %'
        end
        object sePointTextTransp: TSpinEdit
          AlignWithMargins = True
          Left = 263
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 100
        end
        object lblPointFontSize: TLabel
          AlignWithMargins = True
          Left = 310
          Top = 3
          Width = 43
          Height = 13
          Caption = 'Font size'
        end
        object sePointFontSize: TSpinEdit
          AlignWithMargins = True
          Left = 359
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 24
          MinValue = 0
          TabOrder = 2
          Value = 11
        end
      end
      object flwpnlPointShadowParams: TFlowPanel
        Left = 0
        Top = 28
        Width = 498
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitWidth = 605
        object lblPointShadowColor: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 64
          Height = 13
          Caption = 'Shadow color'
        end
        object clrbxPointShadowColor: TColorBox
          AlignWithMargins = True
          Left = 73
          Top = 3
          Width = 120
          Height = 22
          Style = [cbStandardColors, cbExtendedColors, cbSystemColors, cbCustomColor, cbPrettyNames]
          ItemHeight = 16
          TabOrder = 0
        end
        object btnPointShadowColor: TSpeedButton
          AlignWithMargins = True
          Left = 199
          Top = 3
          Width = 17
          Height = 22
          Caption = '...'
          OnClick = btnPointShadowColorClick
        end
        object lblPointShadowAlfa: TLabel
          AlignWithMargins = True
          Left = 222
          Top = 3
          Width = 51
          Height = 13
          Caption = 'Opacity %'
        end
        object sePointShadowAlfa: TSpinEdit
          AlignWithMargins = True
          Left = 279
          Top = 3
          Width = 41
          Height = 22
          MaxValue = 100
          MinValue = 0
          TabOrder = 1
          Value = 100
        end
      end
    end
  end
  object pnlCategory: TPanel
    Left = 0
    Top = 0
    Width = 508
    Height = 26
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 615
  end
  object grp1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 29
    Width = 502
    Height = 84
    Align = alTop
    Caption = 'Icon parameters'
    TabOrder = 2
    ExplicitWidth = 609
    object lblPointIconSize: TLabel
      AlignWithMargins = True
      Left = 106
      Top = 38
      Width = 336
      Height = 13
      Caption = 'Icon size:'
    end
    object pnlImage: TPanel
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 63
      Height = 61
      Align = alLeft
      BevelOuter = bvLowered
      TabOrder = 0
      OnResize = pnlImageResize
      ExplicitLeft = 26
      ExplicitHeight = 68
    end
    object sePointIconSize: TSpinEdit
      AlignWithMargins = True
      Left = 106
      Top = 57
      Width = 63
      Height = 22
      MaxValue = 64
      MinValue = 1
      TabOrder = 1
      Value = 64
    end
    object chk1: TCheckBox
      Left = 106
      Top = 15
      Width = 336
      Height = 17
      Caption = 'Use thumbnail as Icon'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
  end
  object ColorDialog1: TColorDialog
    Left = 222
    Top = 1
  end
end
