object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Internal Browser'
  ClientHeight = 589
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnClick = OnEngineChange
  OnCreate = FormCreate
  TextHeight = 15
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 689
    Height = 63
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object cbbTestMethod: TComboBox
      Left = 12
      Top = 39
      Width = 162
      Height = 23
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Navigate'
      Items.Strings = (
        'Navigate'
        'NavigateAndWhait'
        'ShowMessage')
    end
    object cbbSource: TComboBox
      Left = 180
      Top = 39
      Width = 415
      Height = 23
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 1
      Text = 'about:blank'
      OnKeyDown = cbbSourceKeyDown
      Items.Strings = (
        'about:blank'
        'file:///../../Test/data/html/test-doc1.html'
        'https://www.7-zip.org/'
        'https://sasgis.github.io/'
        'https://www.microsoft.com/')
    end
    object btnRun: TButton
      Left = 601
      Top = 38
      Width = 85
      Height = 25
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = 'Run'
      TabOrder = 2
      OnClick = btnRunClick
    end
    object rbIE: TRadioButton
      Tag = 1
      Left = 12
      Top = 10
      Width = 120
      Height = 17
      Caption = 'Internet Explorer'
      TabOrder = 3
      OnClick = OnEngineChange
    end
    object rbEdgeSystem: TRadioButton
      Tag = 2
      Left = 264
      Top = 10
      Width = 120
      Height = 17
      Caption = 'Edge (System)'
      TabOrder = 4
      OnClick = OnEngineChange
    end
    object rbEdgePortable: TRadioButton
      Tag = 3
      Left = 138
      Top = 10
      Width = 120
      Height = 17
      Caption = 'Edge (Portable)'
      TabOrder = 5
      OnClick = OnEngineChange
    end
    object lnklblDownloadEdgeRuntime: TLinkLabel
      Left = 496
      Top = 6
      Width = 191
      Height = 17
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = 
        '<a href="https://developer.microsoft.com/en-us/microsoft-edge/we' +
        'bview2/?form=MA13LH#download">Download Edge WebView2 Runtime</a>'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Segoe UI'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnLinkClick = lnklblDownloadEdgeRuntimeLinkClick
    end
  end
  object pnlBrowser: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 66
    Width = 683
    Height = 357
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 429
    Width = 683
    Height = 157
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object mmoLog: TMemo
      Left = 0
      Top = 0
      Width = 683
      Height = 157
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 0
    end
  end
end
