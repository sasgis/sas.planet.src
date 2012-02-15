object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsDialog
  Caption = 'Images available'
  ClientHeight = 396
  ClientWidth = 305
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 288
    Width = 299
    Height = 105
    Align = alBottom
    Caption = 'Description:'
    TabOrder = 0
    object ValueListEditor1: TValueListEditor
      Left = 2
      Top = 15
      Width = 295
      Height = 88
      Align = alClient
      DisplayOptions = [doColumnTitles, doKeyColFixed]
      TabOrder = 0
      TitleCaptions.Strings = (
        'Parameter'
        'Value')
      ExplicitHeight = 68
      ColWidths = (
        76
        193)
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 95
    Width = 299
    Height = 187
    Align = alClient
    Caption = 'Images available'
    TabOrder = 1
    ExplicitHeight = 180
    object TreeView1: TTreeView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 208
      Height = 164
      Align = alClient
      HideSelection = False
      HotTrack = True
      Indent = 19
      ParentShowHint = False
      ShowHint = True
      SortType = stText
      TabOrder = 0
      OnChange = TreeView1Change
      OnClick = TreeView1Click
      OnDeletion = TreeView1Deletion
      OnMouseDown = TreeView1MouseDown
      ExplicitHeight = 157
    end
    object pnlRight: TPanel
      Left = 216
      Top = 15
      Width = 81
      Height = 170
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 163
      object Button1: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 75
        Height = 25
        Align = alTop
        Caption = 'Up'
        TabOrder = 0
        OnClick = Button1Click
      end
      object Button2: TButton
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 75
        Height = 25
        Align = alTop
        Caption = 'Down'
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        AlignWithMargins = True
        Left = 3
        Top = 65
        Width = 75
        Height = 25
        Hint = 'Copy TID'#39's to clipboard'
        Align = alTop
        Caption = 'Copy'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 2
        OnClick = Button3Click
      end
    end
  end
  object GroupBox4: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 299
    Height = 86
    Align = alTop
    Caption = 'Image services'
    TabOrder = 2
    object cbDGstacks: TComboBox
      Left = 71
      Top = 52
      Width = 222
      Height = 21
      Style = csDropDownList
      ItemHeight = 0
      TabOrder = 3
    end
    object chkBing: TCheckBox
      Left = 13
      Top = 21
      Width = 52
      Height = 17
      Caption = 'Bing'
      TabOrder = 0
    end
    object chkNMC: TCheckBox
      Left = 71
      Top = 21
      Width = 109
      Height = 17
      Caption = 'Nokia map creator'
      TabOrder = 1
    end
    object chkDG: TCheckBox
      Left = 13
      Top = 54
      Width = 41
      Height = 17
      Caption = 'DG'
      TabOrder = 2
    end
    object btnRefresh: TButton
      Left = 216
      Top = 21
      Width = 75
      Height = 25
      Caption = 'Refresh'
      TabOrder = 4
      OnClick = btnRefreshClick
    end
  end
end
