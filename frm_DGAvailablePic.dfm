object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsDialog
  Caption = 'DigitalGlobe Availability'
  ClientHeight = 369
  ClientWidth = 355
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 281
    Width = 349
    Height = 85
    Align = alBottom
    Caption = 'Description:'
    TabOrder = 0
    object LabelDate: TLabel
      Left = 176
      Top = 16
      Width = 65
      Height = 13
      Caption = 'Date'
    end
    object LabelResolution: TLabel
      Left = 176
      Top = 32
      Width = 61
      Height = 13
      Caption = 'Quality'
    end
    object LabelColor: TLabel
      Left = 176
      Top = 48
      Width = 26
      Height = 13
      Caption = 'Color'
    end
    object LabelProv: TLabel
      Left = 176
      Top = 64
      Width = 48
      Height = 13
      Caption = 'Source'
    end
    object Label2: TLabel
      Left = 100
      Top = 16
      Width = 69
      Height = 13
      Alignment = taRightJustify
      Caption = 'Date:'
    end
    object Label4: TLabel
      Left = 104
      Top = 32
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Caption = 'Quality:'
    end
    object Label5: TLabel
      Left = 139
      Top = 48
      Width = 30
      Height = 13
      Alignment = taRightJustify
      Caption = 'Color:'
    end
    object Label6: TLabel
      Left = 117
      Top = 64
      Width = 52
      Height = 13
      Alignment = taRightJustify
      Caption = 'Source:'
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 58
    Width = 349
    Height = 217
    Align = alClient
    Caption = 'Images available'
    TabOrder = 1
    object TreeView1: TTreeView
      AlignWithMargins = True
      Left = 5
      Top = 18
      Width = 258
      Height = 194
      Align = alClient
      HideSelection = False
      HotTrack = True
      Indent = 19
      ParentShowHint = False
      ShowHint = True
      SortType = stText
      TabOrder = 0
      OnMouseDown = TreeView1MouseDown
    end
    object pnlRight: TPanel
      Left = 266
      Top = 15
      Width = 81
      Height = 200
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
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
        Hint = 'Copy TID''s to clipboard'
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
    Width = 349
    Height = 49
    Align = alTop
    Caption = 'Image stack'
    TabOrder = 2
    object ComboBox2: TComboBox
      Left = 8
      Top = 16
      Width = 329
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = ComboBox2Change
    end
  end
end
