object FDGAvailablePic: TFDGAvailablePic
  Left = 730
  Top = 247
  BorderStyle = bsDialog
  Caption = #1048#1085#1092#1086#1088#1084#1072#1094#1080#1103' '#1086' '#1076#1086#1089#1090#1091#1087#1085#1099#1093' '#1089#1085#1080#1084#1082#1072#1093' '#1085#1072' DigitalGlobe'
  ClientHeight = 369
  ClientWidth = 355
  Color = clBtnFace
  ParentFont = True
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
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
    Caption = #1054#1087#1080#1089#1072#1085#1080#1077
    TabOrder = 0
    object LabelDate: TLabel
      Left = 176
      Top = 16
      Width = 65
      Height = 13
      Caption = #1044#1072#1090#1072' '#1089#1098#1077#1084#1082#1080
    end
    object LabelResolution: TLabel
      Left = 176
      Top = 32
      Width = 61
      Height = 13
      Caption = #1056#1072#1079#1088#1077#1096#1077#1085#1080#1077
    end
    object LabelColor: TLabel
      Left = 176
      Top = 48
      Width = 26
      Height = 13
      Caption = #1062#1074#1077#1090
    end
    object LabelProv: TLabel
      Left = 176
      Top = 64
      Width = 48
      Height = 13
      Caption = #1048#1089#1090#1086#1095#1085#1080#1082
    end
    object Label2: TLabel
      Left = 100
      Top = 16
      Width = 69
      Height = 13
      Alignment = taRightJustify
      Caption = #1044#1072#1090#1072' '#1089#1098#1077#1084#1082#1080':'
    end
    object Label4: TLabel
      Left = 104
      Top = 32
      Width = 65
      Height = 13
      Alignment = taRightJustify
      Caption = #1056#1072#1079#1088#1077#1096#1077#1085#1080#1077':'
    end
    object Label5: TLabel
      Left = 139
      Top = 48
      Width = 30
      Height = 13
      Alignment = taRightJustify
      Caption = #1062#1074#1077#1090':'
    end
    object Label6: TLabel
      Left = 117
      Top = 64
      Width = 52
      Height = 13
      Alignment = taRightJustify
      Caption = #1048#1089#1090#1086#1095#1085#1080#1082':'
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 58
    Width = 349
    Height = 217
    Align = alClient
    Caption = #1044#1086#1089#1090#1091#1087#1085#1099#1077' '#1089#1085#1080#1084#1082#1080
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
        Caption = #1042#1099#1096#1077
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
        Caption = #1053#1080#1078#1077
        TabOrder = 1
        OnClick = Button2Click
      end
      object Button3: TButton
        AlignWithMargins = True
        Left = 3
        Top = 65
        Width = 75
        Height = 25
        Hint = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077' tid '#1076#1083#1103' '#1074#1089#1090#1072#1074#1082#1080' '#1074' URL'
        Align = alTop
        Caption = #1050#1086#1087#1080#1088#1086#1074#1072#1090#1100
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
    Caption = #1042#1099#1073#1086#1088' '#1089#1090#1077#1082#1072' '#1089#1085#1080#1084#1082#1086#1074
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
