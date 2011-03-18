object FMarksExplorer: TFMarksExplorer
  Left = 341
  Top = 186
  Caption = #1059#1087#1088#1072#1074#1083#1077#1085#1080#1077' '#1084#1077#1090#1082#1072#1084#1080
  ClientHeight = 427
  ClientWidth = 577
  Color = clBtnFace
  Constraints.MinHeight = 309
  Constraints.MinWidth = 406
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnActivate = FormActivate
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMainWithButtons: TPanel
    Left = 0
    Top = 0
    Width = 577
    Height = 358
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlButtons: TPanel
      AlignWithMargins = True
      Left = 497
      Top = 3
      Width = 77
      Height = 352
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      object btnExport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 30
        Width = 71
        Height = 21
        Align = alTop
        DropDownCombo = True
        DropDownMenu = PopupExport
        ImageIndex = 0
        TabOrder = 0
        OnClick = btnExportClick
        Caption = #1069#1082#1089#1087#1086#1088#1090
      end
      object btnAccept: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 57
        Width = 71
        Height = 21
        Align = alTop
        ImageIndex = 0
        TabOrder = 1
        OnClick = btnAcceptClick
        ExplicitLeft = -2
        Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
      end
      object btnOk: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 84
        Width = 71
        Height = 21
        Align = alTop
        ImageIndex = 0
        TabOrder = 2
        OnClick = btnOkClick
        Caption = #1054#1050
      end
      object btnImport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 71
        Height = 21
        Align = alTop
        ImageIndex = 0
        TabOrder = 3
        OnClick = btnImportClick
        Caption = #1048#1084#1087#1086#1088#1090
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 494
      Height = 358
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      object splCatMarks: TSplitter
        Left = 178
        Top = 0
        Height = 358
        ExplicitLeft = 237
        ExplicitTop = 14
        ExplicitHeight = 331
      end
      object grpMarks: TGroupBox
        Left = 181
        Top = 0
        Width = 313
        Height = 358
        Align = alClient
        Caption = ' '#1052#1077#1090#1082#1080' '
        TabOrder = 0
        object MarksListBox: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 303
          Height = 284
          OnClickCheck = MarksListBoxClickCheck
          Align = alClient
          ItemHeight = 13
          Sorted = True
          TabOrder = 0
          OnKeyUp = MarksListBoxKeyUp
        end
        object CheckBox1: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 336
          Width = 303
          Height = 17
          Align = alBottom
          Caption = #1042#1089#1077
          TabOrder = 1
          OnClick = CheckBox1Click
        end
        object TBXDockMark: TTBXDock
          Left = 2
          Top = 15
          Width = 309
          Height = 28
          AllowDrag = False
          UseParentBackground = True
          object TBXToolbar1: TTBXToolbar
            Left = 0
            Top = 0
            Align = alTop
            BorderStyle = bsNone
            CloseButton = False
            Images = Fmain.MenusImageList
            ParentBackground = True
            ParentColor = True
            Stretch = True
            TabOrder = 0
            UseThemeColor = False
            Caption = 'TBXToolbar1'
            object btnEditMark: TTBXItem
              ImageIndex = 31
              OnClick = btnEditMarkClick
              Caption = ''
              Hint = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1091#1102' '#1084#1077#1090#1082#1091
            end
            object btnDelMark: TTBXItem
              ImageIndex = 30
              OnClick = btnDelMarkClick
              Caption = ''
              Hint = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1091#1102' '#1084#1077#1090#1082#1091
            end
            object TBXSeparatorItem1: TTBXSeparatorItem
              Caption = ''
              Hint = ''
            end
            object btnGoToMark: TTBXItem
              ImageIndex = 11
              OnClick = btnGoToMarkClick
              Caption = ''
              Hint = #1055#1077#1088#1077#1081#1090#1080' '#1082' '#1074#1099#1073#1088#1072#1085#1085#1086#1084#1091' '#1086#1073#1098#1077#1082#1090#1091
            end
            object btnOpSelectMark: TTBXItem
              ImageIndex = 10
              OnClick = btnOpSelectMarkClick
              Caption = ''
              Hint = #1054#1087#1077#1088#1072#1094#1080#1080' '#1089' '#1086#1073#1083#1072#1089#1090#1100#1102' '#1074' '#1075#1088#1072#1085#1080#1094#1072#1093' '#1074#1099#1073#1088#1072#1085#1085#1086#1075#1086' '#1086#1073#1098#1077#1082#1090#1072
            end
            object btnNavOnMark: TTBXItem
              AutoCheck = True
              ImageIndex = 33
              OnClick = btnNavOnMarkClick
              Caption = ''
              Hint = #1053#1072#1074#1080#1075#1072#1094#1080#1103' '#1085#1072' '#1074#1099#1073#1088#1072#1085#1085#1091#1102' '#1084#1077#1090#1082#1091
            end
            object TBXSeparatorItem2: TTBXSeparatorItem
              Caption = ''
              Hint = ''
            end
            object btnSaveMark: TTBXItem
              ImageIndex = 25
              OnClick = btnSaveMarkClick
              Caption = ''
              Hint = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1073#1088#1072#1085#1085#1086#1081' '#1084#1077#1090#1082#1080
            end
          end
        end
      end
      object grpCategory: TGroupBox
        Left = 0
        Top = 0
        Width = 178
        Height = 358
        Align = alLeft
        Caption = ' '#1050#1072#1090#1077#1075#1086#1088#1080#1080' '#1084#1077#1090#1086#1082' '
        TabOrder = 1
        object CheckBox2: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 336
          Width = 168
          Height = 17
          Align = alBottom
          Caption = #1042#1089#1077
          TabOrder = 0
          OnClick = CheckBox2Click
        end
        object TreeView1: TTreeView
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 168
          Height = 284
          Align = alClient
          Indent = 19
          ReadOnly = True
          StateImages = imlStates
          TabOrder = 1
          OnChange = TreeView1Change
          OnKeyUp = TreeView1KeyUp
          OnMouseUp = TreeView1MouseUp
        end
        object TBXDockCategory: TTBXDock
          Left = 2
          Top = 15
          Width = 174
          Height = 28
          AllowDrag = False
          UseParentBackground = True
          object TBXToolbar2: TTBXToolbar
            Left = 0
            Top = 0
            Align = alTop
            BorderStyle = bsNone
            CloseButton = False
            Images = Fmain.MenusImageList
            ParentBackground = True
            ParentColor = True
            Stretch = True
            TabOrder = 0
            UseThemeColor = False
            Caption = 'TBXToolbar1'
            object BtnAddCategory: TTBXItem
              ImageIndex = 32
              OnClick = TBXItem4Click
              Caption = ''
              Hint = #1044#1086#1073#1072#1074#1080#1090#1100
            end
            object BtnEditCategory: TTBXItem
              ImageIndex = 31
              OnClick = BtnEditCategoryClick
              Caption = ''
              Hint = #1056#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1090#1100
            end
            object BtnDelKat: TTBXItem
              ImageIndex = 30
              OnClick = BtnDelKatClick
              Caption = ''
              Hint = #1059#1076#1072#1083#1080#1090#1100
            end
            object TBXSeparatorItem3: TTBXSeparatorItem
              Caption = ''
              Hint = ''
            end
            object btnExportCategory: TTBXItem
              ImageIndex = 25
              OnClick = btnExportCategoryClick
              Caption = ''
              Hint = #1069#1082#1089#1087#1086#1088#1090' '#1074#1099#1073#1088#1072#1085#1085#1086#1081' '#1082#1072#1090#1077#1075#1086#1088#1080#1080
            end
          end
        end
      end
    end
  end
  object rgMarksShowMode: TRadioGroup
    Left = 0
    Top = 358
    Width = 577
    Height = 69
    Align = alBottom
    ItemIndex = 0
    Items.Strings = (
      #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1090#1086#1083#1100#1082#1086' '#1091#1082#1072#1079#1072#1085#1085#1099#1077' '#1084#1077#1090#1082#1080
      #1055#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1074#1089#1077' '#1084#1077#1090#1082#1080
      #1053#1077' '#1087#1086#1082#1072#1079#1099#1074#1072#1090#1100' '#1084#1077#1090#1082#1080)
    TabOrder = 1
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.kml'
    Filter = 
      'Google KML files (*.kml)|*.kml|OziExplorer Track Point File Vers' +
      'ion 2.1 (*.plt)|*.plt|Google KMZ files (*.kmz)|*.kmz|'#1060#1072#1081#1083' '#1074#1099#1076#1077#1083#1077 +
      #1085#1080#1103' (*.hlg)|*.hlg'
    Left = 352
    Top = 144
  end
  object imlStates: TImageList
    Height = 13
    Width = 13
    Left = 312
    Top = 144
    Bitmap = {
      494C01010300050004000D000D00FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000340000000D0000000100200000000000900A
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D
      9D00A19D9D00A19D9D00A19D9D00A19D9D000000000000000000A19D9D00A19D
      9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D
      9D00A19D9D000000000000000000A19D9D00A19D9D00A19D9D00A19D9D00A19D
      9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A19D9D00F1F3F300F3F5
      F500F6F7F700F8F9F900F9FAFA00FBFCFC00FDFDFD00FEFEFE0000000000A19D
      9D000000000000000000A19D9D00F1F3F300F3F5F500F6F7F700F8F9F900F9FA
      FA00FBFCFC00FDFDFD00FEFEFE0000000000A19D9D000000000000000000A19D
      9D00EFF1F100F1F3F300F4F5F500F6F7F700F8F9F900FAFBFB00FCFDFD00FEFE
      FE0000000000A19D9D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A19D9D00EFF1F100F1F3F300F3F5F50080808000F8F9F900F9FA
      FA00FBFCFC00FDFDFD00FEFEFE00A19D9D000000000000000000A19D9D00EFF1
      F100F1F3F300F3F5F50021A12100F8F9F900F9FAFA00FBFCFC00FDFDFD00FEFE
      FE00A19D9D000000000000000000A19D9D00ECEFEF00EFF1F100F1F3F300F4F5
      F500F6F7F700F8F9F900FAFBFB00FCFDFD00FEFEFE00A19D9D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A19D9D00ECEFEF00EFF1
      F100808080008080800080808000F8F9F900F9FAFA00FBFCFC00FDFDFD00A19D
      9D000000000000000000A19D9D00ECEFEF00EFF1F10021A1210021A1210021A1
      2100F8F9F900F9FAFA00FBFCFC00FDFDFD00A19D9D000000000000000000A19D
      9D00E9ECEC00ECEFEF00EFF1F100F1F3F300F4F5F500F6F7F700F8F9F900FAFB
      FB00FCFDFD00A19D9D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A19D9D00E9ECEC00808080008080800080808000808080008080
      8000F8F9F900F9FAFA00FBFCFC00A19D9D000000000000000000A19D9D00E9EC
      EC0021A1210021A1210021A1210021A1210021A12100F8F9F900F9FAFA00FBFC
      FC00A19D9D000000000000000000A19D9D00E5E8E800E9ECEC00ECEFEF00EFF1
      F100F1F3F300F4F5F500F6F7F700F8F9F900FAFBFB00A19D9D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A19D9D00E6EAEA008080
      800080808000EFF1F100808080008080800080808000F8F9F900F9FAFA00A19D
      9D000000000000000000A19D9D00E6EAEA0021A1210021A12100EFF1F10021A1
      210021A1210021A12100F8F9F900F9FAFA00A19D9D000000000000000000A19D
      9D00E2E5E500E5E8E800E9ECEC00ECEFEF00EFF1F100F1F3F300F4F5F500F6F7
      F700F8F9F900A19D9D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A19D9D00E3E7E70080808000E9ECEC00ECEFEF00EFF1F1008080
      80008080800080808000F8F9F900A19D9D000000000000000000A19D9D00E3E7
      E70021A12100E9ECEC00ECEFEF00EFF1F10021A1210021A1210021A12100F8F9
      F900A19D9D000000000000000000A19D9D00DEE2E200E2E5E500E5E8E800E9EC
      EC00ECEFEF00EFF1F100F1F3F300F4F5F500F6F7F700A19D9D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A19D9D00E1E5E500E3E7
      E700E6EAEA00E9ECEC00ECEFEF00EFF1F1008080800080808000F6F7F700A19D
      9D000000000000000000A19D9D00E1E5E500E3E7E700E6EAEA00E9ECEC00ECEF
      EF00EFF1F10021A1210021A12100F6F7F700A19D9D000000000000000000A19D
      9D00DBE0E000DEE2E200E2E5E500E5E8E800E9ECEC00ECEFEF00EFF1F100F1F3
      F300F4F5F500A19D9D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A19D9D00DFE3E300E1E5E500E3E7E700E6EAEA00E9ECEC00ECEF
      EF00EFF1F10080808000F3F5F500A19D9D000000000000000000A19D9D00DFE3
      E300E1E5E500E3E7E700E6EAEA00E9ECEC00ECEFEF00EFF1F10021A12100F3F5
      F500A19D9D000000000000000000A19D9D00D9DEDE00DBE0E000DEE2E200E2E5
      E500E5E8E800E9ECEC00ECEFEF00EFF1F100F1F3F300A19D9D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A19D9D00DDE2E200DFE3
      E300E1E5E500E3E7E700E6EAEA00E9ECEC00ECEFEF00EFF1F100F1F3F300A19D
      9D000000000000000000A19D9D00DDE2E200DFE3E300E1E5E500E3E7E700E6EA
      EA00E9ECEC00ECEFEF00EFF1F100F1F3F300A19D9D000000000000000000A19D
      9D00D7DCDC00D9DEDE00DBE0E000DEE2E200E2E5E500E5E8E800E9ECEC00ECEF
      EF00EFF1F100A19D9D0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D
      9D00A19D9D00A19D9D00A19D9D00A19D9D000000000000000000A19D9D00A19D
      9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D
      9D00A19D9D000000000000000000A19D9D00A19D9D00A19D9D00A19D9D00A19D
      9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00A19D9D00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000424D3E000000000000003E00000028000000340000000D00000001000100
      00000000680000000000000000000000000000000000000000000000FFFFFF00
      FFFFFFFFFE000000800C006002000000802C01600A000000800C006002000000
      800C006002000000800C006002000000800C006002000000800C006002000000
      800C006002000000800C006002000000800C006002000000800C006002000000
      FFFFFFFFFE000000}
  end
  object ExportDialog: TSaveDialog
    DefaultExt = '.kmz'
    Filter = 
      #1059#1087#1072#1082#1086#1074#1072#1085#1085#1099#1081' Keyhole Markup Language (kmz)|*.kmz|Keyhole Markup L' +
      'anguage (kml)|*.kml'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 256
    Top = 208
  end
  object PopupExport: TPopupMenu
    Left = 376
    Top = 224
    object NExportAll: TMenuItem
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1074#1089#1077#1093' '#1084#1077#1090#1086#1082' '#1080' '#1082#1072#1090#1077#1075#1086#1088#1080#1081
      OnClick = btnExportClick
    end
    object NExportVisible: TMenuItem
      Tag = 1
      Caption = #1069#1082#1089#1087#1086#1088#1090' '#1090#1086#1083#1100#1082#1086' '#1086#1090#1086#1073#1088#1072#1078#1072#1077#1084#1099#1093
      OnClick = btnExportClick
    end
  end
end
