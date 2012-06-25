object frmMarksExplorer: TfrmMarksExplorer
  Left = 341
  Top = 186
  Caption = 'Placemark Manager'
  ClientHeight = 408
  ClientWidth = 545
  Color = clBtnFace
  Constraints.MinHeight = 309
  Constraints.MinWidth = 406
  ParentFont = True
  OldCreateOrder = False
  Position = poDefault
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnHide = FormHide
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMainWithButtons: TPanel
    Left = 0
    Top = 0
    Width = 545
    Height = 333
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 577
    ExplicitHeight = 352
    object pnlButtons: TPanel
      AlignWithMargins = True
      Left = 465
      Top = 3
      Width = 77
      Height = 327
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 497
      ExplicitHeight = 346
      object btnExport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 303
        Width = 71
        Height = 21
        Align = alBottom
        DropDownCombo = True
        DropDownMenu = PopupExport
        ImageIndex = 0
        TabOrder = 0
        OnClick = btnExportClick
        ExplicitTop = 322
        Caption = 'Export'
      end
      object btnImport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 276
        Width = 71
        Height = 21
        Align = alBottom
        ImageIndex = 0
        TabOrder = 1
        OnClick = btnImportClick
        ExplicitTop = 295
        Caption = 'Import'
      end
      object btnCancel: TButton
        AlignWithMargins = True
        Left = 3
        Top = 34
        Width = 71
        Height = 25
        Align = alTop
        Cancel = True
        Caption = 'Cancel'
        ModalResult = 2
        TabOrder = 2
      end
      object btnOk: TButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 71
        Height = 25
        Align = alTop
        Caption = 'Ok'
        ModalResult = 1
        TabOrder = 3
      end
      object btnApply: TButton
        AlignWithMargins = True
        Left = 3
        Top = 65
        Width = 71
        Height = 25
        Align = alTop
        Caption = 'Apply'
        TabOrder = 4
        OnClick = btnApplyClick
      end
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 462
      Height = 333
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 494
      ExplicitHeight = 352
      object splCatMarks: TSplitter
        Left = 181
        Top = 0
        Height = 333
        ExplicitLeft = 237
        ExplicitTop = 14
        ExplicitHeight = 331
      end
      object grpMarks: TGroupBox
        Left = 184
        Top = 0
        Width = 278
        Height = 333
        Align = alClient
        Caption = 'Placemarks'
        TabOrder = 0
        ExplicitLeft = 185
        ExplicitTop = 6
        ExplicitWidth = 310
        ExplicitHeight = 352
        object MarksListBox: TCheckListBox
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 268
          Height = 262
          OnClickCheck = MarksListBoxClickCheck
          Align = alClient
          ItemHeight = 13
          PopupMenu = tbpmnMarks
          Sorted = True
          TabOrder = 0
          OnKeyUp = MarksListBoxKeyUp
          ExplicitWidth = 300
          ExplicitHeight = 278
        end
        object TBXDockMark: TTBXDock
          Left = 2
          Top = 15
          Width = 274
          Height = 28
          AllowDrag = False
          UseParentBackground = True
          ExplicitWidth = 306
          object TBXToolbar1: TTBXToolbar
            Left = 0
            Top = 0
            Align = alTop
            BorderStyle = bsNone
            CloseButton = False
            DockPos = 2
            DockRow = 1
            Images = frmMain.MenusImageList
            ParentBackground = True
            ParentColor = True
            Stretch = True
            TabOrder = 0
            UseThemeColor = False
            object btnAddMark: TTBXItem
              ImageIndex = 32
              OnClick = tbitmAddMarkClick
              Caption = ''
              Hint = 'Create new placemark'
            end
            object btnEditMark: TTBXItem
              ImageIndex = 31
              OnClick = btnEditMarkClick
              Caption = ''
              Hint = 'Edit selected placemark'
            end
            object btnDelMark: TTBXItem
              ImageIndex = 30
              OnClick = btnDelMarkClick
              Caption = ''
              Hint = 'Delete selected placemark'
            end
            object TBXSeparatorItem1: TTBXSeparatorItem
              Caption = ''
              Hint = ''
            end
            object btnGoToMark: TTBXItem
              ImageIndex = 11
              OnClick = btnGoToMarkClick
              Caption = ''
              Hint = 'Go to selected object'
            end
            object btnOpSelectMark: TTBXItem
              ImageIndex = 10
              OnClick = btnOpSelectMarkClick
              Caption = ''
              Hint = 'Selection manager'
            end
            object btnNavOnMark: TTBXItem
              AutoCheck = True
              ImageIndex = 33
              OnClick = btnNavOnMarkClick
              Caption = ''
              Hint = 'Navigate to selected placemark'
            end
            object TBXSeparatorItem2: TTBXSeparatorItem
              Caption = ''
              Hint = ''
            end
            object btnSaveMark: TTBXItem
              ImageIndex = 25
              OnClick = btnSaveMarkClick
              Caption = ''
              Hint = 'Export selected placemark'
            end
          end
        end
        object pnlMarksBottom: TPanel
          Left = 2
          Top = 311
          Width = 274
          Height = 20
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          ExplicitTop = 330
          ExplicitWidth = 306
          object CheckBox1: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 94
            Height = 14
            Align = alLeft
            Caption = 'All'
            TabOrder = 0
            OnClick = CheckBox1Click
          end
          object lblMarksCount: TStaticText
            Left = 270
            Top = 0
            Width = 4
            Height = 20
            Align = alRight
            TabOrder = 1
          end
        end
      end
      object grpCategory: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 0
        Width = 178
        Height = 333
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alLeft
        Caption = 'Placemark Categories'
        TabOrder = 1
        ExplicitHeight = 352
        object CheckBox2: TCheckBox
          AlignWithMargins = True
          Left = 5
          Top = 311
          Width = 168
          Height = 17
          Align = alBottom
          Caption = 'All'
          TabOrder = 0
          OnClick = CheckBox2Click
          ExplicitTop = 330
        end
        object CategoryTreeView: TTreeView
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 168
          Height = 259
          Align = alClient
          Indent = 19
          PopupMenu = tbpmnCategories
          ReadOnly = True
          StateImages = imlStates
          TabOrder = 1
          OnChange = CategoryTreeViewChange
          OnKeyUp = CategoryTreeViewKeyUp
          OnMouseUp = CategoryTreeViewMouseUp
          ExplicitHeight = 278
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
            DockPos = 1
            Images = frmMain.MenusImageList
            ParentBackground = True
            ParentColor = True
            Stretch = True
            TabOrder = 0
            UseThemeColor = False
            object BtnAddCategory: TTBXItem
              ImageIndex = 32
              OnClick = tbitmAddCategoryClick
              Caption = ''
              Hint = 'Add'
            end
            object BtnEditCategory: TTBXItem
              ImageIndex = 31
              OnClick = BtnEditCategoryClick
              Caption = ''
              Hint = 'Edit'
            end
            object BtnDelKat: TTBXItem
              ImageIndex = 30
              OnClick = BtnDelKatClick
              Caption = ''
              Hint = 'Delete'
            end
            object TBXSeparatorItem3: TTBXSeparatorItem
              Caption = ''
              Hint = ''
            end
            object btnExportCategory: TTBXItem
              ImageIndex = 25
              OnClick = btnExportCategoryClick
              Caption = ''
              Hint = 'Export placemarks from selected category'
            end
          end
        end
      end
    end
  end
  object rgMarksShowMode: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 336
    Width = 539
    Height = 69
    Align = alBottom
    Ctl3D = True
    ItemIndex = 0
    Items.Strings = (
      'Show only selected placemarks'
      'Show all placemarks'
      'Hide placemarks')
    ParentCtl3D = False
    TabOrder = 1
    ExplicitTop = 355
    ExplicitWidth = 571
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.kml'
    Filter = 
      'All compatible formats (*.kml,*.plt,*.kmz,*.sls,*.hlg,*.gpx)|*.k' +
      'ml;*.plt;*.kmz;*.sls;*.hlg;*.gpx|Google KML files (*.kml)|*.kml|' +
      'OziExplorer Track Point File Version 2.1 (*.plt)|*.plt|Google KM' +
      'Z files (*.kmz)|*.kmz|Download session (*.sls)|*.sls|Selection (' +
      '*.hlg)|*.hlg|GPS Exchange files (*.gpx)|*.gpx'
    Left = 352
    Top = 144
  end
  object imlStates: TImageList
    Height = 13
    Width = 13
    Left = 312
    Top = 144
    Bitmap = {
      494C01010300080024000D000D00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
      FFFFFFFFFE00000000000000000000000000000000000000000000000000}
  end
  object PopupExport: TPopupMenu
    Left = 376
    Top = 224
    object NExportAll: TMenuItem
      Caption = 'Export all placemarks and all categories'
      OnClick = btnExportClick
    end
    object NExportVisible: TMenuItem
      Tag = 1
      Caption = 'Export visible placemarks'
      OnClick = btnExportClick
    end
  end
  object tbpmnCategories: TTBXPopupMenu
    Images = frmMain.MenusImageList
    Left = 72
    Top = 200
    object tbitmAddCategory: TTBXItem
      ImageIndex = 32
      OnClick = tbitmAddCategoryClick
      Caption = 'Add Category'
      Hint = 'Add'
    end
    object tbitmEditCategory: TTBXItem
      ImageIndex = 31
      OnClick = BtnEditCategoryClick
      Caption = 'Edit Category'
      Hint = 'Edit'
    end
    object tbitmDeleteCategory: TTBXItem
      ImageIndex = 30
      OnClick = BtnDelKatClick
      Caption = 'Delete Category'
      Hint = 'Delete'
    end
    object tbsprtCategoriesPopUp: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbitmExportCategory: TTBXItem
      OnClick = btnExportCategoryClick
      Caption = 'Export Placemarks'
      Hint = 'Export placemarks from selected category'
    end
  end
  object tbpmnMarks: TTBXPopupMenu
    Images = frmMain.MenusImageList
    Left = 264
    Top = 256
    object tbitmAddMark: TTBXItem
      ImageIndex = 32
      OnClick = tbitmAddMarkClick
      Caption = 'Add Placemark'
      Hint = 'Add'
    end
    object tbitmEditMark: TTBXItem
      ImageIndex = 31
      OnClick = btnEditMarkClick
      Caption = 'Edit Placemark'
      Hint = 'Edit'
    end
    object tbitmDeleteMark: TTBXItem
      ImageIndex = 30
      OnClick = btnDelMarkClick
      Caption = 'Delete Placemark'
      Hint = 'Delete'
    end
    object tbsprtMarksPopUp: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbitmExportMark: TTBXItem
      OnClick = btnSaveMarkClick
      Caption = 'Export Placemark'
      Hint = 'Export selected placemark'
    end
  end
end
