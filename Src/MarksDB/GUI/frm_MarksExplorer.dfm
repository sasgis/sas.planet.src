object frmMarksExplorer: TfrmMarksExplorer
  Left = 341
  Top = 186
  Caption = 'Placemark Manager'
  ClientHeight = 483
  ClientWidth = 614
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
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
    Top = 23
    Width = 614
    Height = 385
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 614
      Height = 385
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object splCatMarks: TSplitter
        Left = 181
        Top = 0
        Height = 385
      end
      object grpMarks: TGroupBox
        Left = 184
        Top = 0
        Width = 430
        Height = 385
        Align = alClient
        Caption = 'Placemarks'
        TabOrder = 0
        object TBXDockMark: TTBXDock
          Left = 2
          Top = 15
          Width = 426
          Height = 28
          AllowDrag = False
          UseParentBackground = True
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
            object tbitmAllVisible: TTBXItem
              ImageIndex = 55
              OnClick = tbitmAllVisibleClick
              Caption = 'All Visible'
              Hint = 'Set all marks in all categories visible'
            end
          end
        end
        object pnlMarksBottom: TPanel
          Left = 2
          Top = 363
          Width = 426
          Height = 20
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 1
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
            Left = 422
            Top = 0
            Width = 4
            Height = 20
            Align = alRight
            TabOrder = 1
          end
        end
        object MarksListBox: TTreeView
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 420
          Height = 314
          Align = alClient
          DragMode = dmAutomatic
          HideSelection = False
          Indent = 19
          MultiSelect = True
          MultiSelectStyle = [msControlSelect, msShiftSelect]
          PopupMenu = tbpmnMarks
          ReadOnly = True
          ShowRoot = False
          StateImages = imlStates
          TabOrder = 2
          OnContextPopup = MarksListBoxContextPopup
          OnDblClick = MarksListBoxDblClick
          OnKeyDown = MarksListBoxKeyDown
          OnMouseUp = MarksListBoxMouseUp
        end
      end
      object grpCategory: TGroupBox
        AlignWithMargins = True
        Left = 3
        Top = 0
        Width = 178
        Height = 385
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alLeft
        Caption = 'Placemark Categories'
        TabOrder = 1
        object CategoryTreeView: TTreeView
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 168
          Height = 314
          Align = alClient
          DragMode = dmAutomatic
          HideSelection = False
          Indent = 19
          PopupMenu = tbpmnCategories
          ReadOnly = True
          StateImages = imlStates
          TabOrder = 0
          OnChange = CategoryTreeViewChange
          OnContextPopup = CategoryTreeViewContextPopup
          OnDragDrop = CategoryTreeViewDragDrop
          OnDragOver = CategoryTreeViewDragOver
          OnKeyUp = CategoryTreeViewKeyUp
          OnMouseUp = CategoryTreeViewMouseUp
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
              OnClick = BtnAddCategoryClick
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
        object Panel1: TPanel
          Left = 2
          Top = 363
          Width = 174
          Height = 20
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
          object CheckBox2: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 80
            Height = 14
            Align = alLeft
            Caption = 'All'
            TabOrder = 0
            OnClick = CheckBox2Click
          end
          object chkCascade: TCheckBox
            AlignWithMargins = True
            Left = 89
            Top = 3
            Width = 82
            Height = 14
            Align = alClient
            Caption = 'Cascade'
            Checked = True
            State = cbChecked
            TabOrder = 1
          end
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 408
    Width = 614
    Height = 75
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object rgMarksShowMode: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 484
      Height = 69
      Align = alClient
      Ctl3D = True
      ItemIndex = 0
      Items.Strings = (
        'Show only selected placemarks'
        'Show all placemarks'
        'Hide placemarks')
      ParentCtl3D = False
      TabOrder = 0
      OnClick = rgMarksShowModeClick
    end
    object pnlButtons: TPanel
      AlignWithMargins = True
      Left = 493
      Top = 3
      Width = 118
      Height = 69
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 1
      object lblReadOnly: TLabel
        Left = 0
        Top = 54
        Width = 118
        Height = 15
        Align = alClient
        Alignment = taCenter
        AutoSize = False
        Caption = 'Read only mode'
        Color = clActiveCaption
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Layout = tlCenter
      end
      object btnExport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 112
        Height = 21
        Align = alTop
        DropDownCombo = True
        DropDownMenu = PopupExport
        ImageIndex = 0
        TabOrder = 0
        OnClick = btnExportClick
        Caption = 'Export'
      end
      object btnImport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 30
        Width = 112
        Height = 21
        Align = alTop
        ImageIndex = 0
        TabOrder = 1
        OnClick = btnImportClick
        Caption = 'Import'
      end
    end
  end
  object TBXToolbar3: TTBXToolbar
    AlignWithMargins = True
    Left = 3
    Top = 0
    Width = 608
    Height = 22
    Margins.Top = 0
    Margins.Bottom = 1
    Align = alTop
    BorderStyle = bsNone
    CloseButton = False
    FullSize = True
    Images = TBXImageList1
    MenuBar = True
    ProcessShortCuts = True
    ShrinkMode = tbsmWrap
    Stretch = True
    TabOrder = 2
    Caption = 'TBXToolbar3'
    object tbxConfigList: TTBXSubmenuItem
      DisplayMode = nbdmImageAndText
      ImageIndex = 0
      Options = [tboDropdownArrow]
      Stretch = True
      Caption = ''
      Hint = ''
    end
    object tbxSep1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxAdd: TTBXItem
      ImageIndex = 1
      OnClick = tbxAddClick
      Caption = 'Add a database'
      Hint = ''
    end
    object tbxEdit: TTBXItem
      ImageIndex = 3
      OnClick = tbxEditClick
      Caption = 'Edit the database'
      Hint = ''
    end
    object tbxDelete: TTBXItem
      ImageIndex = 2
      OnClick = tbxDeleteClick
      Caption = 'Delete the database'
      Hint = ''
    end
  end
  object imlStates: TImageList
    Height = 13
    Width = 13
    Left = 312
    Top = 144
    Bitmap = {
      494C01010300050008000D000D00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
    OnPopup = tbpmnCategoriesPopup
    Left = 72
    Top = 200
    object tbitmAddCategory: TTBXItem
      ImageIndex = 32
      OnClick = tbitmAddCategoryClick
      Caption = 'Add SubCategory'
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
      ImageIndex = 25
      OnClick = btnExportCategoryClick
      Caption = 'Export Placemarks'
      Hint = 'Export placemarks from selected category'
    end
    object tbxtmCatAddToMergePolygons: TTBXItem
      ImageIndex = 62
      OnClick = tbxtmCatAddToMergePolygonsClick
      Caption = 'Add to Merge Polygons'
      Hint = ''
    end
  end
  object tbpmnMarks: TTBXPopupMenu
    Images = frmMain.MenusImageList
    OnPopup = tbpmnMarksPopup
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
    object tbitmMarkInfo: TTBXItem
      ImageIndex = 27
      OnClick = tbitmMarkInfoClick
      Caption = 'Placemark Info'
      Hint = ''
    end
    object tbitmExportMark: TTBXItem
      ImageIndex = 25
      OnClick = btnSaveMarkClick
      Caption = 'Export Placemark'
      Hint = 'Export selected placemark'
    end
    object tbxtmAddToMergePolygons: TTBXItem
      ImageIndex = 62
      OnClick = tbxtmAddToMergePolygonsClick
      Caption = 'Add to Merge Polygons'
      Hint = ''
    end
    object tbxtmUngroup: TTBXItem
      ImageIndex = 57
      OnClick = tbxtmUngroupClick
      Caption = 'Ungroup'
      Hint = ''
    end
  end
  object TBXImageList1: TTBXImageList
    Left = 56
    Top = 104
    PngDIB = {
      0400000089504E470D0A1A0A0000000D49484452000000100000004008060000
      00137DF796000006F3494441547801ED577B4C535718FFDDDE3E68CBA305055B
      B1164523A2F101229BA2A8208FB50B381F733EE2E6669C3ACDE236FF58222AD3
      B864D3B8CC2DA831C836E3323333D12190B9B9B9F81898A97B74A8DB9C41B154
      E88352A08FBBEF5C2CB40E90FDEFCDF9CE393D3DDF77BFF33D7EE7BBDCA2458B
      D0C7B30A8011033F7F03289752F79FE6F178E4870F1F2E71B95CE8E8E880CDE6
      465B9B0F6A75246263B5888B8BC3DAB54BD6F33C8F3E0530893E9F0F2D2D2DB0
      DBED78D0E2868B0468B55A48653CA263A2D81691820256214465854291565555
      85A0061E4F17385E098D360602E787003F6DEF6E12363095F7EFDF5FB27BF7EE
      926DDBB6956CDCB8C5346C580A264F9E8BC2C26558B3E60DDC6BBC09CBAF17F1
      73FD39ECDB5B5A4D7C7544BD47789CCAC6E414A4676492161A98162CC9DBB876
      F54912703978049A873781141510E851D9E1B4A3B9D50A891C640355CF668EB9
      918E90A6D3E9B6A5A7A79B982656AB155D5D5D50A954D06834E8E8EC82972C6E
      186540427C0212F53ABCB56113F3C247A20D485CDACE9D3B4D393939C8CECEC6
      DC9C426466CD47E6AC5C64CECEC5C2A52B70DBF23BFEBAF62B2C755750B6F783
      6ABFDF5FE7F57AFBB141AB0BF6760FA235513D2A1BC6246352C614B241347217
      14E4AD5DF6F2098944D28F0D382040EE0A703EB2829FC80787CB01AB9DD94040
      74B41A0F1E3C90920070454545E8ECEC4C4B4C4CECD70602C74116A5843E6938
      B4A4555C5C2C72A7CD7B9302ED3DDE6030808C682A2B2BDB4C42C028413F025A
      FD30248E1989A494D19838752236AD5E5775F0838F3FFFF4D027E70E7D78F07B
      E2715298DBB8B163C7422E97BF5A5353F35163636377E83A9CB079EC50C52831
      2C7E2812757A2CCE7BEE6D8BC5528EF0C7236D6868D0900BD552A99412255674
      9D5CA986B44D01994A8A4872252F95C0E9747611AF87A89528AC8D542A951B28
      FEB793902095CA64B252D2AC3422226207C5C30EDAF31A718D220A6B62203D5C
      31D3A8231A4CBB479B2A89C0A7A6A6B29179229B0C594681649E3973A6392B6B
      9679FCF834736AEA747366E61CF39C3985E6FCFC627351D1F3E6AFBE3A769E98
      7E0A0402BD81440B6061EC7038988F69EE87D54A80D21920208982CFEF838497
      40A556B2AD3D24E999F535A18012685DE05852115148B104A3A59E36B0009691
      8C998D22733898302952D63DA4FA5DBB769D9A346992899DADBDBDFDE1517C20
      4F2032321231313120B79FA1FD2298D088500DD2B66EDD2A6664565616E6CDCB
      41F6DC023C352B0FD3B372317D560E8DF3B0FEF52DF9C4984E24B6500DC87094
      342146BC6F73C04DB8A024C379031A3222A0542944C66017AA4170AD77E4182E
      31E351465276323065D4BB016147085DEF9E332F30233222247ED7BA17C5D797
      A16E76C3D29BE31A61197B3B2C0E0634620B19F15AFC75E49AE6A3BCB962C8DD
      21CD804F009F9292024110D8F9CD7BF6ECD93C62C408E8F57A188D4918AA4B44
      DCF0E1381D7716B5AAEFA190C91163D4E0E6D586769FC4BF9CBC9BC665646488
      C04920FACAD1A3470F304025B4610271BFB5158E2E37DEEBD88751E6E4EE6385
      F4DF7E79B65E7AF9F265304CA06C9486FC1732156010F4B87DE226285B21CF57
      E38F8A5FEE7823FC5670A8E769A792DE184B7FA6AF5BB76EBE44221103875DA4
      125E06994286A713A6A338C98CEA7F6A604F6E43DB855697B0D5351B359DEC72
      412C0999C0F29D226E3BE14010137690D0505C28E50BE557B895F2BB92025935
      E3218A7D82078F0B658A14114CD8F8040FC4DB9A6101230AA09EF61840019538
      04280FF1204098C028C89D6AB1FC3F3CB011A85636349C19DAD2529777FE3C89
      C3E0F04077BA12D1B5B5505FBA84A43F6F250FBDFE4B1EEFF32DA73B63707820
      DFFE0E56CEC80A6ADE331EA9A8181C1E38478DC4BE8B3F42CA4B454C68B234DC
      89F07AAD042083C383B839336058BA0063562E41EACB2BB0F9B3E3A5952ED796
      B3C049A64E2C7583C203C28CD05A6102F13DC10332C2C0A1CC70400CE3DE3BF2
      D15C20579298EE36E0D5F6687D906CB361218573A806FFAB3EC8A74462EF0DD5
      40BCCE428BACFEEA83F1CDCD494936DB8926605FA8064C6038F5511F589B9BE0
      E4F97149252545743F6F0FD3209C9B7E3D521F78DD75B8F5B702EFD7D69AF246
      8FDEBB5410CE840A18D0881D5A2BBC8AF358F6EC33983265CA016B4747F962E0
      DAA0EA03A3C18D28EE1B14E4E5E2C4C9EFE076F33F78BC9115AC6AE3A3A2E8B3
      866E64AAFFA71617179BE923822A542B55E70E34D9AC703B2F80B31F47416E0E
      8E1DAF8671EA7A9CACB494935BEB2D54980C581F705D4D507756C16C7A069F1C
      3B85F8F12F423B7C1AD58F7B787A216EDCB82116594A2A1EE3A94650D1752E16
      93F4FD001D7D64F06D57F1FC0B5FE0D3635F2321E525241867807D3BD86C3625
      31C79399C5C2B95F3CA00DC2C26CC96FE38CB223543B94F68507FF02025129B5
      3246FD0D0000000049454E4400000000}
  end
end
