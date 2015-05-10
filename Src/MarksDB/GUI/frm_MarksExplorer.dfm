object frmMarksExplorer: TfrmMarksExplorer
  Left = 341
  Top = 186
  Caption = 'Placemark Manager'
  ClientHeight = 483
  ClientWidth = 545
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
    Top = 0
    Width = 545
    Height = 408
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 545
      Height = 408
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 0
      object splCatMarks: TSplitter
        Left = 181
        Top = 0
        Height = 408
      end
      object grpMarks: TGroupBox
        Left = 184
        Top = 0
        Width = 361
        Height = 408
        Align = alClient
        Caption = 'Placemarks'
        TabOrder = 0
        object TBXDockMark: TTBXDock
          Left = 2
          Top = 15
          Width = 357
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
          Top = 386
          Width = 357
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
            Left = 353
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
          Width = 351
          Height = 337
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
        Height = 408
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
          Height = 337
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
          Top = 386
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
          object CheckBox3: TCheckBox
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
    Width = 545
    Height = 75
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object rgMarksShowMode: TRadioGroup
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 415
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
      Left = 424
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
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.kml'
    Filter = 
      'All compatible formats (*.kml,*.plt,*.kmz,*.sls,*.hlg,*.gpx,*.cs' +
      'v,*.jpg)|*.kml;*.plt;*.kmz;*.sls;*.hlg;*.gpx;*.csv;*.jpg|Google ' +
      'KML files (*.kml)|*.kml|OziExplorer Track Point File Version 2.1' +
      ' (*.plt)|*.plt|Google KMZ files (*.kmz)|*.kmz|Download session (' +
      '*.sls)|*.sls|Selection (*.hlg)|*.hlg|GPS Exchange files (*.gpx)|' +
      '*.gpx|Universal CSV files (*.csv)|*.csv|JPEG Image whtg GPS Exif' +
      ' (*.jpg)|*.jpg'
    Options = [ofAllowMultiSelect, ofEnableSizing]
    Left = 352
    Top = 144
  end
  object imlStates: TImageList
    Height = 13
    Width = 13
    Left = 312
    Top = 144
    Bitmap = {
      494C01010300900094000D000D00FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
  end
end
