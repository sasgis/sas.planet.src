object frmMarksExplorer: TfrmMarksExplorer
  Left = 341
  Top = 186
  Caption = 'Placemark Manager'
  ClientHeight = 483
  ClientWidth = 614
  Color = clBtnFace
  ParentFont = True
  KeyPreview = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poDefault
  ShowHint = True
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyPress = FormKeyPress
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
    TabOrder = 1
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
        TabOrder = 1
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
            Stretch = True
            TabOrder = 0
            object btnAddMark: TTBXItem
              Hint = 'Create new placemark'
              ImageIndex = 32
              OnClick = tbitmAddMarkClick
            end
            object btnEditMark: TTBXItem
              Hint = 'Edit selected placemark'
              ImageIndex = 31
              ShortCut = 113
              OnClick = btnEditMarkClick
            end
            object btnDelMark: TTBXItem
              Hint = 'Delete selected placemark'
              ImageIndex = 30
              ShortCut = 46
              OnClick = btnDelMarkClick
            end
            object TBXSeparatorItem1: TTBXSeparatorItem
            end
            object btnGoToMark: TTBXItem
              Hint = 'Go to selected object'
              ImageIndex = 11
              OnClick = btnGoToMarkClick
            end
            object btnOpSelectMark: TTBXItem
              Hint = 'Selection manager'
              ImageIndex = 10
              OnClick = btnOpSelectMarkClick
            end
            object btnNavOnMark: TTBXItem
              AutoCheck = True
              Hint = 'Navigate to selected placemark'
              ImageIndex = 33
              OnClick = btnNavOnMarkClick
            end
            object TBXSeparatorItem2: TTBXSeparatorItem
            end
            object btnSaveMark: TTBXItem
              Hint = 'Export selected placemarks'
              ImageIndex = 25
              OnClick = btnSaveMarkClick
            end
            object TBXSeparatorItem4: TTBXSeparatorItem
            end
            object tbitmAllVisible: TTBXItem
              Hint = 'Set all marks in all categories visible'
              ImageIndex = 55
              OnClick = tbitmAllVisibleClick
            end
            object TBXSeparatorItem8: TTBXSeparatorItem
            end
            object tbitmFilter: TTBXItem
              Hint = 'Filter placemarks'
              ImageIndex = 80
              OnClick = tbitmFilterClick
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
          TabOrder = 2
          object chkSetAllMarksInCategoryVisible: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 94
            Height = 14
            Align = alLeft
            Caption = 'All'
            TabOrder = 1
            OnClick = chkSetAllMarksInCategoryVisibleClick
          end
          object lblMarksCount: TStaticText
            Left = 422
            Top = 0
            Width = 4
            Height = 20
            Align = alRight
            TabOrder = 0
          end
        end
        object pnlMarksList: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 420
          Height = 314
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
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
        TabOrder = 0
        object pnlCategoriesTree: TPanel
          AlignWithMargins = True
          Left = 5
          Top = 46
          Width = 168
          Height = 314
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 1
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
            Stretch = True
            TabOrder = 0
            object btnAddCategory: TTBXItem
              Hint = 'Add new category'
              ImageIndex = 32
              OnClick = btnAddCategoryClick
            end
            object btnEditCategory: TTBXItem
              Hint = 'Edit selected category'
              ImageIndex = 31
              ShortCut = 113
              OnClick = btnEditCategoryClick
            end
            object btnDeleteCategory: TTBXItem
              Hint = 'Delete selected category'
              ImageIndex = 30
              ShortCut = 46
              OnClick = btnDeleteCategoryClick
            end
            object TBXSeparatorItem3: TTBXSeparatorItem
            end
            object btnExportCategory: TTBXItem
              Hint = 'Export placemarks from selected category'
              ImageIndex = 25
              OnClick = btnExportCategoryClick
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
          object chkSetAllCategoriesVisible: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 80
            Height = 14
            Align = alLeft
            Caption = 'All'
            TabOrder = 0
            OnClick = chkSetAllCategoriesVisibleClick
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
            OnClick = chkCascadeClick
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
    TabOrder = 2
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
        Caption = 'Export'
        DropDownCombo = True
        DropDownMenu = PopupExport
        TabOrder = 0
        OnClick = btnExportClick
      end
      object btnImport: TTBXButton
        AlignWithMargins = True
        Left = 3
        Top = 30
        Width = 112
        Height = 21
        Align = alTop
        Caption = 'Import'
        TabOrder = 1
        OnClick = btnImportClick
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
    ItemTransparency = itEnable
    ProcessShortCuts = True
    Stretch = True
    TabOrder = 0
    object tbxConfigList: TTBXSubmenuItem
      DisplayMode = nbdmImageAndText
      ImageIndex = 0
      Options = [tboDropdownArrow]
      Stretch = True
    end
    object tbxSep1: TTBXSeparatorItem
    end
    object tbxAdd: TTBXItem
      Caption = 'Add a database'
      ImageIndex = 1
      OnClick = tbxAddClick
    end
    object tbxEdit: TTBXItem
      Caption = 'Edit the database'
      ImageIndex = 3
      OnClick = tbxEditClick
    end
    object tbxDelete: TTBXItem
      Caption = 'Delete the database'
      ImageIndex = 2
      OnClick = tbxDeleteClick
    end
    object tbxSep2: TTBXSeparatorItem
    end
    object tbxWarning: TTBXItem
      Caption = 'Check warnings'
      ImageIndex = 4
      OnClick = tbxWarningClick
    end
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
    object NExportSelected: TMenuItem
      Caption = 'Export selected placemarks'
      OnClick = btnSaveMarkClick
    end
  end
  object tbpmnCategories: TTBXPopupMenu
    Images = frmMain.MenusImageList
    OnPopup = tbpmnCategoriesPopup
    Left = 72
    Top = 200
    object tbitmAddCategory: TTBXItem
      Caption = 'Add SubCategory'
      Hint = 'Add'
      ImageIndex = 32
      OnClick = tbitmAddCategoryClick
    end
    object tbitmEditCategory: TTBXItem
      Caption = 'Edit Category'
      Hint = 'Edit'
      ImageIndex = 31
      ShortCut = 113
      OnClick = btnEditCategoryClick
    end
    object tbitmDeleteCategory: TTBXItem
      Caption = 'Delete Category'
      Hint = 'Delete'
      ImageIndex = 30
      ShortCut = 46
      OnClick = btnDeleteCategoryClick
    end
    object tbsprtCategoriesPopUp: TTBXSeparatorItem
    end
    object tbitmExportCategory: TTBXItem
      Caption = 'Export Placemarks'
      Hint = 'Export placemarks from selected category'
      ImageIndex = 25
      OnClick = btnExportCategoryClick
    end
    object tbxtmCatAddToMergePolygons: TTBXItem
      Caption = 'Add to Merge Polygons'
      ImageIndex = 62
      OnClick = tbxtmCatAddToMergePolygonsClick
    end
  end
  object tbpmnMarks: TTBXPopupMenu
    Images = frmMain.MenusImageList
    OnPopup = tbpmnMarksPopup
    Left = 264
    Top = 256
    object tbitmAddMark: TTBXItem
      Caption = 'Add'
      Hint = 'Add'
      ImageIndex = 32
      OnClick = tbitmAddMarkClick
    end
    object tbitmEditMark: TTBXItem
      Caption = 'Edit Properties'
      Hint = 'Edit'
      ImageIndex = 31
      ShortCut = 113
      OnClick = btnEditMarkClick
    end
    object tbitmEditMarkPosition: TTBXItem
      Caption = 'Edit Position'
      ImageIndex = 31
      ShortCut = 114
      OnClick = tbitmEditMarkPositionClick
    end
    object tbitmDeleteMark: TTBXItem
      Caption = 'Delete'
      Hint = 'Delete'
      ImageIndex = 30
      ShortCut = 46
      OnClick = btnDelMarkClick
    end
    object tbsprtMarksPopUp2: TTBXSeparatorItem
    end
    object tbitmCut: TTBXItem
      Caption = 'Cut'
      ImageIndex = 63
      OnClick = tbitmCutClick
    end
    object tbitmCopy: TTBXItem
      Caption = 'Copy'
      ImageIndex = 67
      OnClick = tbitmCopyClick
    end
    object tbitmPaste: TTBXItem
      Caption = 'Paste'
      ImageIndex = 68
      OnClick = tbitmPasteClick
    end
    object tbsprtMarksPopUp: TTBXSeparatorItem
    end
    object tbitmCopyAsText: TTBXItem
      Caption = 'Copy to Clipboard as Text'
      ImageIndex = 69
      OnClick = tbitmCopyAsTextClick
    end
    object tbxtmCopyBboxToClipboard: TTBXItem
      Caption = 'Copy Bounding Box to Clipboard'
      OnClick = tbxtmCopyBboxToClipboardClick
    end
    object TBXSeparatorItem5: TTBXSeparatorItem
    end
    object tbxSelectAllVisible: TTBXItem
      Caption = 'Select All Visible Placemarks'
      OnClick = tbxSelectAllVisibleClick
    end
    object tbxRevertSelection: TTBXItem
      Caption = 'Revert Selection'
      OnClick = tbxRevertSelectionClick
    end
    object TBXSeparatorItem7: TTBXSeparatorItem
    end
    object tbitmMarkInfo: TTBXItem
      Caption = 'Placemark Info'
      ImageIndex = 27
      OnClick = tbitmMarkInfoClick
    end
    object tbitmExportMark: TTBXItem
      Caption = 'Export selected placemarks'
      ImageIndex = 25
      OnClick = btnSaveMarkClick
    end
    object TBXSeparatorItem6: TTBXSeparatorItem
    end
    object tbxtmAddToMergePolygons: TTBXItem
      Caption = 'Add to Merge Polygons'
      ImageIndex = 62
      OnClick = tbxtmAddToMergePolygonsClick
    end
    object tbxtmGroup: TTBXItem
      Caption = 'Group'
      ImageIndex = 56
      OnClick = tbxtmGroupClick
    end
    object tbxtmUngroup: TTBXItem
      Caption = 'Ungroup'
      ImageIndex = 57
      OnClick = tbxtmUngroupClick
    end
    object tbxShowElevProfile: TTBXItem
      Caption = 'Show Elevation Profile'
      ImageIndex = 81
      OnClick = tbxShowElevProfileClick
    end
  end
  object TBXImageList1: TTBXImageList
    Left = 56
    Top = 104
    PngDIB = {
      0500000089504E470D0A1A0A0000000D49484452000000100000005008060000
      0010ABF50D0000084B4944415478DAED587B5053E915FFDD4712125E09516262
      404040111C1641A55514054458D2019FEBBA4B6DD75AAB5B775ABBB53B3B232A
      95B5335647A7DAC1B58EEBBAD68EB656716595D676573BBB6BC1597C6C5374B7
      2A15210448424220AFDB7343407929DA99F61F3FE6E4DEDCFB9DF39DF33BE7FC
      BE7C304B972EC5306315490C1E3FEE901CE6877BE3743AA5870E1D2AEBECEC44
      777737CC6607EC760F8283431011A1825AADC6DAB5CBD7731C077E24F31E8F07
      EDEDEDB0582C686B77A0930CA8542AF0120E61E1A1FDF3F8E15C96C964E9D5D5
      D5E8F3C0E97481E1E450AAC221305E08F0F61B60FB5CDEB76F5FD98E1D3BCAB6
      6CD952B661C3A6A271E392F0C20BF35158B8126BD6FC040FEEDF86F1E667F8A2
      EE63ECD95D7E9ED46A1FF5E0892EC7C4272163462679A144D1A2E5F91BD6BE76
      86D4AE8C8881E0FFF3F5BB6CB559D0DA61022B051954F4CF63C4345208E95AAD
      764B46464691E889C96482CBE58242A18052A944778F0B6E423C3A2E1A9A480D
      F43A2D7EFAFA1B6216F6B30143E9DBB76F2FCACDCD45767636E6E71622336B01
      32E7E421736E1E96AC7815778DFFC0BFAEDD84B1F62A2A77EF3DEFF57A6BDD6E
      F708187474C2D2E5449832B4DFE5E88478A4CE48230CC290B7A8207FEDCAD5A7
      58961D010306F051EC3EC64328D0151E583BAD3059440C04848505A3ADAD8D27
      03608A8B8BD1D3D393AED7EB47C44060184842E5D0C58E878ABC52AB2390373D
      E74DF27827171D1D2D8258545959B9918C40148D2E0A2ADD38E8132620366922
      A64E9B8A375E5B57FDEEDE5FFFEEE8C1F73F3EF8AB773F211D1B159999494C4C
      84542AFDC1850B17F6DFBF7FBF1703AB0D66A7058A7039C6458E855EABC3B2FC
      C56F1B8DC6C383DB866F686850520A83799EA74689F0BB2D950783B7CB2051F0
      08A1EF1CCFC266B3B94405928EC1904D90CBE5AF53FD6F25237D522E9148CAC9
      B3F2A0A0A06D64741BCDF921CD8D1B82F7237C6020D16274E301499578C32527
      27FB9F5026B209C84A2A24C3ECD9B30D5959730C53A6A41B9293671A3233E719
      E6CD2B342C5C5862282E7EC970FAF4F1CBA4F2779FCF37900FC4145AAD5631C7
      74EFA574523BF7F8884842E1F17AC0722C14C1F201AEB08F7594119B8A84F1F5
      0A7A65F406C48EF42B063A1303C964001FD0A8ABA8A8389B9A9A5A24C6D6D5D5
      1508C503CA04424242101E1E0E4AFB477D6432D883F4CD9B37FB3B322B2B0B39
      39D499F30BF08D39F998999587997372E99A83F53FDAB490E6660CE7C110105B
      CC5638A827E4049CDBA7241001B942F634203E8CDD1760A6C1183C390BFD19F0
      E217A6DD28B9BE12B5731B56DC9E7C1FC6C4BBA307B19D40BC16791D79450B70
      B8F5C898A631AD14B3002E292909822088130DBB76EDDA181515059D4E879898
      588CD5EAA11E3F1E1FAA2FA246F109641229C26394B85DDFD0E561BDAF905BE9
      CC8C1933FCA44104F2BD63C78E1D10C9A46FE5968E0E585D0EECECDE833843FC
      9008FFF2878B75FC952B5720720275233F5231450B3ADC3D751B62CB4B1706E3
      9F476E34BA83BC26C2A88E120339AD18412F33D6AD5BB740E439316671236539
      09243209BEA99989925803CEDFBB004BBC1DF64F3B3A85CD9D7371A147DC5C10
      419222F63B296E251EE8E3846D8378A19C2B945E654AA54D6C8144DCDA5244DD
      E77CF09C0FFEF77CE00B48DF48361A9F8E0FCC0462158138B6BDBD36FFF265BF
      A951F181F6C32A84D5D420F8F3CF11FBF557F163AFDFC8E73C9E57A84646C707
      D2AD3F47E9ACAC2111BE77E4C8E8F8C01637017B3EFB1B788EF77342B3B1A131
      C8ED3691C2E8F8403D6F16A2572C4242E97224AF7E151B3F38595ED5D9B9E922
      F0747C40731EFDADF09C0FFEEF7C106F36630995F333F3C1426AA467E68329AD
      ADB1B166F3A96660CF53F381A9B519368E9B1C5B56564CFBF356FE69F8C0EDA8
      C5577764F8654D4D51FEC489BB5708C247A306B15B65825B76192BBFF522D2D2
      D20E98BABB0F2F03AE8D8A0F62A21D0865FE8C82FC3C9C3AF357381CDC25A73B
      E48898152E34948E35D481F4FB7F5A494989413CF6889C60B359D16C36C161FB
      148CE5240AF27271FCE479C44C5B8F3355C6C394D63A232DFC583E605CCD08EE
      A986A1E845BC7FFC2C22A77C07AAF1D329B45D1C2D885BB76EF9EB404EC51149
      F12A44B210DDA2F303890E9CBD1E2FBD7C02478F9F8326E9BBD0C4CCF29F1DCC
      66B39C942345DDC7F28158CB4BB2D92F27C748DE1B910F4400FF9BC1881F6B5E
      66B066C59077E1240703F7AB49AC8FBE3CF0DB403756560CAB2C8E65DAF86F27
      68E34B13C4FBC12F451D51982BA799A16E3188E2A5AABD53B30E14D21114D72F
      7DFF9CC765D940D1360E9ECBFB7C18CE40A92EA13445E8BE2315394117BF32E5
      CE8D7DA56460FB130D90726A5048F45C953A25EEED8A06FFB3F2B7A6C4C914FA
      B94EFBBFCF9291FA01F32F9D1810024B06764D4ADFB4344822E8DEDAE1F03F7C
      E7677274399D4DB7EAF79D20033F061EF21AEFF30E583D274C9D9C219787EB3C
      CE7BB0DB9DFEE75EB7970EA46A5D48587C86ADE3760E19A9E9D7B9F841BF0772
      86650EA464BE59C40B36A5CBDB8D054BAEA2A5A5057FAA4A858FB458566A69BD
      75EEACE013D6044EB1E0DD9E80EF2C166B74D3A7B26C8FB2CDDE882EB71DF5F5
      BDE17A2593029304A52C5C3FD5D1D6B898B03BEA371028C43160A4CBC6E85312
      DB3AEBE0153C90107D995BB6F95FB69AAA034EBA10AED524DADB9A9609825724
      57334FD42796E32ACD844993DD3EA39C67BBFA89728C66B3FFFAF597690F81E2
      20578D1B3BD974AF7915ADBD9327C2890F92CB0C9AF14171102CFE95078F20CE
      35E0BB56AF8C3337990D3DDD9E3F32C776E3372969FA025D54A896DC42EF5ED4
      3B822316FBAF8EF6DF0F31DAD4687B70F38B966AE66005F6130EE9CFD48974E0
      E8DBDEC4FF2CA903DBBBEE09A2F583DEABC3FE070433F8644191B25700000000
      49454E44AE426082}
  end
end
