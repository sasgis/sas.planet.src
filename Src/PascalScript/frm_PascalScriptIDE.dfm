object frmPascalScriptIDE: TfrmPascalScriptIDE
  Left = 0
  Top = 0
  Caption = 'PascalScript IDE'
  ClientHeight = 535
  ClientWidth = 756
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object tbtlbrMain: TTBXToolbar
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 750
    Height = 21
    Color = 15790320
    Align = alTop
    BorderStyle = bsNone
    ChevronMoveItems = False
    CloseButton = False
    FullSize = True
    ItemTransparency = itEnable
    MenuBar = True
    ParentShowHint = False
    ProcessShortCuts = True
    ShowHint = True
    ShrinkMode = tbsmWrap
    Stretch = True
    TabOrder = 0
    UseThemeColor = False
    Caption = 'tbtlbrMain'
    object tbxsbmntmOpen: TTBXSubmenuItem
      DisplayMode = nbdmImageAndText
      ImageIndex = 34
      Images = frmMain.MenusImageList
      Options = [tboDropdownArrow]
      Caption = 'Open zmp'
      Hint = 'Open zmp'
      object tbxtmFromFolder: TTBXItem
        OnClick = tbxtmFromFolderClick
        Caption = 'From Folder'
        Hint = ''
      end
      object tbxtmFromZip: TTBXItem
        OnClick = tbxtmFromZipClick
        Caption = 'From Archive'
        Hint = ''
      end
      object tbxsprtrtm2: TTBXSeparatorItem
        Caption = ''
        Hint = ''
      end
      object tbxsbmntmMap: TTBXSubmenuItem
        Caption = 'From Map'
        Hint = ''
      end
      object tbxsbmntmLayer: TTBXSubmenuItem
        Caption = 'From Layer'
        Hint = ''
      end
    end
    object tbxSave: TTBXSubmenuItem
      DisplayMode = nbdmImageAndText
      ImageIndex = 25
      Images = frmMain.MenusImageList
      Options = [tboDropdownArrow]
      Caption = 'Save zmp'
      Hint = 'Save zmp'
      object tbxtmToFolder: TTBXItem
        OnClick = tbxtmToFolderClick
        Caption = 'To Folder'
        Hint = ''
      end
      object tbxtmToArchive: TTBXItem
        OnClick = tbxtmToArchiveClick
        Caption = 'To Archive'
        Hint = ''
      end
    end
    object tbxSep2: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxtmParams: TTBXItem
      AutoCheck = True
      GroupIndex = 1
      RadioItem = True
      Stretch = True
      OnClick = tbxtmParamsClick
      Caption = 'Params'
      Hint = 'Params.txt'
    end
    object tbxtmScript: TTBXItem
      AutoCheck = True
      Checked = True
      GroupIndex = 1
      RadioItem = True
      Stretch = True
      OnClick = tbxtmScriptClick
      Caption = 'GetUrlScript'
      Hint = 'GetUrlScript.txt'
    end
    object tbxSep1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxtmRun: TTBXItem
      DisplayMode = nbdmImageAndText
      ImageIndex = 51
      Images = frmMain.MenusImageList
      ShortCut = 120
      Stretch = True
      OnClick = tbxtmRunClick
      Caption = 'Run'
      Hint = 'Run'
    end
    object tbxsprtrtm4: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxtmSyntaxCheck: TTBXItem
      ImageIndex = 52
      Images = frmMain.MenusImageList
      Stretch = True
      OnClick = tbxtmSyntaxCheckClick
      Caption = ''
      Hint = 'Syntax Check'
    end
    object tbxtmDecompile: TTBXItem
      ImageIndex = 53
      Images = frmMain.MenusImageList
      Stretch = True
      OnClick = tbxtmDecompileClick
      Caption = ''
      Hint = 'Decompile'
    end
    object tbxsprtrtm3: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxtmWordWrap: TTBXItem
      AutoCheck = True
      Checked = True
      ImageIndex = 54
      Images = frmMain.MenusImageList
      OnClick = tbxtmWordWrapClick
      Caption = ''
      Hint = 'Word Wrap'
    end
    object tbxsprtrtm1: TTBXSeparatorItem
      Caption = ''
      Hint = ''
    end
    object tbxtmHelp: TTBXItem
      ImageIndex = 26
      Images = frmMain.MenusImageList
      OnClick = tbxtmHelpClick
      Caption = ''
      Hint = 'On-Line Help'
    end
  end
  object statEditor: TStatusBar
    Left = 0
    Top = 516
    Width = 756
    Height = 19
    Panels = <
      item
        Width = 50
      end>
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 27
    Width = 756
    Height = 489
    ActivePage = tsParams
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object tsParams: TTabSheet
      Caption = 'tsParams'
      object splInOut: TSplitter
        Left = 545
        Top = 0
        Height = 458
        Align = alRight
        ExplicitLeft = 367
        ExplicitHeight = 448
      end
      object pnlParamsTxt: TPanel
        Left = 0
        Top = 0
        Width = 545
        Height = 458
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlParamsTxt'
        TabOrder = 0
      end
      object pnlInput: TPanel
        Left = 548
        Top = 0
        Width = 200
        Height = 458
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object grpInput: TGroupBox
          Left = 0
          Top = 0
          Width = 200
          Height = 458
          Align = alClient
          Caption = 'Input Vars'
          TabOrder = 0
          object lblGetX: TLabel
            Left = 6
            Top = 21
            Width = 27
            Height = 13
            Align = alCustom
            Caption = 'GetX:'
          end
          object lblGetY: TLabel
            Left = 6
            Top = 67
            Width = 27
            Height = 13
            Align = alCustom
            Caption = 'GetY:'
          end
          object lblGetZ: TLabel
            Left = 6
            Top = 113
            Width = 27
            Height = 13
            Align = alCustom
            Caption = 'GetZ:'
          end
          object edtGetX: TEdit
            Left = 6
            Top = 40
            Width = 191
            Height = 21
            Align = alCustom
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 0
            Text = '0'
          end
          object edtGetY: TEdit
            Left = 6
            Top = 86
            Width = 191
            Height = 21
            Align = alCustom
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 1
            Text = '0'
          end
          object edtGetZ: TEdit
            Left = 6
            Top = 132
            Width = 191
            Height = 21
            Align = alCustom
            Anchors = [akLeft, akTop, akRight]
            TabOrder = 2
            Text = '0'
          end
          object btnSetXYZ: TButton
            Left = 6
            Top = 159
            Width = 191
            Height = 25
            Align = alCustom
            Anchors = [akLeft, akTop, akRight]
            Caption = 'Screen center'
            TabOrder = 3
            OnClick = btnSetXYZClick
          end
        end
      end
    end
    object tsScript: TTabSheet
      Caption = 'tsScript'
      ImageIndex = 1
      object splEditLog: TSplitter
        Left = 0
        Top = 379
        Width = 748
        Height = 3
        Cursor = crVSplit
        Align = alBottom
        ExplicitTop = 353
        ExplicitWidth = 39
      end
      object pnlScriptEditor: TPanel
        Left = 0
        Top = 0
        Width = 748
        Height = 379
        Align = alClient
        BevelOuter = bvNone
        Caption = 'pnlScriptEditor'
        TabOrder = 0
      end
      object pnlLog: TPanel
        Left = 0
        Top = 382
        Width = 748
        Height = 76
        Align = alBottom
        BevelOuter = bvNone
        Caption = 'pnlLog'
        TabOrder = 1
        object lstLog: TListBox
          Left = 0
          Top = 0
          Width = 748
          Height = 76
          Align = alClient
          ItemHeight = 13
          TabOrder = 0
        end
      end
    end
  end
  object dlgOpenZmpFile: TOpenDialog
    DefaultExt = '*.zmp'
    Filter = 'ZMP | *.zmp'
    Options = [ofFileMustExist, ofEnableSizing]
    Left = 8
    Top = 56
  end
  object dlgSaveZmpFile: TSaveDialog
    DefaultExt = '*.zmp'
    Filter = 'ZMP | *.zmp'
    Options = [ofEnableSizing]
    Left = 40
    Top = 56
  end
end
