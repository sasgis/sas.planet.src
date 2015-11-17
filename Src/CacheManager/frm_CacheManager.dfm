object frmCacheManager: TfrmCacheManager
  Left = 0
  Top = 0
  Caption = 'Cache Manager'
  ClientHeight = 397
  ClientWidth = 572
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 572
    Height = 360
    ActivePage = tsConverter
    Align = alClient
    TabOrder = 0
    ExplicitHeight = 279
    object tsConverter: TTabSheet
      Caption = 'Convert Cache Format'
      ExplicitHeight = 251
      object grpSrc: TGroupBox
        Left = 2
        Top = 0
        Width = 559
        Height = 161
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Source Cache'
        TabOrder = 0
        object lblPath: TLabel
          Left = 10
          Top = 16
          Width = 26
          Height = 13
          Caption = 'Path:'
        end
        object lblCacheType: TLabel
          Left = 10
          Top = 62
          Width = 38
          Height = 13
          Caption = 'Format:'
        end
        object lblDefExtention: TLabel
          Left = 175
          Top = 62
          Width = 50
          Height = 13
          Caption = 'Extention:'
        end
        object edtPath: TEdit
          Left = 10
          Top = 35
          Width = 516
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object pnlCacheTypes: TPanel
          Left = 10
          Top = 81
          Width = 153
          Height = 21
          BevelOuter = bvNone
          TabOrder = 1
        end
        object chkIgnoreTNE: TCheckBox
          Left = 279
          Top = 81
          Width = 130
          Height = 17
          Caption = 'Ignore *.tne'
          TabOrder = 2
        end
        object chkRemove: TCheckBox
          Left = 415
          Top = 81
          Width = 141
          Height = 17
          Caption = 'Remove tiles'
          TabOrder = 3
        end
        object edtDefExtention: TEdit
          Left = 175
          Top = 81
          Width = 98
          Height = 21
          TabOrder = 4
          Text = '*.jpg'
        end
        object btnSelectSrcPath: TButton
          Left = 532
          Top = 34
          Width = 21
          Height = 19
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 5
          OnClick = btnSelectSrcPathClick
        end
        object chkCheckSourceVersion: TCheckBox
          Left = 10
          Top = 108
          Width = 536
          Height = 17
          Caption = 'Process only tiles  with version:'
          TabOrder = 6
        end
        object edtSourceVersion: TEdit
          Left = 10
          Top = 131
          Width = 536
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 7
        end
      end
      object grpDestCache: TGroupBox
        Left = 3
        Top = 167
        Width = 559
        Height = 162
        Align = alCustom
        Anchors = [akLeft, akTop, akRight]
        Caption = 'Dest Cache'
        TabOrder = 1
        object lblDestPath: TLabel
          Left = 10
          Top = 16
          Width = 26
          Height = 13
          Caption = 'Path:'
        end
        object lblDestFormat: TLabel
          Left = 10
          Top = 62
          Width = 38
          Height = 13
          Caption = 'Format:'
        end
        object edtDestPath: TEdit
          Left = 10
          Top = 35
          Width = 516
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
        end
        object pnlDestCacheTypes: TPanel
          Left = 10
          Top = 81
          Width = 153
          Height = 21
          BevelOuter = bvNone
          TabOrder = 1
        end
        object chkOverwrite: TCheckBox
          Left = 175
          Top = 81
          Width = 381
          Height = 17
          Caption = 'Overwrite existing tiles'
          TabOrder = 2
        end
        object btnSelectDestPath: TButton
          Left = 532
          Top = 35
          Width = 21
          Height = 19
          Align = alCustom
          Anchors = [akTop, akRight]
          Caption = '...'
          TabOrder = 3
          OnClick = btnSelectDestPathClick
        end
        object chkReplaceDestVersion: TCheckBox
          Left = 10
          Top = 108
          Width = 536
          Height = 17
          Caption = 'Set same version for all processed tiles:'
          TabOrder = 4
        end
        object edtDestVersion: TEdit
          Left = 10
          Top = 131
          Width = 535
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 5
        end
      end
    end
  end
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 360
    Width = 572
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitTop = 279
    object btnStart: TButton
      AlignWithMargins = True
      Left = 410
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Start'
      Default = True
      TabOrder = 0
      OnClick = btnStartClick
    end
    object btnCansel: TButton
      AlignWithMargins = True
      Left = 491
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCanselClick
    end
    object TBXDontClose: TTBXToolbar
      Left = 4
      Top = 6
      Width = 25
      Height = 24
      Align = alCustom
      Images = frmMain.MenusImageList
      ShrinkMode = tbsmWrap
      TabOrder = 2
      object tbtmDontClose: TTBItem
        AutoCheck = True
        ImageIndex = 46
        Caption = ''
        Hint = 'Do not close this window after start'
      end
    end
  end
end
