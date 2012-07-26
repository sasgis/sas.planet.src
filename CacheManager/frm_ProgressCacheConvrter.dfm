object frmProgressCacheConverter: TfrmProgressCacheConverter
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Please wait...'
  ClientHeight = 114
  ClientWidth = 315
  Color = clBtnFace
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poScreenCenter
  ScreenSnap = True
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 13
  object lblProcessedName: TLabel
    Left = 8
    Top = 8
    Width = 53
    Height = 13
    BiDiMode = bdLeftToRight
    Caption = 'Processed:'
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblSkippedName: TLabel
    Left = 8
    Top = 27
    Width = 41
    Height = 13
    BiDiMode = bdLeftToRight
    Caption = 'Skipped:'
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblSizeName: TLabel
    Left = 8
    Top = 46
    Width = 23
    Height = 13
    BiDiMode = bdLeftToRight
    Caption = 'Size:'
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblLastTileName: TLabel
    Left = 8
    Top = 65
    Width = 53
    Height = 13
    BiDiMode = bdLeftToRight
    Caption = 'Last name:'
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblLastTileValue: TLabel
    Left = 300
    Top = 65
    Width = 3
    Height = 13
    Align = alCustom
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblSizeValue: TLabel
    Left = 300
    Top = 46
    Width = 3
    Height = 13
    Align = alCustom
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblSkippedValue: TLabel
    Left = 300
    Top = 27
    Width = 3
    Height = 13
    Align = alCustom
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object lblProcessedValue: TLabel
    Left = 300
    Top = 8
    Width = 3
    Height = 13
    Align = alCustom
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    BiDiMode = bdLeftToRight
    ParentBiDiMode = False
    Layout = tlCenter
  end
  object pnlBottom: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 85
    Width = 309
    Height = 26
    Margins.Top = 0
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 0
    object btnQuit: TButton
      AlignWithMargins = True
      Left = 231
      Top = 3
      Width = 75
      Height = 20
      Align = alRight
      Caption = 'Quit'
      TabOrder = 0
      OnClick = btnQuitClick
    end
    object btnPause: TButton
      AlignWithMargins = True
      Left = 150
      Top = 3
      Width = 75
      Height = 20
      Align = alRight
      Caption = 'Pause'
      TabOrder = 1
      OnClick = btnPauseClick
    end
    object btnMinimize: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 20
      Align = alLeft
      Caption = 'Minimize'
      TabOrder = 2
      OnClick = btnMinimizeClick
    end
  end
end
