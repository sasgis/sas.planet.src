object frmDebugInfo: TfrmDebugInfo
  Left = 0
  Top = 0
  AlphaBlendValue = 150
  Caption = 'Debug info'
  ClientHeight = 566
  ClientWidth = 792
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object sgrdDebugInfo: TStringGrid
    Left = 0
    Top = 0
    Width = 792
    Height = 537
    Align = alClient
    ColCount = 6
    DefaultColWidth = 80
    DefaultRowHeight = 20
    FixedColor = clWindow
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect]
    TabOrder = 0
    ColWidths = (
      338
      80
      80
      80
      80
      80)
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 537
    Width = 792
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object btnRefresh: TButton
      AlignWithMargins = True
      Left = 715
      Top = 2
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Caption = 'Refresh'
      TabOrder = 0
      OnClick = btnRefreshClick
    end
    object btnReset: TButton
      AlignWithMargins = True
      Left = 2
      Top = 2
      Width = 75
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alLeft
      Caption = 'Reset'
      TabOrder = 1
      OnClick = btnResetClick
    end
    object btnSaveToFile: TButton
      AlignWithMargins = True
      Left = 608
      Top = 2
      Width = 103
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Caption = 'Save to file ...'
      TabOrder = 2
      OnClick = btnSaveToFileClick
    end
    object btnCopyToClipboard: TButton
      AlignWithMargins = True
      Left = 473
      Top = 2
      Width = 131
      Height = 25
      Margins.Left = 2
      Margins.Top = 2
      Margins.Right = 2
      Margins.Bottom = 2
      Align = alRight
      Caption = 'Copy to clipboard'
      TabOrder = 3
      OnClick = btnCopyToClipboardClick
    end
    object chkHideEmtyRows: TCheckBox
      Left = 88
      Top = 6
      Width = 105
      Height = 17
      Caption = 'Hide emty rows'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object chkAutoRefresh: TCheckBox
      Left = 199
      Top = 6
      Width = 97
      Height = 17
      Caption = 'Auto Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkAutoRefreshClick
    end
    object chkAlphaBlend: TCheckBox
      Left = 312
      Top = 8
      Width = 97
      Height = 17
      Caption = 'Alpha Blend'
      TabOrder = 6
      OnClick = chkAlphaBlendClick
    end
  end
  object tmrRefresh: TTimer
    OnTimer = tmrRefreshTimer
    Left = 200
    Top = 496
  end
end
