object frmDebugInfo: TfrmDebugInfo
  Left = 0
  Top = 0
  AlphaBlendValue = 150
  Caption = 'Debug info'
  ClientHeight = 566
  ClientWidth = 792
  Color = clBtnFace
  ParentFont = True
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
    ColCount = 9
    DefaultColWidth = 80
    DefaultRowHeight = 20
    FixedColor = clWindow
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing, goRowSelect]
    PopupMenu = pmFiltering
    TabOrder = 0
    ColWidths = (
      338
      80
      80
      80
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
    object lblFiltering: TLabel
      Left = 378
      Top = 0
      Width = 93
      Height = 29
      Hint = 'Filtering'
      Align = alClient
      Caption = 'Filtering'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clRed
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
      ExplicitWidth = 38
      ExplicitHeight = 13
    end
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
      Left = 79
      Top = 0
      Width = 105
      Height = 29
      Align = alLeft
      Caption = 'Hide emty rows'
      Checked = True
      State = cbChecked
      TabOrder = 4
    end
    object chkAutoRefresh: TCheckBox
      Left = 184
      Top = 0
      Width = 97
      Height = 29
      Align = alLeft
      Caption = 'Auto Refresh'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkAutoRefreshClick
    end
    object chkAlphaBlend: TCheckBox
      Left = 281
      Top = 0
      Width = 97
      Height = 29
      Align = alLeft
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
  object pmFiltering: TPopupMenu
    OnPopup = pmFilteringPopup
    Left = 232
    Top = 496
    object pmiCountIsGreaterOrEqual: TMenuItem
      Caption = 'Filter by Count'
      OnClick = pmiCountIsGreaterOrEqualClick
    end
    object pmiCountReset: TMenuItem
      Caption = 'Reset'
      Hint = 'Reset'
      OnClick = pmiCountResetClick
    end
    object pmiSep1: TMenuItem
      Caption = '-'
    end
    object pmiTotalIsGreaterOrEqual: TMenuItem
      Caption = 'Filter by Total'
      OnClick = pmiTotalIsGreaterOrEqualClick
    end
    object pmiTotalReset: TMenuItem
      Caption = 'Reset'
      Hint = 'Reset'
      OnClick = pmiTotalResetClick
    end
    object pmiSep2: TMenuItem
      Caption = '-'
    end
    object pmiSortBy: TMenuItem
      Caption = 'Sort by'
      object pmiSortByName: TMenuItem
        AutoCheck = True
        Caption = 'Name'
        Checked = True
        RadioItem = True
        OnClick = SortByClick
      end
      object pmiSortByTotalCount: TMenuItem
        Tag = 1
        AutoCheck = True
        Caption = 'Total Count'
        RadioItem = True
        OnClick = SortByClick
      end
      object pmiSortByTotalAvg: TMenuItem
        Tag = 2
        AutoCheck = True
        Caption = 'Total Average time'
        RadioItem = True
        OnClick = SortByClick
      end
      object pmiSortByTotalTime: TMenuItem
        Tag = 3
        AutoCheck = True
        Caption = 'Total Time'
        RadioItem = True
        OnClick = SortByClick
      end
      object pmiSortByUICount: TMenuItem
        Tag = 4
        AutoCheck = True
        Caption = 'UI Count'
        RadioItem = True
        OnClick = SortByClick
      end
      object pmiSortByUiAvg: TMenuItem
        Tag = 5
        AutoCheck = True
        Caption = 'UI Average time'
        RadioItem = True
        OnClick = SortByClick
      end
      object pmiSortByUiTime: TMenuItem
        Tag = 6
        AutoCheck = True
        Caption = 'UI Time'
        RadioItem = True
        OnClick = SortByClick
      end
    end
  end
end
