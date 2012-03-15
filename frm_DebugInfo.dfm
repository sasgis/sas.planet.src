object frmDebugInfo: TfrmDebugInfo
  Left = 0
  Top = 0
  Caption = 'Debug info'
  ClientHeight = 509
  ClientWidth = 400
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
    Width = 400
    Height = 480
    Align = alClient
    ColCount = 4
    DefaultColWidth = 80
    DefaultRowHeight = 20
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
    TabOrder = 0
    ExplicitTop = -3
    ExplicitWidth = 397
    ExplicitHeight = 451
    ColWidths = (
      147
      80
      80
      80)
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 480
    Width = 400
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 451
    ExplicitWidth = 348
    object btnRefresh: TButton
      AlignWithMargins = True
      Left = 323
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
      ExplicitLeft = 271
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
      Left = 216
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
      Left = 81
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
      ExplicitTop = 5
    end
  end
end
