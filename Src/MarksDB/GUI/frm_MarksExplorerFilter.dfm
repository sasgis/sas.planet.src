object frmMarksExplorerFilter: TfrmMarksExplorerFilter
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  Caption = 'Placemarks filter settings'
  ClientHeight = 249
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 218
    Width = 473
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 395
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      TabOrder = 3
      OnClick = btnCancelClick
    end
    object btnOk: TButton
      AlignWithMargins = True
      Left = 314
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Ok'
      Default = True
      ModalResult = 1
      TabOrder = 2
      OnClick = btnOkClick
    end
    object btnApply: TButton
      AlignWithMargins = True
      Left = 233
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Apply'
      TabOrder = 1
      OnClick = btnApplyClick
    end
    object btnReset: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 25
      Align = alLeft
      Caption = 'Reset'
      TabOrder = 0
      OnClick = btnResetClick
    end
  end
  object grpFilterByType: TGroupBox
    Left = 3
    Top = 8
    Width = 467
    Height = 49
    Align = alCustom
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Filter by placemark type'
    TabOrder = 0
    object grdpnlFilterByType: TGridPanel
      Left = 2
      Top = 15
      Width = 463
      Height = 32
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 33.000000000000000000
        end
        item
          Value = 33.000000000000000000
        end
        item
          Value = 34.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = chkAllowPoints
          Row = 0
        end
        item
          Column = 1
          Control = chkAllowPaths
          Row = 0
        end
        item
          Column = 2
          Control = chkAllowPolygons
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      object chkAllowPoints: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 147
        Height = 26
        Align = alClient
        Caption = 'Marks'
        Checked = True
        State = cbChecked
        TabOrder = 0
      end
      object chkAllowPaths: TCheckBox
        AlignWithMargins = True
        Left = 156
        Top = 3
        Width = 147
        Height = 26
        Align = alClient
        Caption = 'Paths'
        Checked = True
        State = cbChecked
        TabOrder = 1
      end
      object chkAllowPolygons: TCheckBox
        AlignWithMargins = True
        Left = 309
        Top = 3
        Width = 151
        Height = 26
        Align = alClient
        Caption = 'Polygons'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object grpFilterByText: TGroupBox
    Left = 3
    Top = 63
    Width = 467
    Height = 149
    Align = alCustom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'Filter by placemark name and description'
    TabOrder = 1
    object edtSearchText: TEdit
      AlignWithMargins = True
      Left = 5
      Top = 21
      Width = 457
      Height = 21
      Margins.Top = 6
      Align = alTop
      TabOrder = 0
    end
    object cbbSearchMethod: TComboBox
      AlignWithMargins = True
      Left = 5
      Top = 48
      Width = 457
      Height = 21
      Align = alTop
      Style = csDropDownList
      ItemIndex = 1
      TabOrder = 1
      Text = 'Contains'
      Items.Strings = (
        'Begins With'
        'Contains'
        'Matches Exactly'
        'Ends With'
        'Regular Expression')
    end
    object chkIgnoreCase: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 75
      Width = 457
      Height = 17
      Align = alTop
      Caption = 'Case-Insensitive Search'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkSearchInName: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 98
      Width = 457
      Height = 17
      Align = alTop
      Caption = 'Search in Name'
      Checked = True
      State = cbChecked
      TabOrder = 3
    end
    object chkSearchInDesc: TCheckBox
      AlignWithMargins = True
      Left = 5
      Top = 121
      Width = 457
      Height = 17
      Align = alTop
      Caption = 'Search in Description'
      TabOrder = 4
    end
  end
end
