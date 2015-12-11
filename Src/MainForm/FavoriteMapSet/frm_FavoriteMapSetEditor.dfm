object frmFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  ClientHeight = 393
  ClientWidth = 335
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pnlMain: TPanel
    Left = 0
    Top = 0
    Width = 335
    Height = 355
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 400
    ExplicitHeight = 414
    object lblName: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 329
      Height = 13
      Align = alTop
      Caption = 'Name:'
      ExplicitWidth = 31
    end
    object chklstMaps: TCheckListBox
      AlignWithMargins = True
      Left = 3
      Top = 124
      Width = 329
      Height = 140
      OnClickCheck = chklstMapsClickCheck
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      ExplicitWidth = 394
      ExplicitHeight = 185
    end
    object edtName: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 22
      Width = 329
      Height = 21
      Align = alTop
      TabOrder = 1
      ExplicitWidth = 394
    end
    object pnlMap: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 72
      Width = 329
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitWidth = 394
    end
    object chkMergeLayers: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 290
      Width = 329
      Height = 17
      Align = alBottom
      Caption = 'Hide non-selected layers'
      Checked = True
      State = cbChecked
      TabOrder = 3
      ExplicitLeft = 6
      ExplicitTop = 347
      ExplicitWidth = 349
    end
    object pnlLayers: TPanel
      Left = 0
      Top = 267
      Width = 335
      Height = 20
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 4
      ExplicitTop = 312
      ExplicitWidth = 400
      object lblLayersCount: TLabel
        AlignWithMargins = True
        Left = 329
        Top = 3
        Width = 3
        Height = 14
        Align = alRight
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        ExplicitLeft = 394
        ExplicitHeight = 13
      end
      object chkAll: TCheckBox
        Left = 3
        Top = 3
        Width = 97
        Height = 17
        Caption = 'All'
        TabOrder = 0
        OnClick = chkAllClick
      end
    end
    object chkLayers: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 101
      Width = 329
      Height = 17
      Align = alTop
      Caption = 'Layers:'
      Checked = True
      State = cbChecked
      TabOrder = 5
      OnClick = chkLayersClick
      ExplicitWidth = 394
    end
    object chkMap: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 49
      Width = 329
      Height = 17
      Align = alTop
      Caption = 'Map:'
      Checked = True
      State = cbChecked
      TabOrder = 6
      OnClick = chkMapClick
      ExplicitWidth = 394
    end
    object grdpnlHotKey1: TGridPanel
      Left = 0
      Top = 310
      Width = 335
      Height = 45
      Align = alBottom
      BevelOuter = bvNone
      ColumnCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = chkZoom
          Row = 0
        end
        item
          Column = 0
          Control = cbbZoom
          Row = 1
        end
        item
          Column = 1
          Control = lblHotKey
          Row = 0
        end
        item
          Column = 1
          Control = pnlZoom
          Row = 1
        end>
      RowCollection = <
        item
          SizeStyle = ssAbsolute
          Value = 20.000000000000000000
        end
        item
          SizeStyle = ssAbsolute
          Value = 50.000000000000000000
        end>
      TabOrder = 7
      ExplicitLeft = 3
      ExplicitTop = 385
      ExplicitWidth = 362
      object chkZoom: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 161
        Height = 17
        Align = alTop
        Anchors = []
        Caption = 'Zoom'
        TabOrder = 0
        OnClick = chkZoomClick
        ExplicitLeft = 4
        ExplicitTop = 4
        ExplicitWidth = 193
      end
      object cbbZoom: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 23
        Width = 67
        Height = 21
        Align = alLeft
        Style = csDropDownList
        TabOrder = 1
        ExplicitLeft = 4
        ExplicitTop = 29
      end
      object lblHotKey: TLabel
        AlignWithMargins = True
        Left = 170
        Top = 3
        Width = 162
        Height = 13
        Align = alTop
        Caption = 'Hotkey'
        ExplicitLeft = 206
        ExplicitTop = 4
        ExplicitWidth = 34
      end
      object pnlZoom: TPanel
        Left = 167
        Top = 20
        Width = 168
        Height = 50
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitLeft = 197
        ExplicitTop = -1
        ExplicitWidth = 197
        ExplicitHeight = 59
        object EditHotKey: THotKey
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 100
          Height = 21
          HotKey = 0
          InvalidKeys = []
          Modifiers = []
          TabOrder = 0
        end
        object btnResetHotKey: TButton
          Left = 109
          Top = 3
          Width = 25
          Height = 19
          Hint = 'Reset'
          Margins.Top = 0
          Align = alCustom
          Caption = '<>'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = btnResetHotKeyClick
        end
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 355
    Width = 335
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 414
    ExplicitWidth = 400
    object btnOk: TButton
      Left = 125
      Top = 7
      Width = 100
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 190
    end
    object btnCancel: TButton
      Left = 231
      Top = 7
      Width = 100
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
      ExplicitLeft = 296
    end
  end
end
