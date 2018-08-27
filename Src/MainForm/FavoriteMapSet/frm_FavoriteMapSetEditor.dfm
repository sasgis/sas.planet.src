object frmFavoriteMapSetEditor: TfrmFavoriteMapSetEditor
  Left = 0
  Top = 0
  BorderStyle = bsSizeToolWin
  ClientHeight = 494
  ClientWidth = 403
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
    Width = 403
    Height = 456
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 335
    ExplicitHeight = 355
    object lblName: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 397
      Height = 13
      Align = alTop
      Caption = 'Name:'
      ExplicitWidth = 31
    end
    object chklstMaps: TCheckListBox
      AlignWithMargins = True
      Left = 3
      Top = 124
      Width = 397
      Height = 141
      OnClickCheck = chklstMapsClickCheck
      Align = alClient
      ItemHeight = 13
      TabOrder = 4
      ExplicitWidth = 329
      ExplicitHeight = 140
    end
    object edtName: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 22
      Width = 397
      Height = 21
      Align = alTop
      TabOrder = 0
      ExplicitWidth = 329
    end
    object pnlMap: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 72
      Width = 397
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitWidth = 329
    end
    object chkMergeLayers: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 291
      Width = 397
      Height = 17
      Align = alBottom
      Caption = 'Hide non-selected layers'
      Checked = True
      State = cbChecked
      TabOrder = 6
      ExplicitTop = 290
      ExplicitWidth = 329
    end
    object pnlLayers: TPanel
      Left = 0
      Top = 268
      Width = 403
      Height = 20
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 5
      ExplicitTop = 267
      ExplicitWidth = 335
      object lblLayersCount: TLabel
        AlignWithMargins = True
        Left = 397
        Top = 3
        Width = 3
        Height = 14
        Align = alRight
        BiDiMode = bdLeftToRight
        ParentBiDiMode = False
        ExplicitLeft = 329
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
      Width = 397
      Height = 17
      Align = alTop
      Caption = 'Layers:'
      Checked = True
      State = cbChecked
      TabOrder = 3
      OnClick = chkLayersClick
      ExplicitWidth = 329
    end
    object chkMap: TCheckBox
      AlignWithMargins = True
      Left = 3
      Top = 49
      Width = 397
      Height = 17
      Align = alTop
      Caption = 'Map:'
      Checked = True
      State = cbChecked
      TabOrder = 1
      OnClick = chkMapClick
      ExplicitWidth = 329
    end
    object grdpnlHotKey1: TGridPanel
      Left = 0
      Top = 311
      Width = 403
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
      ExplicitTop = 310
      ExplicitWidth = 335
      object chkZoom: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 195
        Height = 17
        Align = alTop
        Anchors = []
        Caption = 'Zoom'
        TabOrder = 0
        OnClick = chkZoomClick
        ExplicitWidth = 161
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
      end
      object lblHotKey: TLabel
        AlignWithMargins = True
        Left = 204
        Top = 3
        Width = 196
        Height = 13
        Align = alTop
        Caption = 'Hotkey'
        ExplicitLeft = 170
        ExplicitWidth = 34
      end
      object pnlZoom: TPanel
        Left = 201
        Top = 20
        Width = 202
        Height = 50
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitLeft = 167
        ExplicitWidth = 168
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
    object pnlCoords: TPanel
      Left = 0
      Top = 356
      Width = 403
      Height = 100
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 8
      object chkCoords: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 397
        Height = 17
        Align = alTop
        Caption = 'Coordinates'
        TabOrder = 0
        OnClick = chkCoordsClick
        ExplicitLeft = 40
        ExplicitTop = 16
        ExplicitWidth = 97
      end
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 456
    Width = 403
    Height = 38
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 355
    ExplicitWidth = 335
    object btnOk: TButton
      Left = 193
      Top = 7
      Width = 100
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Ok'
      TabOrder = 0
      OnClick = btnOkClick
      ExplicitLeft = 125
    end
    object btnCancel: TButton
      Left = 299
      Top = 7
      Width = 100
      Height = 25
      Align = alCustom
      Anchors = [akRight, akBottom]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = btnCancelClick
      ExplicitLeft = 231
    end
  end
end
