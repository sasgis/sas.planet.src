object frmMarkSystemConfigEdit: TfrmMarkSystemConfigEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Add Marks Database'
  ClientHeight = 329
  ClientWidth = 402
  Color = clBtnFace
  Constraints.MinHeight = 355
  Constraints.MinWidth = 410
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object grpDBType: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 10
    Width = 382
    Height = 50
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Database type'
    TabOrder = 0
    ExplicitWidth = 451
    object cbbDbType: TComboBox
      AlignWithMargins = True
      Left = 12
      Top = 18
      Width = 358
      Height = 21
      Margins.Left = 10
      Margins.Right = 10
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbDbTypeChange
      ExplicitWidth = 427
    end
  end
  object grpFile: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 66
    Width = 382
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'File name'
    TabOrder = 1
    ExplicitWidth = 451
    object edtFileName: TEdit
      Left = 12
      Top = 16
      Width = 334
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 403
    end
    object btnOpenFile: TButton
      Left = 352
      Top = 16
      Width = 21
      Height = 21
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnOpenFileClick
      ExplicitLeft = 421
    end
  end
  object grpDisplayName: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 120
    Width = 382
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Name (on the list)'
    TabOrder = 2
    ExplicitWidth = 451
    object edtDisplayName: TEdit
      Left = 12
      Top = 16
      Width = 358
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      ExplicitWidth = 427
    end
  end
  object grpOptions: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 174
    Width = 382
    Height = 118
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Options'
    TabOrder = 3
    ExplicitWidth = 389
    object grdpnlOptions: TGridPanel
      Left = 2
      Top = 15
      Width = 378
      Height = 45
      Align = alTop
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
          Control = pnlUser
          Row = 0
        end
        item
          Column = 1
          Control = pnlPass
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      ExplicitWidth = 447
      object pnlUser: TPanel
        Left = 0
        Top = 0
        Width = 189
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 223
        object lblUserName: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 0
          Width = 176
          Height = 13
          Margins.Left = 10
          Margins.Top = 0
          Margins.Bottom = 1
          Align = alTop
          Caption = 'User Name:'
          ExplicitWidth = 56
        end
        object edtUserName: TEdit
          AlignWithMargins = True
          Left = 9
          Top = 17
          Width = 171
          Height = 21
          Margins.Left = 9
          Margins.Right = 9
          Align = alTop
          AutoSize = False
          BevelEdges = [beLeft, beTop, beRight]
          TabOrder = 0
          ExplicitTop = 19
          ExplicitWidth = 205
        end
      end
      object pnlPass: TPanel
        Left = 189
        Top = 0
        Width = 189
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 223
        ExplicitWidth = 224
        object lblPass: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 0
          Width = 178
          Height = 13
          Margins.Left = 8
          Margins.Top = 0
          Margins.Bottom = 1
          Align = alTop
          Caption = 'Password:'
          ExplicitLeft = 6
          ExplicitWidth = 218
        end
        object edtPass: TEdit
          Left = 6
          Top = 19
          Width = 158
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          BevelEdges = [beLeft, beTop, beRight]
          PasswordChar = '*'
          TabOrder = 0
          ExplicitWidth = 192
        end
        object chkShowPass: TCheckBox
          AlignWithMargins = True
          Left = 169
          Top = 17
          Width = 17
          Height = 25
          Hint = 'Show/Hide Password'
          Align = alRight
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = chkShowPassClick
          ExplicitLeft = 204
          ExplicitTop = 46
          ExplicitHeight = 0
        end
      end
    end
    object grdpnlOptions1: TGridPanel
      Left = 2
      Top = 60
      Width = 378
      Height = 45
      Align = alTop
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
          Control = pnlReadOnly
          Row = 0
        end
        item
          Column = 1
          Control = pnlCache
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 1
      ExplicitWidth = 421
      object pnlReadOnly: TPanel
        Left = 0
        Top = 0
        Width = 189
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 48
        ExplicitTop = 24
        ExplicitWidth = 185
        ExplicitHeight = 41
        object chkReadOnly: TCheckBox
          AlignWithMargins = True
          Left = 9
          Top = 25
          Width = 177
          Height = 17
          Margins.Left = 9
          Margins.Top = 5
          Align = alBottom
          Caption = 'Read-Only'
          TabOrder = 0
          ExplicitTop = 24
          ExplicitWidth = 198
        end
      end
      object pnlCache: TPanel
        Left = 189
        Top = 0
        Width = 189
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 256
        ExplicitTop = 8
        ExplicitWidth = 185
        ExplicitHeight = 41
        DesignSize = (
          189
          45)
        object lblCacheSize: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 3
          Width = 178
          Height = 13
          Margins.Left = 8
          Align = alTop
          Caption = 'Cache Size, Mb:'
          ExplicitLeft = 6
          ExplicitTop = 1
          ExplicitWidth = 77
        end
        object seCacheSize: TSpinEdit
          AlignWithMargins = True
          Left = 6
          Top = 20
          Width = 175
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Increment = 10
          MaxValue = 1024
          MinValue = 0
          TabOrder = 0
          Value = 100
          ExplicitWidth = 179
        end
      end
    end
  end
  object btnOk: TButton
    Left = 188
    Top = 298
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnOkClick
    ExplicitLeft = 257
    ExplicitTop = 271
  end
  object btnCancel: TButton
    Left = 294
    Top = 298
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
    ExplicitLeft = 363
    ExplicitTop = 271
  end
  object dlgOpenDb: TOpenDialog
    Left = 16
    Top = 296
  end
end
