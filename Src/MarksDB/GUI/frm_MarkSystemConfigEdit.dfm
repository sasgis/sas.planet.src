object frmMarkSystemConfigEdit: TfrmMarkSystemConfigEdit
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  Caption = 'Add Marks Database'
  ClientHeight = 354
  ClientWidth = 394
  Color = clBtnFace
  Constraints.MinHeight = 360
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
    Width = 374
    Height = 50
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Database type'
    TabOrder = 0
    object cbbDbType: TComboBox
      AlignWithMargins = True
      Left = 12
      Top = 18
      Width = 350
      Height = 21
      Margins.Left = 10
      Margins.Right = 10
      Align = alClient
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbDbTypeChange
    end
  end
  object grpFile: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 66
    Width = 374
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'File name'
    TabOrder = 1
    object edtFileName: TEdit
      Left = 12
      Top = 16
      Width = 326
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object btnOpenFile: TButton
      Left = 344
      Top = 16
      Width = 21
      Height = 21
      Align = alCustom
      Anchors = [akTop, akRight]
      Caption = '...'
      TabOrder = 1
      OnClick = btnOpenFileClick
    end
  end
  object grpDisplayName: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 120
    Width = 374
    Height = 48
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Name (on the list)'
    TabOrder = 2
    object edtDisplayName: TEdit
      Left = 12
      Top = 16
      Width = 350
      Height = 21
      Align = alCustom
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
  end
  object grpOptions: TGroupBox
    AlignWithMargins = True
    Left = 10
    Top = 174
    Width = 374
    Height = 139
    Margins.Left = 10
    Margins.Right = 10
    Align = alTop
    Caption = 'Options'
    TabOrder = 3
    object grdpnlOptions: TGridPanel
      Left = 2
      Top = 15
      Width = 370
      Height = 42
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
      object pnlUser: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 42
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lblUserName: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 0
          Width = 172
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
          Width = 167
          Height = 21
          Margins.Left = 9
          Margins.Right = 9
          Align = alTop
          AutoSize = False
          BevelEdges = [beLeft, beTop, beRight]
          TabOrder = 0
        end
      end
      object pnlPass: TPanel
        Left = 185
        Top = 0
        Width = 185
        Height = 42
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object lblPass: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 0
          Width = 174
          Height = 13
          Margins.Left = 8
          Margins.Top = 0
          Margins.Bottom = 1
          Align = alTop
          Caption = 'Password:'
          ExplicitWidth = 50
        end
        object edtPass: TEdit
          Left = 6
          Top = 17
          Width = 154
          Height = 21
          Align = alCustom
          Anchors = [akLeft, akTop, akRight]
          BevelEdges = [beLeft, beTop, beRight]
          PasswordChar = '*'
          TabOrder = 1
        end
        object chkShowPass: TCheckBox
          AlignWithMargins = True
          Left = 165
          Top = 14
          Width = 17
          Height = 25
          Hint = 'Show/Hide Password'
          Margins.Top = 0
          Align = alRight
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          OnClick = chkShowPassClick
        end
      end
    end
    object grdpnlOptions1: TGridPanel
      Left = 2
      Top = 57
      Width = 370
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
          Control = pnlForcedSchemaName
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
        end
        item
          SizeStyle = ssAuto
        end>
      TabOrder = 1
      object pnlForcedSchemaName: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object lblForcedSchemaName: TLabel
          AlignWithMargins = True
          Left = 10
          Top = 3
          Width = 172
          Height = 13
          Margins.Left = 10
          Align = alTop
          Caption = 'Forced Schema Name:'
          ExplicitTop = 0
          ExplicitWidth = 107
        end
        object edtForcedSchemaName: TEdit
          AlignWithMargins = True
          Left = 9
          Top = 21
          Width = 167
          Height = 21
          Margins.Left = 9
          Margins.Top = 2
          Margins.Right = 9
          Align = alTop
          AutoSize = False
          BevelEdges = [beLeft, beTop, beRight]
          TabOrder = 0
          ExplicitTop = 16
        end
      end
      object pnlCache: TPanel
        Left = 185
        Top = 0
        Width = 185
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        DesignSize = (
          185
          45)
        object lblCacheSize: TLabel
          AlignWithMargins = True
          Left = 8
          Top = 3
          Width = 174
          Height = 13
          Margins.Left = 8
          Align = alTop
          Caption = 'Cache Size, Mb:'
          ExplicitWidth = 77
        end
        object seCacheSize: TSpinEdit
          AlignWithMargins = True
          Left = 6
          Top = 20
          Width = 171
          Height = 22
          Anchors = [akLeft, akTop, akRight]
          Increment = 10
          MaxValue = 1024
          MinValue = 0
          TabOrder = 0
          Value = 100
        end
      end
    end
    object grdpnlOptions2: TGridPanel
      Left = 2
      Top = 102
      Width = 370
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
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 2
      object pnlReadOnly: TPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 45
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object chkReadOnly: TCheckBox
          AlignWithMargins = True
          Left = 9
          Top = 5
          Width = 173
          Height = 17
          Margins.Left = 9
          Margins.Top = 5
          Align = alTop
          Caption = 'Read-Only'
          TabOrder = 0
          ExplicitLeft = 6
          ExplicitTop = 8
        end
      end
    end
  end
  object btnOk: TButton
    Left = 180
    Top = 323
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    TabOrder = 4
    OnClick = btnOkClick
    ExplicitTop = 457
  end
  object btnCancel: TButton
    Left = 286
    Top = 323
    Width = 100
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
    ExplicitTop = 457
  end
  object dlgOpenDb: TOpenDialog
    Left = 296
    Top = 160
  end
end
