object frExportToCE: TfrExportToCE
  Left = 0
  Top = 0
  Width = 451
  Height = 304
  Align = alClient
  Constraints.MinHeight = 260
  Constraints.MinWidth = 451
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 451
    Height = 277
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlZoom: TPanel
      Left = 376
      Top = 0
      Width = 75
      Height = 277
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 376
      Height = 277
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      DesignSize = (
        376
        277)
      object lblMap: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 93
        Height = 13
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alCustom
        Caption = 'Map/Overlay layer:'
      end
      object lVolSize: TLabel
        Left = 271
        Top = 3
        Width = 99
        Height = 13
        Anchors = [akTop, akRight]
        Caption = 'Max volume size, Mb'
        ExplicitLeft = 300
      end
      object EMapName: TEdit
        Left = 111
        Top = 47
        Width = 259
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
      end
      object EComent: TEdit
        Left = 111
        Top = 74
        Width = 259
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object SaveRecoverInfo: TCheckBox
        Left = 3
        Top = 101
        Width = 367
        Height = 17
        Caption = 'Save recovery information'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CComment: TCheckBox
        Left = 3
        Top = 78
        Width = 102
        Height = 17
        Caption = 'Comment'
        TabOrder = 3
        OnClick = CCommentClick
      end
      object CMapName: TCheckBox
        Left = 3
        Top = 51
        Width = 102
        Height = 17
        Caption = 'Map Name'
        TabOrder = 4
        OnClick = CMapNameClick
      end
      object cbbMaxVolSize: TSpinEdit
        Left = 271
        Top = 19
        Width = 99
        Height = 22
        Anchors = [akTop, akRight]
        MaxValue = 2047
        MinValue = 1
        TabOrder = 6
        Value = 1024
      end
      object pnlMap: TPanel
        Left = 3
        Top = 18
        Width = 262
        Height = 23
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        TabOrder = 5
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object lblTargetFile: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 380
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 427
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object dlgSaveTargetFile: TSaveDialog
    DefaultExt = '.d00'
    Filter = '*.d00|*.d00|All (*.*)|*.*'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 336
    Top = 272
  end
end
