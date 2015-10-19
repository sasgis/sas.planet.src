object frExportAUX: TfrExportAUX
  Left = 0
  Top = 0
  Width = 400
  Height = 90
  Align = alClient
  Constraints.MinHeight = 90
  Constraints.MinWidth = 400
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 400
    Height = 63
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlMapSelect: TPanel
      Left = 0
      Top = 0
      Width = 400
      Height = 43
      Align = alTop
      BevelEdges = [beBottom]
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
      DesignSize = (
        400
        41)
      object pnlZoom: TPanel
        Left = 341
        Top = 0
        Width = 59
        Height = 41
        Align = alRight
        Alignment = taLeftJustify
        BevelEdges = []
        BevelKind = bkTile
        BevelOuter = bvNone
        BorderWidth = 3
        TabOrder = 0
        object lblZoom: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 53
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Zoom:'
        end
        object cbbZoom: TComboBox
          Left = 3
          Top = 17
          Width = 53
          Height = 21
          Align = alBottom
          Style = csDropDownList
          ItemHeight = 13
          TabOrder = 0
        end
      end
      object pnlFrame: TPanel
        Left = 3
        Top = 4
        Width = 341
        Height = 43
        Anchors = [akLeft, akTop, akRight]
        BevelOuter = bvNone
        TabOrder = 1
        object lblMapCaption: TLabel
          Left = 0
          Top = 0
          Width = 341
          Height = 13
          Align = alTop
          Caption = 'Map/Overlay layer:'
          ExplicitWidth = 93
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 400
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
      Height = 21
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 329
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 376
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetFileClick
    end
  end
  object dlgTargetFileSelect: TSaveDialog
    DefaultExt = 'aux'
    Filter = 'AUX |*.aux'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 312
    Top = 88
  end
end
