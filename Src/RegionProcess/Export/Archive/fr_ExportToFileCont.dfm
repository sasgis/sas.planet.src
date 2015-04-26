object frExportToFileCont: TfrExportToFileCont
  Left = 0
  Top = 0
  Width = 480
  Height = 120
  Align = alClient
  Constraints.MinHeight = 120
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  Visible = False
  object pnlCenter: TPanel
    Left = 0
    Top = 27
    Width = 480
    Height = 93
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlZoom: TPanel
      Left = 405
      Top = 0
      Width = 75
      Height = 93
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 405
      Height = 93
      Align = alClient
      AutoSize = True
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object lblNamesType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 44
        Width = 399
        Height = 13
        Margins.Left = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Export selection to format:'
      end
      object cbbNamesType: TComboBox
        Left = 3
        Top = 57
        Width = 399
        Height = 21
        Align = alTop
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 1
        TabOrder = 0
        Text = 'SAS.Planet'
        Items.Strings = (
          'GoogleMV'
          'SAS.Planet'
          'ES1.95'
          'GMT (GlobalMapper >=10.02)')
      end
      object pnlFrame: TPanel
        Left = 3
        Top = 3
        Width = 399
        Height = 38
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object lblMap: TLabel
          AlignWithMargins = True
          Left = 0
          Top = 0
          Width = 399
          Height = 13
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Map'
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 480
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
      Alignment = taRightJustify
      Caption = 'Save to:'
      Layout = tlCenter
    end
    object edtTargetFile: TEdit
      Left = 47
      Top = 3
      Width = 409
      Height = 21
      Align = alClient
      TabOrder = 0
    end
    object btnSelectTargetFile: TButton
      Left = 456
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
    DefaultExt = 'zip'
    Filter = 'Zip |*.zip'
    Options = [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing]
    Left = 104
    Top = 104
  end
end
