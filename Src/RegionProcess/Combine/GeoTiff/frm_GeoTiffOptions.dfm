object frmGeoTiffOptions: TfrmGeoTiffOptions
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'GeoTiff Settings'
  ClientHeight = 334
  ClientWidth = 380
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object btnApply: TButton
    Left = 188
    Top = 301
    Width = 85
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Apply'
    Default = True
    TabOrder = 0
    OnClick = btnApplyClick
  end
  object btnCancel: TButton
    Left = 279
    Top = 301
    Width = 85
    Height = 25
    Align = alCustom
    Anchors = [akRight, akBottom]
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = btnCancelClick
  end
  object pnlFormat: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 374
    Height = 23
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 2
    object lblFormat: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 3
      Width = 60
      Height = 15
      Caption = 'File format:'
    end
    object cbbFormat: TComboBox
      Left = 220
      Top = 0
      Width = 141
      Height = 23
      Align = alCustom
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'Auto'
      Items.Strings = (
        'Auto'
        'Classic TIFF'
        'Big TIFF')
    end
  end
  object pnlCompression: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 32
    Width = 374
    Height = 23
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 3
    object lblCompression: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 3
      Width = 73
      Height = 15
      Caption = 'Compression:'
    end
    object cbbCompression: TComboBox
      Left = 220
      Top = 0
      Width = 141
      Height = 23
      Align = alCustom
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 2
      TabOrder = 0
      Text = 'LZW'
      OnChange = cbbCompressionChange
      Items.Strings = (
        'None'
        'ZIP (Deflate)'
        'LZW'
        'JPEG')
    end
  end
  object pnlCompressionLevel: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 61
    Width = 374
    Height = 24
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 4
    object lblCompressionLevel: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 3
      Width = 30
      Height = 15
      Caption = 'Level:'
    end
    object seCompressionLevel: TSpinEdit
      Left = 220
      Top = 0
      Width = 141
      Height = 24
      Align = alCustom
      Anchors = [akTop, akRight]
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
    end
  end
  object pnlOverview: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 120
    Width = 374
    Height = 97
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 5
    object grpOverview: TGroupBox
      Left = 0
      Top = 0
      Width = 374
      Height = 97
      Align = alClient
      Caption = 'Overview levels'
      TabOrder = 0
      object lblCustomOverview: TLabel
        AlignWithMargins = True
        Left = 8
        Top = 51
        Width = 361
        Height = 15
        Margins.Left = 6
        Margins.Top = 6
        Align = alBottom
        Caption = 'Custom (separated by space):'
      end
      object chklstOverview: TCheckListBox
        AlignWithMargins = True
        Left = 5
        Top = 23
        Width = 364
        Height = 15
        Margins.Top = 6
        Align = alTop
        BevelInner = bvNone
        BevelOuter = bvNone
        BorderStyle = bsNone
        Color = clBtnFace
        Columns = 6
        IntegralHeight = True
        ItemHeight = 15
        Items.Strings = (
          '2'
          '4'
          '8'
          '16'
          '32'
          '64')
        TabOrder = 0
      end
      object edtOverview: TEdit
        AlignWithMargins = True
        Left = 5
        Top = 69
        Width = 364
        Height = 23
        Align = alBottom
        TabOrder = 1
        OnChange = edtOverviewChange
      end
    end
  end
  object chkCopyRawJpeg: TCheckBox
    AlignWithMargins = True
    Left = 8
    Top = 226
    Width = 369
    Height = 17
    Margins.Left = 8
    Margins.Top = 6
    Align = alTop
    Caption = 'Copy JPEG tiles directly if possible'
    TabOrder = 6
  end
  object pnlColorspace: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 91
    Width = 374
    Height = 23
    Align = alTop
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 7
    object lblColorspace: TLabel
      AlignWithMargins = True
      Left = 5
      Top = 3
      Width = 65
      Height = 15
      Caption = 'Color space:'
    end
    object cbbColorspace: TComboBox
      Left = 220
      Top = 0
      Width = 141
      Height = 23
      Align = alCustom
      Style = csDropDownList
      Anchors = [akTop, akRight]
      ItemIndex = 0
      TabOrder = 0
      Text = 'RGB'
      OnChange = cbbCompressionChange
      Items.Strings = (
        'RGB'
        'YCbCr')
    end
  end
end
