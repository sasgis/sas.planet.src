object frmGoTo: TfrmGoTo
  Left = 295
  Top = 179
  AlphaBlendValue = 220
  Caption = 'Go to...'
  ClientHeight = 140
  ClientWidth = 264
  Color = clBtnFace
  Constraints.MinHeight = 167
  Constraints.MinWidth = 272
  ParentFont = True
  OldCreateOrder = False
  Position = poMainFormCenter
  ShowHint = True
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 109
    Width = 264
    Height = 31
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    TabOrder = 1
    object lblZoom: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 30
      Height = 20
      Margins.Bottom = 8
      Align = alLeft
      Alignment = taRightJustify
      Caption = 'Zoom:'
      Layout = tlCenter
    end
    object cbbZoom: TComboBox
      AlignWithMargins = True
      Left = 39
      Top = 3
      Width = 39
      Height = 21
      Align = alLeft
      ItemHeight = 13
      ItemIndex = 0
      TabOrder = 0
      Text = '01'
      Items.Strings = (
        '01'
        '02'
        '03'
        '04'
        '05'
        '06'
        '07'
        '08'
        '09'
        '10'
        '11'
        '12'
        '13'
        '14'
        '15'
        '16'
        '17'
        '18'
        '19'
        '20'
        '21'
        '22'
        '23'
        '24')
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 186
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
    object btnGoTo: TButton
      AlignWithMargins = True
      Left = 105
      Top = 3
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Go to'
      Default = True
      ParentShowHint = False
      ShowHint = True
      TabOrder = 2
      OnClick = btnGoToClick
    end
  end
  object pgcSearchType: TPageControl
    Left = 0
    Top = 0
    Width = 264
    Height = 109
    ActivePage = tsPlaceMarks
    Align = alClient
    TabOrder = 0
    object tsPlaceMarks: TTabSheet
      Caption = 'PlaceMarks'
      object cbbAllMarks: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 250
        Height = 21
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        OnChange = cbbAllMarksDropDown
        OnDropDown = cbbAllMarksDropDown
      end
    end
    object tsSearch: TTabSheet
      Caption = 'Search'
      ImageIndex = 1
      DesignSize = (
        256
        81)
      object cbbSearcherType: TComboBox
        AlignWithMargins = True
        Left = 182
        Top = 3
        Width = 71
        Height = 21
        Style = csDropDownList
        Anchors = [akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
      end
      object cbbGeoCode: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 173
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object tsCoordinates: TTabSheet
      Caption = 'Coordinates'
      ImageIndex = 2
    end
  end
end
