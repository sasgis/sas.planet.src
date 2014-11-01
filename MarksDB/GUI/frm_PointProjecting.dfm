object frmPointProjecting: TfrmPointProjecting
  Left = 0
  Top = 0
  Caption = 'Projecting'
  ClientHeight = 178
  ClientWidth = 331
  Color = clBtnFace
  Constraints.MinHeight = 216
  Constraints.MinWidth = 347
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  ShowHint = True
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDist: TPanel
    Left = 0
    Top = 0
    Width = 331
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object lblDist: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 61
      Height = 29
      Align = alLeft
      Caption = 'Distance, km'
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitHeight = 13
    end
    object edtDist: TEdit
      AlignWithMargins = True
      Left = 70
      Top = 3
      Width = 258
      Height = 26
      Align = alClient
      TabOrder = 0
      Text = '10'
      ExplicitLeft = 192
      ExplicitTop = 24
      ExplicitWidth = 121
      ExplicitHeight = 21
    end
  end
  object pnlAzimuth: TPanel
    Left = 0
    Top = 32
    Width = 331
    Height = 32
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 8
    object lblAzimuth: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 29
      Align = alLeft
      Caption = 'Azimuth'
      ExplicitLeft = 4
      ExplicitTop = 4
      ExplicitHeight = 25
    end
    object edtAzimuth: TEdit
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 281
      Height = 26
      Align = alClient
      TabOrder = 0
      Text = '10'
      ExplicitLeft = 71
      ExplicitTop = 4
      ExplicitWidth = 256
      ExplicitHeight = 21
    end
  end
  object rgSourcePointType: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 67
    Width = 325
    Height = 39
    Align = alTop
    Caption = 'Source point type'
    Columns = 2
    ItemIndex = 0
    Items.Strings = (
      'Screen center'
      'Placemark')
    TabOrder = 2
    OnClick = rgSourcePointTypeClick
    ExplicitLeft = 83
    ExplicitTop = 123
  end
  object cbbAllMarks: TComboBox
    AlignWithMargins = True
    Left = 3
    Top = 112
    Width = 325
    Height = 21
    Align = alClient
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    OnDropDown = cbbAllMarksDropDown
    ExplicitTop = 3
    ExplicitWidth = 250
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 141
    Width = 331
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object btnCreatePoint: TButton
      AlignWithMargins = True
      Left = 253
      Top = 3
      Width = 75
      Height = 31
      Align = alRight
      Caption = 'Project'
      TabOrder = 0
      OnClick = btnCreatePointClick
      ExplicitLeft = 200
      ExplicitTop = 16
      ExplicitHeight = 25
    end
  end
end
