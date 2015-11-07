object frmPointProjecting: TfrmPointProjecting
  Left = 0
  Top = 0
  Caption = 'Project New Placemark'
  ClientHeight = 191
  ClientWidth = 384
  Color = clBtnFace
  Constraints.MinHeight = 230
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnHide = FormHide
  PixelsPerInch = 96
  TextHeight = 13
  object pnlDist: TPanel
    Left = 0
    Top = 0
    Width = 384
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 412
    object lblDist: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 378
      Height = 13
      Align = alTop
      Caption = 'Distance, m'
      ExplicitWidth = 56
    end
    object edtDist: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 20
      Width = 378
      Height = 22
      Align = alBottom
      TabOrder = 0
      Text = '1000'
      ExplicitLeft = 70
      ExplicitTop = 3
      ExplicitWidth = 261
      ExplicitHeight = 21
    end
  end
  object pnlAzimuth: TPanel
    Left = 0
    Top = 45
    Width = 384
    Height = 45
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 28
    ExplicitWidth = 412
    object lblAzimuth: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 378
      Height = 38
      Align = alTop
      Caption = 'Azimuth'
      ExplicitWidth = 22
    end
    object edtAzimuth: TEdit
      AlignWithMargins = True
      Left = 3
      Top = 20
      Width = 378
      Height = 22
      Align = alBottom
      TabOrder = 0
      Text = '10'
      ExplicitLeft = 47
      ExplicitTop = 3
      ExplicitWidth = 284
      ExplicitHeight = 21
    end
  end
  object rgSourcePointType: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 93
    Width = 378
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
    ExplicitTop = 59
    ExplicitWidth = 328
  end
  object cbbAllMarks: TComboBox
    AlignWithMargins = True
    Left = 3
    Top = 138
    Width = 378
    Height = 21
    Align = alClient
    Enabled = False
    TabOrder = 3
    OnDropDown = cbbAllMarksDropDown
    ExplicitTop = 104
    ExplicitWidth = 328
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 160
    Width = 384
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    ExplicitTop = 130
    ExplicitWidth = 334
    object btnCreatePoint: TButton
      AlignWithMargins = True
      Left = 253
      Top = 3
      Width = 128
      Height = 25
      Align = alRight
      Caption = 'Project'
      TabOrder = 0
      OnClick = btnCreatePointClick
      ExplicitLeft = 203
    end
  end
end
