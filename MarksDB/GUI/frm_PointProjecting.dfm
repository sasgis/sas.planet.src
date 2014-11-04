object frmPointProjecting: TfrmPointProjecting
  Left = 0
  Top = 0
  Caption = 'Projecting'
  ClientHeight = 161
  ClientWidth = 334
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 350
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
    Width = 334
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 331
    object lblDist: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 61
      Height = 22
      Align = alLeft
      Caption = 'Distance, km'
      ExplicitHeight = 13
    end
    object edtDist: TEdit
      AlignWithMargins = True
      Left = 70
      Top = 3
      Width = 261
      Height = 22
      Align = alClient
      TabOrder = 0
      Text = '10'
      ExplicitWidth = 258
      ExplicitHeight = 26
    end
  end
  object pnlAzimuth: TPanel
    Left = 0
    Top = 28
    Width = 334
    Height = 28
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 331
    object lblAzimuth: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 38
      Height = 22
      Align = alLeft
      Caption = 'Azimuth'
      ExplicitHeight = 13
    end
    object edtAzimuth: TEdit
      AlignWithMargins = True
      Left = 47
      Top = 3
      Width = 284
      Height = 22
      Align = alClient
      TabOrder = 0
      Text = '10'
      ExplicitWidth = 281
    end
  end
  object rgSourcePointType: TRadioGroup
    AlignWithMargins = True
    Left = 3
    Top = 59
    Width = 328
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
    ExplicitLeft = -2
    ExplicitWidth = 325
  end
  object cbbAllMarks: TComboBox
    AlignWithMargins = True
    Left = 3
    Top = 104
    Width = 328
    Height = 21
    Align = alClient
    Enabled = False
    ItemHeight = 13
    TabOrder = 3
    OnDropDown = cbbAllMarksDropDown
    ExplicitTop = 112
    ExplicitWidth = 325
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 130
    Width = 334
    Height = 31
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 4
    object btnCreatePoint: TButton
      AlignWithMargins = True
      Left = 203
      Top = 3
      Width = 128
      Height = 25
      Align = alRight
      Caption = 'Project'
      TabOrder = 0
      OnClick = btnCreatePointClick
      ExplicitLeft = 200
      ExplicitHeight = 24
    end
  end
end
