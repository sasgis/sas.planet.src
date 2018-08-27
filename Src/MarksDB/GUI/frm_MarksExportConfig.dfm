object frmMarksExportConfig: TfrmMarksExportConfig
  Left = 0
  Top = 0
  Caption = 'Marks Export Config'
  ClientHeight = 356
  ClientWidth = 549
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spl1: TSplitter
    Left = 161
    Top = 0
    Height = 319
    ExplicitLeft = 136
    ExplicitTop = 128
    ExplicitHeight = 100
  end
  object tvMenu: TTreeView
    Left = 0
    Top = 0
    Width = 161
    Height = 319
    Cursor = crHandPoint
    Align = alLeft
    HideSelection = False
    HotTrack = True
    Indent = 19
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    OnClick = tvMenuClick
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 319
    Width = 549
    Height = 37
    Align = alBottom
    BevelEdges = [beTop]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 2
    object btnApply: TButton
      AlignWithMargins = True
      Left = 387
      Top = 6
      Width = 75
      Height = 23
      Align = alRight
      Caption = 'Apply'
      TabOrder = 0
      OnClick = btnApplyClick
    end
    object btnClose: TButton
      AlignWithMargins = True
      Left = 468
      Top = 6
      Width = 75
      Height = 23
      Align = alRight
      Caption = 'Close'
      TabOrder = 1
      OnClick = btnCloseClick
    end
  end
  object pgcMain: TPageControl
    Left = 164
    Top = 0
    Width = 385
    Height = 319
    ActivePage = tsExportToKml
    Align = alClient
    Style = tsFlatButtons
    TabOrder = 1
    object tsExportToKml: TTabSheet
      Caption = 'tsExportToKml'
      object rgSorting: TRadioGroup
        Left = 0
        Top = 40
        Width = 377
        Height = 81
        Align = alTop
        Caption = ' Sorting '
        ItemIndex = 0
        Items.Strings = (
          'None'
          'By Name (ascend)'
          'By Name (descend)')
        TabOrder = 1
      end
      object chkAbsPathToIcon: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 124
        Width = 371
        Height = 17
        Align = alTop
        Caption = 'Set absolute path or url to the Icons'
        TabOrder = 2
        OnClick = chkAbsPathToIconClick
      end
      object edtAbsPathToIcon: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 147
        Width = 371
        Height = 21
        Align = alTop
        TabOrder = 3
      end
      object grpCoordinates: TGroupBox
        Left = 0
        Top = 0
        Width = 377
        Height = 40
        Align = alTop
        Caption = ' Coordinates '
        TabOrder = 0
        object GridPanel1: TGridPanel
          Left = 2
          Top = 15
          Width = 373
          Height = 23
          Align = alClient
          BevelOuter = bvNone
          ColumnCollection = <
            item
              Value = 66.666666666666660000
            end
            item
              Value = 33.333333333333330000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = chkFixedCoordPrecision
              Row = 0
            end
            item
              Column = 1
              Control = seCoordDigits
              Row = 0
            end>
          RowCollection = <
            item
              Value = 100.000000000000000000
            end>
          TabOrder = 0
          DesignSize = (
            373
            23)
          object chkFixedCoordPrecision: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 242
            Height = 17
            Align = alClient
            Caption = 'Fixed Precision'
            TabOrder = 1
            OnClick = chkFixedCoordPrecisionClick
          end
          object seCoordDigits: TSpinEdit
            Left = 284
            Top = 0
            Width = 89
            Height = 22
            Anchors = [akTop, akRight]
            MaxValue = 12
            MinValue = 4
            TabOrder = 0
            Value = 6
          end
        end
      end
      object rgIconScale: TRadioGroup
        Left = 0
        Top = 171
        Width = 377
        Height = 62
        Align = alTop
        Caption = ' Icon Scale '
        Columns = 2
        Items.Strings = (
          'Absolute'
          'Small'
          'Medium'
          'Large')
        TabOrder = 4
      end
    end
  end
end
