object frmTileStorageImporter: TfrmTileStorageImporter
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Open Offline Map'
  ClientHeight = 371
  ClientWidth = 374
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBottomButtons: TPanel
    Left = 0
    Top = 334
    Width = 374
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object btnOk: TButton
      AlignWithMargins = True
      Left = 212
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Caption = 'Ok'
      ModalResult = 1
      TabOrder = 0
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      AlignWithMargins = True
      Left = 293
      Top = 6
      Width = 75
      Height = 25
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      OnClick = btnCancelClick
    end
  end
  object pgcMain: TPageControl
    Left = 0
    Top = 0
    Width = 374
    Height = 334
    ActivePage = tsParams
    Align = alClient
    TabOrder = 0
    object tsParams: TTabSheet
      Caption = 'Params'
      object lblMapName: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 360
        Height = 13
        Align = alTop
        Caption = 'Map Name:'
      end
      object lblParentSubMenu: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 49
        Width = 360
        Height = 13
        Align = alTop
        Caption = 'Parent Sub Menu:'
      end
      object lblExtension: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 95
        Width = 360
        Height = 13
        Align = alTop
        Caption = 'Extension:'
      end
      object lblProjection: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 141
        Width = 360
        Height = 13
        Align = alTop
        Caption = 'Projection:'
      end
      object lblCacheType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 187
        Width = 360
        Height = 13
        Align = alTop
        Caption = 'Cache Type:'
      end
      object edtMapName: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 360
        Height = 21
        Align = alTop
        TabOrder = 0
      end
      object edtParentSubMenu: TEdit
        AlignWithMargins = True
        Left = 3
        Top = 68
        Width = 360
        Height = 21
        Align = alTop
        TabOrder = 1
      end
      object cbbExt: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 114
        Width = 360
        Height = 21
        Align = alTop
        Style = csDropDownList
        TabOrder = 2
      end
      object cbbProj: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 160
        Width = 360
        Height = 21
        Align = alTop
        Style = csDropDownList
        TabOrder = 3
      end
      object cbbCacheType: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 206
        Width = 360
        Height = 21
        Align = alTop
        Style = csDropDownList
        TabOrder = 4
      end
    end
    object tsMetadata: TTabSheet
      Caption = 'Metadata'
      ImageIndex = 1
      object lvMetadata: TListView
        Left = 0
        Top = 0
        Width = 366
        Height = 306
        Align = alClient
        Columns = <
          item
            Caption = 'Name'
            Width = 150
          end
          item
            AutoSize = True
            Caption = 'Value'
          end>
        FlatScrollBars = True
        GridLines = True
        ReadOnly = True
        RowSelect = True
        TabOrder = 0
        ViewStyle = vsReport
      end
    end
  end
end
