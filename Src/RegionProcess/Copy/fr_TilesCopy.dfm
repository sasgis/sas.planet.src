object frTilesCopy: TfrTilesCopy
  Left = 0
  Top = 0
  Width = 568
  Height = 348
  Align = alClient
  Constraints.MinHeight = 300
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  ExplicitWidth = 480
  ExplicitHeight = 304
  object pnlCenter: TPanel
    Left = 0
    Top = 49
    Width = 568
    Height = 299
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 480
    ExplicitHeight = 255
    object pnlZoom: TPanel
      Left = 488
      Top = 0
      Width = 80
      Height = 299
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
      ExplicitLeft = 400
      ExplicitHeight = 255
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 488
      Height = 299
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      ExplicitWidth = 400
      ExplicitHeight = 255
      object lblNamesType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 482
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Output format'
        ExplicitWidth = 394
      end
      object pnlCacheTypes: TPanel
        Left = 3
        Top = 20
        Width = 482
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 394
      end
      object chkReplaseTarget: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 41
        Width = 482
        Height = 17
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Overwrite if equal'
        TabOrder = 1
        ExplicitWidth = 394
      end
      object pnSetTargetVersionOptions: TPanel
        Left = 3
        Top = 61
        Width = 482
        Height = 44
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitWidth = 394
        ExplicitHeight = 47
        object chkSetTargetVersionTo: TCheckBox
          Left = 0
          Top = 0
          Width = 482
          Height = 17
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Set Version to'
          Enabled = False
          TabOrder = 0
          OnClick = chkSetTargetVersionToClick
          ExplicitWidth = 394
        end
        object edSetTargetVersionValue: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 20
          Width = 476
          Height = 21
          Align = alTop
          Enabled = False
          TabOrder = 1
          ExplicitWidth = 388
        end
      end
      object pcSource: TPageControl
        Left = 3
        Top = 105
        Width = 482
        Height = 191
        ActivePage = tsOverlay
        Align = alClient
        TabOrder = 3
        ExplicitTop = 108
        ExplicitWidth = 394
        ExplicitHeight = 144
        object tsDirectCopy: TTabSheet
          Caption = 'Direct Copy'
          object chklstMaps: TCheckListBox
            Left = 0
            Top = 37
            Width = 474
            Height = 106
            Align = alClient
            ItemHeight = 13
            TabOrder = 0
          end
          object chkAllMaps: TCheckBox
            AlignWithMargins = True
            Left = 0
            Top = 146
            Width = 474
            Height = 17
            Margins.Left = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alBottom
            Caption = 'All'
            TabOrder = 1
            OnClick = chkAllMapsClick
          end
          object chkDeleteSource: TCheckBox
            AlignWithMargins = True
            Left = 0
            Top = 20
            Width = 474
            Height = 17
            Margins.Left = 0
            Margins.Right = 0
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Move'
            TabOrder = 2
          end
          object chkPlaceInNameSubFolder: TCheckBox
            Left = 0
            Top = 0
            Width = 474
            Height = 17
            Align = alTop
            Caption = 'Make subfolder with map path name'
            Checked = True
            State = cbChecked
            TabOrder = 3
          end
        end
        object tsOverlay: TTabSheet
          Caption = 'Modify'
          ImageIndex = 1
          ExplicitWidth = 386
          ExplicitHeight = 116
          object lblOverlay: TLabel
            Left = 0
            Top = 42
            Width = 474
            Height = 13
            Align = alTop
            Caption = 'Overlay layer:'
            ExplicitWidth = 69
          end
          object lblMap: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 474
            Height = 16
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Align = alTop
            AutoSize = False
            Caption = 'Map:'
            ExplicitWidth = 386
          end
          object pnlOverlay: TPanel
            Left = 0
            Top = 55
            Width = 474
            Height = 23
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            ExplicitWidth = 386
          end
          object pnlImageFormat: TPanel
            Left = 0
            Top = 95
            Width = 474
            Height = 48
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            ExplicitTop = 78
            ExplicitWidth = 386
            object lblJpgQulity: TLabel
              Left = 156
              Top = 6
              Width = 90
              Height = 13
              Caption = 'Quality (for JPEG):'
              Layout = tlCenter
            end
            object lblCompression: TLabel
              Left = 312
              Top = 6
              Width = 113
              Height = 13
              Caption = 'Compression (for PNG):'
              Layout = tlCenter
            end
            object lblImageFormat: TLabel
              Left = 0
              Top = 6
              Width = 69
              Height = 13
              Caption = 'Image format:'
              Layout = tlCenter
            end
            object seJpgQuality: TSpinEdit
              Left = 156
              Top = 22
              Width = 150
              Height = 22
              MaxValue = 100
              MinValue = 1
              TabOrder = 0
              Value = 75
            end
            object seCompression: TSpinEdit
              Left = 312
              Top = 22
              Width = 150
              Height = 22
              MaxValue = 9
              MinValue = 0
              TabOrder = 1
              Value = 2
            end
            object cbbImageFormat: TComboBox
              Left = 0
              Top = 22
              Width = 150
              Height = 21
              ItemIndex = 0
              TabOrder = 2
              Text = 'Auto'
              OnChange = cbbImageFormatChange
              Items.Strings = (
                'Auto'
                'JPEG'
                'BMP'
                'GIF'
                'PNG (Indexed Colors)'
                'PNG (TrueColor)'
                'PNG (TrueColor + Alpha)')
            end
          end
          object pnlMap: TPanel
            Left = 0
            Top = 19
            Width = 474
            Height = 23
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
            ExplicitWidth = 386
          end
          object chkAddVisibleLayers: TCheckBox
            Left = 0
            Top = 78
            Width = 474
            Height = 17
            Align = alTop
            Caption = 'Add visible Layers'
            TabOrder = 3
            OnClick = chkAddVisibleLayersClick
            ExplicitLeft = 6
            ExplicitTop = 61
            ExplicitWidth = 461
          end
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 22
    Width = 568
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    ExplicitWidth = 480
    object lblTargetPath: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 41
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
      ExplicitHeight = 13
    end
    object edtTargetPath: TEdit
      Left = 47
      Top = 3
      Width = 497
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitWidth = 409
    end
    object btnSelectTargetPath: TButton
      Left = 544
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
      ExplicitLeft = 456
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 568
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = 'Copy tiles from selection to folder'
    TabOrder = 2
    ExplicitWidth = 480
  end
end
