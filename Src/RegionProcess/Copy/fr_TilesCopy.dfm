object frTilesCopy: TfrTilesCopy
  Left = 0
  Top = 0
  Width = 480
  Height = 304
  Align = alClient
  Constraints.MinHeight = 300
  Constraints.MinWidth = 480
  ParentShowHint = False
  ShowHint = True
  TabOrder = 0
  object pnlCenter: TPanel
    Left = 0
    Top = 49
    Width = 480
    Height = 255
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object pnlZoom: TPanel
      Left = 400
      Top = 0
      Width = 80
      Height = 255
      Align = alRight
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 0
    end
    object pnlMain: TPanel
      Left = 0
      Top = 0
      Width = 400
      Height = 255
      Align = alClient
      BevelOuter = bvNone
      BorderWidth = 3
      TabOrder = 1
      object lblNamesType: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 394
        Height = 14
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        AutoSize = False
        Caption = 'Output format'
      end
      object pnlCacheTypes: TPanel
        Left = 3
        Top = 20
        Width = 394
        Height = 21
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
      end
      object chkReplaseTarget: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 41
        Width = 394
        Height = 17
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Align = alTop
        Caption = 'Overwrite if equal'
        TabOrder = 1
      end
      object pnSetTargetVersionOptions: TPanel
        Left = 3
        Top = 61
        Width = 394
        Height = 47
        Margins.Left = 0
        Margins.Top = 0
        Margins.Right = 0
        Margins.Bottom = 0
        Align = alTop
        AutoSize = True
        BevelOuter = bvNone
        TabOrder = 2
        object chkSetTargetVersionTo: TCheckBox
          Left = 0
          Top = 0
          Width = 394
          Height = 17
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 0
          Align = alTop
          Caption = 'Set Version to'
          Enabled = False
          TabOrder = 0
          OnClick = chkSetTargetVersionToClick
        end
        object edSetTargetVersionValue: TEdit
          AlignWithMargins = True
          Left = 3
          Top = 20
          Width = 388
          Height = 24
          Align = alTop
          Enabled = False
          TabOrder = 1
        end
      end
      object pcSource: TPageControl
        Left = 3
        Top = 108
        Width = 394
        Height = 144
        ActivePage = tsOverlay
        Align = alClient
        TabOrder = 3
        object tsDirectCopy: TTabSheet
          Caption = 'Direct Copy'
          object chklstMaps: TCheckListBox
            Left = 0
            Top = 37
            Width = 386
            Height = 56
            Align = alClient
            TabOrder = 0
          end
          object chkAllMaps: TCheckBox
            AlignWithMargins = True
            Left = 0
            Top = 96
            Width = 386
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
            Width = 386
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
            Width = 386
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
          object lblOverlay: TLabel
            Left = 0
            Top = 42
            Width = 386
            Height = 16
            Align = alTop
            Caption = 'Overlay layer:'
            ExplicitWidth = 80
          end
          object lblMap: TLabel
            AlignWithMargins = True
            Left = 0
            Top = 0
            Width = 386
            Height = 16
            Margins.Left = 0
            Margins.Top = 0
            Margins.Right = 0
            Align = alTop
            AutoSize = False
            Caption = 'Map'
          end
          object pnlOverlay: TPanel
            Left = 0
            Top = 58
            Width = 386
            Height = 23
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
          end
          object pnlImageFormat: TPanel
            Left = 0
            Top = 81
            Width = 386
            Height = 48
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 1
            object lblJpgQulity: TLabel
              Left = 156
              Top = 6
              Width = 105
              Height = 16
              Caption = 'Quality (for JPEG):'
              Layout = tlCenter
            end
            object lblCompression: TLabel
              Left = 312
              Top = 6
              Width = 136
              Height = 16
              Caption = 'Compression (for PNG):'
              Layout = tlCenter
            end
            object lblImageFormat: TLabel
              Left = 0
              Top = 6
              Width = 83
              Height = 16
              Caption = 'Image format:'
              Layout = tlCenter
            end
            object seJpgQuality: TSpinEdit
              Left = 156
              Top = 22
              Width = 150
              Height = 26
              MaxValue = 100
              MinValue = 1
              TabOrder = 0
              Value = 75
            end
            object seCompression: TSpinEdit
              Left = 312
              Top = 22
              Width = 150
              Height = 26
              MaxValue = 9
              MinValue = 0
              TabOrder = 1
              Value = 2
            end
            object cbbImageFormat: TComboBox
              Left = 0
              Top = 22
              Width = 150
              Height = 24
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
            Width = 386
            Height = 23
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 2
          end
        end
      end
    end
  end
  object pnlTop: TPanel
    Left = 0
    Top = 22
    Width = 480
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    BorderWidth = 3
    TabOrder = 1
    object lblTargetPath: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 48
      Height = 18
      Margins.Left = 0
      Margins.Top = 0
      Align = alLeft
      Caption = 'Save to:'
      Layout = tlCenter
      ExplicitHeight = 16
    end
    object edtTargetPath: TEdit
      Left = 54
      Top = 3
      Width = 402
      Height = 21
      Align = alClient
      TabOrder = 0
      ExplicitHeight = 24
    end
    object btnSelectTargetPath: TButton
      Left = 456
      Top = 3
      Width = 21
      Height = 21
      Align = alRight
      Caption = '...'
      TabOrder = 1
      OnClick = btnSelectTargetPathClick
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 480
    Height = 22
    Align = alTop
    Alignment = taLeftJustify
    BevelEdges = [beBottom]
    BevelKind = bkTile
    BevelOuter = bvNone
    BorderWidth = 3
    Caption = 'Copy tiles from selection to folder'
    TabOrder = 2
  end
end
