object frmDGAvailablePic: TfrmDGAvailablePic
  Left = 40
  Top = 80
  BorderStyle = bsSizeToolWin
  Caption = 'Images available'
  ClientHeight = 456
  ClientWidth = 572
  Color = clBtnFace
  Constraints.MinHeight = 480
  Constraints.MinWidth = 500
  ParentFont = True
  OldCreateOrder = False
  PopupMode = pmExplicit
  Position = poMainFormCenter
  ShowHint = True
  OnCanResize = FormCanResize
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object spltDesc: TSplitter
    Left = 0
    Top = 220
    Width = 572
    Height = 5
    Cursor = crVSplit
    Align = alBottom
    OnCanResize = spltDescCanResize
  end
  object gbImageParams: TGroupBox
    Left = 0
    Top = 225
    Width = 572
    Height = 231
    Align = alBottom
    Caption = 'Description:'
    Constraints.MinHeight = 80
    TabOrder = 0
    object veImageParams: TValueListEditor
      Left = 2
      Top = 15
      Width = 568
      Height = 214
      Align = alClient
      DefaultColWidth = 160
      TabOrder = 0
      TitleCaptions.Strings = (
        'Parameter'
        'Value')
      OnDblClick = veImageParamsDblClick
      ColWidths = (
        160
        402)
    end
  end
  object Up: TPanel
    Left = 0
    Top = 0
    Width = 572
    Height = 220
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object gbAvailImages: TGroupBox
      Left = 267
      Top = 0
      Width = 305
      Height = 220
      Align = alClient
      Caption = 'Images available'
      Constraints.MinHeight = 220
      TabOrder = 0
      object tvFound: TTreeView
        AlignWithMargins = True
        Left = 5
        Top = 18
        Width = 173
        Height = 197
        Align = alClient
        HideSelection = False
        HotTrack = True
        Indent = 19
        ParentShowHint = False
        ShowHint = True
        SortType = stText
        TabOrder = 0
        OnChange = tvFoundChange
        OnClick = tvFoundClick
        OnDeletion = tvFoundDeletion
        OnMouseDown = tvFoundMouseDown
      end
      object pnlRight: TPanel
        AlignWithMargins = True
        Left = 184
        Top = 18
        Width = 116
        Height = 197
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object chkALLImages: TCheckBox
          Left = 0
          Top = 180
          Width = 116
          Height = 17
          Align = alBottom
          Caption = 'All Images'
          TabOrder = 0
          OnClick = chkALLImagesClick
        end
        object TBXOperationsToolbar: TTBXToolbar
          Left = 0
          Top = 42
          Width = 116
          Height = 24
          Align = alTop
          Images = frmMain.MenusImageList
          ShrinkMode = tbsmWrap
          TabOrder = 1
          object TBXUp: TTBXItem
            ImageIndex = 22
            OnClick = btnUpClick
            Caption = ''
            Hint = 'Up'
          end
          object TBXDown: TTBXItem
            ImageIndex = 21
            OnClick = btnDownClick
            Caption = ''
            Hint = 'Down'
          end
          object TBXCopyTIDs: TTBXItem
            ImageIndex = 28
            OnClick = btnCopyClick
            Caption = ''
            Hint = 'Copy TID'#39's to clipboard'
          end
        end
        object btnRefresh: TButton
          Left = 0
          Top = 0
          Width = 116
          Height = 25
          Hint = 'Refresh'
          Align = alTop
          Caption = 'Refresh'
          TabOrder = 2
          OnClick = btnRefreshClick
        end
        object chkShowOnlyNew: TCheckBox
          Left = 0
          Top = 25
          Width = 116
          Height = 17
          Hint = 'Show only new'
          Align = alTop
          Caption = 'Show only new'
          TabOrder = 3
          OnClick = chkShowOnlyNewClick
        end
        object btnMakePoly: TButton
          Left = 0
          Top = 66
          Width = 116
          Height = 25
          Hint = 'Make Polygon'
          Align = alTop
          Caption = 'Make Polygon'
          TabOrder = 4
          OnClick = btnMakePolyClick
        end
      end
    end
    object pcAvailPicsSearch: TPageControl
      Left = 0
      Top = 0
      Width = 267
      Height = 220
      ActivePage = tsImageServices
      Align = alLeft
      TabOrder = 1
      object tsImageServices: TTabSheet
        Caption = 'Image services'
        object pnlImgSvcFooter: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 173
          Width = 253
          Height = 16
          Align = alBottom
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object chkALLServices: TCheckBox
            Left = 0
            Top = 0
            Width = 112
            Height = 16
            Align = alLeft
            Caption = 'All Services'
            TabOrder = 0
            OnClick = chkALLServicesClick
          end
          object chkLowResolutionToo: TCheckBox
            Left = 125
            Top = 0
            Width = 128
            Height = 16
            Align = alRight
            Caption = 'Low Resolution too'
            TabOrder = 1
            OnClick = chkLowResolutionTooClick
          end
        end
        object pnImgSvcTop: TPanel
          Left = 0
          Top = 0
          Width = 259
          Height = 45
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 1
          object lbZoom: TLabel
            Left = 188
            Top = 4
            Width = 47
            Height = 13
            Hint = '(zoom %)'
            Caption = '(zoom %)'
            ParentShowHint = False
            ShowHint = False
          end
          object cbDGstacks: TComboBox
            AlignWithMargins = True
            Left = 53
            Top = 21
            Width = 204
            Height = 21
            Style = csDropDownList
            ItemHeight = 13
            TabOrder = 0
          end
          object chkBing: TCheckBox
            AlignWithMargins = True
            Left = 140
            Top = 3
            Width = 44
            Height = 17
            Caption = 'Bing'
            TabOrder = 1
          end
          object chkDG: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 23
            Width = 40
            Height = 17
            Caption = 'DG'
            TabOrder = 2
          end
          object chkESRI: TCheckBox
            Left = 92
            Top = 3
            Width = 44
            Height = 17
            Caption = 'ESRI'
            TabOrder = 3
          end
          object chkTerraserver: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 80
            Height = 17
            Caption = 'Terraserver'
            TabOrder = 4
          end
        end
        object pnlImgSvcOthers: TPanel
          Left = 0
          Top = 79
          Width = 259
          Height = 88
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          Padding.Left = 3
          Padding.Top = 3
          TabOrder = 2
          object chkDataDoors: TCheckBox
            Left = 3
            Top = 37
            Width = 256
            Height = 17
            Align = alTop
            Caption = 'DataDoors (www.datadoors.net)'
            TabOrder = 0
          end
          object chkDG2: TCheckBox
            Left = 3
            Top = 3
            Width = 256
            Height = 17
            Align = alTop
            Caption = 'DigitalGlobe (Catalogservice)'
            TabOrder = 1
          end
          object chkGeoFuse: TCheckBox
            Left = 3
            Top = 20
            Width = 256
            Height = 17
            Align = alTop
            Caption = 'GeoFuse.GeoEye (GeoEye+IKONOS)'
            TabOrder = 2
          end
          object chkSearchKosmosnimki: TCheckBox
            Left = 3
            Top = 71
            Width = 256
            Height = 17
            Align = alTop
            Caption = 'Kosmosnimki (search.kosmosnimki.ru)'
            TabOrder = 3
          end
          object chkRoscosmos: TCheckBox
            Left = 3
            Top = 54
            Width = 256
            Height = 17
            Align = alTop
            Caption = 'Roscosmos (geoportal.ntsomz.ru)'
            TabOrder = 4
          end
        end
        object pnlNMCMain: TPanel
          Left = 0
          Top = 45
          Width = 259
          Height = 17
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 3
          object LblNMC: TLabel
            Left = 0
            Top = 0
            Width = 26
            Height = 17
            Align = alLeft
            Caption = 'Nokia'
            Layout = tlCenter
            ExplicitHeight = 13
          end
          object chkMNCasColorOnly: TCheckBox
            Left = 79
            Top = -1
            Width = 153
            Height = 17
            Caption = 'Color only'
            TabOrder = 0
          end
        end
        object pnlNMCZoom: TPanel
          Left = 0
          Top = 62
          Width = 259
          Height = 17
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 4
          object lbNMCZoom: TLabel
            Left = 0
            Top = 0
            Width = 26
            Height = 17
            Align = alLeft
            Caption = 'Zoom'
            ParentShowHint = False
            ShowHint = False
            Layout = tlCenter
            ExplicitHeight = 13
          end
          object chkNMC13: TCheckBox
            Left = 79
            Top = 0
            Width = 36
            Height = 17
            Align = alRight
            Caption = '13'
            TabOrder = 0
          end
          object chkNMC15: TCheckBox
            Left = 115
            Top = 0
            Width = 36
            Height = 17
            Align = alRight
            Caption = '15'
            TabOrder = 1
          end
          object chkNMC16: TCheckBox
            Left = 151
            Top = 0
            Width = 36
            Height = 17
            Align = alRight
            Caption = '16'
            TabOrder = 2
          end
          object chkNMC18: TCheckBox
            Left = 187
            Top = 0
            Width = 36
            Height = 17
            Align = alRight
            Caption = '18'
            TabOrder = 3
          end
          object chkNMC20: TCheckBox
            Left = 223
            Top = 0
            Width = 36
            Height = 17
            Align = alRight
            Caption = '20'
            TabOrder = 4
          end
        end
      end
      object tsDDandRC: TTabSheet
        Caption = 'DD+RC'
        ImageIndex = 1
        ExplicitLeft = 0
        ExplicitTop = 0
        ExplicitWidth = 0
        ExplicitHeight = 0
        object grpbxDatadoors: TGroupBox
          Left = 0
          Top = 0
          Width = 259
          Height = 51
          Align = alTop
          Caption = ' DataDoors '
          TabOrder = 0
          object ChkDD1: TCheckBox
            Left = 6
            Top = 14
            Width = 87
            Height = 17
            Caption = 'WorldView-1'
            TabOrder = 0
          end
          object ChkDD2: TCheckBox
            Left = 6
            Top = 30
            Width = 85
            Height = 17
            Caption = 'WorldView-2'
            TabOrder = 1
          end
          object ChkDD3: TCheckBox
            Left = 108
            Top = 14
            Width = 62
            Height = 17
            Caption = 'QuickBird'
            TabOrder = 2
          end
          object ChkDD4: TCheckBox
            Left = 108
            Top = 30
            Width = 63
            Height = 17
            Caption = 'GeoEye'
            TabOrder = 3
          end
          object ChkDD5: TCheckBox
            Left = 180
            Top = 14
            Width = 58
            Height = 17
            Caption = 'IKONOS'
            TabOrder = 4
          end
        end
        object grpbxRosCosmos: TGroupBox
          Left = 0
          Top = 51
          Width = 259
          Height = 90
          Margins.Top = 0
          Margins.Bottom = 0
          Align = alTop
          Caption = ' RosCosmos '
          TabOrder = 1
          object pnlRoscosmosPassword: TPanel
            AlignWithMargins = True
            Left = 119
            Top = 15
            Width = 135
            Height = 73
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object lblRoscosmosPassword: TLabel
              Left = 0
              Top = 34
              Width = 46
              Height = 13
              Align = alTop
              Caption = 'Password'
              Layout = tlCenter
            end
            object lbllRosCosmosUsername: TLabel
              Left = 0
              Top = 0
              Width = 48
              Height = 13
              Align = alTop
              Caption = 'Username'
              Layout = tlCenter
            end
            object edtRoscosmosPassword: TEdit
              Left = 0
              Top = 47
              Width = 135
              Height = 21
              Align = alTop
              PasswordChar = '*'
              TabOrder = 1
            end
            object edtRosCosmosUsername: TEdit
              Left = 0
              Top = 13
              Width = 135
              Height = 21
              Align = alTop
              TabOrder = 0
            end
          end
          object pnlRosCosmosFlags: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 15
            Width = 108
            Height = 73
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object ChkRC1: TCheckBox
              Left = 0
              Top = 0
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'Resurs-DK'
              TabOrder = 0
            end
            object ChkRC2: TCheckBox
              Left = 0
              Top = 17
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'Canopus-B_PSS'
              TabOrder = 1
            end
            object ChkRC3: TCheckBox
              Left = 0
              Top = 34
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'Meteor-M1'
              TabOrder = 2
            end
            object ChkRC4: TCheckBox
              Left = 0
              Top = 51
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'Canopus-B_MSS'
              TabOrder = 3
            end
          end
        end
      end
      object tsStorage: TTabSheet
        Caption = 'KS+Storage'
        ImageIndex = 2
        object grpbxStorage: TGroupBox
          Left = 0
          Top = 0
          Width = 259
          Height = 62
          Align = alTop
          Caption = ' Storage '
          TabOrder = 0
          object LblOldAfterDays: TLabel
            Left = 8
            Top = 38
            Width = 69
            Height = 13
            Caption = 'Old after days'
          end
          object chkUseStorage: TCheckBox
            AlignWithMargins = True
            Left = 5
            Top = 15
            Width = 249
            Height = 17
            Margins.Top = 0
            Margins.Bottom = 0
            Align = alTop
            Caption = 'Use storage to check new images'
            TabOrder = 0
            OnClick = chkUseStorageClick
          end
          object spnOldAfterDays: TSpinEdit
            Left = 192
            Top = 34
            Width = 57
            Height = 22
            Hint = 'Old after days'
            MaxValue = 99
            MinValue = 1
            TabOrder = 1
            Value = 1
          end
        end
        object grpbxKosmosnimki: TGroupBox
          Left = 0
          Top = 62
          Width = 259
          Height = 130
          Align = alClient
          Caption = ' Kosmosnimki '
          TabOrder = 1
          object pnlKosmosnimki1: TPanel
            AlignWithMargins = True
            Left = 5
            Top = 18
            Width = 52
            Height = 107
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 0
            object chkKS1: TCheckBox
              Left = 0
              Top = 0
              Width = 52
              Height = 17
              Align = alTop
              Caption = 'GE-1'
              TabOrder = 0
            end
            object chkKS2: TCheckBox
              Left = 0
              Top = 17
              Width = 52
              Height = 17
              Align = alTop
              Caption = 'WV01'
              TabOrder = 1
            end
            object chkKS3: TCheckBox
              Left = 0
              Top = 34
              Width = 52
              Height = 17
              Align = alTop
              Caption = 'WV02'
              TabOrder = 2
            end
            object chkKS4: TCheckBox
              Left = 0
              Top = 51
              Width = 52
              Height = 17
              Align = alTop
              Caption = 'QB02'
              TabOrder = 3
            end
          end
          object pnlKosmosnimki2: TPanel
            AlignWithMargins = True
            Left = 63
            Top = 18
            Width = 66
            Height = 107
            Align = alLeft
            BevelOuter = bvNone
            TabOrder = 1
            object chkKS5: TCheckBox
              Left = 0
              Top = 0
              Width = 66
              Height = 17
              Align = alTop
              Caption = 'EROS-B'
              TabOrder = 0
            end
            object chkKS6: TCheckBox
              Left = 0
              Top = 17
              Width = 66
              Height = 17
              Align = alTop
              Caption = 'IK-2'
              TabOrder = 1
            end
            object chkKS7: TCheckBox
              Left = 0
              Top = 34
              Width = 66
              Height = 17
              Align = alTop
              Caption = 'EROS-A1'
              TabOrder = 2
            end
            object chkKS8: TCheckBox
              Left = 0
              Top = 51
              Width = 66
              Height = 17
              Align = alTop
              Caption = 'Pleiades'
              TabOrder = 3
            end
          end
          object pnlKosmosnimkiSpot: TPanel
            Left = 132
            Top = 15
            Width = 125
            Height = 113
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 2
            object chkKS9: TCheckBox
              Left = 0
              Top = 0
              Width = 125
              Height = 17
              Align = alTop
              Caption = 'SPOT 5 - 10m Color'
              TabOrder = 0
            end
            object chkKS10: TCheckBox
              Left = 0
              Top = 17
              Width = 125
              Height = 17
              Align = alTop
              Caption = 'SPOT 5 - 5m BW'
              TabOrder = 1
            end
            object chkKS11: TCheckBox
              Left = 0
              Top = 34
              Width = 125
              Height = 17
              Align = alTop
              Caption = 'SPOT 5 - 5m Color'
              TabOrder = 2
            end
            object chkKS13: TCheckBox
              Left = 0
              Top = 68
              Width = 125
              Height = 17
              Align = alTop
              Caption = 'SPOT 5 - 2.5m Color'
              TabOrder = 3
            end
            object chkKS12: TCheckBox
              Left = 0
              Top = 51
              Width = 125
              Height = 17
              Align = alTop
              Caption = 'SPOT 5 - 2.5m BW'
              TabOrder = 4
            end
          end
        end
      end
    end
  end
end
